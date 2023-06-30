install.packages("tidyverse")
install.packages("tidymodels")

install.packages("tictoc")
install.packages("doParallel")
install.packages("furrr")
install.packages("xgboost")
# 대응 패키지
install.packages("ranger")
install.packages("glmnet")
# 데이터 셋
install.packages("palmerpenguins")
# 테마
install.packages("hrbrthemes")

# 패키지 불러오기
library(tidyverse)
library(tidymodels)
library(tictoc)
library(doParallel)
library(furrr)
library(xgboost)
library(palmerpenguins)
library(ranger)
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()


# 환경 설정
theme_set(hrbrthemes::theme_ipsum_rc()) # 배경 흰색 설정
plan(multicore, workers = availableCores()) # 멀티코어 사용
#plan(multiprocess, workers = availableCores())
cores <- parallel::detectCores(logical = FALSE) 
cl <- makePSOCKcluster(cores)
registerDoParallel(cores = cl)
set.seed(42) # random결과를 seed값 42 기준으로 가져옴.시드값 가져오면 아무리 데이터 셔플링해도 동일한 셔플링 진행

# 데이터 불러오기 및 전처리
penguins_data <- palmerpenguins::penguins # (패키지명::안에 있는 내용) 가져와줘임
penguins_data

# NA값 확인 및 처리
glimpse(penguins_data)
t(map_df(penguins_data, ~sum(is.na(.)))) # NA값 어디있는지 요약 확인

penguins_df <-
  penguins_data %>% # 팽귄 데이터에서
  filter(!is.na(sex)) %>% # 필터 적용해 sex의 NA값이 아닌거를 가져와서
  select(-year, -island) # 연도와, 사는곳을 제외로 선택
head(penguins_df) # 보여줘

penguins_split <-
  rsample::initial_split( # 나눌거야(샘플링할거야), 초기 데이터를 기준으로
    penguins_df, # 팽귄 df를 가지고
    prop = 0.7, # 70% 비율로
    strata = species # 종을 기준으로 나눠줘
  )

# 베이스라인 설정
tic(" Baseline XGBoost training duration ") # tic ~toc사이 코드 넣기
xgboost_fit <- # 
  boost_tree() %>% # boost 알고리즘 이용
  set_engine("xgboost") %>% # 엔진을 xgboost를 사용할거야
  set_mode("classification") %>% # 분류(classification)모델을 쓸꺼야
  fit(species ~ ., data = training(penguins_split)) # 팽귄 스플릿에있는 데이터다 가져다 씀
toc(log = TRUE)

preds <- predict(xgboost_fit, new_data = testing(penguins_split)) # 예측 모델 넣어주기
actual <- testing(penguins_split) %>% select(species) # 모든 데이터를 선택해서 오버피팅된다
yardstick::f_meas_vec(truth = actual$species, estimate = preds$.pred_class) # 벡터레이저?못들었어어어엉 1에 가까운 숫자가 나와야함

# 모델 설정(tune : 적당히 적절히)
ranger_model <- # ranger 모델을 설정할건데
  parsnip::rand_forest(mtry = tune(), min_n = tune()) %>% # 랜덤 포레스트를 사용(튜닝할거니깐 값넣어줘) / min_n : 가지치기를 몇번까지 할꺼야?
  set_engine("ranger") %>% # 
  set_mode("classification") # 
# ---------------------------------------------------------------------------------
glm_model <- # glm 모델을 설정할건데(제약조건을 기반으로 한 알고리즘)
  parsnip::multinom_reg(penalty = tune(), mixture = tune()) %>% # 패널티를 줄거야.얼마나?적당히^^
  set_engine("glmnet") %>% # 
  set_mode("classification") # 
# ---------------------------------------------------------------------------------
xgboost_model <- # xgboost 모델을 설정할건데
  parsnip::boost_tree(mtry = tune(), learn_rate = tune()) %>% # 
  set_engine("xgboost") %>% # 
  set_mode("classification") # 
hardhat::extract_parameter_dials(glm_model, "mixture") # 

# grid 검색(전역 검색)
ranger_grid <-
  hardhat::extract_parameter_set_dials(ranger_model) %>%
  finalize(select(training(penguins_split), -species)) %>%
  grid_regular(levels = 4)
ranger_grid

ranger_grid %>% 
  ggplot(aes(mtry, min_n)) + # 이미지로 확인 -> mtry가 4인거는 날려야함....하이퍼파라미터로 지정하기 어려워서
  geom_point(size = 4, alpha = 0.6) +
  labs(title = "Ranger: Regular grid for min_n & mtry combinations")
# ---------------------------------------------------------------------------------
glm_grid <-
  parameters(glm_model) %>%
  grid_random(size = 20)
glm_grid

glm_grid %>% 
  ggplot(aes(penalty, mixture)) +# 이미지로 확인
  geom_point(size = 4, alpha = 0.6) +
  labs(title = "GLM: Random grid for penalty & mixture combinations")
# ---------------------------------------------------------------------------------
xgboost_grid <-
  parameters(xgboost_model) %>%
  finalize(select(training(penguins_split), -species)) %>%
  grid_max_entropy(size = 20)
xgboost_grid

xgboost_grid %>% 
  ggplot(aes(mtry, learn_rate)) +# 이미지로 확인
  geom_point(size = 4, alpha = 0.6) +
  labs(title = "XGBoost: Max Entropy grid for LR & mtry combinations")

# 데이터값 보여주기위한 2차 전처리
recipe_base <-
  recipe(species ~ ., data = training(penguins_split)) %>%
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) # 더미갑 기준으로 한번에 다 알아서 함
# one_hot : 데이터를 0과1로 나눔

recipe_1 <-
recipe_base %>%
  step_YeoJohnson(all_numeric())

recipe_1 %>%
  prep() %>%
  juice() %>% # 섞어서
  summary() # 결과 보여주기

recipe_2 <-
  recipe_base %>%
  step_normalize(all_numeric()) # 정규화 되어있음

recipe_2 %>%
  prep() %>%
  juice() %>%
  summary()

# 일괄처리 정의
# metrics
model_metrics <- yardstick::metric_set(f_meas, pr_auc) # 일괄 처리해 f값과 정확도만 볼거야.(모델 메트릭은 절대 바꾸면 안된다)

# k-fold CV (k겹 교차검증)
data_penguins_3_cv_folds <-
  rsample::vfold_cv(
    v = 5,
    data = training(penguins_split), # 사용된 데이터를 training set으로 보내서
    strata = species # 종을 기준으로
  )

# 일괄 작업 작성
ranger_r1_workflow <- # 일의 흐름을 만들어라
  workflows::workflow() %>% #
  add_model(ranger_model) %>% # 모델 추가해라
  add_recipe(recipe_1) # 레시피1의 전처리 형태(레시피)로
# ---------------------------------------------------------------------------------
glm_r2_workflow <-
  workflows::workflow() %>%
  add_model(glm_model) %>%
  add_recipe(recipe_2)
# ---------------------------------------------------------------------------------
xgboost_r2_workflow <-
  workflows::workflow() %>%
  add_model(xgboost_model) %>%
  add_recipe(recipe_2)

# Gridsearch를 활용한 학습
tic("Ranger tune grid training duration ")
ranger_tuned <-
  tune::tune_grid(
    object = ranger_r1_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = ranger_grid,
    metrics = model_metrics,
    control = tune::control_grid(save_pred = TRUE)
  )
toc(log = TRUE)
# ---------------------------------------------------------------------------------
tic("GLM tune grid training duration ")
glm_tuned <-
  tune::tune_grid(
    object = glm_r2_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = glm_grid,
    metrics = model_metrics,
    control = tune::control_grid(save_pred = TRUE)
  )
toc(log = TRUE)
# ---------------------------------------------------------------------------------
tic("XGBoost tune grid training duration ")
xgboost_tuned <-
  tune::tune_grid(
    object = xgboost_r2_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = xgboost_grid,
    metrics = model_metrics,
    control = tune::control_grid(save_pred = TRUE)
  )
toc(log = TRUE)

# 학습 결과 확인
install.packages("finetune") # 결과 확인용
library(finetune)

tic("Tune race training duration ")
ft_xgboost_tuned <-
  finetune::tune_race_anova(
    object = xgboost_r2_workflow,
    resamples = data_penguins_3_cv_folds,
    grid = xgboost_grid,
    metrics = model_metrics,
    control = control_race(verbose_elim = TRUE) # 66
  )
toc(log = TRUE)

# 시각화를 통한 확인
plot_race(ft_xgboost_tuned) + labs(title = "Parameters Race by Fold")


# 결과(코드추가해야함)
bind_cols(
  tibble(model = c("Ranger", "GLM", "XGBoost")),
  bind_rows(
    ranger_tuned %>%
      collect_metrics() %>% group_by(.metric) %>% summarise(best_va = max(mean, na.rm = TRUE)) %>% arranglm_tuned %>%
      glm_tuned %>%
      collect_metrics() %>% group_by(.metric) %>% summarise(best_va = max(mean, na.rm = TRUE)) %>% arranglm_tuned %>% 
    xgboost_tuned %>%
      collect_metrics() %>% group_by(.metric) %>% summarise(best_va = max(mean, na.rm = TRUE)) %>% arran_tuned)
)

# 전체 모델 확인
glm_tuned %>% collect_metrics() # 20 models and 2 metrics

glm_tuned %>%
  collect_metrics() %>%
  group_by(.metric) %>%
  summarise(best_va = max(mean, na.rm = TRUE)) %>%
  arrange(.metric)

glm_tuned %>%
  collect_metrics() %>%
  group_by(.metric) %>%
  summarise(best_va = max(mean, na.rm = TRUE)) %>%
  arrange(.metric)

glm_tuned %>% select_best(metric = "f_meas")

# F1값 확인(값의 밸런스수치?)
glm_tuned %>%
  collect_metrics() %>%
  filter(.metric == "f_meas") %>%
  select(mean, penalty, mixture) %>%
  pivot_longer(penalty:mixture,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "F1", title = "F1 MetricEvolution")

# 모델 계선
best_f1 <-
  select_best(xgboost_tuned, metric = "f_meas")
final_model_op1 <-
  finalize_workflow(
    x = xgboost_r2_workflow,
    parameters = best_f1 # 파라미터값으로 베스트f1을 넣고 
  )
final_model_op1

#  Last Fit Tune Model
tic("Train final model Tune")
penguins_last_fit <-
  last_fit(final_model_op1,
           penguins_split,
           metrics = model_metrics
  )
toc(log = TRUE)

collect_metrics(penguins_last_fit) %>%
  arrange(.metric)

penguins_last_fit %>%
  collect_predictions() %>%
  conf_mat(truth = species, estimate = .pred_class)

penguins_last_fit %>%
  pull(.predictions) %>%
  as.data.frame() %>%
  filter(.pred_class != species)

# 주요 특징 분석
#install.packages("vip")
library(vip)
final_model_op1 %>%
  fit(data = penguins_df) %>%
  pull_workflow_fit() %>%
  vip(
    geom = "col",
    aesthetics = list(fill = "steelblue")
  ) +
  labs(title = "Feature Importance")

# 모델별 지표 확인
tic.log() %>%
  unlist() %>%
  tibble()


