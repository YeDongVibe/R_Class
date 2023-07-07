## 0. 라이브러리 불러오기
library("tidyverse")
library("reshape2")
library("boot")
install.packages("randomForest")
library("randomForest")
library("xgboost")
## 1. 데이터 불러오기
housing = read_csv("data/housing.csv")

# 앞/뒤를 확인해 해당 데이터 확인
head(housing)
tail(housing)
summary(housing)

#데이터 전체 구조 확인
str(housing)

# EDA -> 시각화
# 가정과 결과를 정의해 해당 문제 빠르게 해결
ggplot(data = melt(housing), mapping = aes(x = value)) + # ggplot을 이용
# data를 melt를 이용해 가져옴
  geom_histogram(bins = 30) + #bins가 30인 histogram 만들기
  facet_wrap(~variable, scales = 'free_x') # 감싸라 변수를. scale은 현재 보여지는 x의 사이즈에 맞춰서
plot_histo

ggplot(data = housing, mapping = aes(x = longitude, y = latitude,
                                     color = median_house_value)) + 
  geom_point(aes(size = population), alpha = 0.4)


## 2. 전처리
#이상치 처리
# bedroom_mean <- mean(housing$total_bedrooms, na.rm = T)
# bedroom_median <- median(housing$total_bedrooms, na.rm = T)
# bedroom_mean
# bedroom_median
# 
# ggplot(data = housing, mapping = aes(x = total_bedrooms)) +
#   geom_histogram(bins = 30, color = "black", fill = "blue")+
#   geom_vline(aes(xintercept = bedroom_mean, color = "red"), lwd = 1.5) +
#   geom_vline(aes(xintercept = bedroom_median, color = "yellow"), lwd = 1.5)

housing$total_bedrooms[is.na(housing$total_bedrooms)] <-  median(housing$total_bedrooms, na.rm = T) # na값을 중위값으로 넣어서 살리겠다
housing$mean_bedrooms <- housing$total_bedrooms / housing$households
housing$mean_rooms <- housing$total_rooms / housing$households
head(housing)

drops <- c('total_bedrooms', 'total_rooms') # colum지울거야
housing <- housing[,!(names(housing) %in% drops)] # 행은 손대지 않고, 지우고 싶은걸 지우는게 아니라 지우고 싶은거말고 나머질 살리면된다
housing


#범주형
categories <- unique(housing$ocean_proximity) # 내가 코드를 몇개 만들어야 하나?
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)
head(cat_housing)

# 초기화
# for loop 사용 (python에서 사용) -> 실무에서는 잘 사용 X
# 객체 기준으로 돔
for(cat in categories) {
  cat_housing[,cat] = rep(0, times = nrow(cat_housing)) # colum에 관심을 가짐 / row개수 만큼 0채움을 반복할꺼야
}
head(cat_housing)

# index기준으로 돔
for(i in 1:length(cat_housing$ocean_proximity)){ # 1부터 length만큼 돌았으면 해
  cat <- as.character(cat_housing$ocean_proximity[i]) # i번째 친구랑 character로 cat을 넣음
  cat_housing[,cat][i] <- 1
}
head(cat_housing)

cat_names <- names(cat_housing)
cat_names

keep_columns <- cat_names[cat_names != "ocean_proximity"] # ocean_proximity 임시로 지우기
cat_housing <- select(cat_housing, one_of(keep_columns))
tail(cat_housing)


### 중위값으로 처리한다고 하자구리구리너구리! : 사라져버렸다.
#결측치 처리(수치 처리)

colnames(housing) # colum이름 확인

drops <- c("ocean_proximity","median_house_value") # 문자와 답안지 뺴기 ㅋㅅㅋ
housing_num <- housing[, !(names(housing) %in% drops)]
colnames(housing_num)

# 표준화 진행
scaled_housing_num <- scale(housing_num)
head(scaled_housing_num)


#결합
head(cat_housing)
head(scaled_housing_num)
head(housing$median_house_value)

cleaned_housing <- cbind(cat_housing,
                         scaled_housing_num,
                         median_house_value = housing$median_house_value) # y값

head(cleaned_housing)

## 3. 머신러닝
set.seed(42) #random값(seed) 42로 고정
#데이터 분리
sample <- sample.int(n = nrow(cleaned_housing), #nrow개수만큼 개수사용
                     size = floor(.8 * nrow(cleaned_housing)), # 80% 사이즈 사용
                     replace = F)

train <- cleaned_housing[sample,] #sample 넣기
test <- cleaned_housing[-sample, ]# sample 제외하고 다가져오기

nrow(train) + nrow(test) == nrow(cleaned_housing) # False나오면 데이터가 누락된거라서 큰일이 생긴거 # 데이터 개수?분포? 100인지 확인


#모델 설계
# 선형모델(단시간내에 답이 나옴)
colnames(cleaned_housing) #col 이름
glm_house <- glm(median_house_value ~ median_income + mean_rooms + population,
    data=cleaned_housing) #median_house_value를 따르는 애들을 찾아줘
glm_house

k_fold_cv_error <- cv.glm(cleaned_housing, glm_house, K=5) # glm_house를 이용해 5번 반복해
k_fold_cv_error$delta # 오차의 delta(간극)

glm_cv_rmse = sqrt(k_fold_cv_error$delta)[1]
glm_cv_rmse # 오차율

glm_house$coefficients # 계수 값 

#랜덤포레스트 -> 중요도를 보기 위해 사용하는게 주 목적임듕
#데이터 분리
names(train)
train_y = train[, 'median_house_value']
train_x = train[, names(train) != 'median_house_value']
head(train)


rf_model = randomForest(train_x,
                        y = train_y,
                        ntree = 500, # 500개
                        importance = T) # 중요도 출력하겠음

rf_model$importance

test_y = test[, 'median_house_value']
test_x = test[, names(train) != 'median_house_value']
y_pred = predict(rf_model, test_x)
test_mse = mean((y_pred - test_y)^2)
test_mse

test_rmse = sqrt(test_mse)
test_rmse

##학습
#XGBoost
dtrain = xgb.DMatrix(data = as.matrix(train_x), label = train_y)
dtest = xgb.DMatrix(data = as.matrix(test_x), label = test_y)
watchlist = list(train = dtrain, test = dtest)
bst <- xgb.train(data = dtrain,
                 max.depth = 8,
                 eta = 0.3, # 오차
                 nthread = 2,
                 nround = 1000,
                 watchlist = watchlist,
                 objective = "reg:linear",
                 early_stopping_rounds = 50,
                 print_every_n = 500)


## 4. 결과 확인
# XGBoost를 활용한 RMSE 값이 가장 낮고(48403 -> 47810), 주요 특징값은 'median_income'임을 알수있음.













