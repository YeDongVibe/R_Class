# tidyverse
# palmerpenguins
library("tidyverse")
library("palmerpenguins")

## 1. 데이터 확인
glimpse(penguins) #데이터 사이즈 확인하는 명령어<무조건 기억하기>

t(map_df(penguins, ~sum(is.na(.)))) # 데이터프레임을 맵핑하는데 penguins안에서 sum을 다 할껀데 본인값이 Na인 것의 개수 세기

plot_data <- penguins %>%
  drop_na() # na삭제

t(map_df(plot_data, ~sum(is.na(.)))) #삭제 후 확인하기

## 2. 데이터 구성(이미지 표현 : 종)
count_data <- plot_data %>% # plot_data를 이용해 어찌저찌해 count_data로 지정
  group_by(species) %>% # group_by()를 이용해 '종'으로 묶기
  tally() # tidyverse를 이용하기 위해 tally()를 사용함.=>tibble로 나옴: count를 사용해도 괜찮지만 group으로 나옴
count_data


ggplot(count_data) + # 그래프로 표현할 데이터셋 선택
  aes(x = species, fill = species, weight = n) + # Aesthetics : 시각적 요소 추가(x,y축 / 사이즈 / 색등)
  geom_bar() # 그래프 종류 선택

