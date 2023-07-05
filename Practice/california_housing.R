## 0. 라이브러리 불러오기
library("tidyverse")
library("reshape2")

## 1. 데이터 불러오기
housing = read_csv("data/housing.csv")

# 앞/뒤를 확인해 해당 데이터 확인
head(housing)
tail(housing)
summary(housing)

#데이터 전체 구조 확인
str(housing)

# EDA -> 시각화
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

housing$total_bedrooms[is.na(housing$total_bedrooms)] <-  median(housing$total_bedrooms, na.rm = T) #na값을 중위값으로 넣어서 살리겠다
housing$mean_bedrooms <- housing$total_bedrooms / housing$households
housing$mean_rooms <- housing$total_rooms / housing$households
head(housing)

drops <- c('total_bedrooms', 'total_rooms') #colum지울거야
housing <- housing[,!(names(housing) %in% drops)] #행은 손대지 않고, 지우고 싶은걸 지우는게 아니라 지우고 싶은거말고 나머질 살리면된다
housing


#범주형
categories <- unique(housing$ocean_proximity)
cat_housing <- data.frame(ocean_proximity = housing$ocean_proximity)
head(cat_housing)

### 중위값으로 처리한다고 하자구리구리너구리!
#결측치 처리




## 3. 머신러닝




## 4. 결과 확인





