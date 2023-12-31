# Chapter 07

# 실습: 실습용 데이터 가져오기 
getwd()
setwd("C:/R_Class/Rwork/Part-II")
dataset <- read.csv("dataset.csv", header = T)
dataset

# 실습: 전체 데잍 보기 
print(dataset)
View(dataset)

# 실습: 데이터의 앞부분과 뒷부분 보기 
head(dataset)
tail(dataset)

# 실습: 데이터 셋 구조 보기 
names(dataset) # 변수명(칼럼) 조회
attributes(dataset) # 열이름, 행이름, 자료구조 정보 제공
str(dataset) # 자료구조, 관측치와 변수의 개수 제공


# 다양한 방법으로 데이터 셋 조회하기 
# 단계 1: 데이터 셋에서 특정 변수 조회하기($)
dataset$age
dataset$resident
length(dataset$age)

#단계 2: 특정 변수으 조회 결과를 변수에 저장하기 
x <- dataset$gender
y <- dataset$price

x
y

# 단계 3: 산점도 그래프로 변수 조회
plot(dataset$price)

# 단계 4: 칼럼명을 사용하여 특정 변수 조회
dataset["gender"]
dataset["price"]


# 단계 5: 색인을 사용하여 특정 변수 조회
dataset[2] #두번째 칼럼(gender) 조회
dataset[6] # 여섯번째 칼럼(price) 조회
dataset[3, ] # 세번째 관측치(행) 전체, 열 공통
dataset[ , 3] # 전체 행의 세번째 변수(열), 행, 공통

# 단계 6: 2개 이상의 칼럼 조히 
dataset[c("job", "price")] #job하고 price 열 조회
dataset[c(2, 6)] #2번째와 6번째 (gender, price) 열 조회
dataset[c(1, 2, 3)] #1~3열(resident, gender, age) 조회
dataset[c(2, 4:6, 3, 1)] # gender, age, position, price, job, resident 열 조회


#단계 7: 특정행/열을 조회
dataset[ , c(2:4)] #모든 행의 2~4열 조회
dataset[c(2:4), ] #2~4행의 모든 열을 조회
dataset[-c(1:100), ] #1~100행을 제외한 나머지 행의 모든 열 조회


# 실습: summary() 함수를 사용하여 결측치 확인하기 : 요약통계량과 결측치 정보를 제공
summary(dataset$price) #결측치 확인
sum(dataset$price) #결측치 출력

# 실습: sum() 함수의 속성을 이용하여 결측치 제거하기 
sum(dataset$price, na.rm = T) # na.rm = T를 적용해 결측치 제거

# 실습: 결측치 제거 함수를 이용하여 결측치 제거 
price2 <- na.omit(dataset$price) # price 열에 잇는 모든 NA제거
sum(price2) # 합계 구하기
length(price2) #결측치 제거


# 실습: 결측치를 0으로 대체하기 
x <- dataset$price #price 열을 대상으로 벡터 생성
x[1:30] #price 열의 벡터 확인
dataset$price2 = ifelse(!is.na(x), x, 0) # NA이면 0으로 대체 : ifelse(a, b, c) :  a이면 b로 바꾸고 아니면 c로 바꿔라
dataset$price2[1:30] # 결측치를 0으로 대체한 price 열의 백터 확인


# 실습: 결측치를 평균으로 대체하기 
x <- dataset$price #price 열을 대상으로 벡터 생성
x[1:30] # price 열의 벡터 확인
dataset$price3 = ifelse(!is.na(x), x, round(mean(x, na.rm = TRUE), 2)) # 결측치를 평균값으로 대체
dataset$price3[1:30] # 결측치를 평균으로 대체한 price 열의 벡터 확인
dataset[c('price', 'price2', 'price3')] # 대체한 3개 칼럼 확인


# 실습: 범주형 변수의 극단치 처리하기 
table(dataset$gender)
pie(table(dataset$gender))


# 실습: subset() 함수를 사용하여 데이터 정제하기 
dataset <- subset(dataset, gender == 1 | gender == 2) # 변수 정제 : 극단치 제거
dataset
length(dataset$gender)
pie(table(dataset$gender))
pie(table(dataset$gender), col = c("pink", "lightblue"))


# 실습: 연속형 변수의 극단치 보기 
dataset <- read.csv("dataset.csv", header = T) #파일 읽기
dataset$price # 세부 데이터 보기
length(dataset$price) 
plot(dataset$price)
summary(dataset$price)


# 실습: price 변수의 데이터 정제와 시각화 
dataset2 <- subset(dataset, price >= 2 & price <= 8) # subset : 부분 객체 만들기
length(dataset2$price)
stem(dataset2$price) # 줄기와 잎 도표 보기 / 구매가격대 : 줄기, 세부가격 :잎


# 실습: age 변수의 데이터 정제와 시각화 
# 단계 1: age 변수에서 NA 발견
summary(dataset2$age)
length(dataset2$age)

# 단계 2: age 변수 정제(20 ~ 69)
dataset2 <- subset(dataset2, age >= 20 & age <= 69)
length(dataset2)

# 단계 3: box 플로팅으로 평균연령 분석
boxplot(dataset2$age)


# 실습: boxplot와 통계를 이용한 극단치 처리하기 
# 단계 1: boxplot로 price의 극단치 시각화
boxplot(dataset$price)

# 단계 2: 극단치 통계 확인
boxplot(dataset$price)$stats

# 단계 3: 극단치를 제거한 서브 셋 만들기 
dataset_sub <- subset(dataset, price >= 2 & price <= 7.9) # 2.1이상 7.0이하 가격으로만 재구성
summary(dataset_sub$price)

# 실습: 가독성을 위해 resident 갈럼을 대상으로 코딩 변경하기 
dataset2$resident2[dataset2$resident == 1] <- '1.서울특별시' # 기존 칼럼이 1이면 서울특별시로 변경
dataset2$resident2[dataset2$resident == 2] <- '2.인천광역시'
dataset2$resident2[dataset2$resident == 3] <- '3.대전광역시'
dataset2$resident2[dataset2$resident == 4] <- '4.대구광역시'
dataset2$resident2[dataset2$resident == 5] <- '5.시구군'

# 코딩 변경 전과 변경 후의 칼럼 보기 
dataset2[c("resident", "resident2")]

# 실습: 가독성을 위해 job 칼럼을 대상으로 코딩 변경하기
dataset2$job2[dataset2$job == 1] <- '공무원' # 기존 칼럼이 1이면 공무원으로 변경
dataset2$job2[dataset2$job == 2] <- '회사원'
dataset2$job2[dataset2$job == 3] <- '개인사업'
# 코딩 변경 전과 변경 후의 칼럼 보기 
dataset2[c("job", "job2")]

# 나이를 나타내는 age 칼럼을 대상으로 코딩 변경하기 
dataset2$age2[dataset2$age <= 30] <- "청년층" # 기존 칼럼이 30이하이면 청년층으로 변경
dataset2$age2[dataset2$age > 30 & dataset2$age <= 55] <- "중년층" # 기존 칼럼이 30초과 고 55이하이면 중년층으로 변경
dataset2$age2[dataset2$age > 55 ] <- "장년층"
head(dataset2)


# 실습: 만족도(survey)를 긍정순서로 역 코딩 
survey <- dataset2$survey
csurvey <- 6 - survey # 6- 현재값을 이용해 역코딩
csurvey

dataset2$survey <- csurvey 
head(dataset2)

#=================================================================================================================================================================================================================
# 실습: 범주형 vs 범주형 데이터 분포 시각화 
# 단계 1: 실습을 위한 데이터 가져오기 
setwd("C:/R_Class/Rwork/Part-II")
new_data <- read.csv("new_data.csv", header = TRUE, fileEncoding = "CP949", encoding = "UTF-8")
str(new_data)

# 단계 2: 코딩 변경된 거주지역(resident) 칼럼과 성별(gender) 칼럼을 대상으로 빈도수 구하기 
resident_gender <- table(new_data$resident2, new_data$gender2) #table(x,y)
resident_gender
gender_resident <- table(new_data$gender2, new_data$resident2)
gender_resident

# 단계 3: 성별(gender)에 따른 거주지역(resident)의 분포 현황 시각화 
barplot(resident_gender, beside = T, horiz = T, 
        col = heat.colors(5), 
        legend = row.names(resident_gender), 
        main = '성별에 따른 거주지역 분포 현황')

# 단계 4: 거주지역(resident)에 따른 성별(gender)의 분포 현황 시각화 
barplot(gender_resident, beside = T, 
        col = rep(c(2, 4), 5), horiz = T, 
        legend = c("남자", "여자"),
        main = '거주지역별 성별 분포 현황')

# 실습: 연속형 vs 범주형 데이터의 시각화 
# 단계 1: lattice 패키지 설치와 메모리 로딩 및 데이터 준비
install.packages("lattice")
library(lattice)

# 단계 2: 직업 유형에 따른 나이 분포 현황
densityplot(~ age, data = new_data, 
            groups = job2, 
            # plot.points = T: 밀도, auto.key = T: 범례)
            plot.points = T, auto.key = T)


# 실습: 연속형 vs 범주형 vs 범주형
# 단계 1: 성별에 따른 직급별 구매비용 분석
densityplot(~ price | factor(gender), 
            data = new_data, #입력 데이터
            groups = position2, 
            plot.points = T, auto.key = T)


# 단계 2: 직급에 따른 성별 구매비용 분석
densityplot(~ price | factor(position2), 
            data = new_data, 
            groups = gender2, 
            plot.points = T, auto.key = T)


# 실습: 연속형(2개) vs 범주형(1개) 데이터 분포 시각화 
xyplot(price ~ age | factor(gender2), 
       data = new_data)

#=================================================================================================================================================================================================================
# 실습: 파생변수 생성하기 
# 단계 1: 데이터 파일 가져오기 
setwd("C:/R_Class/Rwork/Part-II")
user_data <- read.csv("user_data.csv", header = T, fileEncoding = "CP949", encoding = "UTF-8")
head(user_data)
table(user_data$house_type)

# 단계 2: 파생변수 생성
house_type2 <- ifelse(user_data$house_type == 1 | user_data$house_type == 2, 0 , 1) # house type이 1이거나 2이면 0, 나머지는 1로 변환
house_type2[1:10]
user_data$house_type[1:10]

# 단계 3: 파생변수 추가 
user_data$house_type2 <- house_type2
head(user_data)


# 실습: 1:N의 관계를 1:1 관계로 파생변수 생성하기 
# 단계 1: 데이터 파일 가져오기 
pay_data <- read.csv("pay_data.csv", header = T, fileEncoding = "CP949", encoding = "UTF-8")
head(pay_data, 10)
table(pay_data$product_type)

# 단계 2: 고객별 상품 유형에 따른 구매금액과 합계를 나타내는 파생변수 생성
library(reshape2)
product_price <- dcast(pay_data, user_id ~ product_type, sum, na.rm = T) # dcast(dataset, 행변수~열변수, 계산함수, Na이면 제거) :: 1:1 혹은 1:n으로 바꾸기
head(product_price, 3)
# 단계 3: 칼럼명 수정
names(product_price) <- c('user_id', '식표품(1)', '생필품(2)', '의류(3)', '잡화(4)', '기타(5)')
head(product_price)


# 실습: 고객식별번호(user_id)에 대한 지불유형(pay_method)의 파생변수 생성하기 
# 단계 1: 고객별 지불유형에 따른 구매상품 개수를 나타내는 팡생변수 생성
pay_price <- dcast(pay_data, user_id ~ pay_method, length) #length : 개수세기
head(pay_price, 3)

# 단계 2: 칼럼명 변경하기 
names(pay_price) <- c('user_id', '현금(1)', '직불카드(2)', '신용카드(3)', '상품권(4)')
head(pay_price, 3)

# 실습: 고객정보(user_data) 테이블에 파생변수 추가하기 
# 단계 1: 고객정보 테이블과 고객별 상품 유형에 따른 구매금액 합계 병합하기 
library(plyr)
user_pay_data <- join(user_data, product_price, by = 'user_id')
head(user_pay_data, 10)

# 단계 2: [단계 1]의 병합 결과를 대상으로 고객별 지불유형에 따른 구매상품 개수 병합하기 
user_pay_data <- join(user_pay_data, pay_price, by = 'user_id')
user_pay_data[c(1:10), c(1, 7:15)]


# 실습: 사칙연산으로 총 구매금액 파생변수 생성하기 
# 단계 1: 고객별 구매금액의 합계(총 구매금액) 계산하기 
user_pay_data$총구매금액 <- user_pay_data$`식표품(1)` +
  user_pay_data$`생필품(2)` +
  user_pay_data$`의류(3)` +
  user_pay_data$`잡화(4)` +
  user_pay_data$`기타(5)`

# 단계 2: 고객별 상품 구매 총금액 칼럼 확인하기 
user_pay_data[c(1:10), c(1, 7:11, 16)]

#=================================================================================================================================================================================================================
# 실습: 정제된 데이터 저장하기 
print(user_pay_data)

setwd("C:/R_Class/Rwork/Part-II")
write.csv(user_pay_data, "cleanData.csv", quote = F, row.names = F)

data <- read.csv("cleanData.csv", header = TRUE)
data


# 실습: 표본 샘플링
# 단계 1: 표본 추출하기 
nrow(data) #data 행 수 구하기
choice1 <- sample(nrow(data), 30) #30개 행을 무작위로 추출
choice1

# 50 ~ (data 길이) 사이에서 30개 행을 무작위 추출
choice2 <- sample(50:nrow(data), 30) #50부터 400 사이에서 30개 추출
choice2

# 50~100 사이에서 30개 행을 무작위 추출 
choice3 <- sample(c(50:100), 30) #50~100사이 수 중 30개 추출
choice3

# 다양한 범위를 지정하여 무작위 샘플링
choice4 <- sample(c(10:50, 80:150, 160:190), 30)
choice4

# 단계 2: 샘플링 데이터로 표본추출
data[choice1, ]


# 실습: iris 데이터 셋을 대상으로 7:3 비율로 데이터 셋 생성하기 
# 단계 1: iris 데이터 셋의 관측치와 칼럼 수 확인
data("iris")
dim(iris) #dim: 데이터프레임의 길이를 관측(행과 열의 개수 모두 출력)


# 단계 2: 학습 데이터*70%), 검정 데이터(30%) 비율로 데이터 셋 구성
idx <-sample(1:nrow(iris), nrow(iris) * 0.7)
idx
training <- iris[idx, ] #학습데이터
testing <- iris[-idx, ] #학습데이터 제외 나머지 = 검정 데이터
dim(training)
dim(testing)



 # 실습: 데이터 셋을 대상으로 K겹 교차 검정 데이터 셋 생성하기 
# 단계 1: 데이터프레임 생성
name <- c('a', 'b','c', 'd', 'e', 'f')
score <- c(90, 85, 99, 75, 65, 88)
df <- data.frame(Name = name, Score = score)

# 단계 2: 교차 검정을 위한 패키지 설치 
install.packages("cvTools")
library(cvTools)

# 단계 3: K겹 교차 검정 데이터 셋 생성
cross <- cvFolds(n = 6, K = 3, R = 1, type = "random") #cvFolds(관찰치 수 혹은 데이터 크기, k겹에대한 설정, r회 반복, type) / type('random', 'consecutive', 'interleaved')
cross

# 단계 4: K겹 교차 검정 데이터 셋 구조 보기 
str(cross)
cross$which

# 단계 5: subsets 데이터 참조하기 
cross$subsets[cross$which == 1, 1]
cross$subsets[cross$which == 2, 1]
cross$subsets[cross$which == 3, 1]

# 단계 6: 데이터프레임의 관측치 적용하기 
r = 1
K = 1:3
for(i in K) {
  datas_idx <- cross$subsets[cross$which == i, r]
  cat('K = ', i, '검정데이터 \n') #cat : 출력하고 싶은 내용을 출력 == 강화된 print
  print(df[datas_idx, ])
  
  cat('K = ', i, '훈련데이터 \n')
  print(df[-datas_idx, ])
}


