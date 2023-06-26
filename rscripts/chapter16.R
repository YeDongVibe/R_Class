# Chapter 16

# 실습: 유클리디안 거리 계산법
# 단계 1: matrix 객체 생성
x <- matrix(1:9, nrow = 3, by = T) # 1~9까지 정수벡터 생성. 3개의 행으로 지정
x
# matrix(data, nrow, ncol, byrow, dimnames)
# data: 행렬을 구성하는 데이터 / nrow: 행렬의 행(row) 개수 / ncol: 행렬의 열(column) 개수 / byrow: 논리값으로, 행렬을 채우는 순서를 지정 / dimnames: 행렬의 행과 열에 대한 이름을 지정할 수 있는 옵션

# 단계 2: 유클리디안 거리 생성
dist <- dist(x, method = "euclidean")
dist
# dist(x, method = "euclidean")
# x: 거리를 계산할 데이터 / method: 거리 계산 방법을 지정하는 문자열


# 실습: 1행과 2행 변량의 유클리디안 거리 구하기 
s <- sum((x[1, ] - x[2, ]) ^ 2)
sqrt(s)
# sum(..., na.rm = FALSE)
# ...: 합계를 계산할 숫자 벡터들 / na.rm: 논리값으로, TRUE로 설정하면 NA(결측값)를 제외하고 합계를 계산(기본갑 : FALSE)

# 실습: 1행과 3행 변량의 유클리디안 거리 구하기 
s <- sum((x[1, ] - x[3, ]) ^ 2)
sqrt(s)


# 실습: 유클리디안 거리를 이용한 군집화
# 단계 1: 군집분석(Clustering)을 위한 패키지 설치
install.packages("cluster")
library(cluster)

# 단계 2: 데이터 셋 생성
x <- matrix(1:9, nrow = 3, by = T)

# 단계 3: matrix 객체 대상 유클리디안 거리 생성
dist <- dist(x, method = "euclidean")

# 단계 4: 유클리디안 거리 matrix를 이용한 군집화 
hc <- hclust(dist)
# hclust(d, method = "complete")
# d: 거리 행렬 또는 유사도 행렬 / method: 군집 형성 방법을 지정하는 문자열. 주로 사용되는 방법은 "complete" (완전연결법). 다른 옵션으로는 "single" (단일연결법), "average" (평균연결법), "ward.D2" (워드 연결법) 등

# 단계 5: 클러스터 시각화 
plot(hc)


# 실습: 신입사원의 면접시험 결과를 군집분석
# 단계 1: 데이터 셋 가져오기 
interview <- read.csv("C:/Rwork/Part-IV/interview.csv", header = TRUE)
names(interview)
head(interview)

#단계 2: 유클리디안 거리 계산
interview_df <- interview[c(2:7)]
idist <- dist(interview_df)
head(idist)

# 단계 3: 계층적 군집분석
hc <- hclust(idist)
hc

# 단계 4: 군집분석 시각화
plot(hc, hang = -1)

# 단계 5: 군집 단위 테두리 생성
rect.hclust(hc, k = 3, border ="red")
# rect.hclust(hc, k, border = "red")
# hc: hclust() 함수로 생성된 계층적 군집 객체 / k: 그릴 군집 경계 상자의 개수 또는 높이(height) / border: 경계 상자의 테두리 색상을 지정



# 실습: 군집별 특징 보기 
# 단계 1: 군집별 서브 셋 만들기 
g1 <- subset(interview, no == 108 | no == 110 | no == 107 |
               no == 112 | no == 115)
g2 <- subset(interview, no == 102 | no == 101 | no == 104 |
               no == 106 | no == 113)
g3 <- subset(interview, no == 105 | no == 114 | no == 109 |
               no == 103 | no == 111)

# 단계 2: 각 서브 셋의 요약 통계량 보기 
summary(g1)
summary(g2)
summary(g3)


# 실습: iris 데이터 셋을 대상으로 군집 수 자르기 
# 단계 1: 유클리디안 거리 계산
idist <- dist(iris[1:4])
hc <- hclust(idist)
plot(hc, hang = -1)

# 단계 2: 군집 수 자르기 
ghc <- cutree(hc, k = 3)
ghc
# cutree(hc, k)
# hc는 계층 군집화 결과를 나타내는 객체 / k는 추출할 클러스터의 개수

# 단계 3: iris 데이터 셋에 ghc 칼러 ㅁ추가 
iris$ghc <- ghc
table(iris$ghc)
head(iris)

# 단계 4: 요약 통계량 구하기 
g1 <- subset(iris, ghc == 1)
summary(g1[1:4])
g2 <- subset(iris, ghc == 2)
summary(g2[1:4])
g3 <- subset(iris, ghc == 3)
summary(g3[1:4])



# 실습: K-means 알고리즘에 군집 수를 적용하여 군집별로 시각화 
# 단계 1: 군집분석에 사용할 변수 추출
library(ggplot2)
data(diamonds)
t <- sample(1:nrow(diamonds), 1000)
# sample(x, size, replace = FALSE, prob = NULL) : 벡터나 집합에서 무작위로 샘플을 추출하는 데 사용
# x: 샘플을 추출할 벡터나 집합 / size: 추출할 샘플의 크기 / replace: 논리값으로, TRUE인 경우 반복 추출을 허용하고 FALSE인 경우 반복 추출을 허용하지 않음 / rob: 선택적인 매개변수로, x의 각 요소가 선택될 확률을 지정하는 벡터
test <- diamonds[t, ]
dim(test)
head(test)

mydia <- test[c("price", "carat", "depth", "table")]
head(mydia)


# 단계 2: 계층적 군집분석(탐색적 분석)
result <- hclust(dist(mydia), method = "average")
result
plot(result, hang = -1)


# 단계 3: 비계층적 군집분석
result2 <- kmeans(mydia, 3)
names(result2)
result2$cluster

mydia$cluster <- result2$cluster
head(mydia)

# 단계 4: 변수 간의 상관계수 보기 
cor(mydia[ , -5], method = "pearson")
# cor(x, y = NULL, use = "everything", method = c("pearson", "kendall", "spearman")) : 두 변수 또는 여러 변수 간의 상관 관계를 계산하는 데 사용
# x, y: 상관 관계를 계산할 변수 / use: 결측값(missing values)을 처리하는 방법을 지정 / method: 사용할 상관 계수의 유형을 지정하는 문자열 또는 문자 벡터
plot(mydia[ , -5])

# 단계 5: 상관계수를 색상으로시각화 
install.packages("mclust")
library(mclust)
install.packages("corrgram")
library(corrgram)
corrgram(mydia[ , -5], upper.panel = panel.conf)
corrgram(mydia[ , -5], lower.panel = panel.conf)
# corrgram(x, order = NULL, panel = NULL, text.panel = NULL, upper.panel = NULL, lower.panel = NULL, main = NULL, xlab = NULL, ylab = NULL, cex.labels = 1, cex.main = 1, cex.axis = 1, pch.corr = 16, type = "full", col = NULL, col.axis = NULL, col.labels = NULL)



# 단계 6: 비계층적 군집 시각화 
plot(mydia$carat, mydia$price, col = mydia$cluster)
points(result2$centers[ , c("carat", "price")],
       col = c(3, 1, 2), pch = 8, cex = 5)


# 실습: 트랜잭션 객체를 대상으로 연관규칙 생성
# transactions은 아이템들의 집합을 표현하는 객체로, 일반적으로 장바구니 분석, 연관 규칙 분석, 시퀀스 분석 등과 같은 작업에서 사용
# 단계 1: 연관분석을 위한 패키지 설치
install.packages("arules")
library(arules)

# 단계 2: 트랜잭션 객체 생성
setwd("C:/Rwork/Part-IV")
tran <- read.transactions("tran.txt", format = "basket", sep = ",")
tran


# 단계 3: 트랜잭션 데이터 보기 
inspect(tran)

# 단계 4: 규칙 발견
# apriori(data, parameter = NULL, appearance = NULL, control = NULL) : 연관 규칙 분석을 수행하는 데 사용
# data는 transactions 객체 또는 관련된 데이터 구조를 나타내는 객체 / parameter는 알고리즘 매개변수를 설정하는 데 사용되는 객체 / appearance은 생성될 연관 규칙의 조건을 지정하는 객체 / control은 알고리즘 실행을 제어하는 객체
rule <- apriori(tran, parameter = list(supp = 0.3, conf = 0.1))
inspect(rule)

# 단계 5: 규칙 발견
rule <- apriori(tran, parameter = list(supp = 0.1, conf = 0.1))
inspect(rule)


# 실습: single 트랜잭션 객체 생성
setwd("C:/Rwork/Part-IV")
stran <- read.transactions("demo_single", format = "single", cols = c(1, 2))
inspect(stran)

# 실습: 중복 트랜잭션 제거 
# 단계 1: 트랜잭션 데이터 가져오기 
setwd("C:/Rwork/Part-IV")
stran2 <- read.transactions("single_format.csv", format = "single",
                             sep = ",", cols = c(1, 2), rm.duplicates = T)

# 단계 2: 트랜잭선과 상품수 확인
stran2

# 단계 3: 요약 통계량 제공
summary(stran2)


# 실습: 규칙 발견(생성)
# 단계 1: 규칙 생성하기 
astran2 <- apriori(stran2)

# 단계 2: 발견된 규칙 보기 
inspect(astran2)

# 단계 3: 상위 5개의 향상도를 내림차순으로 정렬하여 출력
inspect(head(sort(astran2, by = "lift")))



# 실습: basket 형식으로 트랜잭션 객체 생성
setwd("C:/Rwork/Part-IV")
btran <- read.transactions("demo_basket", format = "basket", sep = ",")
inspect(btran)


# 실습: Adult 데이터 셋 가져오기 
data(Adult)
Adult


# 실습: AdultUCI 데이터 셋 보기 
data("AdultUCI")
str(AdultUCI)


# 실습: Adult 데이터 셋의 요약 통계량 보기 
# 단계 1: data.frame 형식으로 보기 
adult <- as(Adult, "data.frame")
str(adult)
head(adult)

# 단계 2: 오약 통계량
summary(Adult)


# 실습: 지지도 10%와 신뢰도 80%가 적용된 연관규칙 발견
ar <- apriori(Adult, parameter = list(supp = 0.1, conf = 0.8))


# 실습: 다양한 신뢰도와 지지도를 적용한 예 
# 단계 1: 지지도를 20%로 높인 경우 1,306개 규칙 발견
ar1 <- apriori(Adult, parameter = list(supp = 0.2))

# 단계 2: 지지도를 20%, 신뢰도를 95%로 높인 경우 348개 규칙 발견
ar2 <- apriori(Adult, parameter = list(supp = 0.2, conf = 0.95))

# 단계 3: 지지도를 30%, 신뢰도를 95%로 높인 경우 124개 규칙 발견
ar3 <- apriori(Adult, parameter = list(supp = 0.3, conf = 0.95))

# 단계 4: 지지도를 35%, 신뢰도를 95%로 높인 경우 67개 규칙 발견
ar4 <- apriori(Adult, parameter = list(supp = 0.35, conf = 0.95))

# 단계 5: 지지도를 40%, 신뢰도를 95%로 높인 경우 36개 규칙 발견
ar5 <- apriori(Adult, parameter = list(supp = 0.4, conf = 0.95))


# 실습: 규칙 결과 보기 
# 단계 1: 상위 6개 규칙 보기 
inspect(head(ar5))

# 단계 2: confidence(신뢰도) 기준 내림차순 정렬 상위 6개 출력
inspect(head(sort(ar5, decreasing = T, by = "confidence")))

# 단계 3: lift(향상도) 기준 내림차순 정렬 상위 6개 출력
inspect(head(sort(ar5, by = "lift")))



# 실습: 연관규칙 시각화 
# 단계 1: 패키지 설치 
install.packages("arulesViz")
library(arulesViz)

# 단계 2: 연관규칙 시각화
plot(ar3, method = "graph", control = list(type = "items"))
plot(ar3, method = "graph", control = list(type = "items"), engine = "htmlwidget")


# 실습: Groceries 데이터 셋으로 연관분석 하기 
# 단계 1: Groceries 데이터 셋 가져오기 
data("Groceries")
str(Groceries)
Groceries

# 단계 2: data.frame으로 변환
Groceries.df <- as(Groceries, "data.frame")
head(Groceries.df)

# 단계 3: 지지도 0.001, 신뢰도 0.8 적용규칙 발견
rules <- apriori(Groceries, parameter = list(supp = 0.001, conf = 0.8))

# 단계 4: 규칙을 구성하는 왼쪽(LHS) -> 오른쪽(RHS)의 item 빈도수 보기 
plot(rules, method = "grouped")



# 실습: 최대 길이가 3이하인 규칙 생성
rules <- apriori(Groceries, 
                 parameter = list(supp = 0.001, conf = 0.80, maxlen = 3))


# 실습: Confidence(신뢰도) 기준 내림차순으로 규칙 정렬
rules <- sort(rules, decreasing = T, by = "confidence")
inspect(rules)


# 실습: 발견된 규칙 시각화 
library(arulesViz)
plot(rules, method = "graph")



# 실습: 특정 상품(item)으로 서브 셋 작성과 시각화 
# 단계 1: 오른쪽 item이 전지분유(whole milk)인 규칙만 서브 셋으로 작성
wmilk <- subset(rules, rhs %in% 'whole milk')
wmilk

inspect(wmilk)
plot(wmilk, method = "graph")

# 단계 2: 오른쪽 item이 other vegetables인 규칙만 서브 셋으로 작성
oveg <- subset(rules, rhs %in% 'other vegetables')
oveg
inspect(oveg)
plot(oveg, method = "graph")


# 단계 3: 오른쪽 item이 vegetables 단어가 포함된 규칙만 서브 셋으로 작성
oveg <- subset(rules, rhs %in% 'vegetables')

oveg
inspect(oveg)

# 단계 4: 왼쪽 item이 butter 또는 yogurt인 규칙만 서브 셋으로 작성
butter_yogurt <- subset(rules, lhs %in% c('butter', 'yogurt'))
butter_yogurt
inspect(butter_yogurt)
