library(tidyverse)
library(DBI)
library(RSQLite)
install.packages("RSQLite")
install.packages("readxl")

# 데이터베이스 연결
con <- dbConnect(RSQLite::SQLite(), "test.db")
dbListTables(con)

# 파일 읽기
moving_data <- read_csv("./data/seoul_moving_202107_09_hr.csv") # csv 파일 읽기
reference <- readxl::read_excel("./data/reference.xlsx") # excel 파일 읽기

# 데이터 사이즈 확인
glimpse(moving_data)
glimpse(reference)

# colum명 재정의
names(moving_data) <- gsub(" ", "", names(moving_data))
names(moving_data)[9:10] <- c("평균이동시간_분", "이동인구_합") # 9,10열의 이름을 재정의
names(reference) <- c("시도코드", "시군구코드", "시군구이름", "전체이름")
names(reference)
                      
# 데이터 베이스로 복사하기 (DB에 넣기)
copy_to(con, moving_data, "moving_data", #con으로 moving_data를 "moving_data"라는 이름인 테이블로 만들어 복사
        temporary = FALSE, #  변수나 객체가 일시적이 아니라 영구적으로 유지되도록 지정. 현재 세션의 범위를 벗어나도 사용가능
        indexes = list("대상연월", "요일", "도착시간", "출발시군구코드", "도착시군구코드"), #index 목록 생성. 데이터 프레임에서 indexes 목록에 해당하는 열만 선택하거나, 벡터에서 indexes 목록에 해당하는 원소만 추출하는 등의 작업에 사용가능
        overwrite = TRUE) # 변수나 객체가 이미 존재하는 경우에도 덮어쓰기(재할당)를 허용. 이를 통해 새로운 값을 할당하거나 수정가능

copy_to(con, reference, "reference", #con으로 moving_data를 "moving_data"라는 이름인 테이블로 만들어 복사
        temporary = FALSE, #  변수나 객체가 일시적이 아니라 영구적으로 유지되도록 지정. 현재 세션의 범위를 벗어나도 사용가능
        indexes = list("시군구코드"), #index 목록 생성. 데이터 프레임에서 indexes 목록에 해당하는 열만 선택하거나, 벡터에서 indexes 목록에 해당하는 원소만 추출하는 등의 작업에 사용가능
        overwrite = TRUE) # 변수나 객체가 이미 존재하는 경우에도 덮어쓰기(재할당)를 허용. 이를 통해 새로운 값을 할당하거나 수정가능

# 데이터를 테이블로 만들어 넣기
moving_db <- tbl(con, "moving_data")
moving_db %>% head(6)

reference_db <- tbl(con, "reference")
reference_db

# 평균 이동시간 기준으로 이동 거리를 중/단/장기로 구분
moving_db <- moving_db %>% 
  mutate(평균이동시간_시 = 평균이동시간_분 / 60) %>% 
  mutate(이동타입 = case_when(
    between(평균이동시간_시, 0, 0.5) ~ "단기",
    between(평균이동시간_시, 0, 1) ~ "중기",
    평균이동시간_시 >= 1 ~ "장기",
    TRUE ~ as.character(평균이동시간_시)
    )) %>% 
  relocate(이동타입)

moving_db %>%  colnames()

glimpse(moving_db)
moving_db$평균이동시간_시


