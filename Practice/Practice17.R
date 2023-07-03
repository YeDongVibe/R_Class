# 미국 나스닥 데이터 분석

## 0. 패키지 불러오기
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)
library(corrr)
library(rstatix)
library(prophet)
library(astsa)
library(forecast)
library(sysfonts)
library(showtext)

## 1. 데이터 프레임 작성
# 파일 목록 다 들고오기
files <- list.files(path = "data/nasdaq_stock/")
files
# 들고온 파일 목록을 다 읽어서, 데이터 프레임 제작
stocks <- read_csv(paste0("data/nasdaq_stock/", files), id = "name") %>% # 각파일명을 name을 index로 잡아 들고오기
  mutate(name = gsub("data/nasdaq_stock/", "", name),  # name을 data~~로적힌 부분을 ""으로 수정할거d야
         name = gsub("\\.csv", "", name)) %>% 
  rename_with(tolower)
stocks
# 데이터 프레임 결함
df <- read_csv("data/nasdaq_stock_names.csv")
stocks <- 
  stocks %>% 
  inner_join(df, by = c("name" = "stock_symbol"))

## 2. 시계열 데이터 시각화
end_labels <- (stocks %>% 
  group_by(company) %>% # company를 묶을건데
  filter(date == max(date)) %>% #date가 가장 큰아이를 기준으로 필터함
  arrange(-open) %>%  # -open값 기준으로 전체를 배열해
  select(open, company))[c(1:3, 12:14),] # open과 company를 가져올거야

# 좀 더 해봐여!
stocks %>% 
  ggplot(aes(date, open)) + 
  geom_line(aes(color = company)) + 
  scale_y_continuous(sec.axis = sec_axis(~., breaks = end_labels$open,
                                         labels = end_labels$company)) +
  scale_x_date(expand = c(0,0)) + 
    labs(x = "", y="Open", color="", title = "주요 회사의 시작 가격")+
    theme(legend.position = "none")

(stocks %>%
    filter(company %in% end_labels$company[1:3]) %>%
    ggplot(aes(date, open)) +
    geom_line(aes(color = company)) +
    facet_wrap(~company) +
    theme_bw() +
    theme(legend.position = "none") +
    labs(title = "Top 3", x = "")) /
  (stocks %>%
     filter(company %in% end_labels$company[-(1:3)]) %>%
     ggplot(aes(date, open)) +
     geom_line(aes(color = company)) +
     facet_wrap(~company) +
     theme_bw() +
     theme(legend.position = "none") +
     labs(title = "Bottom 3", x = ""))

# 시계열 분석
aapl <- stocks %>% 
  filter(name == "AAPL") %>% 
  select(ds = date, y = open)

(aapl %>% 
    mutate(diff = c(NA, diff(y))) %>% 
    ggplot(aes(ds, diff)) +
    geom_point(color = "steelblue4", alphat = 0.7) +
    labs(y = "Difference", x = "Date",
         title = "One Day Returns")
) /
  (aapl %>% 
     mutate(diff = c(NA, diff(y))) %>% 
     ggplot(aes(diff)) +
     geom_histogram(bins = 50, fill = "steelblue4", color="black")
  )

#prophet
m_aapl <- prophet(aapl)
forecast <- predict(m_aapl, make_future_dataframe(m_aapl, periods = 140))
plot(m_aapl, forecast)
prophet_plot_components(m_aapl, forecast)


## 3. 시계열 데이터 분리


## 4. 종가 예측



