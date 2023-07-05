# 미국 나스닥 데이터 분석

## 0. 패키지 불러오기
library(tidyverse) # 데이터 분석용

library(lubridate) # 유틸리티(문법의 몇개를 이용해 그림이나 데이터를 이용하는 것)
library(scales)
library(patchwork)
library(corrr)
library(rstatix)

library(prophet) # 시계열 데이터 예측 기반
library(forecast) # 통계기반 시계열 데이터 예측
library(astsa)

library(sysfonts) # 화면 출력용
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

## 2. 시계열 데이터 시각화 -> EDA
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

# 시계열 분석(apple 주가를 분석)
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

# prophet (가능한 prophet사용하기)
m_aapl <- prophet(aapl)
forecast <- predict(m_aapl, make_future_dataframe(m_aapl, periods = 140))
plot(m_aapl, forecast)
prophet_plot_components(m_aapl, forecast)


# ARIMA (prophet의 결과를 대조비교하기 위해 사용하는 용도 정도로 하기)
ts_aapl <- ts(aapl$y, start = c(2010, 4), frequency = 365) # 구획 구축
aapl_fit <- window(ts_aapl, end = 2018) 

auto_arima_fit <- auto.arima(aapl_fit)
plot(forecast(auto_arima_fit, h = 365), ylim = c(0,200))
lines(window(ts_aapl, start = 2018), col = "red") # 예측 구간 

# Prophet
ibm <- stocks %>% 
  filter(name == "IBM") %>% 
  select(ds = date, y = open)

m_ibm <- prophet(ibm)
forecast_ibm <- predict(m_ibm, 
                        make_future_dataframe(m_ibm, periods = 140))
plot(m_ibm, forecast_ibm)
prophet_plot_components(m_ibm, forecast_ibm)

plot(forecast(auto.arima(ibm$y), h = 365), ylim = c(0,250))

## 3. 시계열 데이터 분리
library("widyr")
(stock_corr <- stocks %>% 
    widyr::pairwise_cor(company, date, open) %>% 
    filter(item1 > item2) %>% 
    mutate(corrstr = ifelse(abs(correlation > 0.5), "Strong", "Weak"),
           type = ifelse(correlation > 0, "Positive", "Negative")) %>% 
    arrange(-abs(correlation)))

stock_corr %>% 
  ggplot(aes(correlation)) +
  geom_histogram(aes(fill = type), 
                 alpha = 0.7, binwidth = 0.05) +
  xlim(c(-1,1)) +
  labs(title = "Distribution of Correlation Values",
       subtitle = "The majority of companies have a strong positive correlation",
       fill = "Positive Correlation") +
  theme(legend.position = c(0.2,0.8),
        legend.background = element_rect(fill = "white", color = "white"))

(stocks %>% 
    widyr::pairwise_cor(name, date, open) %>% 
    rename(var1 = item1, var2 = item2) %>% 
    cor_spread(value = "correlation") %>% 
    rename(term = rowname))[c(14,1:13),] %>% 
  network_plot() +
  labs(title = "Correlation of Tech Stocks") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(family = "Roboto"))

stocks %>% 
  ggplot(aes(date, open, color = name)) +
  geom_line() +
  gghighlight::gghighlight(name == "IBM", use_direct_label = FALSE) +
  labs(x = "", y = "", color = "",
       title = "IBM is an Outlier Among Tech Stocks")

## 4. 종가 예측



