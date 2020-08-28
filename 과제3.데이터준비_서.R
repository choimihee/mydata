##############################
# 과제2. 데이터 준비
##############################

# 참고 파일 : 
# 310.다양한 데이터 처리_tidyverse
# 320.다양한 데이터 처리_join
# 410 추가변수_날짜데이터다루기



# 1. 데이터 준비 ----


# 0. 데이터 준비 -------------------------------------------------------------------
# 필요한 라이브러리 불러오기
if(!require(tidyverse)) {install.packages("tidyverse") ; library(tidyverse) }
if(!require(lubridate)) {install.packages("lubridate") ; library(lubridate) }
if(!require(roll)) {install.packages("roll") ; library(roll) }
if(!require(zoo)) {install.packages("zoo") ; library(zoo) }

# 데이터를 가져옵니다.


sk_stock <- read.csv('https://raw.githubusercontent.com/qdwe93/mydata/master/SK.csv'
                     , na.strings = c("  -  ", "null", "  null  "))
exchange <- read.csv('https://raw.githubusercontent.com/qdwe93/mydata/master/USD_KRW.csv')
data_co <- read.csv('https://raw.githubusercontent.com/qdwe93/mydata/master/CrudeOil.csv'
                     , na.strings = c("  -  ", "null"))
data_gd <- read.csv('https://raw.githubusercontent.com/qdwe93/mydata/master/Gold.csv'
                    , na.strings = c("  -  ", "null"))
data_dji <- read.csv('https://raw.githubusercontent.com/qdwe93/mydata/master/dowjones.csv'
                     , na.strings = c("  -  ", "null"))
data_trend <- read.csv('https://raw.githubusercontent.com/qdwe93/mydata/master/googletrend_SK.csv'
                       , na.strings = c("  -  ", "null"))
sk_stock$Adj.Close <- NULL

# 데이터 형식 변환
sk_stock$date <- as.Date(sk_stock$date)
exchange$date <- as.Date(exchange$date)
data_co$date <- as.Date(data_co$date)
data_gd$date <- as.Date(data_gd$date)
data_dji$date <- as.Date(data_dji$date)
data_trend$date <- as.Date(data_trend$date)

# 데이터에 이상이 있을경우 형변환용 (필요없음)
#for(i in c(2:ncol(sk_stock))) {
#  code = paste('sk_stock$',names(sk_stock[i])[1],
#               ' <- as.numeric(sk_stock$',names(sk_stock[i])[1],')')
#  eval(parse(text = code))
#}

# 결측치 처리
sk_stock <- na.omit(sk_stock)
exchange <- na.omit(exchange)
data_co <- na.omit(data_co)
data_gd <- na.omit(data_gd)
data_dji <- na.omit(data_dji)
data_trend <- na.omit(data_trend)


# 컬럼명 지정
exchange <-
  exchange %>%
  rename_with(~paste0('ex_',.), -c(date))
data_co <-
  data_co %>%
    rename_with(~paste0('o_',.), -c(date))
data_gd <-
  data_gd %>%
  rename_with(~paste0('g_',.), -c(date))
data_dji <-
  data_dji %>%
  rename_with(~paste0('d_',.), -c(date))
data_trend <-
  data_trend %>%
  rename_with(~paste0('trd_',.), -c(date))

# google trend 데이터 정제
sk_stock_date <-
  sk_stock %>%
    select(date) %>%
    mutate(trend_views = NA)

data_trend <-
  data_trend %>%
  union(sk_stock_date) %>%
  arrange(date)

# google trend 데이터는 2020년치까지 존재하므로 fromLast로 하면 누락이 없음
data_trend$trend_views <-
  na.locf(data_trend$trend_views, na.rm = FALSE,fromLast = TRUE) 

# join
sk_stock_master <-
sk_stock %>%
  left_join(exchange,by = 'date') %>%
  left_join(data_co,by = 'date') %>%
  left_join(data_gd,by = 'date') %>%
  left_join(data_dji,by = 'date') %>%
  left_join(data_trend,by = 'date') 
  

# join후 결측치 처리 - 연속형 데이터 이므로 locf
for(i in c(2:ncol(sk_stock_master))) {
  sk_stock_master[i] <-
    na.locf(sk_stock_master[i], na.rm = FALSE)
}

for(i in c(2:ncol(sk_stock_master))) {
  sk_stock_master[i] <-
    na.locf(sk_stock_master[i], na.rm = FALSE, fromLast = TRUE)
}

# 1. 데이터 준비 완료 ---------------------------------------------------
sk_stock <- sk_stock_master


# 2. 분석을 위한 데이터 프레임 만들기 ---------------------------------------------------
# 가설로 도출한 요인들을 변수로 붙여서 하나의 데이터프레임으로 만드시오.
# 데이터 프레임에 date 칼럼이 있어야 합니다.

# 적어도 15개의 변수들을 추가하시오.
# 범주형 변수가 전체에서 3개 이상은 되도록 추가 하시오.


# 연도
sk_stock$year <- year(sk_stock$date)

# 월
sk_stock$month <- month(sk_stock$date)

# 요일
sk_stock$weekdays <- weekdays(sk_stock$date, abbreviate = T)

# 일 등락폭 (high - low)
sk_stock <-
  sk_stock %>% 
    mutate(diff = high - low)

  

str(sk_stock)  
    
head(sk_stock)

# 전일 종가 (lag)
sk_stock$bd_close <- lag(sk_stock$close)

sk_stock$ex_diff <- NULL
# 변수 1
#전일 환율
sk_stock$ex_eclose <- lag(sk_stock$ex_close)

# 변수 2
# 전일종가
sk_stock$bd_close <- NULL

sk_stock$bd_close <- lag(sk_stock$close)
# 변수 3
# 전전일종가
sk_stock$bd_close2 <- lag(sk_stock$close, 2)
head(sk_stock)
str(sk_stock)
# 변수 4
# 전일거래량
sk_stock$bd_volume <- lag(sk_stock$volume)

# 변수 5
# 전일 국제 유가
sk_stock <- sk_stock %>% left_join(select(data_co, date, o_close))
sk_stock$o1_close <- lag(sk_stock$o_close)

# 변수 6
# 전전일 국제 유가
sk_stock$o2_close <- lag(sk_stock$o_close, 2)

# 변수 7
# 전일 장중고가
sk_stock$bd_high <- lag(sk_stock$high)

# 변수 8
#전일 다우지수
sk_stock <- sk_stock %>% left_join(select(data_dji, date, d_close))
head(sk_stock, 60)
# 변수 9
#전전일 다우지수
sk_stock$d1_close <- lag(sk_stock$d_close)
sk_stock$d2_close <- lag(sk_stock$d_close, 2)
head(sk_stock)
# 변수 10
# 전일 금값
sk_stock <- sk_stock %>% left_join(select(data_gd, date, g_close))
sk_stock$g_close1 <- lag(sk_stock$g_close)
# 변수 11
# 전전일 금값 
sk_stock$g_close2 <- lag(sk_stock$g_close, 2)

# 변수 12
# 전 30일 평균 거래량 
sk_stock$volume_30 <- roll_mean(sk_stock$volume, 30)

# 변수 13
# 전일 종가 - 전전일 종가 가 + 인지, - 인지
sk_stock$bd_close_qu <- ifelse((sk_stock$bd_close - sk_stock$bd_close2) >= 0, "PLUS", ifelse((sk_stock$bd_close - sk_stock$bd_close2) < 0, "MINUS",""))

# 변수 14
# 전일 장중 저가
sk_stock$bd_low <- lag(sk_stock$low)

# 변수 15
# sk_stock$bd_high_low_qu <- ifelse((sk_stock$bd_high - sk_stock$bd_low) >= 0, "PLUS", ifelse((sk_stock$bd_high - sk_stock$bd_low) < 0, "MINUS",""))
sk_stock$bd_high_low_qu <- NULL
# 전일 장중 고가 - 전일 장중저가
sk_stock$bd_high_low_mins <- (sk_stock$bd_high - sk_stock$bd_low)

# 변수 16
#이전 평균 30일 전일 장중 고가 - 전일 장중 저가
sk_stock$bd_h_l_mean <- roll_mean(sk_stock$bd_high_low_mins, 30)


# 추가로 더 만드시기를...............







# 3. 2번의 결과 데이터프레임을 파일로 저장하시오.
setwd('c:/temp/r')
write.csv( air2, "dataset3.csv" ,  row.names = F)




