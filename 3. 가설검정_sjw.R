#################################################################################
# 3. 가설 검정
#################################################################################

# 1. 준비작업 -----------------------------------------------------------

# 1.1 준비작업 : 필요한 라이브러리 로딩, 데이터셋 가져오기


# 1.1 준비작업 : 필요한 라이브러리 로딩, 데이터셋 가져오기
if (!require(caret)) { install.packages('caret') ; library(caret)}
if (!require(tidyverse)) { install.packages('tidyverse') ; library(tidyverse)}
if (!require(mosaic)) { install.packages('mosaic') ; library(mosaic)}

setwd("D:/공부/회사_데이터분석_2020/7_최종프로젝트/1day")  #작업폴더 지정
data <- read.csv("credit_NA.csv", na.strings = c("", "NA", "NULL","unkown"))
str(data)


# 1.2 데이터 구조를 살펴봅니다.

data$Creditability <- as.factor(data$Creditability)
data$AccountBalance <- as.factor(data$AccountBalance)

data$Payment <- as.factor(data$Payment)
data$Purpose <- as.factor(data$Purpose)

data$Employment <- as.factor(data$Employment)
data$SexMarital <- as.factor(data$SexMarital)
data$CurrentAddress <- as.factor(data$CurrentAddress)
data$MostValuable <- as.factor(data$MostValuable)

data$AppartmentType <- as.factor(data$AppartmentType)
data$NoCredits <- as.factor(data$NoCredits)
data$Occupation <- as.factor(data$Occupation)
data$Telephone <- as.factor(data$Telephone)
data$ForeignWorker <- as.factor(data$ForeignWorker)

str(data)


# 2. 가설검정을 수행합니다. ----------------------------------------------

# 설명 ---------------------------------
# 범주형 Target vs 연속형 Feature는 로지스틱 회귀 모형을 만들고 모델을 열어서 p value를 살펴봅니다.

# model <- glm(Creditability ~ Age, data = data, family = 'binomial')
# summary(model)

# 범주형 Target vs 범주형 Feature는 chi-square Test를 수행합니다.

# chisq.test(data$Creditability, data$AccountBalance)


# ------------------------------------------


# 범주(feature) ==> 범주(Target)
# 
# chi <- chisq.test(mobile$REPORTED_SATISFACTION, mobile$CHURN)
# chi$p.value
# ifelse(chi$p.value >= 0.05, "귀무가설 채택", "귀무가설 기각")
# 
# # 숫자(feature) ==> 범주(Target)
# 
# model <- glm( CHURN ~ INCOME, data = mobile  , family = 'binomial')
# summary(model)$coefficients
# ifelse(summary(model)$coefficients[2,4] >= 0.05, "귀무가설 채택", "귀무가설 기각")


# EDA 실습 파일의 2.3 결과에서 3그룹으로 feature들을 분류하였습니다.
# 2.1 1그룹 : 관련이 높은 변수들에 대해서 가설검정을 수행하시오.
# AccountBalance - 잔액여부, 통장개설여부에 따라 명확하게 신용도가 나뉨
# AppartmentType - 렌트인 경우 신용도 높음
# NoCredits - 대출건수가 4~5건일때 신용도가 가장 높으며, 정규분포에 가까운 곡선을 보인다.

chi <- chisq.test(data$Creditability, data$AccountBalance)
chi$p.value
ifelse(chi$p.value >= 0.05, "귀무가설 채택", "귀무가설 기각")

chi <- chisq.test(data$Creditability, data$AppartmentType)
chi$p.value
ifelse(chi$p.value >= 0.05, "귀무가설 채택", "귀무가설 기각")

chi <- chisq.test(data$Creditability, data$NoCredits)
chi$p.value
ifelse(chi$p.value >= 0.05, "귀무가설 채택", "귀무가설 기각")

# 2.2 2그룹 : 애매한 변수들에 대해서 가설검정을 수행하시오.
# Telephone - 비슷비슷
# MostValuable - 추가 정보 필요
# Employment (나이와 조합필요)
# CreditDuration, CreditAmount (보다 많은 데이터 필요)
chi <- chisq.test(data$Creditability, data$Telephone)
chi$p.value
ifelse(chi$p.value >= 0.05, "귀무가설 채택", "귀무가설 기각")

chi <- chisq.test(data$Creditability, data$MostValuable)
chi$p.value
ifelse(chi$p.value >= 0.05, "귀무가설 채택", "귀무가설 기각")

chi <- chisq.test(data$Creditability, data$Employment)
chi$p.value
ifelse(chi$p.value >= 0.05, "귀무가설 채택", "귀무가설 기각")

model <- glm( Creditability ~ CreditDuration, data = data  , family = 'binomial')
summary(model)$coefficients
ifelse(summary(model)$coefficients[2,4] >= 0.05, "귀무가설 채택", "귀무가설 기각")

model <- glm( Creditability ~ CreditAmount, data = data  , family = 'binomial')
summary(model)$coefficients
ifelse(summary(model)$coefficients[2,4] >= 0.05, "귀무가설 채택", "귀무가설 기각")

# 2.3 3그룹 : 관련이 적은 변수들에 대해서 가설검정을 수행하시오.
# CurrentAddress

chi <- chisq.test(data$Creditability, data$CurrentAddress)
chi$p.value
ifelse(chi$p.value >= 0.05, "귀무가설 채택", "귀무가설 기각")




# 2.4 연속형 변수(feature)들끼리 상관계수를 살펴봅시다.

cor(na.omit(data[,sapply(data, is.numeric)]))
