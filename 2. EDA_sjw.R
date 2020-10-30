#################################################################################
# 2. EDA
#################################################################################

# 가설수립 결과를 참조하며 EDA를 수행합시다.

# 1. 준비작업 ---------------------------------------------

# 1.1 준비작업 : 필요한 라이브러리 로딩, 데이터셋 가져오기
library(tidyverse)

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

str(data$Creditability)


# 2. EDA ----------------------------------------------------
# 차트 그리는 연습이 아닙니다. 
# 차트를 그리고, 데이터를 보고, 파악(혹은 상식적으로 예상)되는 내용을 주석으로 적어봅시다.

# 2.1 1단계 : 모든 변수에 대해서, 기초통계량과 시각화를 통해 개별 분포를 확인하시오.

# 기초통계량 
summary(data)

# 범주형 데이터
table(data$Creditability)
prop.table(table(data$Creditability))
qplot(Creditability, data= data, geom = 'bar')

# 숫자형
qplot(Age, data= data, geom = 'histogram')
hist(data$Age, breaks = 50)
boxplot(data$Age, horizontal = T)
# 나이 구간은 19~75, 2~30 


# 2.2 2단계 : 가설에서 도출된 요인과 Target 변수와의 관계를 시각화하여 파악하시오.
# 가설 외의 변수도 포함하여 살펴 봅시다.
# 차트 그리는 연습이 아니라 데이터를 보고, 파악(혹은 상식적으로 예상)되는 내용을 적어 봅시다.



#  가설 1: 현재 신용도가 높으면 미래의 신용도도 높을 것이다.
table(data$Creditability)

#  가설 2: 은행잔고가 있으면 신용도가 높을 것이다.
table(data$AccountBalance)
prop.table(table(data$AccountBalance))
qplot(AccountBalance, data= data, geom = 'bar')

mosaicplot(AccountBalance ~ Creditability, data = data, color = TRUE)
abline(a= 0.7, b=0, col = 'red')
# 주석: 잔고가 있는 사람이 신용도가 높고, 통장이 있으면 통장이 없는 사람보다 신용도가 높다.

#  가설 3: 신청한 대출기간이 길수록 신용도가 높을 것이다
qplot(CreditDuration, data= data, geom = 'histogram')
hist(data$CreditDuration, breaks = 50)
boxplot(data$CreditDuration, horizontal = T)

qplot(CreditDuration, data = data, geom = 'density', colour = Creditability)
# 주석: 대출기간이 짧을수록 신용도가 높은 경향이 있다.


#  가설 4: 신청한 대출금액이 많을수록 신용도가 높을 것이다
qplot(CreditAmount, data= data, geom = 'histogram')
hist(data$CreditAmount, breaks = 50)
boxplot(data$CreditAmount, horizontal = T)

qplot(CreditAmount, data = data, geom = 'density', colour = Creditability)
# 주석: 대출금액이 적을수록 신용도가 높은 경향이 있다.


#  가설 5: 근무기간이 길면 신용도가 높을 것이다
table(data$Employment)
prop.table(table(data$Employment))
qplot(Employment, data= data, geom = 'bar')

mosaicplot(Employment ~ Creditability, data = data, color = TRUE)
abline(a= 0.7, b=0, col = 'red')
# 주석: 근무기간이 길수록 신용도은 높은 경향이 있다.


#  가설 6: 거주기간이 길면 신용도가 높을 것이다
table(data$CurrentAddress)
prop.table(table(data$CurrentAddress))
qplot(CurrentAddress, data= data, geom = 'bar')

mosaicplot(CurrentAddress ~ Creditability, data = data, color = TRUE)
abline(a= 0.7, b=0, col = 'red')
# 주석: 유의미한 차이가 없는것으로 보인다.


#  가설 7: 부동산이 가장 가치있는 자산인 경우 신용도가 높을 것이다
table(data$MostValuable)
prop.table(table(data$MostValuable))
qplot(MostValuable, data= data, geom = 'bar')

mosaicplot(MostValuable ~ Creditability, data = data, color = TRUE)
abline(a= 0.7, b=0, col = 'red')
# 주석: 부동산을 소유한 경우 대출을 일으킬 필요가 적어 신용도가 오히려 낮은 것으로 보인다.


#  가설 8: 자가인 경우 신용도가 높을 것이다
table(data$AppartmentType)
prop.table(table(data$AppartmentType))
qplot(AppartmentType, data= data, geom = 'bar')

mosaicplot(AppartmentType ~ Creditability, data = data, color = TRUE)
abline(a= 0.7, b=0, col = 'red')
# 주석: 부동산을 소유한 경우 대출을 일으킬 필요가 적어 신용도가 오히려 낮은 것으로 보인다.
#       주택 무소유자의 경우에는 대출상환능력이 없어 신용도가 낮은 것으로 판단된다.


#  가설 9: 대출건수가 적을수록 신용도가 높을 것이다
table(data$NoCredits)
prop.table(table(data$NoCredits))
qplot(NoCredits, data= data, geom = 'bar')

mosaicplot(NoCredits ~ Creditability, data = data, color = TRUE)
abline(a= 0.7, b=0, col = 'red')
# 대출상환능력과 대출상환으로 인한 신용도 상승을 감안했을때,
# 4~5 건 정도의 대출이 있는 사람이 가장 신용등급이 높으며,
# 이보다 대출이 적으면 대출상환에 따른 신용도 상승이 적어 신용도가 떨어지고,
# 이보다 대출이 많으면 대출상환능력에 의문이 있어 신용도가 떨어진다.


#  가설 10: 전화기가 있으면 신용도가 높을 것이다
table(data$Telephone)	
prop.table(table(data$Telephone))	
qplot(Telephone, data= data, geom = 'bar')

mosaicplot(Telephone ~ Creditability, data = data, color = TRUE)
abline(a= 0.7, b=0, col = 'red')
# 주석: 전화기가 있는 사람들이 신용도가 높은 경향이 있다.





# 가설1


# 가설2


# 가설3


# ...


# 가설n



# 가설 외의 변수1



# 가설 외의 변수2

library(mosaic)
data$Purpose <-  as.factor(data$Purpose)
data$Creditability <-  as.factor(data$Creditability)
data$ForeignWorker <-  as.factor(data$ForeignWorker)


mosaicplot(Creditability ~ Purpose, data = data, color = TRUE)
mosaicplot(Purpose ~ Creditability, data = data, color = TRUE)
abline(a= 0.68, b=0, col = 'red')


mosaicplot(ForeignWorker ~ Creditability, data = data, color = TRUE)
abline(a= 0.68, b=0, col = 'red')

prop.table(table(data$Creditability, data$Purpose),2)


# Age와 Telephone를 조합하여 변수를 만들려면...
# 1) 먼저 연속형 변수를 범주로...
data$AgeGroup <- cut(data$Age, breaks = c(0,40,100), labels = c(0,1))
data %>% head()
# 2) 두 범주를 조합.
ifelse(data$AgeGroup == 0 & data$Telephone == 1
       , 1
       , ifelse(data$AgeGroup == 1 & data$Telephone == 1
          , 2
          , ifelse(data$AgeGroup == 1 & data$Telephone == 2, 3, 4)
          )
       )

# 2.3 Target와의 관계에 대해서 다음 세가지 그룹으로 변수를 구분하시오.
# ① 관련이 높은 변수들
AccountBalance - 잔액여부, 통장개설여부에 따라 명확하게 신용도가 나뉨
AppartmentType - 렌트인 경우 신용도 높음
NoCredits - 대출건수가 4~5건일때 신용도가 가장 높으며, 정규분포에 가까운 곡선을 보인다.

# ② 애매한 변수들
Telephone - 비슷비슷
MostValuable - 추가 정보 필요
Employment (나이와 조합필요)
CreditDuration, CreditAmount (보다 많은 데이터 필요)

# ③ 관련이 없어 보이는 변수들
CurrentAddress





