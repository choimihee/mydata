#################################################################################
# 4. 모델링 : 기본 알고리즘(선형회귀, 로지스틱회귀, knn, 의사결정나무)
#################################################################################

# 1. 환경준비 -------------------------------------------------------------------------------

# 1.1 준비작업 : 필요한 라이브러리 로딩, 데이터셋 가져오기

if (!require(caret)) { install.packages('caret') ; library(caret)}
if (!require(tidyverse)) { install.packages('tidyverse') ; library(tidyverse)}
if (!require(mosaic)) { install.packages('mosaic') ; library(mosaic)}
if (!require(rattle)) { install.packages('rattle') ; library(rattle)}

setwd("D:/공부/회사_데이터분석_2020/7_최종프로젝트/1day")  #작업폴더 지정
data <- read.csv("credit_NA.csv", na.strings = c("", "NA", "NULL","unkown")
  , stringsAsFactors = T)

str(data)



# 1.2 데이터 구조를 살펴봅니다.
data$Creditability <- as.factor(data$Creditability)
data$AccountBalance <- as.factor(data$AccountBalance)

# data$Payment <- as.factor(data$Payment)
# data$Purpose <- as.factor(data$Purpose)

data$Employment <- as.factor(data$Employment)
# data$SexMarital <- as.factor(data$SexMarital)
data$CurrentAddress <- as.factor(data$CurrentAddress)
data$MostValuable <- as.factor(data$MostValuable)

data$AppartmentType <- as.factor(data$AppartmentType)
data$NoCredits <- as.factor(data$NoCredits)
# data$Occupation <- as.factor(data$Occupation)
data$Telephone <- as.factor(data$Telephone)
# data$ForeignWorker <- as.factor(data$ForeignWorker)

str(data)


# 2. 데이터 준비 ----------------------------------------------------------------------------

# 2.1 Target과 관련이 거의 없는 변수를 제외한 데이터프레임을 만듭시다.

str(data)
names(data)
data2 <- data[ , c(-4,-5,-8,-11,-14,-16)]

str(data2)

# 2.2 다음의 전처리 작업을 수행합니다.
# 아래 항목에 대해서 필요하다고 판단될 경우 순서에 맞게 수행합니다.
# NA 확인 및 조치
sum(is.na(data2))
# 만약 결측치가 있는 경우 na.omit() 을 사용한다. (강사님이 쓰지 말랬음)
# na.rm = TRUE 으로 처리, 아니면 채워넣거나

# Scaling

#preProcess(   ,   method = c( 'range', 'knnImpute'))

# data split
#   -- set.seed 를 사용하시오.
#   -- 먼저 train_val : test = 8 : 2 로 분할합니다.
#   -- 그 다음 train_val을 train : validation = 8 : 2 로 분할합니다.
set.seed(12345)
idx1 <- sample(nrow(data2), size = 0.8 * nrow(data2))
train_val <- data2[idx1,]
test <- data2[-idx1,]

set.seed(12345)
idx2 <- sample(nrow(train_val), size = 0.8 * nrow(train_val))
train <- train_val[idx2,]
val <- train_val[-idx2,]

proc_fn <- preProcess(train[ ,names(train) !="Creditability"], method = c("knnImpute", "range") )

train <- predict(proc_fn, newdata = train)
val <- predict(proc_fn, newdata = val)
test <- predict(proc_fn, newdata = test)





# 3. Training ----------------------------------------------------------------------------------
# cross validation은 수행하지 않습니다.
# 다음의 기본 알고리즘으로 모델링 수행
# 모든 모델링은 caret package 사용을 권장합니다.

# 3.1 로지스틱 회귀로 모델링을 수행하시오. 모델의 내용을 열어봅시다.

model_glm <- train(Creditability ~ . , data = train, method = 'glm')
model_glm
summary(model_glm)


# 전진 선택법으로 변수 선택하는 모델링 수행-  적절한것 하나씩 추가
# 1단계 Base Model 

base <- glm(Creditability ~ 1, data = train, family = 'binomial')
base <- train(Creditability ~  , data = train, method = 'glm')
base


# 2단계 Step
paste0(names(train), collapse = '+')

step_model <- step(base, 
                   Creditability ~ AccountBalance+CreditDuration+CreditAmount+Employment+CurrentAddress+MostValuable+AppartmentType+NoCredits+Telephone, direction = 'forward')

warnings()
step_model
summary(step_model)
plot(step_model)


# 3.2 knn, 모델의 내용을 열어봅시다.
model_knn <- train(Creditability ~ . , data = train, method = 'knn')
model_knn
summary(model_knn)



# 3.3 decision tree
# 모델 생성 후 모델을 시각화 합시다.
model_dt <- train(Creditability ~ . , data = train, method = 'rpart2')
model_dt
summary(model_dt)
model_dt$finalModel
fancyRpartPlot(model_dt$finalModel)


# 4. 예측 및 평가------------------------------------------------------------------------------
# 3번에서 만든 모델들에 대해서 validation set으로 예측하고, 평가해 봅시다.

pred_step <- predict(step_model, newdata = val, type = 'response')
pred_knn <- predict(model_knn, newdata = val)
pred_dt <- predict(model_dt, newdata = val)

pred_step_f <- ifelse(pred_step >=0.5, 1,0)
pred_step_f <- as.factor(pred_step_f)

confusionMatrix(pred_step_f, val$Creditability, positive = '1')
confusionMatrix(pred_knn, val$Creditability, positive = '1')
confusionMatrix(pred_dt, val$Creditability, positive = '1')



#------------------------------------------------------------------------------