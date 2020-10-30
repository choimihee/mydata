#################################################################################
# 4. 모델링 : 기본 알고리즘(선형회귀, 로지스틱회귀, knn, 의사결정나무)
#################################################################################


# 1. 환경준비 -------------------------------------------------------------------------------

# 1.1 Library와 데이터 를 로딩합니다.

if (!require(caret)) { install.packages('caret') ; library(caret)}
if (!require(tidyverse)) { install.packages('tidyverse') ; library(tidyverse)}
if (!require(mosaic)) { install.packages('mosaic') ; library(mosaic)}
if (!require(rattle)) { install.packages('rattle') ; library(rattle)}


# 1.2 데이터 구조를 살펴보고, 형식에 맞게 변환합니다.

setwd("D:/공부/회사_데이터분석_2020/7_최종프로젝트/1day")  #작업폴더 지정
data <- read.csv("credit_NA.csv", na.strings = c("", "NA", "NULL","unkown")
                 , stringsAsFactors = T)

str(data)

data$Creditability <- as.factor(data$Creditability)
data$AccountBalance <- as.factor(data$AccountBalance)
data$Payment <- as.factor(data$Payment)
data$Purpose <- as.factor(data$Purpose)
data$Employment <- as.factor(data$Employment)
data$MostValuable <- as.factor(data$MostValuable)
data$SexMarital <- as.factor(data$SexMarital)
data$CurrentAddress <- as.factor(data$CurrentAddress)
data$AppartmentType <- as.factor(data$AppartmentType)
data$NoCredits <- as.factor(data$NoCredits)
data$Telephone <- as.factor(data$Telephone)
data$ForeignWorker <- as.factor(data$ForeignWorker)
data$Occupation <- as.factor(data$Occupation)

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




# 3. 모델링  ----------------------------------------------------------------------------

# 3.1 random forest 알고리즘을 이용하여 모델링하시오.
# cv을 수행하시오.
# random search, grid search 방법을 이용하여 최적화하시오.

fitC_5 <- trainControl(method = "cv", number = 5)

rf_model1 <- train(Creditability ~ . , data = train
                   , method = 'rf'
                   , trControl = fitC_5
                   , tuneLength = 5  )

rf_model1
# mtry  Accuracy   Kappa    
# 2    0.7062500  0.1282639
# 6    0.7171875  0.2783856
# 11    0.7187500  0.2866300
# 15    0.7187500  0.2927918
# 20    0.7078125  0.2664661

hparams <- expand.grid(mtry = 8:14)
rf_model2 <- train(Creditability ~ . , data = train
                   , method = 'rf'
                   , trControl = fitC_5
                   , tuneGrid = hparams)

rf_model2
# mtry  Accuracy   Kappa    
# 3     0.7078125  0.2212021
# 4     0.7031250  0.2307934
# 5     0.7093750  0.2550796
# 6     0.7078125  0.2620397
# 7     0.7140625  0.2795667

# 3.3 xgbTree 알고리즘을 이용하여 모델링하시오.
# cv을 수행하시오.
# random search, grid search 방법을 이용하여 최적화하시오.


xgb_model1 <- train(Creditability ~ . , data = train
                    , method = 'xgbTree'
                    , trControl = fitC_5
                    , tuneLength = 3  )

xgb_model1

xgb_model1$bestTune
# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 29     100         2 0.3     0              0.8                1       0.5

hparams <- expand.grid(eta = c(0.1, 0.2, 0.3)  # 3
                       , nrounds = c(45, 50, 55) # 4
                       , max_depth = 2:5  #4
                       , subsample = 1  
                       , gamma = 0
                       , colsample_bytree = 0.8
                       , min_child_weight = 1
)

xgb_model2 <- train(Creditability ~ . , data = train
                    , method = 'xgbTree'
                    , trControl = fitC_5
                    , tuneGrid = hparams)

xgb_model2$bestTune
# nrounds max_depth eta gamma colsample_bytree min_child_weight subsample
# 26      50         2 0.3     0              0.8                1         1

# 4. 예측 및 평가를 수행하시오.

val_pred_rf <- predict(rf_model2, newdata = val)
confusionMatrix(val_pred_rf, val$Creditability, positive = '1')
# Accuracy : 0.7625

val_pred_xgb <- predict(xgb_model2, newdata = val)
confusionMatrix(val_pred_xgb, val$Creditability, positive = '1')
# Accuracy : 0.7562

# 두 모델의 평가결과(confusion matrix)를 별도 변수로 저장하시오.
# 7번 과제 수행시 사용하게 됩니다.
cm_6_rf <- confusionMatrix(val_pred_rf, val$Creditability, positive = '1')
cm_6_xgb <- confusionMatrix(val_pred_xgb, val$Creditability, positive = '1')




#-----------------------------------------------------------------------------------------------------