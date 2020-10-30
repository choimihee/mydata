#################################################################################
# 5. 일반화 성능과 overfitting
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



#---------------------------------------------------------------------------------------------
# 2번문제까지는 4 모델링1의 결과를 가져다 붙여 넣으세요.

# 3. cross validation ---------------------------------------------------------------------
# 10 fold cv 을 적용하여 모델링을 수행하시오.
fitC_5 <- trainControl(method = "cv", number = 5)

# 3.1 knn
model_knn <- train(Creditability ~ ., data= train, method = "knn"
                   , trControl = fitC_5)

val_pred_knn <- predict(model_knn, newdata = val)
confusionMatrix(val_pred_knn, val$Creditability, positive = '1')


# 모델 생성 후 validation accuracy를 조회하시오.
# Accuracy : 0.75



# 3.2 decision tree
model_dt <- train(Creditability ~ ., data= train, method = "rpart2"
                  , trControl = fitC_5)
val_pred_dt <- predict(model_dt, newdata = val)
confusionMatrix(val_pred_dt, val$Creditability, positive = '1')



# 모델 생성 후 validation accuracy를 조회하시오.
# Accuracy : 0.725  

# 4. overfitting ----------------------------------------------------------------------------
# cv 을 적용하지 말고 모델링을 수행하시오.
# 다음 두가지 알고리즘에 대해서 복잡도를 증가시켜가며
fitC_none <- trainControl(method = "none")

# 4.1 knn
# 모델을 만들고
# train과 validation 셋으로 예측하고
# 결과(정분류율)에 대해 fitting graph를 그려보시오.
# overfitting을 회피하기 위한 적절한 지점의 복잡도는?
# k = 8


fitting_graph <- data.frame(k = integer(), tr = numeric(), va = numeric())

for (i in 1:20) {
  model <- train(Creditability ~ ., data= train
                 , method = "knn"
                 , trControl = fitC_none
                 , tuneGrid = expand.grid(k = i))
  # 예측
  ds.results.tr <- predict(model, newdata=train)
  ds.results.va <- predict(model, newdata=val)
  
  # 평가
  tr.conf <- confusionMatrix(ds.results.tr, train$Creditability, positive = "1")
  va.conf <- confusionMatrix(ds.results.va, val$Creditability, positive = "1")
  #tr.conf$overall
  fitting_graph[nrow(fitting_graph)+1,] <- c(i
                                             , tr.conf$overall[1]
                                             , va.conf$overall[1])
  print(i)
}

fitting_graph


# fitting graph
ggplot() + 
  geom_line(data = fitting_graph, aes(x = k, y = tr), color = "blue") +
  geom_line(data = fitting_graph, aes(x = k, y = va), color = "red") +
  xlab('k') +
  ylab('accuracy')





# 4.2 decision tree
# 모델을 만들고
# train과 validation 셋으로 예측하고
# 결과(정분류율)에 대해 fitting graph를 그려보시오.
# overfitting을 회피하기 위한 적절한 지점의 복잡도는?
# k = 7
fitting_graph <- data.frame(maxdepth = integer(), tr = numeric(), va = numeric())

for (i in 1:20) {
  model <- train(Creditability ~ ., data= train
                 , method = "rpart2"
                 , trControl = fitC_none
                 , tuneGrid = expand.grid(maxdepth = i))
  # 예측
  ds.results.tr <- predict(model, newdata=train)
  ds.results.va <- predict(model, newdata=val)
  
  # 평가
  tr.conf <- confusionMatrix(ds.results.tr, train$Creditability, positive = "1")
  va.conf <- confusionMatrix(ds.results.va, val$Creditability, positive = "1")
  #tr.conf$overall
  fitting_graph[nrow(fitting_graph)+1,] <- c(i
                                             , tr.conf$overall[1]
                                             , va.conf$overall[1])
  print(i)
}

fitting_graph


# fitting graph
ggplot() + 
  geom_line(data = fitting_graph, aes(x = maxdepth, y = tr), color = "blue") +
  geom_line(data = fitting_graph, aes(x = maxdepth, y = va), color = "red") +
  xlab('maxdepth') +
  ylab('accuracy')



# 5. 예측 및 평가 -----------------------------------------------------------------
# 4번에서 적절한 복잡도로 만든 두 모델에 대해서 test 셋으로 예측하고 평가해 봅시다.
# 4.1 knn
model_knn <- train(Creditability ~ ., data= train
                   , method = "knn"
                   , trControl = fitC_none
                   , tuneGrid = expand.grid(k = 8))

val_pred_knn <- predict(model_knn, newdata = val)
confusionMatrix(val_pred_knn, val$Creditability, positive = '1')

# 3.2 decision tree
model_dt <- train(Creditability ~ ., data= train, method = "rpart2"
                  , trControl = fitC_5
                  , tuneGrid = expand.grid(maxdepth = 7))
val_pred_dt <- predict(model_dt, newdata = val)
confusionMatrix(val_pred_dt, val$Creditability, positive = '1')




# 두 모델의 평가결과(confusion matrix)를 별도 변수로 저장하시오.
# 7번 과제 수행시 사용하게 됩니다.
cm_5_knn <- confusionMatrix(val_pred_knn, val$Creditability, positive = '1')
cm_5_dt <- confusionMatrix(val_pred_dt, val$Creditability, positive = '1')



#------------------------------------------------------------------------------