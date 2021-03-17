##  Supervised Learning(지도학습)

Standardization : (data - avg) / s.d
MinMax Scale = (data - min(data)) / (max(data) - min(data)) : 0~1 사이로 데이터를 위치시킴

모형평가개념 
![image](https://user-images.githubusercontent.com/80673078/111432382-4cbc5600-8740-11eb-96b6-5dfbe666e963.png)

ROC curve (x축에 false positive rate, y축에 민감도) : 곡선 아래의 면적(AUC)가 넓을수록 좋은 모델이며 1에 가까울수록 좋다. 
MSE(Mean Squared Error) 

Overfitting, Underfitting

Cross-Validation : 데이터를 train용과 test용으로 분리한 이후 정확도 측정에 test 데이터 사용 (종류는 다양)


#### 1\. k-nearest neighbor
가장 가까이 있는 데이터에 속한다고 보는 방법

kappa통계량 : (관측된 정확도 - 기대 정확도) / (1 - 기대 정확도) -1과 1사이이며 1에 가까울수록 좋다.

``` r
install.packages("caret", dependencies = TRUE)
library(caret)
rawdata <- read.csv("data.csv", header= TRUE)
rawdata$Class <- as.factor(rawdata$Class)
str(rawdata)

analdata <- rawdata
set.seed(2021)                        # 같은 결과 도출하도록 특정 숫자 지정. 
datatotal <- sort(sample(nrow(analdata), nrow(analdata)*0.7)) 
# 7:3으로 train, test 데이터 분리, sort는 결과 편의를 위한 오름차순 정리, nrow는 데이터행수, sample(a,b)는 1부터 a까지 숫자중 b추출
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]
train_x <- train[,1:13]
train_y <- train[,14]
test_x <- test[,1:13]
test_y <- test[,14]

ctrl <- trainControl(
          method = "repeatedcv",      # cross-validation 반복
          number = 10,                # 훈련 데이터 fold 개수
          repeats = 5                 # cv반복 횟수
) 
customGrid <- expand.grid(k=1:10)     # 벡터, 인자 조합인 데이터프레임을 생성
knnFit <- train(Class~.,              # 타겟
                data = train,
                method = "knn",                     # 사용할 머신러닝 방법
                trControl = trainControl(),         # 학습 방법
                preProcess = c("center", "scale"),  # 표준화
                tuneGrid = expand.grid(k=1:10)      # 튜닝 파라미터 값 목록
                metric = "Accuracy")                # 모형 평가 방식
knnFit
plot(knnFit)

pred_test <- predict(knnFit, newdata=test) #test데이터를 잘 예측하는지
confusionMatrix(pred_test, test$Class)

importance_knn <- varImp(knnFit, scale = FALSE) #변수 중요도
plot(importance_knn)
``` 

#### 2\. Logistic Regression
선형회귀분석이 종속변수가 연속형만 가능하며 무제한범위를 갖지만, 로지스틱 회귀분석은 범주형, 연속형 모두 가능하나 범위는 제한이 있다.   
z=α+𝛽𝑥 에서 z에 제한이 있도록 하기 위해서 log⁡(𝑦/(1−𝑦))=𝛼+𝛽𝑥 형태로 변형.   
로지스틱 회귀분석 train method에는    
Boosted Logistic Regression : method = "LogitBoost" 가장 간단한 모형에서 시작해 여러 피쳐를 더하는 방식   
Logistic Model Trees : method = "LMT" 의사결정나무와 결합된 모형   
Penalized Logistic Regression : method = "plr" 정규화를 두어서 모델의 복잡성을 조절함으로써 오버피팅 회피 (𝜆의 크기에 따라 베타값의 영역 달라짐)   
Regularized Logistic Regression : method = "regLogistic"    

``` r
rawdata2 <- read.csv("data2.csv", header= TRUE)
str(rawdata2)
rawdata2$target <- as.factor(rawdata2$target)
# 이하 연속형 독립변수 표준화
rawdata$age <- scale(rawdata$age)
rawdata$trestbps <- scale(rawdata$trestbps)
rawdata$chol <- scale(rawdata$chol)
rawdata$thalach <- scale(rawdata$thalach)
rawdata$oldpeak <- scale(rawdata$oldpeak)
rawdata$slope <- scale(rawdata$slope)
# 이하 범주형 독립변수 표준화
newdata <- rawdata
factorVar <- c("sex", "cp", "fbs", "restecg", "exang", "ca", "thal")
newdata[ ,factorVar] = lapply(newdata[ ,factorVar], factor)

set.seed(2021)                        
datatotal <- sort(sample(nrow(newdata), nrow(newdata)*0.7)) 
train <- rawdata[datatotal,]
test <- rawdata[-datatotal,]
train_x <- train[,1:13]
train_y <- train[,14]
test_x <- test[,1:13]
test_y <- test[,14]

ctrl <- trainControl(method = "repeatedcv", repeats=5)
logitFit <- train(target~.,
                  data=train,
                  method = "LogitBoost",
                  trControl = ctrl,
                  metric = "Accuracy")
logitFit
plot(logitFit)

pred_test <- predict(logitFit, newdata=test)
confusionMatrix(pred_test, test$target)

importance_logit <- varImp(logitFit, scale =FALSE)
plot(importance_logit)



