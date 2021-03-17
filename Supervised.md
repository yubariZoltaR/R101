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
knnFit <- train(Class~.,              # 타겟 / 물결표시 다음 점은 피쳐를 모두 
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
```

#### 3\. Naive Bayes Classification
Bayes Theorem 활용 𝑃(𝑋│𝑌)=(𝑃(𝑌│𝑋)𝑃(𝑋))/(𝑃(𝑌)) -> 자세한건 pdf참조(p.30)     
앞에서와 마찬가지로 method에는 단순한 "naive_bayes" 외에도 "nbDiscrete", "manb", "awnb", "nb"가존재     
결과에 나오는 usekernel : 커널밀도추정으로 smoothing / adjust : bandwidth조정으로 추정커널밀도함수 모양변화 / laplace : 라플라스 스무딩.적은 데이터에서 극단적인 값 추정 방지      
결과창 마지막의 adjust는 usekernel = True일때 유의미한 값이다.     
앞에서까지 train,test구분까지 모든 것은 동일하다. 이후 predict이나 변수중요도 과정 역시 동일로 생략한다.   
``` r
ctrl <- trainControl(method = "repeatedcv", repeats = 5)
nbFit <- train(Class~.,
               data=train,
               method = "naive_bayes",
               trControl = ctrl,
               preProcess = c("center", "scale"),
               metric="Accuracy")
nbFit
plot(nbFit)
``` 

#### 4\. Decision Tree & Random Forest
이론 자체는 pdf참조. 여기서 설명하기엔 복잡.      
Decision Tree의 단점인 Overfitting 보완하기위해 Random Forest 활용   
Random Forest : n개의 랜덤한 데이터 샘플 중복해서 추출, d개의 feature을 중복없이 추출하여 Decision Tree학습하고 결과할당 ->for detail pdf   

train,test까지는 위와 동일한 과정으로 수행함.  
``` r
#Decision Tree

install.packages("tree")
library(tree)
treeRaw <- tree(Class~., data=train)
plot(treeRaw)
text(treeRaw) # 여기까지 하면 그림이 그려질 것

cv_tree <- cv.tree(treeRaw, FUN=prune.misclass) # FUN : 가지치기 함수 선택 (prune.misclass는 오분류기준)
plot(cv_tree)
prune_tree <- prune.misclass(treeRaw, best=4) #best=4는 cv를 통해 구한 사이즈
plot(prune_tree)
text(prune_tree, pretty=0) #pretty=0은 분할피쳐 이름을 바꾸지 않겠다는 것

pred <- predict(prune_tree, test, typle='class')
confusionMatrix(pred,test$Class)

#Random Forest
ctrl <- trainControl(method="repeatedcv",repeats = 5)  
rfFit <- train(Class ~ ., 
               data = train, 
               method = "rf", 
               trControl = ctrl, 
               preProcess = c("center","scale"),
               metric="Accuracy")                     # 결과 창의 mtry 는 트리에서 랜덤하게 선택되는 분할 피쳐 후보 개수
```

#### 5\. SVM(Support Vector Machine)
이론은 복잡하므로 pdf참고
선형 SVM과 비선형 SVM / 결과창을 보면 선형의 경우 단순 / 비선형은 degree: polynomial degree, scale은 다항식의 parameter scaling, C는 cost   
마찬가지로 train,test 설정까지 동일. 이후 prediction, importance까지 동일.   
선형은 비선형의 비해 train데이터의 정확도는 낮지만, test 데이터의 정확도는 높음. 이는 비선형방식이 overfitting 가능성이 높다는 뜻.    
``` r
ctrl <- trainControl(method="repeatedcv",repeats = 5)  
svm_linear_fit <- train(Class ~ ., 
                         data = train, 
                         method = "svmLinear",                    # 비선형의 경우 method = "svmPoly"
                         trControl = ctrl, 
                         preProcess = c("center","scale"),
                         metric="Accuracy")
svm_linear_fit
``` 







