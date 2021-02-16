Random Forest
================

> 배깅은 의사결정나무 만들 시, 데이터를 부트스트래핑 하여 여러개의 트리를 만들고, 이를 합쳐서(보통은 평균을 낸다고 한다)
> 분산을 줄이는 방식이라고 함. bias는 약간 상승할 수 있으나 분산을 줄임으로서 오버피팅을 방지한다는 것이다.

> 랜덤포레스트는, 부트스트랩시 분할에서 고려하는 변수를 계속 다르게 한다고 한다. 즉, 데이터 자체도 샘플링하지만, 변수의 개수
> 또한 샘플링하여 배깅보다 조금 더 정교하게 분산을 더 효과적으로 줄일 수 있다. 이를 통해 상관성이 적은 트리들이 생성되는
> 원리 (배깅과는 다른 과정 ) –\> 결과적으로 분산을 조금 더 줄일 수 있다.

> 전체 변수의 개수가 p개고 선택되는 개수는 m이라고 보자. m=p이면 배깅, 즉 모든 변수들이 각 분할 단계에서 고려된다는
> 의미

> 특정 변수가 있을때 없을때 OOB에러의 차이가 많이나면 당연히 중요한 변수로 인식하게 된다\!

> parameter는 ntree 와 mtry가 통상적. mtry가 변수의 개수와 똑같다면 배깅과 같은 방식이 된다. 즉 랜포로
> 배깅까지 할 수 있다는 의미일 것이다.

> parameter tuning과정은 CV를 통해 최적의 mtry값을 찾고 이를 전체 training set에 대해 모델을
> 적합시킨다. 그리고 홀드아웃으로 테스트를 하여 최종 모델을 선택할 수 있겠지.

# \[0\] 데이터 준비

``` r
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 3.6.3

``` r
library(rmarkdown)
```

    ## Warning: package 'rmarkdown' was built under R version 3.6.3

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
data <- fread('heart.csv') %>% na.omit
data <- data %>% select(-V1)
str(data)
```

    ## Classes 'data.table' and 'data.frame':   297 obs. of  14 variables:
    ##  $ Age      : int  63 67 67 37 41 56 62 57 63 53 ...
    ##  $ Sex      : int  1 1 1 1 0 1 0 0 1 1 ...
    ##  $ ChestPain: chr  "typical" "asymptomatic" "asymptomatic" "nonanginal" ...
    ##  $ RestBP   : int  145 160 120 130 130 120 140 120 130 140 ...
    ##  $ Chol     : int  233 286 229 250 204 236 268 354 254 203 ...
    ##  $ Fbs      : int  1 0 0 0 0 0 0 0 0 1 ...
    ##  $ RestECG  : int  2 2 2 0 2 0 2 0 2 2 ...
    ##  $ MaxHR    : int  150 108 129 187 172 178 160 163 147 155 ...
    ##  $ ExAng    : int  0 1 1 0 0 0 0 1 0 1 ...
    ##  $ Oldpeak  : num  2.3 1.5 2.6 3.5 1.4 0.8 3.6 0.6 1.4 3.1 ...
    ##  $ Slope    : int  3 2 2 3 1 1 3 1 2 3 ...
    ##  $ Ca       : int  0 3 2 0 0 0 2 0 1 0 ...
    ##  $ Thal     : chr  "fixed" "normal" "reversable" "normal" ...
    ##  $ AHD      : chr  "No" "Yes" "Yes" "No" ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
data$Sex <- data$Sex %>% as.factor()
data$ChestPain <- data$ChestPain %>% as.factor()
data$Thal <- data$Thal %>% as.factor()
data$AHD <- data$AHD %>% as.factor()
str(data)
```

    ## Classes 'data.table' and 'data.frame':   297 obs. of  14 variables:
    ##  $ Age      : int  63 67 67 37 41 56 62 57 63 53 ...
    ##  $ Sex      : Factor w/ 2 levels "0","1": 2 2 2 2 1 2 1 1 2 2 ...
    ##  $ ChestPain: Factor w/ 4 levels "asymptomatic",..: 4 1 1 2 3 3 1 1 1 1 ...
    ##  $ RestBP   : int  145 160 120 130 130 120 140 120 130 140 ...
    ##  $ Chol     : int  233 286 229 250 204 236 268 354 254 203 ...
    ##  $ Fbs      : int  1 0 0 0 0 0 0 0 0 1 ...
    ##  $ RestECG  : int  2 2 2 0 2 0 2 0 2 2 ...
    ##  $ MaxHR    : int  150 108 129 187 172 178 160 163 147 155 ...
    ##  $ ExAng    : int  0 1 1 0 0 0 0 1 0 1 ...
    ##  $ Oldpeak  : num  2.3 1.5 2.6 3.5 1.4 0.8 3.6 0.6 1.4 3.1 ...
    ##  $ Slope    : int  3 2 2 3 1 1 3 1 2 3 ...
    ##  $ Ca       : int  0 3 2 0 0 0 2 0 1 0 ...
    ##  $ Thal     : Factor w/ 3 levels "fixed","normal",..: 1 2 3 2 2 2 2 2 3 3 ...
    ##  $ AHD      : Factor w/ 2 levels "No","Yes": 1 2 2 1 1 1 2 1 2 2 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
head(data)
```

    ##    Age Sex    ChestPain RestBP Chol Fbs RestECG MaxHR ExAng Oldpeak Slope Ca
    ## 1:  63   1      typical    145  233   1       2   150     0     2.3     3  0
    ## 2:  67   1 asymptomatic    160  286   0       2   108     1     1.5     2  3
    ## 3:  67   1 asymptomatic    120  229   0       2   129     1     2.6     2  2
    ## 4:  37   1   nonanginal    130  250   0       0   187     0     3.5     3  0
    ## 5:  41   0   nontypical    130  204   0       2   172     0     1.4     1  0
    ## 6:  56   1   nontypical    120  236   0       0   178     0     0.8     1  0
    ##          Thal AHD
    ## 1:      fixed  No
    ## 2:     normal Yes
    ## 3: reversable Yes
    ## 4:     normal  No
    ## 5:     normal  No
    ## 6:     normal  No

# \[1\] 가장 기본적이면서도 복잡하고 정교한 방법

## \[1 - 1\] 데이터 준비

``` r
library(caret)
```

    ## Loading required package: lattice

    ## Loading required package: ggplot2

    ## Warning: package 'ggplot2' was built under R version 3.6.3

``` r
set.seed(1234)
index <- createDataPartition(data$AHD, p = .7, list = F) #이건 데이터를 나눠주는 함수.. 개꿀딱지임
data.train <- data[index,]
data.holdout <- data[-index,]
```

## \[1 - 2\] 기본 parameter로 적합시키기.

``` r
library(randomForest)
```

    ## randomForest 4.6-14

    ## Type rfNews() to see new features/changes/bug fixes.

    ## 
    ## Attaching package: 'randomForest'

    ## The following object is masked from 'package:ggplot2':
    ## 
    ##     margin

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
trControl <- trainControl(method = "cv",number = 10,  search = "grid") 
#cv방법으로 10fold. 즉 10-fold cv를 한다는 의미이다.
rf_default <- train(AHD~., data = data.train,
    method = "rf",
    metric = "Accuracy",
    trControl = trControl)

#결과를 추출하자
print(rf_default)
```

    ## Random Forest 
    ## 
    ## 208 samples
    ##  13 predictor
    ##   2 classes: 'No', 'Yes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 187, 188, 187, 187, 187, 188, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    2    0.8511905  0.7002209
    ##    9    0.8461905  0.6890933
    ##   16    0.8369048  0.6699299
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 2.

> #### 물론 이때 trainControl을 다르게 해줄 수 있다. 예를 들면

``` r
trControl <- trainControl(method = "repeatedcv",number = 10,  repeats = 3, search = "grid") 
```

> 이렇게 해주면 repeatedcv를 해준다는 뜻이다(3번) 근데 사용해보니 오히려 위 방법이 holdout에 대한 정확도를
> 떨어뜨렸다. 아마 과적합되게 만들지 않았을까 생각이 들긴 하는데… 사실 위 방법을 쓰니 maxnodes 값이 달라졌다.

> #### 당연히 search = ’random’으로 해주면 랜덤 서치를 해준다. mtry값은 데이터의 변수의 개수가 그 최대값이라서 오히려 그리드 서치 방법이 합리적일 수 있다.

### mtry = 2에서 가장 결과가 좋음을 알 수 있다. 여기서 더 발전시켜서 찾아보자.

### \[1 - 3\] mtry 찾기

``` r
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 10)) #기본에서 찾은 가장 좋은 mtry를 기준으로 넣어주면 된다. 
rf_mtry <- train(AHD~., data = data.train,
    method = "rf", metric = "Accuracy",
    tuneGrid = tuneGrid, #이렇게 그리드 서치를 진행해준다. 
    trControl = trControl,  importance = TRUE, nodesize = 14, ntree = 300)
print(rf_mtry)
```

    ## Random Forest 
    ## 
    ## 208 samples
    ##  13 predictor
    ##   2 classes: 'No', 'Yes' 
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 187, 187, 187, 187, 186, 188, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  Accuracy   Kappa    
    ##    1    0.8603896  0.7145781
    ##    2    0.8267965  0.6469603
    ##    3    0.8408658  0.6740855
    ##    4    0.8458658  0.6857825
    ##    5    0.8263420  0.6442382
    ##    6    0.8506277  0.6944733
    ##    7    0.8361039  0.6651380
    ##    8    0.8311039  0.6544295
    ##    9    0.8361039  0.6645554
    ##   10    0.8361039  0.6646240
    ## 
    ## Accuracy was used to select the optimal model using the largest value.
    ## The final value used for the model was mtry = 1.

``` r
plot(rf_mtry)
```

![](Random_Forest_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

> plot 으로 보면 조금 더 편할 수 있다. 근데 만약 최적의 mtry가 마지막 부분에서 급상승하는 그래프를 그린다면, 그보다
> 더 큰 mtry를 포함하는 parameter tuning을 다시 진행해줘야 할 것이다.

> 아마 여기의 결과는 5번의 cv의 평균 에러(여기서는 accuracy)일 것이다. 그니께 1:10까지의 파라미터의 결과가 총
> 다섯번이 나오는데 print(rf\_mtry)의 값은 이 다섯번의 결과를 평균낸 cv\_error일 것으로 예상된다. 사실
> 그래야 말이 되긴 함.

``` r
opt.mtry <- rf_mtry$bestTune$mtry
max(rf_mtry$results$Accuracy) #이거는 가장 높은 정확도!
```

    ## [1] 0.8603896

``` r
opt.mtry #가장 좋은 mtry는 1이다.. 즉 변수 하나만 쓰라는 이야기
```

    ## [1] 1

> #### 위와 같은 방식으로 mtry를 찾아주면 된다. 시간이 없다면 밑의 과정들은 생략해도 될듯? 실제로 랜포의 장점은 mtry만 찾고도 꽤나 좋은 성능을 내는 것이라고 하니

## \[1 - 4\] maxnodes 찾기

``` r
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = opt.mtry) #위에서 찾은 최고의 mtry를 넣어준다. 
for (maxnodes in c(5: 15)) {
    set.seed(1234)
    rf_maxnode <- train(AHD~.,
        data = data.train, method = "rf", metric = "Accuracy",
        tuneGrid = tuneGrid, #여기는 아까 찾은 베스트 mtry가 들어있다.
        trControl = trControl,importance = TRUE,nodesize = 14,
        maxnodes = maxnodes, #이제 이 값을 5부터 15까지 바꿔줄 것이다. 
        ntree = 300)
    current_iteration <- toString(maxnodes)
    store_maxnode[[current_iteration]] <- rf_maxnode
}
results_mtry <- resamples(store_maxnode)
result.maxnode<- summary(results_mtry)$statistics$Accuracy %>% as.data.frame()
opt.maxnodes <- as.numeric(row.names(result.maxnode[which.max(result.maxnode$Max.),])) # 뭐 대충 이런 방식으로 넣어주면 된다. 
```

## \[1 - 5\] ntree 찾기

``` r
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
    set.seed(1234)
    rf_maxtrees <- train(AHD~.,
        data = data.train,
        method = "rf",
        metric = "Accuracy",
        tuneGrid = tuneGrid,
        trControl = trControl,
        importance = TRUE,
        nodesize = 14,
        maxnodes = opt.maxnodes,
        ntree = ntree)
    key <- toString(ntree)
    store_maxtrees[[key]] <- rf_maxtrees
}
results_tree <- resamples(store_maxtrees)
summary(results_tree)
```

    ## 
    ## Call:
    ## summary.resamples(object = results_tree)
    ## 
    ## Models: 250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000 
    ## Number of resamples: 10 
    ## 
    ## Accuracy 
    ##           Min.   1st Qu. Median      Mean   3rd Qu.     Max. NA's
    ## 250  0.7500000 0.7646104  0.875 0.8510823 0.9047619 0.952381    0
    ## 300  0.7500000 0.7646104  0.850 0.8556061 0.9404762 1.000000    0
    ## 350  0.7500000 0.7646104  0.875 0.8558442 0.9404762 0.952381    0
    ## 400  0.7500000 0.7646104  0.875 0.8510823 0.9047619 0.952381    0
    ## 450  0.7142857 0.7646104  0.875 0.8510823 0.9404762 0.952381    0
    ## 500  0.7142857 0.7646104  0.875 0.8510823 0.9404762 0.952381    0
    ## 550  0.7142857 0.7646104  0.875 0.8510823 0.9404762 0.952381    0
    ## 600  0.7500000 0.7646104  0.875 0.8510823 0.9047619 0.952381    0
    ## 800  0.7500000 0.7646104  0.875 0.8510823 0.9047619 0.952381    0
    ## 1000 0.7619048 0.8023810  0.875 0.8608442 0.9047619 0.952381    0
    ## 2000 0.7142857 0.7646104  0.875 0.8463203 0.9047619 0.952381    0
    ## 
    ## Kappa 
    ##           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## 250  0.4791667 0.5300288 0.7406572 0.6964234 0.8099430 0.9041096    0
    ## 300  0.4791667 0.5300288 0.6875000 0.7032820 0.8799171 1.0000000    0
    ## 350  0.4791667 0.5300288 0.7427399 0.7048278 0.8799171 0.9049774    0
    ## 400  0.4791667 0.5300288 0.7427399 0.6953259 0.8086530 0.9049774    0
    ## 450  0.4000000 0.5300288 0.7427399 0.6955524 0.8799171 0.9049774    0
    ## 500  0.4000000 0.5300288 0.7427399 0.6955524 0.8799171 0.9049774    0
    ## 550  0.4000000 0.5300288 0.7427399 0.6955524 0.8799171 0.9049774    0
    ## 600  0.4791667 0.5300288 0.7427399 0.6953259 0.8086530 0.9049774    0
    ## 800  0.4791667 0.5300288 0.7427399 0.6953259 0.8086530 0.9049774    0
    ## 1000 0.4927536 0.5952671 0.7427399 0.7155016 0.8086530 0.9049774    0
    ## 2000 0.4000000 0.5300288 0.7427399 0.6860505 0.8086530 0.9049774    0

``` r
result.ntree<- summary(results_tree)$statistics$Accuracy %>% as.data.frame()
opt.ntree <- as.numeric(row.names(result.ntree[which.max(result.ntree$Max.),]))
```

> 결과적으로 opt.mtry, opt.ntree, opt.maxnodes를 얻게 되었다. 이 세가지의 파라미터를 가지고 최종
> 모델을 적합한다.

## \[1 - 6\] 최종모델 적합.

``` r
set.seed(1234)
fit_rf <- randomForest(AHD~.,
    data = data.train,
    metric = "Accuracy",
    mtry = opt.mtry, #우리가 찾은 베스트 mtry
    importance = TRUE,
    nodesize = 14,
    ntree = opt.ntree, #우리가 찾은 베스트 ntree
   maxnodes = opt.maxnodes) #우리가 찾은 베스트 maxnodes
```

## \[1 - 7\] 모델 평가

``` r
rf.prediction <-predict(fit_rf, data.holdout)
confusionMatrix(rf.prediction, data.holdout$AHD) #최종 accuracy : 0.7978
```

    ## Confusion Matrix and Statistics
    ## 
    ##           Reference
    ## Prediction No Yes
    ##        No  43  13
    ##        Yes  5  28
    ##                                           
    ##                Accuracy : 0.7978          
    ##                  95% CI : (0.6993, 0.8755)
    ##     No Information Rate : 0.5393          
    ##     P-Value [Acc > NIR] : 3.388e-07       
    ##                                           
    ##                   Kappa : 0.5871          
    ##                                           
    ##  Mcnemar's Test P-Value : 0.09896         
    ##                                           
    ##             Sensitivity : 0.8958          
    ##             Specificity : 0.6829          
    ##          Pos Pred Value : 0.7679          
    ##          Neg Pred Value : 0.8485          
    ##              Prevalence : 0.5393          
    ##          Detection Rate : 0.4831          
    ##    Detection Prevalence : 0.6292          
    ##       Balanced Accuracy : 0.7894          
    ##                                           
    ##        'Positive' Class : No              
    ## 

``` r
varImpPlot(fit_rf)
```

![](Random_Forest_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

# \[2\] CV 과정에서 data셋을 customize해주는 방법.

> #### 사실 이 방법은 **시계열 데이터**에 대해 모델링을 진행할 때 써야한다. 단순히 cv로 하게 되면 미래의 데이터로 학습하고 과거의 데이터를 예측하는 이상한 모델이 될 수 있기 때문이다\! 그래서 롤링 윈도우를쓰긴 하는데 ML에서는 아무래도 train set을 적당히 나눠주는 것 같다.

## \[2 - 1\] 일단 caret 패키지로 cv를 만드는 방법을 알아보자.

``` r
#train data 만들어주기
set.seed(1234) #set.seed 해주는 습관 = good
folds <- createFolds(data.train$AHD, k = 5, returnTrain = T)
```

> #### 놀랍게도 이렇게 cv를 위한 셋을 만들 수 있다. 여기서 원래 데이터를 쓰면 안된다. data.holdout은 무조건 나중에 딱 한번만 건드리는 것이다.

``` r
#test 셋 만들기
fold_test <- list()
for (i in 1:5) { 
fold_test[[paste0('fold', i)]] <- setdiff(1:nrow(data.train), folds[[i]])
  }
```

## \[2 - 2\] index를 어떻게 적용하는가?

``` r
trControl <- trainControl(method = "cv",number = 10,
                          index = folds, #이렇게 index라는 argument로 넣어주면 된당!
                          savePredictions = 'final' , search = "grid") 
tuneGrid <- expand.grid(.mtry = (1:ncol(data.train)))
#cv방법으로 10fold. 즉 10-fold cv를 한다는 의미이다.
rf_default <- train(AHD~., data = data.train,
    method = "rf",
    metric = "Accuracy",
    trControl = trControl,
    tuneGrid = tuneGrid)
rf_default$results
```

    ##    mtry  Accuracy     Kappa AccuracySD    KappaSD
    ## 1     1 0.8461929 0.6879734 0.04023244 0.08405686
    ## 2     2 0.8316749 0.6584711 0.03813022 0.07976591
    ## 3     3 0.8172730 0.6299957 0.05508095 0.11407294
    ## 4     4 0.7928828 0.5809151 0.05098956 0.10605955
    ## 5     5 0.8027550 0.6003689 0.06447541 0.13489518
    ## 6     6 0.8075169 0.6117873 0.05672081 0.11752587
    ## 7     7 0.7931151 0.5834801 0.06902024 0.14108096
    ## 8     8 0.8075169 0.6113679 0.05672081 0.11710041
    ## 9     9 0.7931151 0.5823152 0.06902024 0.14326653
    ## 10   10 0.7978770 0.5915148 0.06045892 0.12643960
    ## 11   11 0.7979931 0.5918783 0.06429538 0.13408880
    ## 12   12 0.7834751 0.5618803 0.07562457 0.15813483
    ## 13   13 0.7931151 0.5823152 0.06902024 0.14326653
    ## 14   14 0.7979931 0.5924395 0.06656829 0.13855164

## \[2 - 3\] 시계열처럼? -\> 아직 미완성

``` r
library(dummies)
fuck <- dummy.data.frame(data.train, remove_first_dummy = T)
parameter <- rep(0,5)
result.a <- rep(0,5)
result.f <- rep(0,5)

for (i in 1:5){
  index.tr <- folds[[i]]
  index.te <- fold_test[[i]]
  test_x <- data.train[index.te,] 
  test_y <- data.train[index.te,] %>% select(AHD)
  
  control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, savePredictions = 'final')
  tunegrid <- expand.grid(mtry = c(1:13))
  
  rf_cv <- train(AHD~., data = data.train[index.tr,], method = "rf",
                 metric = "Accuracy",ntree=500,
                 trControl=control,tuneGrid=tunegrid)
  
  fm <- rf_cv$finalModel 
  pred <- predict(fm, test_x) #이렇게 한 윈도우에 대해 예측해주면 된다. 
  accuracy <- Accuracy(pred, test_y)
  fone <- F1_Score(test_y, pred)
  result.a[i] <- accuracy
  result.f[i] <- fone
  parameter[i] <- fm[["mtry"]]
}

where <- which(result.a == max(result.a))
opt.mtry <- parameter[where]
opt.mtry

result.a
mean(result.a)

result.f
mean(result.f)
```
