random\_forest\_regression
================

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
data <- fread('day.csv')
str(data)
```

    ## Classes 'data.table' and 'data.frame':   731 obs. of  16 variables:
    ##  $ instant   : int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ dteday    : chr  "2011-01-01" "2011-01-02" "2011-01-03" "2011-01-04" ...
    ##  $ season    : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ yr        : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ mnth      : int  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ holiday   : int  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ weekday   : int  6 0 1 2 3 4 5 6 0 1 ...
    ##  $ workingday: int  0 0 1 1 1 1 1 0 0 1 ...
    ##  $ weathersit: int  2 2 1 1 1 1 2 2 1 1 ...
    ##  $ temp      : num  0.344 0.363 0.196 0.2 0.227 ...
    ##  $ atemp     : num  0.364 0.354 0.189 0.212 0.229 ...
    ##  $ hum       : num  0.806 0.696 0.437 0.59 0.437 ...
    ##  $ windspeed : num  0.16 0.249 0.248 0.16 0.187 ...
    ##  $ casual    : int  331 131 120 108 82 88 148 68 54 41 ...
    ##  $ registered: int  654 670 1229 1454 1518 1518 1362 891 768 1280 ...
    ##  $ cnt       : int  985 801 1349 1562 1600 1606 1510 959 822 1321 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
sapply(data, class)
```

    ##     instant      dteday      season          yr        mnth     holiday 
    ##   "integer" "character"   "integer"   "integer"   "integer"   "integer" 
    ##     weekday  workingday  weathersit        temp       atemp         hum 
    ##   "integer"   "integer"   "integer"   "numeric"   "numeric"   "numeric" 
    ##   windspeed      casual  registered         cnt 
    ##   "numeric"   "integer"   "integer"   "integer"

``` r
data <- data %>% select(-instant, -dteday)
data <- transform(
  data,
  season = as.factor(season),
  yr = as.factor(yr), 
  mnth = as.factor(mnth), 
  holiday = as.factor(holiday),
  weekday = as.factor(weekday),
  workingday = as.factor(workingday), 
  weathersit = as.factor(weathersit), 
  temp = as.numeric(temp), 
  atemp = as.numeric(atemp),
  hum = as.numeric(hum),
  windspeed = as.numeric(windspeed), 
  casual = as.numeric(casual), 
  registered = as.numeric(registered), 
  cnt = as.numeric(cnt)
)
sapply(data, class)
```

    ##     season         yr       mnth    holiday    weekday workingday weathersit 
    ##   "factor"   "factor"   "factor"   "factor"   "factor"   "factor"   "factor" 
    ##       temp      atemp        hum  windspeed     casual registered        cnt 
    ##  "numeric"  "numeric"  "numeric"  "numeric"  "numeric"  "numeric"  "numeric"

``` r
head(data)
```

    ##    season yr mnth holiday weekday workingday weathersit     temp    atemp
    ## 1:      1  0    1       0       6          0          2 0.344167 0.363625
    ## 2:      1  0    1       0       0          0          2 0.363478 0.353739
    ## 3:      1  0    1       0       1          1          1 0.196364 0.189405
    ## 4:      1  0    1       0       2          1          1 0.200000 0.212122
    ## 5:      1  0    1       0       3          1          1 0.226957 0.229270
    ## 6:      1  0    1       0       4          1          1 0.204348 0.233209
    ##         hum windspeed casual registered  cnt
    ## 1: 0.805833 0.1604460    331        654  985
    ## 2: 0.696087 0.2485390    131        670  801
    ## 3: 0.437273 0.2483090    120       1229 1349
    ## 4: 0.590435 0.1602960    108       1454 1562
    ## 5: 0.436957 0.1869000     82       1518 1600
    ## 6: 0.518261 0.0895652     88       1518 1606

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
index <- createDataPartition(data$cnt, p = .7, list = F) #이건 데이터를 나눠주는 함수.. 개꿀딱지임
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
rf_default <- train(cnt~., data = data.train,
    method = "rf",
    metric = "RMSE",
    trControl = trControl)

#결과를 추출하자
print(rf_default)
```

    ## Random Forest 
    ## 
    ## 515 samples
    ##  13 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 463, 464, 463, 463, 463, 464, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared   MAE      
    ##    2    743.9494  0.9191003  583.63699
    ##   16    183.9577  0.9922731  122.35600
    ##   31    137.7704  0.9950529   92.36354
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 31.

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
tuneGrid <- expand.grid(.mtry = c(1: 13)) #기본에서 찾은 가장 좋은 mtry를 기준으로 넣어주면 된다. 
rf_mtry <- train(cnt~., data = data.train,
    method = "rf", metric = "RMSE",
    tuneGrid = tuneGrid, #이렇게 그리드 서치를 진행해준다. 
    trControl = trControl,  importance = TRUE, nodesize = 14, ntree = 300)
print(rf_mtry)
```

    ## Random Forest 
    ## 
    ## 515 samples
    ##  13 predictor
    ## 
    ## No pre-processing
    ## Resampling: Cross-Validated (10 fold) 
    ## Summary of sample sizes: 463, 464, 463, 464, 463, 464, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE       Rsquared   MAE      
    ##    1    1251.9449  0.8438342  1015.8871
    ##    2     774.5067  0.9108660   611.1298
    ##    3     581.6266  0.9394859   438.1967
    ##    4     490.5927  0.9537841   359.9023
    ##    5     448.5986  0.9594262   321.8506
    ##    6     398.7034  0.9671786   283.6668
    ##    7     367.9544  0.9712717   256.9862
    ##    8     346.6590  0.9743387   242.6622
    ##    9     323.6389  0.9771510   223.6513
    ##   10     296.5467  0.9806947   204.4829
    ##   11     277.6826  0.9828497   191.5997
    ##   12     257.3287  0.9856895   177.2646
    ##   13     243.2734  0.9869199   166.4224
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 13.

``` r
plot(rf_mtry)
```

![](Random_Forest_regression_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

> plot 으로 보면 조금 더 편할 수 있다. 근데 만약 최적의 mtry가 마지막 부분에서 급상승하는 그래프를 그린다면, 그보다
> 더 큰 mtry를 포함하는 parameter tuning을 다시 진행해줘야 할 것이다.

> 아마 여기의 결과는 5번의 cv의 평균 에러(여기서는 accuracy)일 것이다. 그니께 1:10까지의 파라미터의 결과가 총
> 다섯번이 나오는데 print(rf\_mtry)의 값은 이 다섯번의 결과를 평균낸 cv\_error일 것으로 예상된다. 사실
> 그래야 말이 되긴 함.

``` r
opt.mtry <- rf_mtry$bestTune$mtry
max(rf_mtry$results$Accuracy) #이거는 가장 높은 정확도!
```

    ## Warning in max(rf_mtry$results$Accuracy): max에 전달되는 인자들 중 누락이 있어 -
    ## Inf를 반환합니다

    ## [1] -Inf

``` r
opt.mtry #가장 좋은 mtry는 1이다.. 즉 변수 하나만 쓰라는 이야기
```

    ## [1] 13

> #### 위와 같은 방식으로 mtry를 찾아주면 된다. 시간이 없다면 밑의 과정들은 생략해도 될듯? 실제로 랜포의 장점은 mtry만 찾고도 꽤나 좋은 성능을 내는 것이라고 하니

## \[1 - 4\] maxnodes 찾기

``` r
store_maxnode <- list()
tuneGrid <- expand.grid(.mtry = opt.mtry) #위에서 찾은 최고의 mtry를 넣어준다. 
for (maxnodes in c(5: 30)) {
    set.seed(1234)
    rf_maxnode <- train(cnt~.,
        data = data.train, method = "rf", metric = "RMSE",
        tuneGrid = tuneGrid, #여기는 아까 찾은 베스트 mtry가 들어있다.
        trControl = trControl,importance = TRUE,nodesize = 14,
        maxnodes = maxnodes, #이제 이 값을 5부터 30까지 바꿔줄 것이다. 
        ntree = 300)
    current_iteration <- toString(maxnodes)
    store_maxnode[[current_iteration]] <- rf_maxnode
}

results_mtry <- resamples(store_maxnode)
result.maxnode<- summary(results_mtry)$statistics$RMSE %>% as.data.frame()
opt.maxnodes <- as.numeric(row.names(result.maxnode[which.min(result.maxnode$Mean),])) # 뭐 대충 이런 방식으로 넣어주면 된다. 
#근데 여기서 최소값을 봐야할지 평균값을 봐야할지는 모르겠다. 이전의 classification은 최대값을 봤으니 여기서는 평균값을 봐보자. 
```

> 여기서는 RMSE이기 때문에 최저값을 찾아줘야하는게 포인트\! classification과는 다르다.

## \[1 - 5\] ntree 찾기

``` r
store_maxtrees <- list()
for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
    set.seed(1234)
    rf_maxtrees <- train(cnt~.,
        data = data.train,
        method = "rf",
        metric = "RMSE",
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
    ## MAE 
    ##          Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## 250  180.0868 213.2136 229.2011 227.0902 242.6986 270.1263    0
    ## 300  179.0793 214.8114 227.7474 226.5110 237.9754 269.5435    0
    ## 350  178.7319 214.0206 228.2111 226.1169 236.5949 265.0901    0
    ## 400  179.3643 212.8742 228.5416 226.3593 240.0066 264.3746    0
    ## 450  177.6588 212.5712 229.0541 225.9393 239.5427 264.4299    0
    ## 500  176.5422 212.3186 230.8883 225.9197 237.6907 263.3946    0
    ## 550  177.0201 212.9166 230.4404 225.6345 236.9998 263.4677    0
    ## 600  180.3415 212.6175 230.5911 225.6002 236.6181 260.7796    0
    ## 800  184.2333 210.6800 230.3303 225.7161 235.3233 262.8936    0
    ## 1000 181.5483 207.7677 230.3936 224.7749 236.2236 260.6838    0
    ## 2000 178.1975 206.4023 232.8981 225.0228 238.6149 264.0809    0
    ## 
    ## RMSE 
    ##          Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## 250  248.5470 281.5614 307.9187 311.0445 338.5259 373.4362    0
    ## 300  246.5839 282.6200 308.9878 311.1679 339.9481 379.0323    0
    ## 350  248.7375 283.6037 308.9965 311.6074 338.5650 388.3900    0
    ## 400  248.1385 281.8211 311.2640 312.6285 339.2530 390.0129    0
    ## 450  244.3361 280.7596 310.5977 311.9515 339.6735 386.2804    0
    ## 500  243.8961 282.3818 310.8421 311.8749 339.5959 385.3624    0
    ## 550  244.8683 281.2983 310.8990 311.7665 340.1068 386.5870    0
    ## 600  248.0954 282.9311 310.2313 312.1593 337.7677 391.0169    0
    ## 800  253.5279 280.9624 309.0282 312.2028 339.8125 387.8730    0
    ## 1000 251.9616 278.9858 308.6338 311.3795 337.5720 388.3209    0
    ## 2000 246.6752 279.0774 311.2293 310.4833 339.5688 377.7352    0
    ## 
    ## Rsquared 
    ##           Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## 250  0.9634438 0.9777515 0.9807531 0.9789846 0.9829241 0.9847983    0
    ## 300  0.9618122 0.9774348 0.9805263 0.9789073 0.9830854 0.9847436    0
    ## 350  0.9596792 0.9779930 0.9805879 0.9788102 0.9828339 0.9848202    0
    ## 400  0.9594045 0.9779167 0.9802173 0.9786658 0.9828847 0.9849454    0
    ## 450  0.9601515 0.9775756 0.9802497 0.9787449 0.9832733 0.9850101    0
    ## 500  0.9603455 0.9776492 0.9803299 0.9787809 0.9829437 0.9848864    0
    ## 550  0.9599584 0.9773917 0.9804645 0.9787762 0.9832395 0.9849062    0
    ## 600  0.9588209 0.9776552 0.9805623 0.9786603 0.9829630 0.9850181    0
    ## 800  0.9595367 0.9773866 0.9805329 0.9787254 0.9824079 0.9853270    0
    ## 1000 0.9594184 0.9776784 0.9806646 0.9788484 0.9825805 0.9853413    0
    ## 2000 0.9617589 0.9772533 0.9806578 0.9790669 0.9831076 0.9854601    0

``` r
result.ntree<- summary(results_tree)$statistics$RMSE %>% as.data.frame()
opt.ntree <- as.numeric(row.names(result.ntree[which.min(result.ntree$Mean),]))
```

> 결과적으로 opt.mtry, opt.ntree, opt.maxnodes를 얻게 되었다. 이 세가지의 파라미터를 가지고 최종
> 모델을 적합한다.

## \[1 - 6\] 최종모델 적합.

``` r
set.seed(1234)
fit_rf <- randomForest(cnt~.,
    data = data.train,
    metric = "RMSE",
    mtry = opt.mtry, #우리가 찾은 베스트 mtry
    importance = TRUE,
    nodesize = 14,
    ntree = opt.ntree, #우리가 찾은 베스트 ntree
   maxnodes = opt.maxnodes) #우리가 찾은 베스트 maxnodes
```

## \[1 - 7\] 모델 평가

``` r
rf.prediction <-predict(fit_rf, data.holdout)
RMSE(rf.prediction, data.holdout$cnt) #최종 RMSE : 190.769
```

    ## [1] 190.769

``` r
varImpPlot(fit_rf)
```

![](Random_Forest_regression_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

> #### 우선 summary(data$cnt) 하면 평균값이 4504인 것을 보면 꽤나 괜찮게 나온 것임을 알 수 있다\!

# \[2\] CV 과정에서 data셋을 customize해주는 방법.

> #### 사실 이 방법은 **시계열 데이터**에 대해 모델링을 진행할 때 써야한다. 단순히 cv로 하게 되면 미래의 데이터로 학습하고 과거의 데이터를 예측하는 이상한 모델이 될 수 있기 때문이다\! 그래서 롤링 윈도우를쓰긴 하는데 ML에서는 아무래도 train set을 적당히 나눠주는 것 같다.

## \[2 - 1\] 일단 caret 패키지로 cv를 만드는 방법을 알아보자.

``` r
#train data 만들어주기
set.seed(1234) #set.seed 해주는 습관 = good
folds <- createFolds(data.train$cnt, k = 5, returnTrain = T)
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
rf_default <- train(cnt~., data = data.train,
    method = "rf",
    metric = "RMSE",
    trControl = trControl,
    tuneGrid = tuneGrid)
rf_default$results
```

    ##    mtry      RMSE  Rsquared       MAE   RMSESD  RsquaredSD    MAESD
    ## 1     1 1241.5370 0.8488190 1005.9240 57.97850 0.021723488 44.00134
    ## 2     2  740.6486 0.9205040  579.4992 71.14042 0.011567188 39.65213
    ## 3     3  559.2603 0.9451948  418.8201 59.19542 0.009536864 31.15485
    ## 4     4  467.0366 0.9583403  339.0125 63.86924 0.010034637 30.39395
    ## 5     5  416.8596 0.9651287  295.3594 59.87889 0.008722783 23.88029
    ## 6     6  372.5509 0.9714510  261.8142 57.35123 0.007305784 23.20773
    ## 7     7  349.6633 0.9741534  241.9650 51.87897 0.006475354 23.46618
    ## 8     8  316.5732 0.9789460  219.4602 55.61097 0.006354038 25.68354
    ## 9     9  302.2070 0.9804955  207.2060 58.09190 0.006689278 26.89048
    ## 10   10  276.9954 0.9834995  189.1039 49.55955 0.004830644 23.50501
    ## 11   11  257.5075 0.9857519  175.9468 49.26155 0.004535988 22.62720
    ## 12   12  241.0076 0.9874743  163.8411 41.47916 0.003448074 19.98113
    ## 13   13  229.1501 0.9886413  155.2298 42.62672 0.003388268 20.33960
    ## 14   14  216.9700 0.9896711  146.0822 43.25803 0.003304891 23.15568

## \[2 - 3\] 이 커스텀 한 방법은 OOF를 적용할 수 있다고 한다…? 근데 이건 뭐하는 건지 잘 모르겠다.

``` r
rf_oof_pred <- rf_default$pred
rf_oof_pred$Resample <- as.factor(rf_oof_pred$Resample)
rf_oof_pred <- rf_oof_pred[order(rf_oof_pred$rowIndex),]
str(rf_oof_pred)
```

    ## 'data.frame':    515 obs. of  5 variables:
    ##  $ mtry    : int  14 14 14 14 14 14 14 14 14 14 ...
    ##  $ pred    : num  1355 1364 1596 1628 1018 ...
    ##  $ obs     : num  801 1349 1562 1606 959 ...
    ##  $ rowIndex: int  1 2 3 4 5 6 7 8 9 10 ...
    ##  $ Resample: Factor w/ 5 levels "Fold1","Fold2",..: 5 1 3 3 4 2 5 4 4 4 ...
