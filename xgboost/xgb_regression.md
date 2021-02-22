xgboost
================

  - 이제는 xgboost. 이 방법은 부트스트랩이 아닌 부스팅을 이용한다. 부트스트랩이 랜덤한 샘플을 여러개 뽑은 방법이라면,
    부스팅은 병렬적이 아닌 연속적으로 진행한다는 점에서 그 차이점을 가진다. 즉 하나의 트리에서 시작하여 잔차를 계속해서
    줄여나가는 방향으로 트리를 학습시키는 것이다.

  - GBM은 역시 약한 트리를 만들어서 잔차를 줄여가는 방향으로 만들어지는 방법이다. 잔차를 최대한 줄이기 위한 잔차에 대한
    모델링을 진행하고, 약한 트리로부터 최종적으로는 완벽한 트리를 만든다. 경사하강법이기때문에 최적의 에러지점을 뛰어넘지
    않기 위해 약한 모델을 사용하는 것이다.

  - XGB는 이러한 GBM 모델을 발전시킨 것. 다양한 파라미터를 조정할 수 있고 NA를 자동처리해주고, sparse
    matrix를 처리해주고, information gain을 사용해 classification 할때 좋다고 한다.

  - Loss function + regularization function으로 objective function가 구성돼
    있다. Lossfunction은 뭐 MSE나 로지스틱 리그레션 등이 있다.

  - 그 다음 최적화 방식(손실함수의 결과 값을 최소화하는 방법)으로는 경사하강법을 사용하는 것이쥐\!

> 1)  max\_depth : 라운드 한개당 만들어지는 나무의 깊이\! 나무가 깊어질수록 복잡도가 증가하여 분산이 증가하고
>     **오버피팅**이 발생할 수 있다고 한다.. 디폴트 값은 6이고 1부터 무한까지 값을 가진다.

> 2)  min\_child\_weight : 라운드당 한 트리의 마지막 노드에 남는 obs의 개수 혹은 IG(가중치)의 양\!
>     클수록 분산은 down, 편향은 up default = 1

> 3)  eta : Learning rate(학습률), 작을수록 더 촘촘하게 최적점을 찾아 내려간다.(경사하강법),
>     default = 0.3 이걸 줄이면 nround는 늘려야 한다

> 4)  gamma : 손실함수를 조정하는 파라미터 default = 0(정규화 안해\!) 그리고 lambda(L2
>     regression, 디폴트는 1), alpha(L1 regression, 디폴트는 0)도 정규화 파라미터로 존재한다.

> 5)  nrounds : 최대 반복 횟수. 아까 말했듯 많이 하면 할수록 성능은 좋아지나 overfitting\!\!\!
>     default = 100. early stopping rounds 변수로 overfitting을 조정 가능하다.

> 6)  early\_stopping\_rounds : CV 할 때 이용 가능, 최적의 error를 가진 라운드 선정 ,
>     Validation error 가 지정한 라운드가 지나도록 감소하지 않으면 학습을 중단하고 최적의 validation
>     error가 나온 round 선정. (nrounds를 굉장히 크게 지정하고 early stopping rounds로
>     overfitting 방지)

> 7)  colsample\_bytree : 매 반복 시 추출할 열의 비율. 0\~1이고 default는 1

> 8)  subsample : 샘플링하는 obs의 비율, default = 1

> 9)  eval\_metric : 평가지표\! rmse,mae, logloss , error(Binary
>     classification error rate (0.5 threshold), merror – Multiclass
>     classification error rate, mlogloss – Multiclass logloss, auc -
>     Area under the curve

# \[0\] 데이터 준비

    ## Warning: package 'knitr' was built under R version 3.6.3

    ## Warning: package 'rmarkdown' was built under R version 3.6.3

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

    ## 
    ## Attaching package: 'data.table'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last

``` r
data <- fread('scores.csv', stringsAsFactors = T)
data <- data %>% select(-V1)
str(data)
```

    ## Classes 'data.table' and 'data.frame':   1044 obs. of  33 variables:
    ##  $ school    : Factor w/ 2 levels "GP","MS": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ sex       : Factor w/ 2 levels "F","M": 1 1 1 1 1 2 2 1 2 2 ...
    ##  $ age       : int  18 17 15 15 16 16 16 17 15 15 ...
    ##  $ address   : Factor w/ 2 levels "R","U": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ famsize   : Factor w/ 2 levels "GT3","LE3": 1 1 2 1 1 2 2 1 2 1 ...
    ##  $ Pstatus   : Factor w/ 2 levels "A","T": 1 2 2 2 2 2 2 1 1 2 ...
    ##  $ Medu      : int  4 1 1 4 3 4 2 4 3 3 ...
    ##  $ Fedu      : int  4 1 1 2 3 3 2 4 2 4 ...
    ##  $ Mjob      : Factor w/ 5 levels "at_home","health",..: 1 1 1 2 3 4 3 3 4 3 ...
    ##  $ Fjob      : Factor w/ 5 levels "at_home","health",..: 5 3 3 4 3 3 3 5 3 3 ...
    ##  $ reason    : Factor w/ 4 levels "course","home",..: 1 1 3 2 2 4 2 2 2 2 ...
    ##  $ guardian  : Factor w/ 3 levels "father","mother",..: 2 1 2 2 1 2 2 2 2 2 ...
    ##  $ traveltime: int  2 1 1 1 1 1 1 2 1 1 ...
    ##  $ studytime : int  2 2 2 3 2 2 2 2 2 2 ...
    ##  $ failures  : int  0 0 3 0 0 0 0 0 0 0 ...
    ##  $ schoolsup : Factor w/ 2 levels "no","yes": 2 1 2 1 1 1 1 2 1 1 ...
    ##  $ famsup    : Factor w/ 2 levels "no","yes": 1 2 1 2 2 2 1 2 2 2 ...
    ##  $ paid      : Factor w/ 2 levels "no","yes": 1 1 2 2 2 2 1 1 2 2 ...
    ##  $ activities: Factor w/ 2 levels "no","yes": 1 1 1 2 1 2 1 1 1 2 ...
    ##  $ nursery   : Factor w/ 2 levels "no","yes": 2 1 2 2 2 2 2 2 2 2 ...
    ##  $ higher    : Factor w/ 2 levels "no","yes": 2 2 2 2 2 2 2 2 2 2 ...
    ##  $ internet  : Factor w/ 2 levels "no","yes": 1 2 2 2 1 2 2 1 2 2 ...
    ##  $ romantic  : Factor w/ 2 levels "no","yes": 1 1 1 2 1 1 1 1 1 1 ...
    ##  $ famrel    : int  4 5 4 3 4 5 4 4 4 5 ...
    ##  $ freetime  : int  3 3 3 2 3 4 4 1 2 5 ...
    ##  $ goout     : int  4 3 2 2 2 2 4 4 2 1 ...
    ##  $ Dalc      : int  1 1 2 1 1 1 1 1 1 1 ...
    ##  $ Walc      : int  1 1 3 1 2 2 1 1 1 1 ...
    ##  $ health    : int  3 3 3 5 5 5 3 1 1 5 ...
    ##  $ absences  : int  6 4 10 2 4 10 0 6 0 0 ...
    ##  $ G1        : int  5 5 7 15 6 15 12 6 16 14 ...
    ##  $ G2        : int  6 5 8 14 10 15 12 5 18 15 ...
    ##  $ G3        : int  6 6 10 15 10 15 11 6 19 15 ...
    ##  - attr(*, ".internal.selfref")=<externalptr>

``` r
#G1, G2, G3은 점수이므로 합쳐주도록 한다.
data <- data %>% mutate(grade = G1 + G2 + G3) %>% select(-G1, -G2, -G3)
```
