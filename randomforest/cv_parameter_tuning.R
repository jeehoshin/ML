library(dplyr)
library(data.table)
library(ggplot2)
library(TTR)
library(fTrading)
library(randomForest)
library(ggplot2)
library(reshape)
library(caret)
library(MLmetrics)
library(e1071)
library(caret)
library(forecast)
options(scipen = 100)

setwd('C:/Users/jiho0/OneDrive/바탕 화면/Data Analysis/미래에셋 intern')
high <- fread('고가DF.csv')
low <- fread('저가DF.csv')
fin <- fread('종가DF.csv')

#필요 칼럼들로 만들어준다,
#해당 종목 코드 추출 후 고가 추출
stock_h <- high %>% select(DATA_DATE, codecode)
colnames(stock_h) <- c('date', 'price_high')

#해당 종목 코드 추출 후 저가 추출
stock_l <- low %>% select(codecode)
colnames(stock_l) <- 'price_low'

#해당 종목 코드 추출 후 고가 추출
stock_c <- fin %>% select(codecode)
colnames(stock_c) <- 'price_close'

stock <- cbind(stock_h, stock_l, stock_c)

rm(stock_c, stock_h, stock_l)
############################################################################################
#기술적 지표 생성  
############################################################################################

#1.볼린저 밴드.
bb <- TTR::BBands(HLC = stock[,2:4])
colnames(bb) <- c('BB_lower', 
                  'BB_moving_average', 'BB_upper', 'BB_percent')
stock <- cbind(stock,bb)

#2.SAR 파라볼릭.
sar <- TTR::SAR(HL = stock[,2:3]) %>% as.data.frame()
colnames(sar) <- 'sar'
stock <- cbind(stock, sar)

#3.adx
adx <- TTR::ADX(HLC = stock[,2:4]) %>% as.data.frame()
stock <- cbind(stock, adx)

#4.RSI
rsi <- TTR::RSI(price = stock$price_close) %>% as.data.frame()
colnames(rsi) <- 'rsi'
stock <- cbind(stock,rsi)

#5.ROC
roc <- TTR::ROC(stock$price_close) %>% as.data.frame()
colnames(roc) <- 'roc'
stock <- cbind(stock,roc)

#6.MACD 
macd <- TTR::MACD(stock$price_close)
colnames(macd) <- c('macd', 'macd_signal')
stock <- cbind(stock,macd)

#7.이동평균선
ma5 <- TTR::SMA(stock$price_close, n = 5) %>% as.data.frame()
ma10 <- TTR::SMA(stock$price_close, n = 10) %>% as.data.frame()
ma20 <- TTR::SMA(stock$price_close, n = 20) %>% as.data.frame()
ma60 <- TTR::SMA(stock$price_close, n = 60) %>% as.data.frame()
ma <- cbind(ma5, ma10, ma20,ma60)
colnames(ma) <- c('ma5', 'ma10', 'ma20', 'ma60')
stock <- cbind(stock,ma)

#8.stocastic
stocastic <- TTR::stoch(HLC = stock[,2:4]) %>% as.data.frame()
stock <- cbind(stock, stocastic)

#9.CCI
cci <- TTR::CCI(HLC = stock[,2:4]) %>% as.data.frame()
colnames(cci) <- 'cci'
stock <- cbind(stock, cci)

#10.williams R
wr <- TTR::williamsAD(HLC = stock[,2:4]) %>% as.data.frame()
colnames(wr) <- 'williamsR'
stock <- cbind(stock, wr)
rm(wr,bb,sar,adx,rsi,roc,macd,ma5, ma10, ma20, ma60, ma,stocastic,cci)


#binary구축

stock <- stock %>% as.data.frame()
rownames(stock) <- stock$date
stock <- stock %>% select(-date, -price_high, -price_low)
stock$day3 <- NA
stock$day5 <- NA
stock$day20 <- NA



for(i in 1:(nrow(stock)-3)){ 
  if((stock$price_close[i] < stock$price_close[i+3]) == TRUE) { 
    stock$day3[i] <- 1
  } else { 
    stock$day3[i] <- 0 
  }
}

#Y값으로 만들어주기 (Day3)
stock_bi_3 <- stock %>% select(-price_close, -day5, -day20)
stock_bi_3 <- stock_bi_3 %>% dplyr::rename(Y = day3)
stock_bi_3 <- stock_bi_3[,c(ncol(stock_bi_3),(1:ncol(stock_bi_3)-1))]
stock_bi_3$Y <- stock_bi_3$Y %>% as.factor()
stock_bi_3 <- stock_bi_3 %>% na.omit
stock_bi_3$Y %>% plot


#---------------------------------------------------------------------------------------------------------------------
#[3] Random Forest - 모형적합

#HOLDOUT SET을 만들어준다
holdout_index <-(nrow(stock_bi_3)-181):nrow(stock_bi_3)
rf_holdout <- stock_bi_3[holdout_index,]
rf_data <- stock_bi_3[-holdout_index,]

FOLDS <- list()
for(i in 1:5) {
  FOLDS[[paste0('Ftrain',i)]] <- (((floor(nrow(rf_data)/9*(i-1)))+1):(floor(nrow(rf_data)/9*(i+3))))
}

FOLDS_VAL <- list()
for(i in 1:5) {
  FOLDS_VAL[[paste0('Fold',i)]] <- (((floor(nrow(rf_data)/9*(i+3)))+1):(floor(nrow(rf_data)/9*(i+4))))
}

######################################################
#(1) CV 진행. - 기존의 방법.
#각 fold별 최적의 파라미터를 찾아서 가장 성능이 좋은 파라미터를 전체 모델에 대한 파라미터로 적용한다.
#예를 들면 fold1에서 가장 성능이 좋은 mtry, fold2에서 가장 성능이 좋은 mtry... 이렇게 다섯개 추출 후 
#
######################################################

result_rf <- list() 
set.seed(1111)
for(i in 1:5) { 
  result_rf[[i]] <- rep(0, 10)}

for(i in 1:5) { 
  index.tr <- FOLDS[[i]]
  index.te <- FOLDS_VAL[[i]]
  val_x <- rf_data[index.te,-1]
  val_y <- rf_data[index.te,1]
  
  for(j in 1:10) { 
    model <- randomForest(Y ~ ., data = rf_data[index.tr,], ntree = 500, mtry = j, importance = TRUE)
    predValid <- predict(model, val_x)
    result_rf[[i]][[j]]<- MLmetrics::Accuracy(predValid,val_y)
  }
  rm(model)
}

mtrylist <- rep(0,5)
cv_acc <- rep(0,5)

for(i in 1:5) {
  mtrylist[i] <- which(result_rf[[i]] == max(result_rf[[i]]))
  cv_acc[i] <- max(result_rf[[i]])
}

optimal_mtry <- mtrylist[which(cv_acc == max(cv_acc))]
opt_prm_rf <- list(mtry = mtrylist, validation_acc = cv_acc, opt_mtry = optimal_mtry, mean_acc = mean(cv_acc))

#최종 파라미터로 적용

opt.mtry <- opt_prm_rf$opt_mtry[1]
set.seed(1111)
FinalModel_rf <- randomForest(Y~., data = rf_data, mtry = opt.mtry, ntree = 1500,
                              importance = T)
rf_holdout_x <- rf_holdout[,-1]
rf_holdout_y <- rf_holdout[,1] %>% as.factor()
pred_rf <- predict(FinalModel_rf, rf_holdout_x)
holdout_acc <- MLmetrics::Accuracy(pred_rf, rf_holdout_y)
holdout_acc
holdout_confu <- confusionMatrix(pred_rf, rf_holdout_y)
holdout_confu

################################################################################
#cv진행 - 옳은 방법 
#이 방법은 각 파라미터 별로 다섯개의 fold값에 대한 값이 도출되고, 이에 대해 
#평균값을 비교함으로써 파라미터를 선택한다. 
#cv_mean을 통해 각 파라미터의 전반적인 성능을 평가하고, 이를 바탕으로 파라미터를 선택한다.

################################################################################

FOLDS <- list()
for(i in 1:5) {
  FOLDS[[paste0('Ftrain',i)]] <- (((floor(nrow(rf_data)/9*(i-1)))+1):(floor(nrow(rf_data)/9*(i+3))))
}

FOLDS_VAL <- list()
for(i in 1:5) {
  FOLDS_VAL[[paste0('Fold',i)]] <- (((floor(nrow(rf_data)/9*(i+3)))+1):(floor(nrow(rf_data)/9*(i+4))))
}

result_rf2 <- list() 
set.seed(1111)
result_rf2 <- data.frame(cv1 = rep(0,10), 
                         cv2 = rep(0,10), 
                         cv3 = rep(0,10), 
                         cv4 = rep(0,10), 
                         cv5 = rep(0,10))

for (i in 1:10) {
  for (j in 1:5){
    index.tr2 <- FOLDS[[j]]
    index.te2 <- FOLDS_VAL[[j]]
    val_x <- rf_data[index.te2,-1]
    val_y <- rf_data[index.te2,1]
    rf_model <- randomForest(Y~., data = rf_data[index.tr2,],ntree = 500, mtry = i, importance = TRUE)
    predvalid <- predict(rf_model, val_x)
    result_rf2[i,j] <- MLmetrics::Accuracy(predvalid,val_y)
    rm(rf_model, index.te2, index.tr2, val_x, val_y)
  }
}

cv_average <- apply(result_rf2 %>% as.matrix, 1, mean)
optimal_mtry2 <- which(cv_average == max(cv_average))

#최종 파라미터로 적용
set.seed(1111)
FinalModel_rf2 <- randomForest(Y~., data = rf_data, mtry = optimal_mtry2, ntree = 1500,
                               importance = T)

rf_holdout_x <- rf_holdout[,-1]
rf_holdout_y <- rf_holdout[,1] %>% as.factor()
pred_rf2 <- predict(FinalModel_rf2, rf_holdout_x)
holdout_acc2 <- MLmetrics::Accuracy(pred_rf2, rf_holdout_y)
holdout_acc2
holdout_confu2 <- confusionMatrix(pred_rf, rf_holdout_y)
holdout_confu2

#비교
print(c(holdout_acc, holdout_acc2))
holdout_confu
holdout_confu2
