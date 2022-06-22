setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")

library(tidyverse)

library(scales)
library(knitr)
#library(epiDisplay)
library(forecast)
library(lubridate)
library(grid)
library(magrittr)
library(tibbletime)
library(forecast)
library(caret)
library(glmnet)
#------------------

ridge_model <- function(X.d, Y.d){

  
  # scalling data
  # scaling:
  X.d = X.d[,-caret::nearZeroVar(X.d)]
  X.d = predict(preProcess(X.d, method = c("center", 'scale')),X.d)
  X.d$Y = Y.d
  #------------------------------------
  #------ splitting to train and test
  set.seed(100); index = sample(1:nrow(X.d), .7*nrow(X.d))
  train.d = X.d[index,]
  test.d =  X.d[-index,]
  ##--------------------
  # ----- Transformation to glmnet matrix
  dummies <- dummyVars(Y ~ ., X.d)
  data_dummies <- predict(dummies, X.d)
  train_dummies <- predict(dummies, train.d)
  test_dummies <- predict(dummies, test.d)
  #--------------------
  # Ridge regression
  train_dummies = as.matrix(train_dummies)
  test_dummies = as.matrix(test_dummies)
  y_train = as.matrix(train.d$Y)
  y_test = as.matrix(test.d$Y)
  
  lambdas <- 10^seq(2,-3, -.1)
  opt.lmbd <- cv.glmnet(train_dummies, y_train, alpha=0, lambda = lambdas)$lambda.min
  
  ridge_reg = glmnet(train_dummies, y_train, nlambda=25, alpha=0, family='gaussian',
                     lambda=opt.lmbd)
  summary(ridge_reg)
  
  train_pred = predict(ridge_reg, s=opt.lmbd, newx = train_dummies)
  test_pred = predict(ridge_reg, s=opt.lmbd, newx = test_dummies) 
  train_errors = (y_train - train_pred)[,1]
  test_errors = (y_test - test_pred)[,1]
  
  
  out = t(round(data.frame(train_RMSE = RMSE(train_pred, y_train),
             test_RMSE =  RMSE(test_pred, y_test),
             train_VAR = var(train_errors),
             train_MEAN = mean(train_errors),
             test_VAR = var(test_errors),
             test_MEAN = mean(test_errors),
             train_R2 = caret::R2(train_pred, y_train)[1],
             test_R2 = caret::R2(test_pred, y_test)[1],
             train_Ljung_p = Box.test(train_errors, lag=1, type='Ljung')$p.value,
             test_Ljung_p = Box.test(test_errors, lag=1, type='Ljung')$p.value),3))
  #-------------
  plot(train_pred,train_errors) +title("Training predictions vs Residuals") +abline(0,0)
  plot(test_pred, test_errors) + title("Test predictions vs Residuals") +abline(0,0)
  #--------------------
  return(out)
}
#=================================================================
lag.all <- function(X.a, Y.a, lag.a){
  
  for (col in names(X.a)){
    for(l in 1:lag.a){
      X.a[,paste0(col,'_lag_',l)] = dplyr::lag(X.a[,col],l)
    }
  }
  X.a = X.a[(lag.a+1):nrow(X.a),]
  Y.a = Y.a[(lag.a+1):nrow(Y.a),]
  return(list(X=X.a,Y=Y.a))
  
}

#=================================================================
Y = read.csv('../data/created/timeseries/decomp/residu_all.csv')
X = read.csv('../data/created/timeseries/decomp/predictors_all.csv') %>%
  select(-Date)
lagged = lag.all(X, Y, 7)
X = lagged$X
Y = lagged$Y
write.csv(X, '../data/created/timeseries/phase2/X_lag_7_ALL.csv',
          row.names = F)
write.csv(Y, '../data/created/timeseries/phase2/Y_lag_7_ALL.csv',
          row.names = F)

results = ridge_model(X,Y)
results %<>% as.data.frame %>% rename(ALL = V1)
results
#==================================================================
# clusters:

for (c in 0:5){
  
  YC = read.csv(paste0('../data/created/timeseries/decomp/residu_',c,'.csv'))
  XC = read.csv(paste0('../data/created/timeseries/decomp/predictors_',c,'.csv')) %>%
    select(-Date)
  lagged = lag.all(XC, YC, 7)
  XC = lagged$X
  YC = lagged$Y
  write.csv(XC, paste0('../data/created/timeseries/phase2/X_lag_7_cluster_',c,'.csv'),
            row.names = F)
  write.csv(YC, paste0('../data/created/timeseries/phase2/Y_lag_7_cluster_',c,'.csv'),
            row.names = F)
  #results[,paste0('cluster_',c)] = ridge_model(XC, YC) %>%
  #  as.data.frame() %>% select(V1)
}
results
#====================================================================

# Finding the optimal number of lags:
for (ll in c(1)){
  
  X = read.csv('../data/created/timeseries/decomp/predictors_all.csv') %>%
    select(-Date)
  Y = read.csv('../data/created/timeseries/decomp/residu_all.csv')
  
  lagged = lag.all(X, Y, 2)
  X = lagged$X
  Y = lagged$Y
  results[,paste0('l',ll)] = (ridge_model(X,Y) %>% as.data.frame())$V1

}
results
#====================================================================






