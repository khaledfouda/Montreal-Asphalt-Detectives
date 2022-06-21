setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")

accidentsDF <- read.csv('../data/created/timeseries/ts_accidents_by_date.csv',
                        header = TRUE)
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
# Transfer data into weekly
Y = read.csv('../data/created/timeseries/decomp/residu_all.csv')
X = read.csv('../data/created/timeseries/decomp/predictors_all.csv') %>%
  select(-Date)

























#######
#######################
#######################################
accidentsDF %<>% 
  select(-starts_with('cluster'),
         -starts_with('NB'), -starts_with('as.factor.Accident'),
         NB_Accidents) %>%
  mutate(Date = as.Date(Date)) %>%
  mutate(y = year(Date)) %>%
  group_by(y) %>%
  arrange(Date) %>%
  mutate(Date = as.Date(cut(Date, '7 d'))) %>%
  arrange(Date) %>%
  group_by(Date) %>%
  summarise_all(sum) %>% 
  select(-y) -> accidentsDF.weekly

accidentsDF.weekly %>%
  select(-Date, -NB_Accidents)  -> data.l

accidentsDF.weekly %<>%
  select(Date, NB_Accidents)



dts.w = ts(accidentsDF.weekly$NB_Accidents, start=c(2012,1),
         end = c(2020,53), frequency = 53)
#-------------------------
# 1. time series model

model_ts <- arima(dts.w, c(5,1,2), 
                  seasonal=list(order=c(1,0,0), period=53))
summary(model_ts)
data.l$Y = residuals(model_ts)
#--------------------------------
# 2. preparation for regression
# dividing data to train, test
set.seed(100)
index = sample(1:nrow(data.l), .7*nrow(data.l))
train.l = data.l[index,]
test.l = data.l[-index,]
# scaling:
pre.proc.X = preProcess(data.l,
                        method = c("center", 'scale'))
data.l = predict(pre.proc.X, data.l)
train.l = predict(pre.proc.X, train.l)
test.l = predict(pre.proc.X, test.l)
summary(train.l)

# Transformation to glmnet matrix
dummies <- dummyVars(Y ~ ., data.l)
data_dummies <- predict(dummies, data.l)
train_dummies <- predict(dummies, train.l)
test_dummies <- predict(dummies, test.l)
# Ridge regression
X = as.matrix(train_dummies)
y_train = train.l$Y

x_test = as.matrix(test_dummies)
y_test = test.l$Y

lambdas <- 10^seq(2,-3, -.1)
opt.lmbd <- cv.glmnet(X, y_train, alpha=0, lambda = lambdas)$lambda.min

ridge_reg = glmnet(X, y_train, nlambda=25, alpha=0, family='gaussian',
                   lambda=opt.lmbd)
summary(ridge_reg)

RMSE(predict(ridge_reg, s=opt.lmbd, newx = X), y_train)
RMSE(predict(ridge_reg, s=opt.lmbd, newx = x_test), y_test)
var(y_train - predict(ridge_reg, s=opt.lmbd, newx = X))
var(y_test - predict(ridge_reg, s=opt.lmbd, newx = x_test))
#-------------
plot(y_train - predict(ridge_reg, s=opt.lmbd, newx = X))
#-------------------------------
# Applying Ridge to the whole dataset
X = as.matrix(data.l)
Y = data.l$Y
opt.lmbd <- cv.glmnet(X, as.matrix(Y), alpha=0, lambda = lambdas)$lambda.min

ridge_reg = glmnet(X, Y, nlambda=25, alpha=0, family='gaussian',
                   lambda=opt.lmbd)
summary(ridge_reg)
coef(ridge_reg)
Y_pred = predict(ridge_reg, s=opt.lmbd, newx = X)
RMSE(Y_pred, Y)
var(Y_pred - Y)
plot(Y_pred-Y)



data.l %>%
  mutate(y_pred = Y_pred[,1]) %>%
  select(Y, y_pred) %>% View

