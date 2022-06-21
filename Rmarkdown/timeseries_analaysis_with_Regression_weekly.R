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
  select(-Date, -NB_Accidents 
         #-EnvironmentCommercial,
         #-EnvironmentResidential,- Auth_Speed40, -Auth_Speed50,
         #-Accident_CategoryMoving.Object, -IlluminationNight.and.lighted,
         #-WeatherSnow, -Surface_CondSnow, -WeatherClear.Sky
         )  -> data.l

accidentsDF.weekly %<>%
  select(Date, NB_Accidents)



dts.w = ts(accidentsDF.weekly$NB_Accidents, start=c(2012,1),
         end = c(2020,53), frequency = 53)


fit <- auto.arima(dts.w, stepwise = T,trace = F,
                  parallel = TRUE);fit;checkresiduals(fit)
plot(fit)
plot(forecast(fit.w,h=20))
residuals(fit) %>% autoplot()
Pacf(residuals(fit))

dts %>% diff(lag=52) %>% ggtsdisplay()
#----------------------

library(corrr)

cor.mat = correlate(data.l, diagonal = .2)
cor.mat  %>% shave %>%
  gather(-term, key = "colname", value = "cor") %>%
  filter(abs(cor) > 0.8) %>% View
#-------------------------
# Linear regression

model_ts <- arima(dts.w, c(5,1,2), 
                  seasonal=list(order=c(1,0,0), period=53))
summary(model_ts)
data.l$Y = residuals(model_ts)

# dividing data to train, test
set.seed(100)
index = sample(1:nrow(data.l), .7*nrow(data.l))
train.l = data.l[index,]
test.l = data.l[-index,]
# scaling:
pre.proc.X = preProcess(data.l,
                        method = c("center", 'scale'))
train.l = predict(pre.proc.X, train.l)
test.l = predict(pre.proc.X, test.l)
summary(train.l)
# linear regression model
lr = lm(Y ~ ., data=train.l)
summary(lr)
sqrt(mean(lr$residuals^2))
RMSE(predict(lr, test.l[,-49]), test.l[['Y']])
# Regularization
dummies <- dummyVars(Y ~ ., data.l)
train_dummies <- predict(dummies, train.l)
test_dummies <- predict(dummies, test.l)
# Ridge regression
X = as.matrix(train_dummies)
y_train = train.l$Y

x_test = as.matrix(test_dummies)
y_test = test.l$Y

lambdas <- 10^seq(2,-3, -.1)
ridge_reg = glmnet(X, y_train, nlambda=25, alpha=0, family='gaussian',
                   lambda=lambdas)
summary(ridge_reg)
opt.lmbd <- cv.glmnet(X, y_train, alpha=0, lambda = lambdas)$lambda.min
RMSE(predict(ridge_reg, s=opt.lmbd, newx = X), Y_train)
RMSE(predict(ridge_reg, s=opt.lmbd, newx = x_test), Y_test)
var(Y_train - predict(ridge_reg, s=opt.lmbd, newx = X))
var(Y_test - predict(ridge_reg, s=opt.lmbd, newx = x_test))
#-------------
# LASSO
opt.lmbd.lasso = cv.glmnet(X, y_train, alpha=1, lambda=lambdas,
                           standardize=TRUE, nfolds=5)$lambda.min
lasso.model <- glmnet(X, y_train, alpha=1, lambda=opt.lmbd.lasso,
                      standardize = T)
RMSE(predict(lasso.model, s=opt.lmbd.lasso, newx = X), Y_train)
RMSE(predict(lasso.model, s=opt.lmbd.lasso, newx = x_test), Y_test)
#---------------------------------
# lr after regularization
lr.train = as.data.frame(X)
lr.train$Y = y_train
lr = lm(Y ~ ., lr.train)
RMSE(predict(lr, newx = lr.train), y_train)
RMSE(predict(lr, newdata = as.data.frame(x_test)), y_test)

summary(lr)







