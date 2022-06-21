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
library(olsrr)

# Transfer data into weekly
accidentsDF %<>% 
  select(-starts_with('cluster'),
         -starts_with('NB'), -starts_with('as.factor.Accident'),
         NB_Accidents) %>%
  mutate(Date = as.Date(Date)) 
 
accidentsDF %>%
  select(-Date, -NB_Accidents)  -> predictors
accidentsDF %<>%
  select(Date, NB_Accidents)



#d = as_tbl_time(accidentsDF, index=Date)

accidentsDF %<>% 
  filter(! (day(Date) == 29 & month(Date)==2)) %>%
  filter( day(Date) < 29)


results <- accidentsDF$NB_Accidents %>% 
  msts(start=c(2012,1), ts.frequency = 335, 
       seasonal.periods = c(7,100,30,335) ) %>%
  mstl(iterate = 10) %T>%
  plot() 

residuals.mstl = results[,ncol(results)]
var(residuals.mstl)
mean(residuals.mstl)
Box.test(residuals.mstl, lag=1, type='Ljung')
Box.test(residuals.mstl, lag=10)

ols_test_normality(residuals.mstl)

day(as.Date(accidentsDF[[15,1]]))

#d %<>% as_period('week')

dts.w = ts(accidentsDF.weekly$NB_Accidents, start=c(2012,1),
         end = c(2020,53), frequency = 53)
dts.d = ts(accidentsDF$NB_Accidents, start=c(2012,1),
          end = c(2020, 335), frequency = 335)
# 
# d %>% as.data.frame() %>%
#   mutate(y = year(Date)) %>%
#   count(y) 
#   summarise(c = count)

#accidentsDF %>%
#  mutate(YMD=paste0(year(Date),'-',sprintf("%02d",month(Date)),'-01')) %>% 
#  mutate(YMD = as.Date(YYM, format='%Y-%m-%d')) %>%
#  arrange(year(Date),month(Date)) %>%
#  group_by(YYM) %>%
#  summarise(count = sum(NB_Accidents)) -> ts.data
# Define time series
#dts = ts(accidentsDF$NB_Accidents, start = c(2012,1),
 #          frequency=365)


# dts %>% 
#   stl(s.window='periodic') %>%
#   seasadj() -> acci.adj
# autoplot(acci.adj)
# fit = stl(dts, s.window = "period");fit$weights
# plot(fit)
# monthplot(dts)
# seasonplot(dts)


fit <- auto.arima(dts.d, stepwise = T,seasonal = F,
                  #xreg = fourier(dts.d, K=8),
                  parallel = TRUE);fit;checkresiduals(fit)
plot(fit)



fit.w <- auto.arima(dts.w, stepwise = F,trace = F,
                  parallel = TRUE);fit.w;checkresiduals(fit.w)
plot(fit.w)
plot(forecast(fit.w,h=20))
residuals(fit) %>% autoplot()
checkresiduals(fit)
Pacf(residuals(fit.w))
fit.tb = tbats(dts.w); summary(fit.tb)

fit.tb

dts %>% diff(lag=52) %>% ggtsdisplay()
#----------------------
# timeseries plot


library(corrr)




cor.mat = correlate(regressors, diagonal = .2)
cor.mat  %>% shave %>%
  gather(-term, key = "colname", value = "cor") %>%
  filter(abs(cor) > 0.8) %>% View





