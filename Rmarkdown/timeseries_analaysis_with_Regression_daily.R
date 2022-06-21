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
  select(-NB_Accidents) %T>%
  write.csv('../data/created/timeseries/decomp/predictors_all.csv',row.names = F) %>%
  select(-Date) ->
  predictors
accidentsDF %<>%
  select(Date, NB_Accidents)



results <- accidentsDF$NB_Accidents %>% 
  msts(start=c(2012,1), ts.frequency = 365.3333, 
       seasonal.periods = c(28,362:366) ) %>%
  mstl(iterate = 100) %T>%
  autoplot() 
  


results %>%
  as.data.frame() %T>%
  model.check() %T>%
  write.csv('../data/created/timeseries/decomp/decomp_all.csv',row.names = F) %>%
  select(Remainder) %T>%
  write.csv('../data/created/timeseries/decomp/residu_all.csv',row.names=F) ->
  residuals.mstl
#---------------------------------------------
var(residuals.mstl)
mean(residuals.mstl)
range(residuals.mstl)
pacf(residuals.mstl)
1 - (var(residuals.mstl)/var(accidentsDF$NB_Accidents))
Box.test(residuals.mstl, lag=1, type='Ljung')
Box.test(residuals.mstl, lag=10)
max(0, 1 - var(residuals.mstl)/(var(residuals.mstl+results[2,])))
for (i in 3:8){
  print(max(0, 1 - var(residuals.mstl)/(var(residuals.mstl[,1]+results[,i]))))
  
}

results %>% forecast(method= 'naive',model='stlm') %T>% autoplot %>% View
ols_test_normality(residuals.mstl)
#----------------------------------------------------------------------------
#-----------------

accidentsDF.cluster <- read.csv('../data/created/timeseries/ts_accidents_by_date_cluster.csv',
                        header = TRUE) %>%
  select(-starts_with('NB'), -starts_with('as.factor.Accident'),
       NB_Accidents) %>%
  mutate(Date = as.Date(Date))

  
accidentsDF.cluster %>%
  group_by(cluster_id) %>% 
  select(-NB_Accidents) %T>%
  group_walk(~ write.csv(.x,paste0('../data/created/timeseries/decomp/predictors_',
  .y$cluster_id,'.csv'),row.names = F)) %>%
  select(-Date) ->
  predictors.cluster



accidentsDF.cluster %<>%
  select(Date, NB_Accidents, cluster_id)


apply.mstl <- function(d){
    d %<>%
    msts(start=c(2012,1), ts.frequency = 365.3333, 
       seasonal.periods = c(28,362:366) ) %>%
    mstl(iterate = 100)
    plot(d) 
    return(as.data.frame(d))
}
results.clusters <- accidentsDF.cluster %>%
  group_by(cluster_id) %>%
  do(apply.mstl(.$NB_Accidents))

model.check <- function(decomp){
  
  avS = c()
  for (i in 4:9){
    avS = c(avS,max(0, 1 - var(decomp$Remainder)/(var(decomp$Remainder+decomp[,i]))))
    
  }
  avS = round(mean(avS),3)
  
  cat(paste0('\n-----------------------------------------------------------------',
          "\nCluster number ", decomp$cluster_id[1],
          "\nVar(R)=",round(var(decomp$Remainder),3),
         "\tMean(R)=", round(mean(decomp$Remainder),3),
         "\t CV=", round(sd(decomp$Remainder)/mean(decomp$Remainder),3),
         '\nLjung p-value=',
         round((Box.test(decomp$Remainder, lag=1, type='Ljung'))$p.value,3),
         '\nVariability explained = ',
         round(1 - (var(decomp$Remainder)/var(accidentsDF.cluster$NB_Accidents)),3),
         '\nTrend importance: ',
        round(max(0, 1 - var(decomp$Remainder)/(var(decomp$Remainder+decomp$Trend))),3),
        '\t mean seasonality importance: ', avS,
        '\n-----------------------------------------------------------------'
  ))
    
  write.csv(decomp, 
            paste0('../data/created/timeseries/decomp/decomp_',
            decomp$cluster_id[1],'.csv'),row.names = F)
    write.csv(decomp$Remainder, 
                paste0('../data/created/timeseries/decomp/residu_',
                       decomp$cluster_id[1],'.csv'),row.names = F)
  return (data.frame(0))
}

#decomp=as.data.frame(results) 

results.clusters %>% 
  as.data.frame %>%
  arrange(cluster_id) %>%
  group_by(cluster_id) %>% 
  do(model.check(.)) 

