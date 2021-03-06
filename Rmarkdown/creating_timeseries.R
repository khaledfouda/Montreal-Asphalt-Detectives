setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")

library(tidyverse)
library(ggpubr)
library(gridExtra) # Allows for multiple plots with grid arrangement
library(ggiraph) # Expand on ggplot library
library(ggiraphExtra) # Expand on ggplot library
library(waffle)
library(scales)
library(knitr)

library(padr)

accidentsDF <- read.csv('../data/created/Road_Accidents_clean.csv') %>%
  mutate(Date = as.Date(Date))
countsDF <- read.csv('../data/created/counts_cars_pedast_cyclists_2017_2020.csv')
conditionsDF <- read.csv('../data/created/Road_conditions_2019_clean_coordinates_fixed.csv')
LOC.MAP <- read.csv('../data/created/Location_mapper.csv')

weatherDF <- read.csv('../data/created/weather_no_nas.csv') %>%
  rename(Date=date) %>%
  select(-num_accidents) %>%
  mutate(Date = as.Date(Date))

clusters <- read.csv('../data/created/cluster_id.csv') %>%
  select(-Longitude, -Latitude)

#------------------------------------------------------------------------
LOC.MAP %>% 
  select(ID, cluster_id) -> LOC.MAP

accidentsDF %<>%
  left_join(clusters, by='ID') %>% 
  select(-ID, -Longitude, -Latitude)  

countsDF %>%
  left_join(LOC.MAP, by='ID') %>% 
  select(-ID, -Longitude, -Latitude) -> countsDF

conditionsDF %>%
  left_join(LOC.MAP, by='ID') %>% 
  select(-ID, -Longitude, -Latitude,-Coord_X, -Coord_Y) -> conditionsDF

#--------------------------------------------------------------------------


#--------------------------------------------------------------------------
accidentsDF %<>%
  # 1. Severity
  cbind(with(accidentsDF,model.matrix(~ Severity + 0))) %>%
  select(-Severity) %>%
  # 2. Accident type
  cbind(with(accidentsDF,model.matrix(~ as.factor(Accident_Type) + 0))) %>%
  select(-Accident_Type, -`as.factor(Accident_Type)-1`) %>%
  # 3. Accident category
  cbind(with(accidentsDF,model.matrix(~ Accident_Category + 0))) %>%
  select(-Accident_Category, -`Accident_CategoryUnknown`) %>%
  # 4. Authorized Speed
  cbind(with(accidentsDF,model.matrix(~ Auth_Speed + 0))) %>%
  select(-Auth_Speed, -`Auth_SpeedUnknown`) %>%
  # 5. Surface Condition
  cbind(with(accidentsDF,model.matrix(~ Surface_Cond + 0))) %>%
  select(-Surface_Cond, -`Surface_CondOther or Unknown`) %>%
  # 6. Illumination
  cbind(with(accidentsDF,model.matrix(~ Illumination + 0))) %>%
  select(-Illumination, -`IlluminationUnknown`) %>%
  # 7. Environment
  cbind(with(accidentsDF,model.matrix(~ Environment + 0))) %>%
  select(-Environment, -`EnvironmentOther or Unknown`) %>%
  # 7. Weather
  cbind(with(accidentsDF,model.matrix(~ Weather + 0))) %>%
  select(-Weather, -`WeatherOther or Unknown`) 
#-----------------------------------------------------------------

# for data per cluster / group by clusters then fill empty days
empty.days <- function(DF){
  cluster_id = DF$cluster_id[1]
  DF = pad(DF,start_val = as.Date("2012-01-01"),end_val = as.Date("2020-12-31"),
       interval = 'day')
  DF$cluster_id = cluster_id
  return(DF)
}

accidentsDF %>%
  group_by(Date, cluster_id) %>%
  summarise(NB_Accidents = n(), across(everything(), sum)) %>%
  arrange(cluster_id, Date) %>%
  group_by(cluster_id) %>%
  do(empty.days(.)) %>%
  replace(is.na(.), 0) %>%
  left_join(weatherDF, by='Date') %T>%
  write.csv('../data/created/timeseries/ts_accidents_by_date_cluster.csv',
            row.names = F) ->
  accidentsDF.c

#----------------------------------------------------------------
# for data per day only (clusters are factored)
accidentsDF.d <- accidentsDF %>%
  mutate(cluster_id_ = as.factor(cluster_id)) %>%
  cbind(with(., model.matrix(~ cluster_id_ + 0))) %>%
  select(- cluster_id, -cluster_id_) %>% 
  group_by(Date) %>%
  summarise(NB_Accidents = n(), across(everything(), sum)) %>%
  left_join(weatherDF, by='Date') %T>% 
  write.csv('../data/created/timeseries/ts_accidents_by_date.csv',
            row.names = F)
#----------------------------------------------------------------------
conditionsDF %>%
  #1. Arrondissement
  rename(arrondissement_ = arrondissement) %>%
  cbind(with(., model.matrix(~ arrondissement_ + 0))) %>%
  select(-arrondissement_) %>%
  # 2. Surface
  mutate(Surface_ = as.factor(Surface)) %>%
  cbind(with(., model.matrix(~ Surface_ + 0))) %>%
  select(-Surface, -Surface_) %>%
  #3. Drainage
  mutate(Drainage_ = as.factor(Drainage)) %>%
  cbind(with(., model.matrix(~ Drainage_ + 0))) %>%
  select(-Drainage, -Drainage_) %>% 
  # 4. Total
  mutate(Total = as.factor(round(Total))) %>%
  rename(Total_ = Total) %>%
  cbind(with(., model.matrix(~ Total_ + 0))) %>%
  select(-Total_) %>% 
  # 5. Surface type
  rename(Surface_type_ = Surface_type) %>%
  cbind(with(., model.matrix(~ Surface_type_ + 0))) %>%
  select(-Surface_type_, -Surface_type_None) %>% 
  group_by(cluster_id) %>%
  summarise(NB_Obs = n(), across(everything(), sum)) -> conditionsDF
#---------------------------------------------------------------------  
write.csv(conditionsDF, '../data/created/timeseries/ts_conditions_by_cluster.csv',
          row.names = F)
#---------------------------------------------------------------
countsDF %>%
  select(-Id_Intersection, -Intersection_1, -Intersection_2) -> countsDF
#---------------------------------------------------------------------
countsDF %>%
  mutate(cluster_id_ = as.factor(cluster_id)) %>%
  cbind(with(., model.matrix(~ cluster_id_ + 0))) %>%
  select(- cluster_id, -cluster_id_) %>% 
  group_by(Date) %>%
  summarise(NB_Accidents = n(), across(everything(), sum)) %>%
  write.csv('../data/created/timeseries/ts_counts_by_date.csv',
            row.names = F)

countsDF %>%
  group_by(Date, cluster_id) %>%
  summarise(NB_Accidents = n(), across(everything(), sum)) %>%
  write.csv('../data/created/timeseries/ts_counts_by_date_cluster.csv',
            row.names = F)
#-----------------------------------------------------------------------


