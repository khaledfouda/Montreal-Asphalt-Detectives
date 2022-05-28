setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")

library(tidyverse)
library(ggpubr)
library(gridExtra) # Allows for multiple plots with grid arrangement
library(ggiraph) # Expand on ggplot library
library(ggiraphExtra) # Expand on ggplot library
#library(ggradar) # Expands ggplot library with radar plot
library(waffle)
library(scales)
library(knitr)
#library(epiDisplay)



accidentsDF <- read.csv('../data/created/Road_Accidents_clean.csv')
countsDF <- read.csv('../data/created/counts_cars_pedast_cyclists_2017_2020.csv')
conditionsDF <- read.csv('../data/created/Road_conditions_2019_clean_coordinates_fixed.csv')
LOC.MAP <- read.csv('../data/created/Location_mapper_random.csv')
#------------------------------------------------------------------------
LOC.MAP %>% 
  select(ID, Cluster) -> LOC.MAP
accidentsDF %>%
  left_join(LOC.MAP, by='ID') %>% 
  select(-ID, -Longitude, -Latitude) -> accidentsDF
countsDF %>%
  left_join(LOC.MAP, by='ID') %>% 
  select(-ID, -Longitude, -Latitude) -> countsDF
conditionsDF %>%
  left_join(LOC.MAP, by='ID') %>% 
  select(-ID, -Longitude, -Latitude) -> conditionsDF


#--------------------------------------------------------------------------
accidentsDF %>%
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
  select(-Weather, -`WeatherOther or Unknown`) -> accidentsDF
#-----------------------------------------------------------------
write.csv(accidentsDF, '../data/created/timeseries/ts_accidents.csv',
          row.names = F)

accidentsDF %>% 
  group_by(Cluster, Date) %>%
  summarise(across(everything(), sum)) %>%
  View



with(accidentsDF,model.matrix(~ Accident_Category + 0)) %>% View



precaret::dummyVars(Accident_Type ~ ., data=accidentsDF) %>% View
