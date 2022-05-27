
setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")
library(tidyr)
library(tidyverse)





counts <- read_csv("../data/created/counts_cars_pedast_cyclists_2017_2020.csv")
conditions <- read_csv('../data/created/Road_conditions_2019_clean_coordinates_fixed.csv')
accidents <- read_csv('../data/created/Road_Accidents_clean.csv')

conditions %>% 
  mutate(ID = paste0('COND_',1:nrow(conditions))) -> conditions
counts %>% 
  mutate(ID = paste0('COUN_',1:nrow(counts))) -> counts
accidents %>% 
  mutate(ID = paste0('ACCI_',1:nrow(accidents))) -> accidents

conditions %>% write.csv('../data/created/Road_conditions_2019_clean_coordinates_fixed.csv',
                      row.names = FALSE)
counts %>% write.csv('../data/created/counts_cars_pedast_cyclists_2017_2020.csv',
                     row.names = FALSE)
accidents %>% write.csv('../data/created/Road_Accidents_clean.csv',
                        row.names = FALSE)
#--------------------------------------------------------------

conditions %>% select(ID, Longitude, Latitude) -> LOC.MAP

counts %>% select(ID, Longitude, Latitude) %>% 
  rbind(LOC.MAP) -> LOC.MAP

accidents %>% select(ID, Longitude, Latitude) %>% 
  rbind(LOC.MAP) -> LOC.MAP

LOC.MAP %>% mutate(Cluster = NA) -> LOC.MAP
#-------------------------------------------------------------
write.csv(LOC.MAP, '../data/created/Location_mapper.csv',
          row.names = FALSE)
#-------------------------------------------------------------
# Random clusters:
LOC.MAP = read_csv('../data/created/Location_mapper.csv')
LOC.MAP$Cluster = sample(1:10, nrow(LOC.MAP),replace = T)
write.csv(LOC.MAP, '../data/created/Location_mapper_random.csv',
          row.names = FALSE)
