
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

accidents %>% select(ID, Longitude, Latitude) -> LOC.MAP

counts %>% select(ID, Longitude, Latitude) %>% 
  rbind(LOC.MAP) -> LOC.MAP.ALL

conditions %>% select(ID, Longitude, Latitude) %>% 
  rbind(LOC.MAP.ALL) -> LOC.MAP.ALL

LOC.MAP %>% mutate(Cluster = NA) -> LOC.MAP
LOC.MAP.ALL %>% mutate(Cluster = NA) -> LOC.MAP.ALL
#-------------------------------------------------------------
write.csv(LOC.MAP, '../data/created/Location_mapper.csv',
          row.names = FALSE)
write.csv(LOC.MAP.ALL, '../data/created/Location_mapper_all.csv',
          row.names = FALSE)
#-------------------------------------------------------------
# Random clusters:
LOC.MAP %>% 
  mutate(Cluster = sample(1:40, nrow(LOC.MAP),replace = T)) %>%
  write.csv('../data/created/Location_mapper_random.csv',
          row.names = FALSE)
LOC.MAP.ALL %>% 
  mutate(Cluster = sample(1:40, nrow(LOC.MAP.ALL),replace = T)) %>%
  write.csv('../data/created/Location_mapper_all_random.csv',
            row.names = FALSE)
