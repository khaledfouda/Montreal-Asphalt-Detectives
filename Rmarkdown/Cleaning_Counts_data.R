setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")
#------------------------------------------------------
library(knitr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(scales)
#-----------------------------------------------------------------------
counts19_21 <- read_csv("../data/counts_cars_pedast_cyclists_2019_2021.csv")
counts17_19 <- read_csv("../data/counts_cars_pedast_cyclists_2017_2019.csv")
counts14_16 <- read_csv('../data/counts_cars_pedast_cyclists_2014_2016.csv')
counts11_13 <- read_csv('../data/counts_cars_pedast_cyclists_2011_2013.csv')
#-------------------------------------------------------------------

counts17_19 %>%
  mutate(Date = ymd(Date)) %>%
  filter(Date < "2019-03-06") %>%
  rbind(mutate(counts19_21, Date=ymd(Date))) %>%
  rbind(mutate(counts14_16, Date=ymd(Date))) %>%
  rbind(mutate(counts11_13, Date=ymd(Date))) %>%
  select(-one_of(c('Periode', 'Heure', 'Minute', 'Seconde', 'Id_Reference',
                 'Localisation_X', 'Localisation_Y'))) %>%
  group_by(Id_Intersection, Date, Code_Banque, Longitude, Latitude,
           Nom_Intersection, Description_Code_Banque) %>%
  summarise(across(everything(), sum), .groups = 'drop') %>%
  separate(Nom_Intersection, c('Intersection_1','Intersection_2'),
           ' / ', fill="right", extra="drop") %>%
  mutate(COUNT = NBLT + NBT + NBRT + SBLT + SBT + SBRT + EBLT + 
           EBT + EBRT + WBLT + WBT + WBRT + Approche_Nord + 
           Approche_Sud + Approche_Est + Approche_Ouest) %>%
  select(-one_of(c('NBLT' , 'NBT' , 'NBRT' , 'SBLT' , 'SBT' , 'SBRT','EBLT', 
                   'EBT' , 'EBRT' , 'WBLT' , 'WBT' , 'WBRT' , 'Approche_Nord' , 
                   'Approche_Sud' , 'Approche_Est' , 'Approche_Ouest'))) -> 
  counts

counts %>%
  filter(Id_Intersection == 17234) %>%
  separate(Intersection_1, c('Intersection_1','Intersection_2'),
           '_') -> counts[counts$Id_Intersection == 17234,]
counts %>%
  filter(Id_Intersection == 18026) %>%
  separate(Intersection_1, c('Intersection_1','Intersection_2'),
           ' traverse piétonne niveau ') -> 
  counts[c11$Id_Intersection == 18026,]
counts %>%
  filter(Id_Intersection == 19170) %>%
  mutate(Intersection_1 = "Lajeunesse") -> 
  counts[counts$Id_Intersection == 19170,]


counts %>% 
  filter(Code_Banque != 3) %>%
  select(-Code_Banque) %>%
  spread(key = Description_Code_Banque, value=COUNT, fill=0) -> 
  counts
#-------------------------------------------------------------------

write.csv(counts, '../data/created/counts_cars_pedast_cyclists_2017_2020.csv',
          row.names = FALSE)
#-----------------------------------------------------------