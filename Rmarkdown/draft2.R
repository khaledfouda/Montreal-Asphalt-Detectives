setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")

#accidents <- read.csv('../data/collisions_routieres_road_accidents.csv',
#                      header = TRUE)
library(tidyverse)
library(ggpubr)
library(gridExtra) # Allows for multiple plots with grid arrangement
library(ggiraph) # Expand on ggplot library
library(ggiraphExtra) # Expand on ggplot library
#library(ggradar) # Expands ggplot library with radar plot
library(waffle)
library(scales)
library(knitr)
library(lubridate)



counts19_21 <- read_csv("../data/counts_cars_pedast_cyclists_2019_2021.csv")
counts17_19 <- read_csv("../data/counts_cars_pedast_cyclists_2017_2019.csv")

counts19_21$Description_Code_Banque %>% table(useNA = 'ifany') / nrow(counts19_21) * 100
counts17_19$Description_Code_Banque %>% table(useNA = 'ifany') %>% kable

(colSums(is.na(counts19_21))) %>% kable

counts17_19 %>%
  mutate(da = ymd(Date)) %>%
  summarise(min = min(da), max = max(da))


counts19_21 %>%
  mutate(da = ymd(Date)) %>%
  summarise(min = min(da), max = max(da))
