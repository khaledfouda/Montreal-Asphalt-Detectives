setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")

accidents <- read.csv('../data/collisions_routieres_road_accidents.csv',
                      header = TRUE)
summary(accidents)
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



accidents %>%
  select(DT_ACCDN, contains('NB_',ignore.case = TRUE),
         VITESSE_AUTOR, GRAVITE, CD_GENRE_ACCDN, CD_ETAT_SURFC,
         CD_ECLRM, CD_ENVRN_ACCDN, CD_COND_METEO, RUE_ACCDN,
         REG_ADM, MRC, LOC_LONG, LOC_LAT) %>%
  select(-NB_METRE_DIST_ACCD) -> accidents

round(accidents %>% is.na() %>% colSums() / nrow(accidents)*100,3) %>%
  sort() %>% 
  kable

