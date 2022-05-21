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
