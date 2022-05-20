setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")

accidentsDF <- read.csv('../data/collisions_routieres_road_accidents.csv',
                      header = TRUE)
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



accidentsDF %>%
  select(DT_ACCDN, contains('NB_',ignore.case = TRUE),
         VITESSE_AUTOR, GRAVITE, CD_GENRE_ACCDN, CD_ETAT_SURFC,
         CD_ECLRM, CD_ENVRN_ACCDN, CD_COND_METEO, RUE_ACCDN,
         MRC, LOC_LONG, LOC_LAT) %>%
  select(-NB_METRE_DIST_ACCD) %>%
  # remove the 3 null rows of NB_*
  filter_at(vars(contains('NB',ignore.case = FALSE)), all_vars(!is.na(.))) %>%
  # drop the 14 rows with no location data
  filter_at(vars(contains('LOC',ignore.case = FALSE)), all_vars(!is.na(.))) %>%
  # Translate categories in Severity
  mutate(Severity=factor(GRAVITE,
                        levels = c("Dommages matériels inférieurs au seuil de rapportage",
                                   "Dommages matériels seulement", "Grave", "Léger", "Mortel"),
                        labels = c('Material Low', 'Material High',
                                   'Severe Injury', 'Light Injury', "Deadly")),
         .keep='unused') %>%
  # Fixing null values and renaming accident type. Moreover, adding
  # a new categorical variable.
  mutate(Accident_Type = ifelse(is.na(CD_GENRE_ACCDN), -1, CD_GENRE_ACCDN),
         .keep='unused') %>%
  mutate(Accident_Category = case_when(Accident_Type %in% 31:39 ~ 'Moving Object',
                                       Accident_Type %in% 40:59 ~ 'Fixed Object',
                                       Accident_Type %in% c(71:75,99) ~ 'No Collision',
                                       Accident_Type == -1 ~ 'Unknown',
                                       TRUE ~ 'ERROR')) %>%
  # Renaming authorized speed and replace null variables with a category
  mutate(Auth_Speed = ifelse(is.na(VITESSE_AUTOR), 'Unknown', VITESSE_AUTOR),
         .keep='unused') %>%
  # Translating and assigning null values for surface conditions
  mutate(Surface_Cond = factor(CD_ETAT_SURFC,
                               levels=c(11,12,13,14,15,16,17,
                                        18,19,20,99,0, NA),
                               labels = c('Dry', 'Wet', 'Water accumulation',
                                          'Sand or gravel','Slush','Snow',
                                          'Hard snow','Ice','Mud','Oil',
                                          'Other or Unknown',
                                          'Other or Unknown',
                                          'Other or Unknown'),exclude = NULL),
         .keep = 'unused') %>%
  # Translating and assigning null values for illumination
  mutate( Illumination = factor(CD_ECLRM, levels = c(1,2,3,4,0, NA),
                                labels =  c('Day and clear', 'Day and semi-dark',
                                            'Night and lighted','Night and dark',
                                            'Unknown','Unknown' ),exclude = NULL),
          .keep = 'unused') %>%
  # same as above for the environment
  mutate( Environment = factor(CD_ENVRN_ACCDN, levels = c(1,2,3,4,5,6,7,9,0, NA),
                               labels =  c('School area','Residential','Commercial',
                                           'Industrial','Rural','Forest','Park',
                                           'Other or Unknown',
                                           'Other or Unknown',
                                           'Other or Unknown')
                               ,exclude = NULL), .keep = 'unused') %>%
  # same as above for the weather
  mutate( Weather = factor(CD_COND_METEO, levels = c(11,12,13,14,15,16,
                                                     17,18,19,99,0, NA),
                           labels =  c('Clear Sky','Cloudy','Foggy','Drizzle',
                                       'Heavy Rain','Strong Wind', 'Snow',
                                       'Snowstorms', 'Black Ice',
                                       'Other or Unknown',
                                       'Other or Unknown',
                                       'Other or Unknown'),
                           exclude = NULL), .keep = 'unused') %>%
  # Dropping street names for now. Too complicated for the moment.
  select(-RUE_ACCDN) %>%
  # Removing accidents outside Montreal
  filter(MRC == 'Montréal (66 )') %>%
  # no longer need the variable.
  select(-MRC) %>%
  # Rename the date column
  mutate(Date = DT_ACCDN, .keep='unused') ->
  accidents
#-----------------------------------------------------------------------
  # shows the percentage of null values in each column
  (accidents %>% is.na() %>% colSums() / nrow(accidents)*100) %>%
  sort(decreasing = TRUE) %>%
  as.data.frame %>%
  filter(. > 0) %>%
  kable
#---------------------------------------------------------------------
  # shows the rows of null values in specific column(s)
accidents %>%
  #select(contains('NB',ignore.case = FALSE)) %>%
  filter_at(vars(contains('NB',ignore.case = TRUE)), any_vars(is.na(.))) %>%
  View()
#------------------------------------------------------------------------
accidents %>% 
  select(REG_ADM) %>%
  table(useNA = 'ifany') %>% sort(decreasing = TRUE)  %>% kable
  View()

  
  
  
  
  
  
  
  
  
  