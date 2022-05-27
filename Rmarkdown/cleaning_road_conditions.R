library(geojsonio)
library(knitr)
library(lubridate)
library(tidyr)
library(tidyverse)
library(scales)
setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")

#-------------------------------------------------------
r.cond = geojson_read('../data/condition-ruelles-2019.json', what='sp')
coords = data.frame()
for (i in 1:length(r.cond@polygons)) {
  coords = rbind(coords,cbind(r.cond@polygons[[i]]@Polygons[[1]]@coords,i))
}
colnames(coords) = c('Coord_X', 'Coord_Y', 'ID')
#--------------------------------------------------------------
# cleaning
r.cond %>% 
  as.data.frame %>%
  mutate(ID = 1:length(r.cond)) %>%
  merge(coords, by="ID", all.y=TRUE) %>%
  select(Coord_X, Coord_Y, arrondissement, type_surface,
         condition_surface, condition_drainage, cote_globale) %>%
  mutate(Surface = factor(condition_surface,
                          levels = c('bon','Bon','Mauvais','Passable',
                          'Très bon','Très bon','Très mauvais'),
                          labels = c(2,2,4,3,1,1,5)),
         Total = rescale(as.numeric(cote_globale),c(1,5), c(.5,5)),
         Drainage = factor(condition_drainage,
                           levels = c("-","Bon","Mauvais","Passable",
                                      "Très bon", "Très mauvais"),
                           labels = c( NA, 2, 4, 3, 1, 5)),
         Surface = as.numeric(as.character(Surface)),
         Total = as.numeric(as.character(Total)),
         Drainage = as.numeric(as.character(Drainage))) %>%
  select(-c('condition_drainage', 'condition_surface', 'cote_globale')) %>%
  filter(!(is.na(Surface) & is.na(Drainage) & is.na(Total))) %>%
  mutate(Surface = ifelse(is.na(Surface),
                          median(Surface, na.rm=TRUE), Surface),
         Total = ifelse(is.na(Total),
                          median(Total, na.rm=TRUE), Total),
         Drainage = ifelse(is.na(Drainage),
                          median(Drainage, na.rm=TRUE), Drainage),
         Surface_type = factor(type_surface, 
                               levels= c("Asphalte", "Asphalte ",
                                         "aucun", "Aucun","Béton","Béton ",
                                         "Briques ", "Pavé","Pavé ",
                                         "Pavé uni",NA),
                               labels = c('Asphalt', 'Asphalt', 'None', 'None',
                                          'Concrete', 'Concrete', 'Bricks',
                                          'Paved', 'Paved', 'Paved','None'),
                               exclude = NULL)) %>%
  select(-type_surface)->
  r.cond.df
#------------------------------------------------------------------
write.csv(r.cond.df, '../data/created/Road_conditions_2019_clean.csv',
          row.names = FALSE)
#---------------------------------------------------------------------

