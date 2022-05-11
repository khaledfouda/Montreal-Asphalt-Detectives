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

accidents %>% ggplot(aes(x=factor(AN))) +
  geom_bar() +
  stat_count( geom="text", aes(label=..count..) ,vjust = -1)+ 
  labs(title="Accidents Per Year", x="Year", y="Number of Accidents") ->an
 

accidents %>%
  ggplot(aes(x=factor(JR_SEMN_ACCDN,
                      levels=c("LU","MA","ME","JE","VE","SA","DI"),
                      labels = c("Monday", "Tuesday", "Wednesday", "Thursday",
                                 "Friday", "Saturday", "Sunday")))) +
  geom_bar() +
  stat_count( geom="text", aes(label=..count..) ,vjust = -1)+ 
  labs(title="Accidents Per Day", x="Year", y="Number of Accidents") ->jr


accidents$HEURE_ACCDN %>%
  lapply(function(x) gsub("Non précisé", NA, strsplit(x,':')[[1]][1])) %>%
  as.numeric() -> accidents$HR_ACCDN

hr_labels = c("12AM", sprintf("%dAM", 1:11), "12PM", sprintf("%dPM", 1:11))
accidents %>%
  drop_na(HR_ACCDN) %>%
  ggplot(aes(x=factor(HR_ACCDN,
                                  levels=seq(0,23,1),
                                  labels= hr_labels))) +
  geom_bar() +
  scale_x_discrete(breaks=hr_labels[seq(1,25,3)])+
  labs(title="Accidents Per Hour", x="Year", y="Number of Accidents")  ->hr


grid.arrange(valueBox(nrow(accidents), "Entries"),
             valueBox(ncol(accidents), "Features"),nrow= 1)


ggarrange(an, jr, hr,
          ncol = 2, nrow = 2)

#------------------------------------------------------------

accidents %>%
  count(grav=factor(GRAVITE,
         levels = c("Dommages matériels inférieurs au seuil de rapportage",
                    "Dommages matériels seulement", "Grave", "Léger", "Mortel"),
         labels = c('Material dammages only <= $2k', 'Material dammages only > $2k',
                    'Severe injuries', 'Light injuries', 
                    "Deadly"))) %>%
  mutate(pct = prop.table(n)) %>%
  ggplot(aes(x=reorder(grav,desc(pct)), y= n, label = scales::percent(pct))) +
  #geom_bar() +
  coord_flip() +
  geom_col(position='dodge' ) +
  theme(axis.text.y = element_text(face='bold', angle = 0, size=10)) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = 0.5,
            hjust = -1,
            size = 6)  +
  labs(title="The Severity of Accidents", y="Count", x="Severity") +
  scale_y_continuous(expand = expansion(add=c(0,20000)))
#-----------------------------------------------------------------------

round(table(accidents$HEURE_ACCDN)[['Non précisé']] / nrow(accidents) * 100,2)
  

accidents[accidents$HEURE_ACCDN == 'Non précisé']



accidents$GRAVITE %>% table(useNA = 'always') %>% kable

#-----------------------------------------------------------------

accidents$NB_VICTIMES_TOTAL %>% table


accidents %>% 
  select(NB_VICTIMES_TOTAL, NB_MORTS, NB_BLESSES_GRAVES, NB_BLESSES_LEGERS,
         NB_VICTIMES_PIETON, NB_DECES_PIETON, NB_BLESSES_PIETON,
         NB_VICTIMES_MOTO, NB_DECES_MOTO, NB_BLESSES_MOTO,
         NB_VICTIMES_VELO, NB_DECES_VELO, NB_BLESSES_VELO) %>%
  
  colSums() %>%  
  as.data.frame  %>% 
  rename( COUNT = ".") %>%
  # rownames_to_column(var = "FEATURE") %>%
  mutate(FEATURE = c("Total", "Deaths", "Injuries", "Injuries",
                     rep(c("Total", "Deaths", "Injuries"),3))) %>%
  mutate(CATEGORY = c(replicate(4,"Total"),
                      replicate(3, "Pedestrians"),
                      replicate(3, "Motorcyclists"),
                      replicate(3, "Cyclists"))) %>%
  filter( CATEGORY != 'Total') %>%
  ggplot(aes(x=factor(FEATURE,
                      levels = c("Total","Deaths", "Severe",
                                 "Light", "Injuries")),
                      y=COUNT, fill=CATEGORY)) +
  geom_bar(stat = 'identity',position = 'stack')+  
  #scale_y_log10()+
  facet_wrap(. ~ factor(FEATURE,levels=c("Total", "Injuries", "Deaths")),
             scales = "free",nrow = 1) +
  geom_text(aes(label=comma(COUNT)), position=position_stack(vjust = .3))+
  geom_text(aes(label=CATEGORY), position=position_stack(vjust = .7)) +
  theme(legend.position = "none", axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) +
labs(title="Victims Breakdown", y="", x="") 


#factor(CATEGORY,
   #                    levels=c("Total", "Pedestrians", "Motorcyclists",
  #                              "Cyclists")),
    #          scales = "free",nrow = 1) +
  
#----------------------------------------------------------------------


accidents %>% 
  select(NB_VICTIMES_TOTAL, NB_MORTS, NB_BLESSES_GRAVES, NB_BLESSES_LEGERS,
         NB_VICTIMES_PIETON, NB_DECES_PIETON, NB_BLESSES_PIETON,
         NB_VICTIMES_MOTO, NB_DECES_MOTO, NB_BLESSES_MOTO,
         NB_VICTIMES_VELO, NB_DECES_VELO, NB_BLESSES_VELO) %>%
  
  colSums() %>%  
  as.data.frame  %>% 
  rename( COUNT = ".") %>%
  mutate(FEATURE = c("Total", "Deaths", "Severe Injuries", "Light Injuries",
                     rep(c("Total", "Deaths", "Injuries"),3))) %>%
  mutate(CATEGORY = c(replicate(4,"Total"),
                      replicate(3, "Pedestrians"),
                      replicate(3, "Motorcyclists"),
                      replicate(3, "Cyclists"))) %>%
  filter( CATEGORY == 'Total' & FEATURE != 'Total') %>%
  ggplot(aes(x=factor(FEATURE, levels = c("Light Injuries", "Severe Injuries",
                                          "Deaths")),y=COUNT,fill=FEATURE))+
  geom_bar(stat='identity') +
  geom_text(aes(label=comma(COUNT)), 
            position=position_dodge(width=.2),vjust=-2) +
  scale_y_continuous(expand = expansion(add=c(0,6000))) +
  labs(title="Breakdown by Injury Type", x='', y='') +
  theme(legend.position = 'none')
#------------------------------------------------------------------------

accidents %>%
  select( nb_automobile_camion_leger, nb_camionLourd_tractRoutier,
         nb_outil_equipement, nb_tous_autobus_minibus, nb_bicyclette,
         nb_cyclomoteur, nb_motocyclette, nb_taxi, nb_urgence,
         nb_motoneige, nb_VHR, nb_autres_types, nb_veh_non_precise) %>%
  filter(complete.cases(.)) %>%
  colSums() %>%
  as.data.frame %>%
  rename(COUNT = ".") %>%
  mutate(PERC = COUNT/
           sum(accidents$NB_VEH_IMPLIQUES_ACCDN, na.rm=TRUE)*100) %>%
  mutate(PERC = paste0('(', round(PERC, 3),"%)")) %>%
  mutate(TYPE = c( "Automobiles & Light Trucks", "Heavy Trucks & Tractors",
                  "Equipements", "Buses", "Bicycles", "Scooters", "Motorcycles",
                  "Taxis", "Emergencies", "Snowmobiles", "Dirt Bikes", "Others",
                  "Not Specified")) %>%
  ggplot(aes(x=reorder(TYPE, - COUNT), y=COUNT, fill = TYPE)) +
  geom_bar(stat='identity', alpha=.4,,colour='#00abff') +
  coord_flip() +
  geom_text(aes(label = paste0(comma(COUNT),'  ',PERC)),position=position_dodge(width=.2),
            hjust = -.2) +
  theme(legend.position = 'none', 
        axis.text.y = element_text(face='bold', size=10)) +
  scale_y_continuous(expand = expansion(add=c(0,60000))) +
  labs(title = "Breakdown of the total number of vehicles involved by the type",
       x='', y='') +
  theme(panel.background = element_blank(), axis.ticks.y = element_blank())


#--------------------------------------------------

accidents  %>%
  select(VITESSE_AUTOR, AN) %>%
  group_by(AN) %>%
  table %>%
  as.data.frame %>%
  ggplot(aes(x=factor(VITESSE_AUTOR), y=Freq, fill=VITESSE_AUTOR)) +
  geom_bar(stat='identity') +
  facet_wrap(. ~ factor(AN), nrow = 3) +
  labs(title = "Authorised speed per year for when the accidents happenned",
       x = 'Authorised Speed', y ='Frequency of Accidents')+
  theme(legend.position = 'none', panel.background = element_blank())
#------------------------------------------------------------------------  
accidents %>%
  select(CD_ETAT_SURFC) %>%
  replace(is.na(.), 0) %>%
  table(useNA = 'ifany') %>%
  as.data.frame %>%
  mutate(CD_ETAT_SURFC = factor(CD_ETAT_SURFC,
                                levels=c(11,12,13,14,15,16,17,
                                         18,19,20,99,0),
                                labels = c('Dry', 'Wet', 'Water accumulation',
                                           'Sand or gravel','Slush','Snow',
                                           'Hard snow','Icy','Muddy','Oily',
                                           'Other','Not specified'))) %>%
  mutate(PERC = Freq / nrow(accidents) * 100) %>%
  mutate(PERC = paste0('(', round(PERC, 2),"%)")) %>%
  ggplot(aes(x=reorder(CD_ETAT_SURFC,-Freq), y= Freq, fill=CD_ETAT_SURFC)) +
  geom_bar(stat='identity', alpha=.4,colour='#00abff') +
  coord_flip() +
  geom_text(aes(label = paste0(comma(Freq),'  ',PERC)),position=position_dodge(width=.2),
            hjust = -.2) +
  theme(legend.position = 'none', 
        axis.text.y = element_text(face='bold', size=10)) +
  scale_y_continuous(expand = expansion(add=c(0,60000))) +
  labs(title = "Frequency of accidents per surface type",
       x='', y='') +
  theme(panel.background = element_blank(), axis.ticks.y = element_blank())

#----------------------------------------------------------------------

accidents %>%
  select(CD_ECLRM) %>%
  replace(is.na(.), 0) %>%
  table(useNA = 'ifany') %>%
  as.data.frame %>%
  mutate(CD_ECLRM = factor(CD_ECLRM, levels = c(1,2,3,4,0),
                           labels =  c('Day and clear', 'Day and semi-dark',
                                       'Night and lighted','Night and dark',
                                       'Not specified' ))) %>%
  mutate(PERC = Freq / nrow(accidents) * 100) %>%
  mutate(PERC = paste0('(', round(PERC, 2),"%)")) %>%
  ggplot(aes(x=reorder(CD_ECLRM,-Freq), y= Freq, fill=CD_ECLRM)) +
  geom_bar(stat='identity', alpha=.4,colour='#00abff') +
  coord_flip() +
  geom_text(aes(label = paste0(comma(Freq),'  ',PERC)),position=position_dodge(width=.2),
            hjust = -.2) +
  theme(legend.position = 'none', 
        axis.text.y = element_text(face='bold', size=10)) +
  scale_y_continuous(expand = expansion(add=c(0,60000))) +
  labs(title = "Lightning at the time of the accident",
       y='Number of Accidents', x='') +
  theme(panel.background = element_blank(), axis.ticks.y = element_blank())
#-------------------------------------------------------------------------


accidents %>%
  select(CD_ENVRN_ACCDN) %>%
  replace(is.na(.), 0) %>%
  table(useNA = 'ifany') %>%
  as.data.frame %>%
  mutate(CD_ENVRN_ACCDN = factor(CD_ENVRN_ACCDN, levels = c(1,2,3,4,5,6,7,9,0),
                           labels =  c('School area','Residential','Commercial',
                                       'Industrial','Rural','Forest','Park',
                                       'Other','Not specified' ))) %>%
  mutate(PERC = Freq / nrow(accidents) * 100) %>%
  mutate(PERC = paste0('(', round(PERC, 2),"%)")) %>%
  ggplot(aes(x=reorder(CD_ENVRN_ACCDN,-Freq), y= Freq, fill=CD_ENVRN_ACCDN)) +
  geom_bar(stat='identity', alpha=.4,colour='#00abff') +
  coord_flip() +
  geom_text(aes(label = paste0(comma(Freq),'  ',PERC)),position=position_dodge(width=.2),
            hjust = -.2) +
  theme(legend.position = 'none', 
        axis.text.y = element_text(face='bold', size=10)) +
  scale_y_continuous(expand = expansion(add=c(0,20000))) +
  labs(title = "The domminant activity in the area where accidents happenned",
       y='Number of Accidents', x='') +
  theme(panel.background = element_blank(), axis.ticks.y = element_blank())

#-------------------------------------------------------------------
accidents %>%
  select(CD_COND_METEO) %>%
  table(useNA = 'ifany') / nrow(accidents) * 100




#-------------------------------------------------------------------------


accidents %>%
  select(CD_ETAT_CHASS) %>%
  replace(is.na(.), 0) %>%
  table(useNA = 'ifany') %>%
  as.data.frame %>%
  mutate(CD_ETAT_CHASS = factor(CD_ETAT_CHASS, levels = c(1,2,3,4,5,6,9,0),
                                 labels =  c('In a good condition',
                                             'Under construction', 'Ruts',
                                             'Significant cracks', 'Potholes',
                                             'Slopes', 'Other',
                                             'Not specified' ))) %>%
  mutate(PERC = Freq / nrow(accidents) * 100) %>%
  mutate(PERC = paste0('(', round(PERC, 2),"%)")) %>%
  ggplot(aes(x=reorder(CD_ETAT_CHASS,-Freq), y= Freq, fill=CD_ETAT_CHASS)) +
  geom_bar(stat='identity', alpha=.4,colour='#00abff') +
  coord_flip() +
  geom_text(aes(label = paste0(comma(Freq),'  ',PERC)),position=position_dodge(width=.2),
            hjust = -.2) +
  theme(legend.position = 'none', 
        axis.text.y = element_text(face='bold', size=10)) +
  scale_y_continuous(expand = expansion(add=c(0,30000))) +
  labs(title = "The Quality of the Road",
       y='Number of Accidents', x='') +
  theme(panel.background = element_blank(), axis.ticks.y = element_blank())
#----------------------------------------------------------------------------



accidents %>%
  select(CD_COND_METEO) %>%
  replace(is.na(.), 0) %>%
  table(useNA = 'ifany') %>%
  as.data.frame %>%
  mutate(CD_COND_METEO = factor(CD_COND_METEO, levels = c(11,12,13,14,15,16,
                                                          17,18,19,99,0),
                                labels =  c('Clear Sky','Cloudy','Foggy','Drizzle',
                                            'Heavy Rain','Strong Wind', 'Snow',
                                            'Snowstorms', 'Black Ice', 'Other',
                                            'Not specified' ))) %>%
  mutate(PERC = Freq / nrow(accidents) * 100) %>%
  mutate(PERC = paste0('(', round(PERC, 2),"%)")) %>%
  ggplot(aes(x=reorder(CD_COND_METEO,-Freq), y= Freq, fill=CD_COND_METEO)) +
  geom_bar(stat='identity', alpha=.4,colour='#00abff') +
  coord_flip() +
  geom_text(aes(label = paste0(comma(Freq),'  ',PERC)),position=position_dodge(width=.2),
            hjust = -.2) +
  theme(legend.position = 'none', 
        axis.text.y = element_text(face='bold', size=10)) +
  scale_y_continuous(expand = expansion(add=c(0,30000))) +
  labs(title = "Weather Conditions",
       y='Number of Accidents', x='') +
  theme(panel.background = element_blank(), axis.ticks.y = element_blank())







