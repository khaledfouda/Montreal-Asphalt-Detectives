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

