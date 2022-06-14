
library(tidyverse)

library(scales)
library(knitr)
#library(epiDisplay)
library(forecast)
library(lubridate)
library(grid)

setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")

accidentsDF <- read.csv('../data/created/timeseries/ts_accidents_by_date.csv',
                        header = TRUE) #%>%
  #select(Date, NB_Accidents)


accidentsDF %>%
  mutate(YYM=paste0(year(Date),'-',sprintf("%02d",month(Date)),'-01')) %>% 
  mutate(YYM = as.Date(YYM, format='%Y-%m-%d')) %>%
  arrange(year(Date),month(Date)) %>%
  group_by(YYM) %>%
  summarise(count = sum(NB_Accidents)) -> ts.data



draw_ts <- function(ts.df){
  p = ggplot(ts.df, aes(YYM, count)) +
    geom_vline(xintercept=as.numeric(ts.df$YYM[yday(ts.df$YYM)==1]), colour="grey60", alpha=.4) +
    geom_line() +
    scale_x_date(date_labels="%b", date_breaks="3 month", expand=c(0,0)) +
    scale_y_continuous(labels = scales::comma,
                       breaks=scales::pretty_breaks(n=10))+
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(), panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),axis.line = element_line(colour = "black")) +
    labs(x="", y="Number of accidents", title="Number of reported road accidents happenning in Montreal during 10 years span",
         subtitle = "Data is aggregated by month and the x-axis is labelled for every 3 months") 
  
  # Get the grob
  g <- ggplotGrob(p)
  
  # Get the y axis
  index <- which(g$layout$name == "axis-b")  # Which grob
  xaxis <- g$grobs[[index]]   
  
  # Get the ticks (labels and marks)
  ticks <- xaxis$children[[2]]
  
  # Get the labels
  ticksB <- ticks$grobs[[2]]
  
  # Edit x-axis label grob
  # Find every index of Jun in the x-axis labels and add a newline and
  # then a year label
  junes = which(ticksB$children[[1]]$label == "Jul")
  ticksB$children[[1]]$label[junes] = paste0(ticksB$children[[1]]$label[junes],
                                             "\n      ", unique(year(ts.df$YYM))) 
  
  # Make the edit
  # Center the month labels between ticks
  #ticksB$children[[1]]$label = paste0(paste(rep(" ",7),collapse=""), ticksB$children[[1]]$label)
  
  # Find every index of Jun in the x-axis labels and a year label
  junes = grep("Jun", ticksB$children[[1]]$label)
  ticksB$children[[1]]$label[junes] = paste0(ticksB$children[[1]]$label[junes],
                                             "\n      ", unique(year(ts.df$YYM))) 
  
  # Put the edited labels back into the plot
  ticks$grobs[[2]] <- ticksB
  xaxis$children[[2]] <- ticks
  g$grobs[[index]] <- xaxis
  
  
  
  # Draw the plot
  grid.newpage()
  grid.draw(g)
  
}

draw_ts(ts.data)


accidentsDF %>%
  mutate(Year = year(Date)) %>%
  select(Year, NB_MORTS, NB_BLESSES_GRAVES, NB_BLESSES_LEGERS) %>%
  rename(nb_Dead = NB_MORTS, nb_lightly_injured = NB_BLESSES_LEGERS,
         nb_severly_injured = NB_BLESSES_GRAVES) %>%
  arrange((Year)) %>%
  group_by(Year) %>%
  summarise_all(sum) %>% 
  gather(key="Type", value = "Count", -Year) %>% 
  mutate(Type = factor(Type, levels=c('nb_severly_injured',
                                      'nb_lightly_injured','nb_Dead'),
                       labels=c( 'Severe Injuries', "Light  Injuries",'Deaths'))) %>%
  arrange(Type) %>%
  group_by(Year)%>% 
  mutate( pos = cumsum(Count)-.5*Count) %>%  
  ungroup() %>%
  group_by(Type) %>%
  mutate(pct_change = round((Count/lag(Count) - 1 )*100 ,0) ) %>%
  mutate(pct_color = case_when(
    is.na(pct_change) ~ "black",
    pct_change >= 0 ~ "red",
    pct_change < 0 ~ "green",
    TRUE ~ 'purple'
  ))%>%
  
  ungroup() %>%
  mutate(pct_change = ifelse(is.na(pct_change), 
                            paste0('(',scales::comma(Count),')'),
                             case_when (
    pct_change >= 0 ~ paste0('+', pct_change,'%'),
    pct_change < 0 ~ paste0('-', abs(pct_change), '%')
    ))) %>% 
  mutate(pct = round(prop.table(Count),4),
         hjust.p = c(rep(-.9,9),rep(0,9),rep(1.15,9))) %>% 
  ggplot(aes(x=as.factor(Year), y=Count, label=pct_change)) +
  geom_bar(aes(fill=forcats::fct_rev(Type)),stat="identity") +
  geom_text(aes(y=pos, hjust=hjust.p), stat='identity',color='white',
            size=6)+
            #position=position_stack()) +
  scale_fill_manual(values=c("#fc3279", "#74cf8c",'#201f24'))+ 
  scale_x_discrete(limits=rev)+ 
  scale_y_continuous(expand = c(0, 0))+
  coord_flip()+
  theme(panel.background = element_blank(),
        panel.grid.minor.x = element_blank(), panel.grid = element_blank(),
        panel.border = element_blank(),axis.line.x = element_line(colour = "black"),
        axis.text.y = element_text(size = 20),
        axis.ticks.y = element_blank(),
        legend.position='top',
        legend.text = element_text(size=20),
        legend.title = element_text(size=20),
        title = element_text(size=20))+
  guides(fill=guide_legend(title="Injury Type:")) +
  labs(x="", y="Total Number of Injuries", 
       title="Changes in the total number of injuries caused by car accidents",
       subtitle = "Data is aggregated by year and the first row contains the number of injuries per category for the first year") 

#---------------Severity --------------------------------------
accidentsDF %>%
  mutate(Year = year(Date)) %>%
  select(Year, SeverityDeadly, SeverityLight.Injury, SeveritySevere.Injury,
         SeverityMaterial.High, SeverityMaterial.Low) %>%
  #rename(nb_Dead = NB_MORTS, nb_lightly_injured = NB_BLESSES_LEGERS,
  #       nb_severly_injured = NB_BLESSES_GRAVES) %>%
  arrange(Year) %>%
  group_by(Year) %>%
  summarise_all(sum) %>%
  gather(key="Type", value = "Count", -Year) %>%
  mutate(pct = round(prop.table(Count),4)) %>%
  ggplot(aes(x=as.factor(Year), y=Count, label=scales::percent(pct))) +
  geom_bar(aes(fill=Type),stat="identity") +
  geom_text(stat='identity') +
  coord_flip()
#-------------------------------------------------------------
accidentsDF %>%
  mutate(Year = year(Date)) %>%
  select(Year, NB_BLESSES_MOTO, NB_BLESSES_PIETON, NB_BLESSES_VELO,
         NB_DECES_MOTO, NB_DECES_PIETON, NB_DECES_VELO) %>%
  
  arrange(Year) %>%
  group_by(Year) %>%
  summarise_all(sum) %>% View
#-----------------------------------------------------------------
accidentsDF %>%
  mutate(Year = year(Date)) %>%
  select(Year, starts_with('nb',ignore.case = FALSE), -NB_Accidents) %>%
  arrange(Year) %>%
  group_by(Year) %>%
  summarise_all(sum) %>%
  mutate(newSum = select_if(., is.numeric) %>% 
           reduce(`+`)) %>% 
  mutate_if(is.numeric, list(~ ./newSum)) %>% 
  select(-newSum) %>%

  `*`(100) %>%
  round()%>%
  gather(key="Type", value = "Count", -Year) %>%
  ggplot(aes(x=Year, y=Count)) +
  geom_line(aes(color=Type))


  


