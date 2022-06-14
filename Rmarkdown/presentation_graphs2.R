

library(tidyverse)

library(scales)
library(knitr)
#library(epiDisplay)
library(forecast)
library(lubridate)
library(grid)

setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")
# counts.df <- read_csv("E:/Montreal-Asphalt-Detectives/data/created/timeseries/ts_counts_by_date.csv")
# 
# 
# 
# counts.df %>%
#   mutate(Year = year(Date)) %>%
#   mutate(Trucks = Camions + Camions + Camions.articules + Camions.legers + 
#            Camions.Lourds + Camions.porteurs) %>%
#   select(Year, Trucks, Autos, Bus, Pietons, Velos) %>%
#   arrange(Year) %>%
#   group_by(Year) %>%
#   summarise_all(sum) %>% 
#   gather(key="Type", value = "Count", -Year) %>%
#   group_by(Type) %>%
#   mutate(pct_inc = ( (Count-lag(Count)) / lag(Count) *100)) %>%
#   mutate(pct_inc = ifelse(is.na(pct_inc), 0, round(pct_inc,2))) %>%
#   
#   #mutate(newSum = select_at(., vars(-Year)) %>% 
#   #         reduce(`+`)) %>% 
#   #mutate_at(vars(-Year), list(~ ./newSum)) %>% 
#   #select(-newSum) %>%
#   #`*`(100) %>%
#   #round(2) %>%
#   #mutate(Year = Year/100) %>%
#   #gather(key="Type", value = "Count", -Year) %>%
#   ggplot(aes(x=Year, y=pct_inc)) +
#   geom_line(aes(color=Type))
#------------------------------------------------------------------

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



draw_ts <- function(ts.df, new.seg, all.df){
    ts.df %>%
    ggplot(aes(YYM, count)) +
    geom_vline(xintercept=as.numeric(ts.df$YYM[yday(ts.df$YYM)==1]), colour="grey60", alpha=.4) +
    geom_line(color='blue', size=.8, alpha=.3) +
    geom_line(data=new.seg,color='blue', size=1,alpha=.7) +
    scale_x_date(date_labels="%b", date_breaks="3 month", expand=c(0,0),
                 limits = as.Date(c('2012-01-01','2020-12-01'))) +
    scale_y_continuous(labels = scales::comma, limits = c(0,3800),
                       breaks=scales::pretty_breaks(n=10))+
    theme_bw() +
    theme(panel.grid.minor.x = element_blank(), panel.grid = element_blank(),
          panel.background = element_blank(),
          panel.border = element_blank(),axis.line = element_line(colour = "black"),
          title = element_text(size=20),
          axis.text = element_text(size = 16)) +
    labs(x="", y="Number of accidents", title="Number of reported road accidents happenning in Montreal during 10 years span",
         subtitle = "Data is aggregated by month and the x-axis is labelled for every 3 months") -> p
  
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
                                             "\n      ", unique(year(all.df$YYM))) 
  
  # Make the edit
  # Center the month labels between ticks
  #ticksB$children[[1]]$label = paste0(paste(rep(" ",7),collapse=""), ticksB$children[[1]]$label)
  
  # Find every index of Jun in the x-axis labels and a year label
  junes = grep("Jun", ticksB$children[[1]]$label)
  ticksB$children[[1]]$label[junes] = paste0(ticksB$children[[1]]$label[junes],
                                             "\n      ", unique(year(all.df$YYM))) 
  
  # Put the edited labels back into the plot
  ticks$grobs[[2]] <- ticksB
  xaxis$children[[2]] <- ticks
  g$grobs[[index]] <- xaxis
  
  
  
  # Draw the plot
  grid.newpage()
  grid.draw(g)
  
}


ts.df = ts.data
draw_ts(ts.df[0,],ts.df[0,], ts.df)
draw_ts(ts.df[0,], ts.df[1:12,], ts.df)
draw_ts(ts.df[1:24,],ts.df[13:24,], ts.df)
draw_ts(ts.df[1:(12*8),],ts.df[25:(12*8),], ts.df)
draw_ts(ts.df[1:108,],ts.df[97:108,], ts.df)
draw_ts(ts.data,ts.data, ts.data)




#--------------
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
  #theme(axis.text.y = element_text(face='bold', angle = 0, size=10),
  #      panel.background = element_rect(fill = 'green'),
  #      axis.ticks.y = element_blank()) +
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = 0.5,
            hjust = -1,
            size = 6)  +
  labs(title="The Severity of Accidents", y="Count", x="") +
  scale_y_continuous(expand = expansion(add=c(0,20000)))






