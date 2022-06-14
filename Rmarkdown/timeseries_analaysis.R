setwd("E:/Montreal-Asphalt-Detectives/Rmarkdown")

accidentsDF <- read.csv('../data/created/timeseries/ts_accidents_by_date.csv',
                        header = TRUE)
library(tidyverse)

library(scales)
library(knitr)
#library(epiDisplay)
library(forecast)
library(lubridate)
library(grid)


accidentsDF %>%
  select(Date, NB_Accidents) -> accidentsDF


accidentsDF %>%
  mutate(YYM=paste0(year(Date),'-',sprintf("%02d",month(Date)),'-01')) %>% 
  mutate(YYM = as.Date(YYM, format='%Y-%m-%d')) %>%
  arrange(year(Date),month(Date)) %>%
  group_by(YYM) %>%
  summarise(count = sum(NB_Accidents)) -> ts.data
# Define time series
dts = ts(ts.data$count, start = c(2012,1), end=c(2020,12),frequency = 12)


dts %>% 
  stl(s.window='periodic') %>%
  seasadj() -> acci.adj
autoplot(acci.adj)



fit = stl(dts, s.window = "period");fit$weights
plot(fit)

monthplot(dts)
seasonplot(dts)
fit <- auto.arima(dts);fit
plot(fit)
plot(forecast(fit,h=20))
#----------------------
# timeseries plot

ts.data %>% 
  ggplot(aes(x=YYM, y=count)) +
  geom_line(color='steelblue') +
  geom_point() +
  scale_x_date(date_breaks = '6 month', date_labels = '%b%y',
               date_minor_breaks = '2 month',
               limit=c(as.Date("2012-01-01"),as.Date("2020-12-01"))) +
  ylim(0,3505)
#----------------------------
# timeseries plot 2
p = ggplot(ts.data, aes(YYM, count)) +
  geom_vline(xintercept=as.numeric(ts.data$YYM[yday(ts.data$YYM)==1]), colour="grey60") +
  geom_line() +
  scale_x_date(date_labels="%b", date_breaks="3 month", expand=c(0,0)) +
  theme_bw() +
  theme(panel.grid.minor.x = element_blank()) +
  labs(x="", y="Number of accidents", title="Number of reported road accidents happenning in Montreal during 10 year span",
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
                                           "\n      ", unique(year(ts.data$YYM))) 

# Put the edited labels back into the plot
ticks$grobs[[2]] <- ticksB
xaxis$children[[2]] <- ticks
g$grobs[[index]] <- xaxis



# Make the edit
# Center the month labels between ticks
ticksB$children[[1]]$label = paste0(paste(rep(" ",7),collapse=""), ticksB$children[[1]]$label)

# Find every index of Jun in the x-axis labels and a year label
junes = grep("Jun", ticksB$children[[1]]$label)
ticksB$children[[1]]$label[junes] = paste0(ticksB$children[[1]]$label[junes],
                                           "\n      ", unique(year(ts.data$YYM))) 

# Draw the plot
grid.newpage()
grid.draw(g)














