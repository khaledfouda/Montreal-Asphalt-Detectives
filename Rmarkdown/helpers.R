library(tidyverse)
library(ggpubr)
library(gridExtra) # Allows for multiple plots with grid arrangement
library(ggiraph) # Expand on ggplot library
library(ggiraphExtra) # Expand on ggplot library
#library(ggradar) # Expands ggplot library with radar plot
library(waffle)
library(scales)
valueBox <- function(value, label, dim = c(4,6), colorPalette = "Dark2") {
  
  # error msg when vectors have different sizes
  if (length(value) != length(label)) stop("Number of labels is different to values")
  
  # Converts vectors into dataframe to use on ggplot
  df <- data.frame(value, label,
                   x = (rep(seq(2, 3*as.numeric(dim[2]), as.numeric(dim[2]) + 0.25), ceiling(length(value)/3)))[1:length(value)],
                   y = rep(seq(0, as.numeric(dim[1])*ceiling(length(value)/3)+0.5, as.numeric(dim[1])+0.25),each=3)[1:length(value)]) %>% 
    mutate(h = rep(as.numeric(dim[1]),nrow(.)),
           w = rep(as.numeric(dim[2]),nrow(.)),
           color = factor(1:nrow(.)))
  
  # Uses ggplot to create boxes
  ggplot(df, aes(x, y, height = h, width = w, label = label)) +
    geom_tile(aes(fill = color)) +
    geom_text(color = "white", fontface = "bold", size = 8,
              aes(label = value, x = x - 0.3 , y = y + 0.3), hjust = 0) +
    geom_text(color = "white", fontface = "bold", size = 3,
              aes(label = label, x = x - 0.3 , y = y - 0.3), hjust = 0) +
    coord_fixed() +
    scale_fill_brewer(type = "qual",palette = colorPalette) +
    #geom_text(size = 20, aes(label = shape, family = font_family, x = x + 1.5, y = y + 0.5), alpha = 0.25) +
    theme_void() +
    guides(fill = FALSE)
}