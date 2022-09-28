
bigfoot <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-09-13/bigfoot.csv')

library(tidyverse)
library(ggplot2)
library(maps)


bigfoot_df <- bigfoot %>% 
               drop_na() %>% 
               filter(season != "Unknown") %>% 
               filter(longitude > -130)

  
map2 <- ggplot(data = bigfoot_df)+
       
        geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group),
                     color = "#f0f7e7",fill = "#adcaa4", size = 1)+
        geom_point(aes(x = longitude, y = latitude, color = visibility))+
        scale_color_gradient(high = "#a4502d", low = "#f8ccbb")+
        facet_wrap(~season,nrow = 2)+ 
        theme_void()+
        theme(#legend.position = "none",
              plot.background = element_rect(fill = "#e1e2e6"),
              plot.title = element_text(face = "bold", hjust = 0.5, colour = "#3e7250", size = 18))+
        labs(title = "BigFoot Visibility" )
  
        
map2
       


