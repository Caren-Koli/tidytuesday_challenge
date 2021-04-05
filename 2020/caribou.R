individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')
library(tidyverse)
library(ggplot2)
library(sf)
library(ggthemes)
library(patchwork)
library(lubridate)


##location mapping trial with scatterplot                
loc <- ggplot(data = locations, aes(x = longitude, y = latitude,color = season))+
         geom_point(size = 1)+
         scale_color_manual(values = c("blue","red"))
loc
##mapping with ggplot2
###
loc1 <- locations %>% 
        st_as_sf(coords =c("longitude","latitude"),crs = 4326)
       
map1 <- ggplot(data = loc1)+
        geom_sf(aes(geometry = geometry, fill = season,color = season))+
         theme_map()+
        theme_void()
  map1
  
loc3 <- locations %>% 
        select(timestamp,longitude,latitude)%>% 
        mutate(month = month(timestamp,label = T,abbr = T)) %>% 
        st_as_sf(coords = c("longitude","latitude"), crs = 4326)
map <- ggplot(data = loc3)+
       geom_sf(aes(geometry = geometry, fill = month, color = month))+
       theme_map()+
       theme(
         legend.position = "bottom",
         plot.title = element_text(face = 12,hjust = 0.5, colour = "maroon"))+
       labs(title = "Month of deployment")
map
   
loc2 <- locations %>%
        st_as_sf(coords = c("longitude","latitude"))
map2 <- ggplot(data = loc2)+
        geom_sf(aes(geometry = geometry, fill = study_site, color = study_site))+
        theme_map()+
        theme(legend.position = "bottom",
               plot.title = element_text(face = 12,color = "maroon", hjust = 0.5))+
        #theme_void()+
  facet_wrap(~season,ncol = 2)+
  labs(title = "seasonal mapping of deployment study sites")
       
map2

output = map / map2
output

