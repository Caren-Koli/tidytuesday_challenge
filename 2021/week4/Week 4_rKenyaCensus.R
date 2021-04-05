gender <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/gender.csv')
crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/crops.csv')
households <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-19/households.csv')

library(rKenyaCensus)
library(tidyverse)
library(sf)
library(tmap)
library(ggthemes)
library(patchwork)



##extract shapefile
         
Kenya_SHP <- KenyaCounties_SHP %>%
             st_as_sf() %>% 
             st_transform(crs = 4326) 
      
##drop first row in gender data to remain with counties only
gender_data <- gender[-1,]

##change shapefile county data to lowercase
Kenya_SHP1 <- Kenya_SHP %>% 
               mutate(County = tools::toTitleCase(tolower(County)))

##merge the shapefile data and gender data

new_df <- left_join(Kenya_SHP1,gender_data, by = "County")

## map males
map_male <- ggplot(data = new_df)+
  geom_sf(aes(geometry = geometry,fill = Male))+
  theme_map()+
  labs(title = "Males")+
  theme(legend.position = "left",
        plot.title = element_text(hjust = 0.5, face = 12, color = "#3db35d", size = 13),
        plot.caption = element_text(hjust = 0.8))+
       
  scale_fill_gradient(low = "#cafad7",high = "#3db35d")
map_male

## map intersex
map_intersex <- ggplot(data = new_df)+
  geom_sf(aes(geometry = geometry,fill = Intersex))+
  theme_map()+
  labs(title = "Intersex", caption = "tidytuesday//RKenyaCensus//shelKariuki\n visualized by:CarenKoli")+
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = 12, color = "#ee1f25", size = 13),
        plot.caption = element_text(hjust = 0.8))+
  
  scale_fill_gradient(low = "#fbd0d2", high = "#ee1f25")
map_intersex

## map females
map_female <- ggplot(data = new_df)+
  geom_sf(aes(geometry = geometry,fill = Female))+
  theme_map()+
  labs(title = "Females")+
  theme(legend.position = "right",
        plot.title = element_text(hjust = 0.5, face = 12, color = "#3538a6", size = 13),
        plot.caption = element_text(hjust = 0.8))+
  
  scale_fill_gradient(low = "#cbccf7", high = "#3538a6")
map_female

map_layout <- map_male + map_intersex +map_female
map_layout

