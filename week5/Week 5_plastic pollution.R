plastics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-26/plastics.csv')
library(tidyverse)
library(ggplot2)
library(countrycode)
library(patchwork)

df_plastics <- plastics %>% 
  mutate(continent = countrycode(country,origin = "country.name",destination = "continent")) %>% 
              filter(parent_company != "Grand Total") %>%  
              select(continent,parent_company,grand_total) %>% 
  mutate(parent_company = if_else(parent_company == "PepsiCo","Pepsico",parent_company)) %>% 
  filter(parent_company != "null") %>% filter(parent_company != "Unbranded")

##Africa top pollutants
africa <- df_plastics %>% 
          filter(continent == "Africa") %>% 
          group_by(parent_company) %>% 
          summarise(total = sum(grand_total)) %>% arrange(desc(total)) %>% head(10)
##plot

plot_africa <- ggplot(data = africa, aes(x = reorder(parent_company,-total),y = total))+
  geom_bar(stat = "identity",fill = "#d1261d")+
  coord_flip()+
  theme(plot.background = element_rect(fill = NA),
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#d1261d",face = 12))+
   annotate(geom = "text", size = 4, x = 7.8, y = 9500, label = 
              "Africa has the highest \n emission count of all the \n plastic categories")+
  labs(title = "Africa", x = " ", y = " ")

plot_africa
##america top pollutants
 america <- df_plastics %>% 
            filter(continent == "Americas") %>% 
            group_by(parent_company) %>% 
            summarise(total = sum(grand_total)) %>% arrange(desc(total)) %>% head(10)
 ##plot
 
 plot_america <- ggplot(data = america, aes(x = reorder(parent_company,-total),y = total))+
   geom_bar(stat = "identity",fill = "#548c45")+
   coord_flip()+
   theme(plot.background = element_rect(fill = NA),
         panel.grid = element_blank(),
         plot.title = element_text(hjust = 0.5, color = "#548c45",face = 12))+
   labs(title = "America", x = " ", y = " ")
 
 plot_america
 ##Asia top pollutants
 
 asia <- df_plastics %>% 
   filter(continent == "Asia") %>% 
   group_by(parent_company) %>% 
   summarise(total = sum(grand_total)) %>% arrange(desc(total)) %>% 
   mutate(parent_company = if_else(parent_company == "Tamil Nadu Co-operative Milk Producers' Federation Ltd","Tamil Nadu Milk",
                           if_else(parent_company == "Liwayway Holdings Company Limited", "Liwayway Holdings", parent_company))) %>% 
   head(10)
 ##plot
 plot_asia <- ggplot(data = asia, aes(x = reorder(parent_company,-total),y = total))+
   geom_bar(stat = "identity",fill = "#2e0b80")+
   coord_flip()+
   theme(plot.background = element_rect(fill = NA),
         panel.grid = element_blank(),
         plot.title = element_text(hjust = 0.5, color = "#2e0b80",face = 12))+
   labs(title = "Asia", x = " ", y = " ")
 plot_asia
           
 ##Europe top pollutants
 
 Europe <- df_plastics %>% 
   filter(continent == "Europe") %>% 
   group_by(parent_company) %>% 
   summarise(total = sum(grand_total)) %>% arrange(desc(total)) %>% 
  filter(parent_company != "NULL") %>% head(10)
 
 ##plot
 plot_Europe <- ggplot(data = Europe, aes(x = reorder(parent_company,-total),y = total))+
   geom_bar(stat = "identity",fill = "#b0b30e")+
   coord_flip()+
   theme(plot.background = element_rect(fill = NA),
         panel.grid = element_blank(),
         plot.title = element_text(hjust = 0.5, color = "#b0b30e",face = 12))+
   labs(title = "Europe", x = " ", y = " ")+
   annotate(geom = "text", size = 4, x = 6, y = 2000, label = 
              "While Pepsico is among the \n top 10 pollutant companies in other continents,\n it's not the case for Europe")
 plot_Europe
 
 ##oceania top pollutants
 Oceania <- df_plastics %>% 
   filter(continent == "Oceania") %>% 
   group_by(parent_company) %>% 
   summarise(total = sum(grand_total)) %>% arrange(desc(total)) %>%head(10)
 
 ##plot
 
 plot_oceania <- ggplot(data = Oceania, aes(x = reorder(parent_company,-total),y = total))+
         geom_bar(stat = "identity",fill = "#82f5f5")+
         coord_flip()+
   theme(plot.background = element_rect(fill = NA),
         panel.grid = element_blank(),
         plot.title = element_text(hjust = 0.5, color = "#82f5f5",face = 12))+
   labs(title = "Oceania", x = " ", y = " ")
         
plot_oceania
   

one_layout <- (plot_africa / plot_Europe)/(plot_america + plot_oceania) 
one_layout + plot_annotation(title = "Top 10 Pollutant companies in every continent",caption = "plastic pollution/tidytuesady \n visual:CarenKoli")
  