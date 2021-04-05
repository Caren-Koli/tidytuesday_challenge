library(tidyverse)
library(ggplot2)
library(lubridate)
library(patchwork)
critic <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/critic.tsv')
user_reviews <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv')
items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
villagers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/villagers.csv')

item_tab <- items %>% 
            group_by(category) %>% 
            summarise(count = n())
mytheme <-  theme(plot.title = element_text(face = "bold", hjust = 0.5, vjust = -0.25,size = 12),
                  axis.title = element_text(face = "italic", size = 12),
                  axis.text = element_text(face = 12),
                  axis.line = element_line(size = 1.5),
                  plot.subtitle = element_text(hjust = 0.5,size = 12),
                  plot.caption = element_text(face = "bold",size = 12),
                  panel.background = element_rect(fill = NA))
item_bargraph <- ggplot(data = item_tab, aes(x = reorder(category,count), y = count))+
                 geom_bar(stat = "identity", fill = "maroon")+
                 #geom_text(aes(label = count, hjust = 0.5, vjust = -0.25))+
                 mytheme+
                 coord_flip()+
                labs(title = "Distribution of category", x = "category",y = "frequency")
item_bargraph


scatterpl <- ggplot(data = items, aes(x = buy_value, y = sell_value))+
             geom_point(size = 3, color = "red")+
             mytheme+
             
            labs(title = "sale vs buy value",x = "sell_value",  y = "buy_value")
scatterpl
  

  
##species and personality
tab3 <- villagers %>% 
        group_by(species,personality) %>% 
        summarise(count = n()) %>%
        ungroup() %>% 
mutate(species = as.factor(species))
graph <- ggplot(data = tab3, aes(x = reorder(species,count), y = count, fill = personality))+
         geom_bar(stat = "identity", position = "dodge")+
         mytheme+
  coord_flip()+
         scale_fill_brewer(palette = "Accent")+
         labs(title = "Personality and species", x = "species", y = "count")+
          facet_wrap(~ personality, ncol = 8)
graph
     

##user reviews   

reviews_dates <- user_reviews %>% 
               mutate(year = year(date)) %>% 
               mutate(month = month(date,label = T, abbr = T)) %>% 
               mutate(weekday = wday(date, label = T, abbr = T)) %>% 
               select(grade,year,month,weekday) 
  
  
   month <- reviews_dates %>% 
             group_by(month) %>% 
               summarise(count = n()) 
     
   
   ##Monthly performance
     month_bargraph <- ggplot(data = month, aes(x = month, y = count))+
                      geom_bar(stat = "identity",fill = "cyan")+
                      geom_text(aes(label = count,hjust = 0.5, vjust = -0.25),
                                    position = position_dodge(width = 0.9))+
                      mytheme+
 
                      labs(title = "Monthly review performance", x = "Month", y = "count")
     month_bargraph
     
  ## weekday performance
    weekday <- reviews_dates %>% 
                group_by(weekday) %>%
                summarize(count = n()) %>% 
      
     
     weekday_graph <- ggplot(data = weekday, aes(x = weekday, y = count, group = 1 ))+
                      geom_line(size = 1, color = "pink")+
                     
                      mytheme+
                      labs(title = "weekday performance", x = "weekday", y = "frequency")
     weekday_graph
  
##critics                
 critic_dfr <- critic %>%
              mutate(month = month(date)) %>% 
              mutate(weekday = wday(date,label = T,abbr = T)) %>% 
              select(month, weekday) %>% 
              group_by(weekday) %>% 
              summarise(count = n()) %>% 
              mutate(perc = round(count/sum(count)*100,0))
 critic_graph <- ggplot(data = critic_dfr, aes(x = weekday, y = perc))+
                geom_bar(stat = "identity",fill = "gray")+
                geom_text(aes(label = paste0(perc,"%"), hjust = 0.5, vjust = -0.25),
                          position = position_dodge(width = 0.9))+
                mytheme+
                coord_flip()+
    
                labs(title = "critics-review monthly performance", x = "critics", y = "perc")
 critic_graph   
   
 
                 
 window <- item_bargraph  + scatterpl  / graph + month_bargraph  +weekday_graph 
 window