energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')
library(tidyverse)
library(ggplot2)
library(patchwork)

reshaped_df <- energy_types %>% 
               select(type,"2016","2017","2018") %>% 
               pivot_longer(c("2016","2017","2018"),names_to = "year",values_to = "energy")
df2 <- reshaped_df %>% 
       group_by(type,year) %>% 
       summarize(energy_totals = sum(energy))
##What amount of energy did every type produce yearly

 

linegraph <- ggplot(data = df2, aes(x = type,y = energy_totals,group = year,color = year))+
             geom_line(size = 1)+
             scale_color_manual(values = c("red","blue","yellow"))+
             coord_flip()+
             labs(title = "What amount of energy did every type produce yearly?",
                      x = "enery type",y = "")+
             theme(
             plot.title = element_text(face = "bold", size = 12, hjust = 0.5, vjust = -0.5, color = "maroon"),
             axis.title = element_text(face = "italic", size = 11),
             axis.text = element_text(face = 11),
             panel.background = element_rect(fill = "gray11"),
             panel.grid = element_blank())+
             theme(legend.position = "bottom")

  
linegraph
## yearly production of energy

df3 <- reshaped_df %>% 
       group_by(type) %>% 
       summarize(energy_total = sum(energy)) %>% 
       mutate(perc = round(energy_total/sum(energy_total)*100,0))
bar_df3 <- ggplot(data = df3, aes(x = reorder(type,-energy_total), y = energy_total))+
           geom_bar(stat = "identity", fill = "cyan")+
           #coord_flip()+
          geom_text(aes(label = paste0(perc,"%"),hjust = 0.5,vjust = -0.25))+
          theme(
          plot.title = element_text(face = "bold", size = 12, hjust = 0.5, vjust = -0.5, color = "maroon"),
          axis.title = element_text(face = "italic", size = 11),
          axis.text = element_text(face = 11, color = "black"),
          panel.background = element_rect(fill = NA),
          panel.grid = element_blank())+
  labs(title = "Energy production per type",x = "energy", y = "")
bar_df3
##
output <- linegraph + bar_df3
output
           