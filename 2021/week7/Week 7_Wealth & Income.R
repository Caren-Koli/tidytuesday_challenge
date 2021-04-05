library(tidytuesdayR)
library(tidyverse)
library(ggplot2)

 

df <- data$race_wealth %>% na.omit %>% 
      mutate(decade = as.factor(year-year %% 10)) %>% 
      mutate(wealth_fam = if_else(type == "Median",-1*wealth_family, wealth_family)) %>% 
      mutate(decade = paste0(decade,"s"))

p2 <- ggplot(data = df, aes( x = decade, y = wealth_fam))+
        geom_bar(data = filter(df,type == "Median"), stat = "identity", fill = "#615c5c")+
        geom_bar(data = filter(df,type == "Average"), stat = "identity", fill = "#c2180c")+
        coord_flip()+
        annotate(geom = "text",label = "median", color = "#615c5c", x = 5.9,y = -250000, family = "mono", size = 3.5)+
        annotate(geom = "text", label = "average",color = "#c2180c",x = 5.9, y = 400000, family = "mono",size = 3.5)+
        scale_x_discrete(expand = c(-1,7))+
        scale_y_continuous(labels = c("2million","1million","0","1million","2million"), breaks = c(2000000,1000000,0,1000000,2000000))+
        facet_wrap(~race)+
  theme(plot.title = element_text(hjust = 0.5,color = "#121682",face = "bold", family = "mono"),
        plot.background = element_rect(fill = "#bdbdd9"),
        panel.background = element_rect(fill = "#ebebf0"),
        panel.grid.major.x  = element_blank())+
        labs(title = "FAMILY WEALTH BY RACE OVER THE DECADES \n WITH MEASURES OF CENTRAL TENDENCY", x = "", y = "")
p2


 
