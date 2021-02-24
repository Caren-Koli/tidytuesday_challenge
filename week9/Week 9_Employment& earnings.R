library(tidytuesdayR)
library(tidyverse)
library(ggplot2)


week9 <- tt_load(week = 9, 2021)

earnings <- week9$earn
employ <- week9$employed

tab1 <- employ %>% 
        select(industry,employ_n,year) %>% 
       filter(year %in% c(2015,2020))

       
# average employment numbers
tab2 <- tab1 %>% 
        group_by(industry,year) %>% 
        summarise(avg_n = mean(employ_n)) %>% na.omit()

#change in numbers between 2015 and 2020
tab3 <- tab2 %>% 
        pivot_wider(names_from = "year", values_from = "avg_n") %>% 
        mutate(change = round(`2020`-`2015`),0)

##plot
plot <- ggplot(data = tab3,aes(x = reorder(industry,change), y = change/1000, fill = change < 0))+
        geom_bar(stat = "identity")+ 
        scale_fill_manual(guide = F,values = c("blue","red"))+
        scale_y_continuous(labels = c("100K","50K","0","50K"), breaks = c(-100,-50,0,50))+
        coord_flip()+
        theme(plot.background = element_rect(fill = "#ffffe6"),
              panel.background = element_rect(fill = NA),
              panel.grid.major.y = element_line(colour = "gray"),
              plot.caption = element_text(hjust = 0.75,face = "italic"))+
        labs(x = "", y = "",
             caption = "#TidyTuesday week 9 \n data source:BLS \n graphic: CarenKoli")+
         
  annotate(geom = "text", x = 19.275, y = -50, size = 5, color = "#800080",family = "arial",
           label = "Change in Industrial employment numbers between 2015 and 2020")+
  annotate(geom = "curve",x = 1,
           y = -106, 
           xend = 6.5,
           yend = -100, 
           curvature = -.3,
           arrow = arrow(length = unit(3,"mm")))+
  annotate(geom = "text", 
           x = 7, 
           y = -78, size = 4, color = "red",
           label = "A huge employment drop in major service \n occupation numbers with no change in production, \n transportation, and material \n moving occupations")+
  annotate(geom = "curve",
           x= 19, y = 63, xend = 13.5, yend = 45,
           curvature = -.3,
           arrow = arrow(length = unit(3,"mm")))+
  annotate(geom = "text", x = 12, y = 48, size = 4, color = "blue",
           label = "A huge spike in employment \n numbers in management,professional \n and  major related occupations")

plot
       