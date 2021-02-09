
library(tidytuesdayR)
library(tidyverse)
library(ggplot2)
library(ggpubr)

tt <- tt_load(2021, week = 6)
df <- tt$hbcu_all



tidy_df <- df %>% 
           select(- `Total enrollment`,-`Total - Public`, -`Total - Private`) %>%
           rename(Y4_college =  `4-year`,
                  Y2_college = `2-year`,
                 Y4_Public = `4-year - Public`,
                 Y2_Public = `2-year - Public`,
                Y4_Private =  `4-year - Private`,
                Y2_Private = `2-year - Private`) %>% 
           
          pivot_longer(cols = 2:3, names_to = "Gender", values_to = "gender_n") %>% 
          pivot_longer(cols = 2:7, names_to = "school enrolled", values_to = "schoolenrolled_n" ) %>% 

          mutate(decade = Year - Year %% 10) %>% 
          mutate(decade = as.factor(decade)) %>% 
          mutate(sch_enrolled = as.factor(`school enrolled`)) 
          

##
stats2 <- tidy_df %>% 
          group_by(sch_enrolled) %>%  
           
          summarise(average_enrollment = round(mean(schoolenrolled_n),0)) %>% 
          mutate(perc = round((average_enrollment/563134)*100),0) %>% 
          mutate(perc1 = paste0(perc,"%")) %>% 
          filter(sch_enrolled!= "Y2_Private") %>% #remove Y2_private since % is zero
          mutate(label = str_c(sch_enrolled,perc1, sep = "\n"))

pie <- ggpie(data = stats2, x = "perc", 
             label = "label", color = "white",lab.pos = "in",
             fill = "sch_enrolled",
             lab.font ="black",
             border.width = 2,
             label.size = 2)+
  scale_fill_brewer(palette = "Set2") + 
  theme(
    plot.title = element_text(face = "bold",family = "mono",size = 12, hjust = 0.5, colour = "purple"),
    panel.background = element_rect(fill = "gray"),     
    legend.position = "none")+
  labs(title = "HBCU enrollment by school 'category' for black students")
  
  
pie



