library(tidytuesdayR)
library(tidyverse)
library(ggplot2)

week8 <- tt_load(week = 8,2021)

conjugal_df <- week8$conjugal
## data management
df <- conjugal_df %>% 
      pivot_longer(cols = 3:5, names_to = "conjugal_condition", values_to = "count") %>% 
      mutate(Population = toupper(Population)) %>% 
      mutate(population = factor(Population, levels = c("NEGROES","GERMANY"))) %>% 
      mutate(Conjugal_condition = factor(conjugal_condition, levels = c("Divorced and Widowed","Married","Single")))

##plot
p1 <- ggplot(data = df, aes(x = population, y = count, fill = Conjugal_condition))+
      geom_bar(stat = "identity",position = "stack")+
      geom_text(aes(label = paste0(count,"%")),
                position = position_stack(vjust = 0.5))+
      scale_fill_manual(values = c( "#004d00","#ffd11a", "#e60000"))+
      scale_y_continuous(limits = c(-4,100))+
  
  
  theme(legend.position = "top",
        legend.title = element_blank(),
        axis.text.x = element_blank(),
        strip.text = element_blank(),#removes labels of facets
        panel.background = element_rect(fill = "#ffffe6"),
        plot.background = element_rect(fill = "#ffffe6"),
        plot.title = element_text(hjust = 0.5, face = "bold"))+
  geom_text(aes(x = 1.5,y = 0,label = str_c("Ages\n",Age), hjust = 1.1))+ #Age labels
  coord_flip()+
  
  facet_wrap(~Age, nrow = 3)+
  labs(title = "CONJUGAL CONDITION", y = "", x = "")
p1
  
