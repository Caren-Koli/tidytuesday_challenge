product_hunt <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-04/product_hunt.csv')

library(tidyverse)
library(stringr)
library(showtext)
library(ggwordcloud)


font_add_google(family = "Julee","Julee")
font_add_google(family = "Philosopher","Philosopher")

showtext_auto()

product_df <- product_hunt %>% 
  mutate(category_tags = str_replace_all(category_tags, "\\[", "")) %>% 
  mutate(category_tags = str_replace_all(category_tags, "\\]", "")) %>% 
  mutate(category_tags = str_replace_all(category_tags, "'", "")) %>% 
  mutate(category_tags = as.vector(str_split(category_tags, ","))) %>% 
  unnest(category_tags) %>% mutate(category_tags = trimws(category_tags))
 

  df2 <- product_df %>% 
         group_by(category_tags) %>% 
         summarize(count = n())

      
    

ggplot(df2,aes(label = category_tags,size = count,color = count,alpha = count, family = "Philosopher"))+
  ggwordcloud::geom_text_wordcloud_area(area_corr_power = 1) +
  scale_radius(range = c(3,18))+
  scale_color_gradient(low = "#17B57A", high = "#E62A06")+
  theme_void() +
  scale_alpha(range = c(1, 9384)) +
  labs(title = "Most Common categories on Product Hunt")+
  theme(
    plot.background = element_rect(fill = "#F8F9C1", color = NA),
    plot.title = element_text(family = "Julee", size = 18, hjust = 0.5, vjust = -1, color = "#E62A06" ))
    

