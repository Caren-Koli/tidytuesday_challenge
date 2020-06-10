marbles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-02/marbles.csv')
library(tidyverse)
library(ggplot2)
library(lubridate)
##Average scores for top 10 teams by every marble
team <- marbles %>% 
        group_by(team_name,marble_name) %>% 
        summarise(avg_points = round(mean(points,na.rm = T),0)) %>% 
        #ungroup() %>% 
        arrange(desc(avg_points)) %>% 
        head(10)

team1 <- ggplot(data = team,aes(x = team_name, y= avg_points,fill = marble_name))+
         geom_bar(stat = "identity",position = "stack")+
         geom_text(aes(label = avg_points,hjust = 0.5, vjust = 5),
                   position = position_stack())+
         scale_fill_brewer(palette = "Set3")+
         labs(title = "Top 10 team performance on every marble", x = "team", y = "avg points")
team1



##Did teams score more points while home or away?
points <- marbles %>% 
          group_by(team_name,host) %>% 
          summarise(points_earned = sum(points,na.rm = T))



points1 <- ggplot(data = points,aes(x = team_name, y= points_earned,fill = host))+
           geom_bar(stat = "identity",position = "stack")+
  scale_fill_manual(values = c("#8a5050","blue"))
           labs(title = "Did teams score more points while home or away", x = "team_name",y = "points earned")+
          facet_wrap(~host,ncol = 2)
points1

##Which days did the teams host most games?
date2 <- marbles %>% 
         select(date,race,host,marble_name,points,team_name) %>% 
         mutate(date1 = ymd(date)) %>% 
         mutate(month = month(date1,abbr = T,label = T)) %>% 
         mutate(weekday = wday(date1,abbr = T,label = T))


host <- date2 %>% 
        group_by(team_name,host,weekday) %>% 
        summarise(n = n())

host1 <- ggplot(data = host,aes(x = weekday, y = n,fill = host))+
         geom_bar(stat = "identity", position = "stack")+
         scale_fill_manual(values = c("blue","red"))+
         facet_wrap(~team_name, nrow = 4)+
         labs(title = "which days did the teams host the games?", x = "weekday", y = "no_days")
host1

##How many points did the marbles roll with to victory?
 marble_point <- date2 %>% 
                 filter(marble_name %in% c("Speedy","Snowy","Smoggy","Prim","Orangim")) %>%
                group_by(marble_name,weekday) %>% 
                summarise(total_points = sum(points,na.rm = T)) 

         
line_graph <- ggplot(data = marble_point,aes(x = weekday,y = total_points,color = marble_name,group = marble_name))+
              geom_line(size = 1)+
              scale_color_brewer(palette = "Accent")+
              labs(title = "How many points did marbles roll \n with to victory on a daily basis",
                   x = "weekday",y = "total points")
                   
  line_graph

         

     
