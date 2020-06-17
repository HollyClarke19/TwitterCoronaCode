
g1<-generic_ca_comb %>% 
  group_by(Date)%>% 
  count() %>% 
  filter(Date != "2020-04-24")%>%
  filter(Date != "2020-05-12")

g2<-locations %>% group_by(Date) %>% count(country, sort = TRUE) %>% ungroup() %>% filter(country == "United Kingdom" | country == "Australia" ) %>% 
  filter(Date != "2020-04-24")%>%
  filter(Date != "2020-05-12")

m1<-ggplot() + 
  geom_line(data = g1, aes(x= Date, y= n, lty= "Number of daily   \nCOVID-19 app\ntweets"),color = "#1DA1F2", size =1.5)+
  labs(x= "\nDate",
       y = "Number of tweets\n",
       title = "Time series of Covid-19 app tweets",
       subtitle = "Daily count of tweets about Covid-19 apps (top) and daily proportion of geo-located tweets produced \nin the UK and Australia (bottom), 25 April - 11 May 2020\n",
       lty = "") +
  theme_minimal() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.line = element_line(colour = "grey", size =1),
        axis.title.y = element_text(size =10),
        axis.text = element_text(size= 11),
        axis.title = element_text(size =12),
        axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        plot.title=element_text(family="Times", face="bold", size=20),
        plot.subtitle=element_text(family="Times", size=14))+
  annotate("text",
           x = as.Date("2020-04-28"),
           y = 1200,
           label = "Peak following the release of the 'COVIDSafe\nApp' by Australian Government on 26 April",
           color = "black", 
           hjust = 0) +
  annotate("text",
           x = as.Date("2020-05-06"),
           y = 1200,
           label = "Peak following UK Government announcement \non 4 May that NHS Covid-19 'track and trace' \napp to be trialled on the Isle of Wight",
           color = "black",
           hjust =0)
  
  m2<-locations%>% mutate(country = ifelse(country != "United Kingdom" & country != "Australia", "Other", country)) %>% group_by(Date) %>%  count(country, sort = TRUE) %>% ungroup() %>%
  filter(Date != "2020-04-24")%>%
  filter(Date != "2020-05-12")%>%
  ggplot(aes(x= Date, y =n, fill = country)) + 
  geom_bar(stat = "identity", position = "fill") + 
  scale_fill_manual(values =c("#24A6F3", "#e8e6e6", "#F78357"))  +
  theme_minimal() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "grey", size =1),
        axis.text.y = element_text(size= 10),
        axis.text.x = element_text(size= 12),
        axis.title = element_text(size = 13),
        plot.title=element_blank(),
        axis.title.y = element_text(size =10),
        plot.subtitle=element_text(family="Times", size=14),
        plot.caption = element_text(family="Times",  size=14)) + 
  labs(
       caption = "",
       x= "\n Date",
       y= "Proportion of Tweets\n",
       fill = "Country")+
  scale_y_continuous(labels = scales::percent,expand = c(0, 0.05))+
  scale_x_date(expand = c(0.025,0))  
  

library(gridExtra)
grid.arrange(m1, m2, ncol = 1, heights = c(5, 2))

