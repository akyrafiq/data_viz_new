dis_all<-rbind(dis %>% mutate(League="Premier League"),
               dis_bund %>% mutate(League="Bundeliga"),
               dis_laliga %>% mutate(League="La Liga"),
               dis_ligue1 %>% mutate(League="Ligue 1"),
               dis_sa %>% mutate(League="Serie A"))

ggplot(dis_all %>% filter(x>25,variable %in% c("Points_4","Points_3")),aes(x=x,y=value))+
  geom_line(aes(color=League))+
  xlab("Points")+
  ylab("Probability")+
  labs(title="Distribution of Points", 
       subtitle="Estimated distribution of points amongst Champions League Teams in Europe's Top 5 leagues")+ 
  theme_tufte()+
  scale_color_manual(values = c("grey","red","blue","pink","lightsteelblue"),
                     guide = guide_legend())+
  theme(legend.position="bottom",
        legend.title = element_blank())
ggsave("dist/comp/points_cl.png")

ggplot(dis_all %>% filter(x<70,variable %in% c("Points_Rel")),aes(x=x,y=value))+
  geom_line(aes(color=League))+
  xlab("Points")+
  ylab("Probability")+
  labs(title="Distribution of Points", 
       subtitle="Estimated distribution of points amongst relegated teams in Europe's Top 5 leagues")+ 
  theme_tufte()+
  scale_color_manual(values = c("grey","red","blue","pink","lightsteelblue"),
                     guide = guide_legend())+
  theme(legend.position="bottom",
        legend.title = element_blank())
ggsave("dist/comp/points_rel.png")

ggplot(dis_all %>% filter(variable %in% c("GD")),aes(x=x,y=value))+
  geom_line(aes(color=League))+
  xlab("Points")+
  ylab("Probability")+
  labs(title="Distribution of Points", 
       subtitle="Estimated distribution of points in Europe's Top 5 leagues")+ 
  theme_tufte()+
  scale_color_manual(values = c("grey","red","blue","pink","lightsteelblue"),
                     guide = guide_legend())+
  theme(legend.position="bottom",
        legend.title = element_blank())
