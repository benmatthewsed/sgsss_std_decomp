set.seed(974209)
df <- tibble(
  G = rep(0:1,e=50),
  D = rep(c(0,1,0,1),times=c(40,10,10,40)), # more D in G1
  ptd = rnorm(100,2,1), # individual D treatment effects
  Y = rnorm(100,2+(G==1)*2+ptd*D*.5+ptd*D*(G==1),1), # effect of D is only in group 1
  Yb = rbinom(100,1,plogis(scale(ptd*D)))
) |> group_by(G,D) |>
  mutate(group_n=n()) |> ungroup()




ggplot(df,aes(x=D,y=Y,col=factor(G,labels=c("Low","High")),
              shape=factor(G,labels=c("Low","High"))))+
  geom_point(position = position_jitterdodge(jitter.width = 0.1, 
                                             jitter.height=0,
                                             dodge.width = 0.3),
             size=3,alpha=.3)+
  stat_summary(geom="point",position=position_nudge(x=.2),
               aes(size=group_n))+theme_minimal()+
  scale_x_continuous("Degree",breaks=c(0,1))+
  scale_y_continuous("",limits=c(0,11),labels=NULL)+
  scale_size_continuous(range=c(4,7))+
  guides(col="none",shape="none",size="none")-> p2

p1 <- ggplot(df,aes(x=1,y=Y,col=factor(G,labels=c("Low","High")),
              shape=factor(G,labels=c("Low","High"))))+
  geom_point(position = position_jitterdodge(jitter.width = 0.1, 
                                             jitter.height = 0,
                                             dodge.width = 0.3),
             size=3,alpha=.3)+
  stat_summary(geom="point",position=position_nudge(x=0),
               size=4)+theme_minimal()+
  scale_x_continuous("",breaks=NULL)+
  scale_y_continuous("Offspring Income",limits=c(0,11),labels=NULL)+
  labs(
    col="Parent Income",shape="Parent Income"
  ) +theme(legend.position="top")
library(patchwork)
p1 + p2 + plot_layout(widths=c(1,2))

# ggsave(file="images/bpes.png",width=20,height=10,units="cm")
