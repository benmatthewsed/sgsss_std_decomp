#set.seed(9502559)
eseed=round(runif(1,1,1e6))
#set.seed(942583)
set.seed(483673)
df <- tibble(
  G = rep(0:1,e=8),
  C = rnorm(16,1,1),
  ptd = rnorm(16,1+1*(G==1),1), # individual D treatment effects
) |> group_by(G) |>
  mutate(ptd2 = cut(rev(ptd),3,labels=F)) |>
  ungroup() |>
  mutate(
    D = case_when(
      G==0 & ptd2 >2 ~ 1,
      G==0 & ptd2 <=2 ~ 0,
      G==1 & ptd2 >2 ~ 0,
      G==1 & ptd2 <=2 ~ 1
    ),
    Y = rnorm(16,2+(G==1)*2+ptd*D,1), # effect of D is only in group 1
    Yb = rbinom(16,1,plogis(scale(ptd*D)))
  ) 

df <- df |> mutate(
  Y0 = ifelse(D==0,Y,Y-2*ptd),
  Y1 = ifelse(D==1,Y,Y+2*ptd),
  DM = ifelse(D==1,0,1),
  YM = ifelse(D==0,Y1,Y0),
  Gs = G,
  xjit = rnorm(n(),0,.01),
  xjit = ifelse(G==1,xjit+.05,xjit-.05)
)



ggplot(df,aes(x=D+xjit,y=Y,col=factor(G,labels=c("Low","High")),
              shape=factor(G,labels=c("Low","High"))))+
  #geom_point(aes(size=4-ptd2),alpha=.3)+
  geom_point(size=3,alpha=.4)+
  # geom_point(aes(x=DM+xjit,y=YM,shape=factor(Gs)),size=3,alpha=.4)+
  # geom_segment(aes(x=0+xjit,xend=1+xjit, y=Y0,yend=Y1,lty=factor(D,levels=1:0)),
  #              alpha=.6)+
  stat_summary(aes(x=D),geom="point",fun=mean,
               position=position_nudge(x=0.2),size=4)+
  theme_minimal()+
  scale_x_continuous("Degree",breaks=c(0,1),limits=c(-.3,1.3))+
  scale_y_continuous("",limits=c(-1,13),labels=NULL)+
  scale_size_continuous(range = c(2, 6))+
  scale_shape_manual(values=c(16,17,1,2))+
  guides(col="none",shape="none",lty="none") -> p2a

ggplot(df,aes(x=D+xjit,y=Y,col=factor(G,labels=c("Low","High")),
              shape=factor(G,labels=c("Low","High"))))+
  #geom_point(aes(size=4-ptd2),alpha=.3)+
  geom_point(size=3,alpha=.8)+
  geom_point(aes(x=DM+xjit,y=YM,shape=factor(Gs)),size=3,alpha=.4)+
  geom_segment(aes(x=0+xjit,xend=1+xjit, y=Y0,yend=Y1,lty=factor(D,levels=1:0)),
               alpha=.6)+
  # stat_summary(aes(x=D),geom="point",fun=mean,
  #              position=position_nudge(x=0.2),size=4)+
  theme_minimal()+
  scale_x_continuous("Degree",breaks=c(0,1),limits=c(-.3,1.3))+
  scale_y_continuous("",limits=c(-1,13),labels=NULL)+
  scale_size_continuous(range = c(2, 6))+
  scale_shape_manual(values=c(16,17,1,2))+
  guides(col="none",shape="none",lty="none") -> p2

p1 <- ggplot(df,aes(x=1,y=Y,col=factor(G,labels=c("Low","High")),
              shape=factor(G,labels=c("Low","High"))))+
  geom_point(position = position_jitterdodge(jitter.width = 0.1, 
                                             jitter.height=0, dodge.width = 0.3),
             size=3,alpha=.3)+
  stat_summary(geom="point",position=position_nudge(x=0),
               size=4)+theme_minimal()+
  scale_x_continuous("",breaks=NULL)+
  scale_y_continuous("Offspring Income",limits=c(-1,13),labels=NULL)+
  labs(
    col="Parent Income",shape="Parent Income"
  ) +theme(legend.position="top")
library(patchwork)
p1 + p2a + plot_layout(widths=c(1,2))
#ggsave(file="images/bpes5.png",width=20,height=10,units="cm")
p1 + p2 + plot_layout(widths=c(1,2))
#ggsave(file="images/bpes6.png",width=20,height=10,units="cm")




head(df)
ggplot(df,aes(x=D+xjit,y=Y,col=factor(G,labels=c("Low","High")),
              shape=factor(G,labels=c("Low","High"))))+
  #geom_point(aes(size=4-ptd2),alpha=.3)+
  geom_point(size=3,alpha=.4)+
  geom_point(aes(x=DM+xjit,y=YM,shape=factor(Gs)),size=3,alpha=.4)+
  geom_segment(aes(x=0+xjit,xend=1+xjit, y=Y0,yend=Y1),lty="dotted",alpha=.6)+
  stat_summary(aes(x=D),geom="point",fun=mean,
               position=position_nudge(x=0.2),size=4)+
  theme_minimal()+
  scale_x_continuous("Degree",breaks=c(0,1),limits=c(-.3,1.3))+
  scale_y_continuous("",limits=c(-1,13),labels=NULL)+
  scale_size_continuous(range = c(2, 6))+
  scale_shape_manual(values=c(16,17,1,2))+
  guides(col="none",shape="none",lty="none") -> p3a

ggplot(df,aes(x=D+xjit,y=Y,col=factor(G,labels=c("Low","High")),
              shape=factor(G,labels=c("Low","High"))))+
  #geom_point(aes(size=4-ptd2),alpha=.3)+
  geom_point(size=3,alpha=.4)+
  geom_point(aes(x=DM+xjit,y=YM,shape=factor(Gs)),size=3,alpha=.4)+
  geom_segment(aes(x=0+xjit,xend=1+xjit, y=Y0,yend=Y1),lty="dotted",alpha=.6)+
  stat_summary(aes(x=0,y=Y0),geom="point",fun=mean,
               position=position_nudge(x=0.2),size=4)+
  stat_summary(aes(x=1,y=Y1),geom="point",fun=mean,
               position=position_nudge(x=0.2),size=4)+
  theme_minimal()+
  scale_x_continuous("Degree",breaks=c(0,1),limits=c(-.3,1.3))+
  scale_y_continuous("",limits=c(-1,13),labels=NULL)+
  scale_size_continuous(range = c(2, 6))+
  scale_shape_manual(values=c(16,17,16,17))+
  guides(col="none",shape="none",lty="none") -> p3

p1 + p3a + plot_layout(widths=c(1,2))
#ggsave(file="images/bpes7.png",width=20,height=10,units="cm")
p1 + p3 + plot_layout(widths=c(1,2))
#ggsave(file="images/bpes8.png",width=20,height=10,units="cm")