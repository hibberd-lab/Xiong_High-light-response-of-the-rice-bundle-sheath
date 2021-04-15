
all1 <- read_csv("Grave value_0-5-30.csv")
head(all1)

all1$Distance<-as.numeric(all1$Distance)
all1$time<-factor(all1$time,levels = c("0 min","5 min","30 min"))
all1$Treatment<-factor(all1$Treatment,levels = c("high light","0 min"))
str(all1)

celltype_position <- read_csv("celltype_position_0-5-30.csv")
head(celltype_position)


celltype_position$time <- factor(celltype_position$time,levels = c("0 min","5 min","30 min"))
celltype_position$Treatment<-factor(celltype_position$Treatment,levels = c("high light","0 min"))
celltype_position$'Cell type' <- factor(celltype_position$'Cell type',levels=c("M","BSS"))

str(celltype_position)
head(celltype_position)


pd <- position_dodge(0.05) # move them .05 to the left and right

ggplot(all1, aes(x=Distance, y=mean)) +
  geom_rect(aes(x=NULL, y=NULL, xmin = start, xmax = end,
                fill = celltype_position$'Cell type'), ymin = -Inf, ymax =Inf,
            data = celltype_position) +
  scale_fill_manual(values=alpha(c("green","brown"),0.1))+  
  geom_errorbar(aes(ymin=mean-se, ymax=mean + se),colour="black", width=.1, position=pd) +
  geom_line(aes(colour=Treatment),position=pd,alpha=1, size=0.5) +
  facet_wrap(~time, ncol=3)+
  xlab("Distance (¦Ìm)")+
  ylab("Gray value")+
  xlim(0,140)+
  ylim(0,160)+
  theme_bw() +
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x =element_text(size = 9,colour = "black"),
        axis.text.y =element_text(size = 9,colour = "black"),
        aspect.ratio = 1,
        strip.text.x = element_text(size = 9,colour = "black")
  )











