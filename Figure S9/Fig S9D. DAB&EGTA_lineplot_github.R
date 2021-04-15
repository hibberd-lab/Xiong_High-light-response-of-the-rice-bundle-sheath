data<-read.csv("DAB&EGTA_phenotype.csv")

data$treat2<-factor(data$treat2,levels=c("HL","CK"))
data$treat1<-factor(data$treat1,levels=c("DAB","EGTA"))

data$time <- as.numeric(data$time)
pd <- position_dodge(0.7)

pdf("Rplot02_Green leaves_DAB&EGTA.pdf",width=10,height=3.5)
ggplot(data, aes(x = time, y = value,group=group,color=treat1))+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=4,
                size=0.3,
                position=pd) +
  geom_line(aes(color=treat1,linetype=treat2),alpha=1, size=0.7, stat = "identity")+
  geom_point(position=pd, size=1, shape=21, fill="white")+
  #facet_wrap(~Type, ncol=2)+
  scale_color_manual(values = c("#00ba38","blue","#00ba38","blue"))+
  scale_fill_manual(values=c("#00ba38","blue","#00ba38","blue"))+
  scale_shape_manual(values = c(17,21)) +
  xlab("Time in light treatment (min)")+
  ylab("Gray value")+
  # ylim(20, 100)+
  scale_x_continuous(breaks=seq(0, 60, 10))+
  theme_bw() +
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text=  element_text(colour="black", size=11),
        axis.title = element_text(colour = "black", size=12),
        aspect.ratio = 0.8,
        legend.position="bottom", 
        legend.box = "vertical",
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))
dev.off()

