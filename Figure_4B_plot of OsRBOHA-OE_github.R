data<-read.csv("OsRBOHA-OE phenotype.csv")

data$Line<-factor(data$Line,levels=c("NIP","OE"))
data$treat<-factor(data$treat,levels=c("HL","Dark"))
data$group<-factor(data$group,levels=c("NIP_Dark","NIP_HL","OE_Dark","OE_HL"))

data$time<- as.numeric(data$time)
str(data)


pd <- position_dodge(0.7)

head(data)
pdf("Rplot01_OsRBOHA_OE.pdf",width=5, height =  3.3)
ggplot(data=data, aes(x=time, y=value,group=group,shape=treat,color=Line)) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=4,
                size=0.3,
                position=pd) +
  geom_line(aes(color=Line,linetype=treat),alpha=1, size=0.7, stat = "identity")+
  geom_point(position=pd, size=1, shape=21, fill="white")+
  scale_color_manual(values = c("#00ba38", "blue"))+
  scale_fill_manual(values=c("#00ba38", "blue"))+
  scale_shape_manual(values = c(17,21)) +
  xlab("Time in light treatment (min)")+
  ylab("Gray value")+
  #ylim(0, 125)+
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

















