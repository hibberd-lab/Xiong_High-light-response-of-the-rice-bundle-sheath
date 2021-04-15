data<-read.csv("DPI_Green&Etiolated leaves phenotype.csv")

data$Type<-factor(data$Type,levels=c("Green","Etio"))
data$treat2<-factor(data$treat2,levels=c("treat","ck"))
data$group1<-factor(data$group1,levels=c("Green_DAB","Green_DPI","Etio_DAB","Etio_DPI"))

#data$time<-factor(data$time,levels=c("0","5","10","15","20","30","60"))
data$time <- as.numeric(data$time)
pd <- position_dodge(0.7)
head(data)

pdf("Rplot02_green&etiolated_DPI.pdf",width=10,height=3.5)
ggplot(data, aes(x = time, y = value,group=group,color=group1))+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=4,
                size=0.3,
                position=pd) +
  geom_line(aes(color=group1,linetype=treat2),alpha=1, size=0.7, stat = "identity")+
  geom_point(position=pd, size=1, shape=21, fill="white")+
  facet_wrap(~Type, ncol=1)+
  scale_color_manual(values = c("#00ba38", "blue","#00ba38", "blue"))+
  scale_fill_manual(values=c("#00ba38", "blue","#00ba38", "blue"))+
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









