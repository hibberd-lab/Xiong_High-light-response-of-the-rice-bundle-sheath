data<-read.csv("OsRBOHB_OE&mutant phenotype.csv")

data$Line<-factor(data$Line,levels=c("mutant","OE","NIP"))
data$treat1<-factor(data$treat1,levels=c("Dark","HL"))
data$group1<-factor(data$group1,levels=c("mutant_Dark","mutant_HL","OE_Dark","OE_HL","NIP_Dark","NIP_HL"))

pd <- position_dodge(0.1)
ggplot(data, aes(x = treat1, y = value,group=group1,fill=Line))+
  geom_bar(stat="identity", color="black",position=position_dodge(),width = 0.8)+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.3, position=position_dodge(.8)) +
  geom_dotplot(data=m_melt,binaxis='y', stackdir='center',stackratio=1, dotsize=0.8,
               position=position_dodge(0.8),fill="red",alpha=0.5)+
  #facet_wrap(~time, ncol=1)+
  scale_fill_brewer(palette="Blues",-1)+
  ylim(0,120)+
  xlab("Time points (minute)")+
  ylab("Gray value")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text=  element_text(colour="black", size=10),
        axis.title = element_text(colour = "black", size=11),
        aspect.ratio = 0.8,
        legend.position="right", 
        legend.box = "vertical",
        strip.background = element_rect(colour = "black", fill = "lightcyan"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


