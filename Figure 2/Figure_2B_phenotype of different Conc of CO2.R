
combine <- read_csv("Different CO2_phenotype.csv")

combine_melt<-melt%>%
  separate(variable, c("conc","time","treat","rep"),sep="[_]")
head(combine_melt)

m_melt<- mutate(combine_melt,
                group = paste(conc,time, treat, sep = '_'))
m_melt1<- mutate(m_melt,
                 group1 = paste(conc,time,  sep = '_'))
m_melt2<- mutate(m_melt1,
                 group2 = paste(time, treat, sep = '_'))
head(m_melt2)

tgc <- summarySE(m_melt2, measurevar="value", groupvars=c("ID","conc", "time"," treat","group","group1","group2"))
head(tgc)

tgc$conc<-factor(tgc$conc,levels=c("C200","C400","D2000"))
tgc$treat<-factor(tgc$treat,levels=c("ck","treat"))
str(tgc)

tgc$time<-factor(tgc$time,levels=c("5min","10min","15min"))
tgc$group<-as.factor(tgc$group)
tgc$group1<-as.factor(tgc$group1)
tgc$group2<-as.factor(tgc$group2)
pd <- position_dodge(0.1)
m_melt2$time<-factor(m_melt2$time,levels=c("5min","10min","15min"))
m_melt2$group<-as.factor(m_melt2$group)
m_melt2$group1<-as.factor(m_melt2$group1)
m_melt2$group2<-as.factor(m_melt2$group2)
pd <- position_dodge(0.1)

head(m_melt2)
head(tgc)


ggplot(tgc, aes(x = treat, y = value,group=group,fill=conc))+
  geom_bar(stat="identity", color="black",position=position_dodge(.8),width = 0.7)+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.3, position=position_dodge(.8)) +
  geom_dotplot(data=m_melt2,binaxis='y', stackdir='center',stackratio=1, alpha=0.4,dotsize=1,position=position_dodge(0.8),fill="red")+
  facet_wrap(~time, ncol=1)+
 # scale_fill_brewer(palette="Blues")+
  xlab("Time points (minute)")+
  ylab("Gray value")+
  #ylim(0,130)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text=  element_text(colour="black", size=14),
        axis.title = element_text(colour = "black", size=14),
        aspect.ratio =1,
        legend.position="right", 
        legend.box = "vertical",
        strip.background = element_rect(colour = "black", fill = "lightcyan"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))





