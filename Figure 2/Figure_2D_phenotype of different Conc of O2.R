combine<-read.csv("Different O2 concentration_phenotype.csv")

combine_melt<-combine%>%
  separate(variable, c("CO2","O2","time","treat","rep"),sep="[_]")
head(combine_melt)

m_melt<- mutate(combine_melt,
                group = paste(O2,time, treat, sep = '_'))
m_melt1<- mutate(m_melt,
                 group1 = paste(O2,time,  sep = '_'))
m_melt2<- mutate(m_melt1,
                 group2 = paste(time, treat, sep = '_'))
head(m_melt2)

m_melt2$time<-factor(m_melt2$time,levels=c("5MIN","10MIN","15MIN"))
m_melt2$O2<-factor(m_melt2$O2,levels=c("O21","O2"))
m_melt2$group<-factor(m_melt2$group,levels = c("O21_5MIN_ck",
                                               "O21_5MIN_treat",
                                               "O21_10MIN_ck",
                                               "O21_10MIN_treat",
                                               "O21_15MIN_ck",
                                               "O21_15MIN_treat",
                                               "O2_5MIN_ck",
                                               "O2_5MIN_treat",
                                               "O2_10MIN_ck",
                                               "O2_10MIN_treat",
                                               "O2_15MIN_ck",
                                               "O2_15MIN_treat"))

tgc <- summarySE(m_melt2, measurevar="value", groupvars=c("ID","CO2","O2", "time"," treat","group","group1","group2"))
head(tgc)

tgc$treat<-factor(tgc$treat,levels=c("ck","treat"))
str(tgc)

tgc$time<-factor(tgc$time,levels=c("5MIN","10MIN","15MIN"))
tgc$O2<-factor(tgc$O2,levels=c("O21","O2"))
tgc$group<-factor(tgc$group,levels = c("O21_5MIN_ck",
                                       "O21_5MIN_treat",
                                       "O21_10MIN_ck",
                                       "O21_10MIN_treat",
                                       "O21_15MIN_ck",
                                       "O21_15MIN_treat",
                                       "O2_5MIN_ck",
                                       "O2_5MIN_treat",
                                       "O2_10MIN_ck",
                                       "O2_10MIN_treat",
                                       "O2_15MIN_ck",
                                       "O2_15MIN_treat"))
tgc$group1<-as.factor(tgc$group1)
tgc$group2<-as.factor(tgc$group2)
pd <- position_dodge(0.1)

ggplot(tgc, aes(x = treat, y = value,group=group,fill=O2))+
  geom_bar(stat="identity", color="black",position=position_dodge(.8),width = 0.7)+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.3, position=position_dodge(.8)) +
  #geom_dotplot(data=m_melt2,binaxis='y', stackdir='center',stackratio=1, alpha=0.5,dotsize=1,position=position_dodge(0.8),fill="red")+
  facet_wrap(~time, ncol=1)+
  #scale_fill_brewer(palette="Blues")+
  xlab("Time points (minute)")+
  ylab("Gray value")+
  ylim(0,110)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text=  element_text(colour="black", size=12),
        axis.title = element_text(colour = "black", size=12),
        aspect.ratio =1,
        legend.position="right", 
        legend.box = "vertical",
        strip.background = element_rect(colour = "black", fill = "lightcyan"),
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


