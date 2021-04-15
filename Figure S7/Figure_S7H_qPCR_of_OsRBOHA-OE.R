#qPCR

qPCR<- read_csv("Cq Results.csv")
head(qPCR)
qPCR$Sample<-factor(qPCR$Sample,levels=c("WT","OE"))
head(qPCR)

#Assumption of sphericity
aov <- anova_test(data = qPCR, dv = RQ, wid = rep, within = Sample)
get_anova_table(aov)

anova<-get_anova_table(aov )
anova

pwc <-qPCR%>%
  pairwise_t_test(RQ~ Sample, paired = TRUE, p.adjust.method = "bonferroni") # Remove details
# Show comparison results for "diet:no,exercises:yes" groups
pwc
write.csv(pwc,file="qPCR_OsRBOHB_pairwise t test of PS.csv")


tgc_qPCR<- summarySE(qPCR, measurevar="RQ", groupvars=c("Sample"))
head(tgc_qPCR)

tgc_qPCR_1<-tgc_qPCR[,c(1,3,5)]
head(tgc_qPCR_1)

ggplot(tgc_qPCR_1, aes(x = Sample, y = RQ,fill=Sample))+
  geom_bar(stat="identity", color="black",position=position_dodge(),width = 0.5)+
  geom_errorbar(aes(ymin=RQ-se, ymax=RQ+se), width=.3, position=position_dodge(.5)) +
  geom_dotplot(data=qPCR,binaxis='y', stackdir='center',stackratio=1, alpha=0.6,dotsize=1,position=position_dodge(0.5),fill="red")+
  #facet_wrap(Color~Name, scale="free",ncol=6,strip.position="top")+
  scale_fill_brewer(palette = "Blues")+
  xlab("Cell Type")+
  ylab("Relative expression level")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text=  element_text(colour="black", size=9),
        axis.title = element_text(colour = "black", size=9),
        aspect.ratio =1,
        legend.position="right", 
        legend.box = "vertical",
        strip.background = element_rect(colour = "black",fill="white")        )
#pdf("20200217_Rplot_qPCR of RBOHB-OE.pdf",width=6,height=6)




