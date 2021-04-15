qPCR<- read_csv("Cq Results.csv")
head(qPCR)

combine<-qPCR%>%
  separate(Sample, c("color","cell","bio_rep","tech_rep"),sep="[_]")
head(combine)

colnames(combine)<-c("Name", "Color", "cell","bio_rep", "tech_rep","RQ")
head(combine)



combine$cell<-factor(combine$cell,levels = c("BSS","M"))
combine$Color<-factor(combine$Color,levels = c("E","G"))
combine$Name<-factor(combine$Name,levels = c("OsRBOHA",
                                               "OsRBOHC",
                                               "OsRBOHI",
                                               "SodA",
                                               "Sodcc1",
                                               "Sodcc2"
))
head(combine)


tgc_ave_tech_rep<- summarySE(combine, measurevar="RQ", groupvars=c("Name","Color","cell","bio_rep"))
head(tgc_ave_tech_rep)

tgc_ave_tech_rep_1<-tgc_ave_tech_rep[,c(1,2,3,4,6)]
head(tgc_ave_tech_rep_1)

tgc_ave_bio_rep<- summarySE(tgc_ave_tech_rep_1, measurevar="RQ", groupvars=c("Name", "cell", "Color"))
head(tgc_ave_bio_rep)

str(tgc_ave_bio_rep)


#rRplot_01_qpcr of rboh and SOD genes with biological replicates_compare all genes.pdf
ggplot(tgc_ave_bio_rep, aes(x = cell, y = RQ,fill=Color))+
  geom_bar(stat="identity", color="black",position=position_dodge(),width = 0.5)+
  geom_errorbar(aes(ymin=RQ-se, ymax=RQ+se), width=.3, position=position_dodge(.5)) +
  geom_dotplot(data=tgc_ave_tech_rep_1,binaxis='y', stackdir='center',stackratio=1, alpha=0.6,dotsize=2,position=position_dodge(0.5),fill="red")+
  facet_wrap(Color~Name, scale="free",ncol=6,strip.position="top")+
  scale_fill_brewer(palette = "Blues")+
  xlab("Cell Type")+
  ylab("Relative expression level")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text=  element_text(colour="black", size=10),
        axis.title = element_text(colour = "black", size=10),
        aspect.ratio =1,
        legend.position="right", 
        legend.box = "vertical",
        strip.background = element_rect(colour = "black",fill="white") )


head(tgc_ave_tech_rep_1)
# Pairwise comparisons
pwc <- tgc_ave_tech_rep_1 %>%
  group_by(Name,Color) %>%
  pairwise_t_test(RQ~ cell, paired = FALSE, p.adjust.method = "bonferroni") # Remove details
# Show comparison results for "diet:no,exercises:yes" groups
pwc     # remove p columns







