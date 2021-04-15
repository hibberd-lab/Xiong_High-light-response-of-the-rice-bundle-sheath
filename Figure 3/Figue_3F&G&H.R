counts0 <-read_csv("normalized_counts.csv")

head(counts0)

colnames(counts0)<-c("GENEID","BSS_0_1_0counts_0","BSS_0_2_0counts_0","BSS_0_3_0counts_0","M_0_1_0counts_0","M_0_2_0counts_0","M_0_3_0counts_0")
head(counts0) 
PSI_mean <- read_csv("PSI_mean.csv")
PSII_mean <- read_csv("PSII_mean.csv")


PSI_mean_1<-as.data.frame(lapply(PSI_mean, rep, length(counts0$GENEID)))
PSII_mean_1<-as.data.frame(lapply(PSII_mean, rep, length(counts0$GENEID)))

PSI_mean_1$GENEID<-counts$GENEID
PSII_mean_1$GENEID<-counts$GENEID

PSI_mean_2<-PSI_mean_1[,c(8,2:7)]
PSII_mean_2<-PSII_mean_1[,c(8,2:7)]

colnames(PSI_mean_2)<-c("GENEID","BSS_0_1_1PSI_1","BSS_0_2_1PSI_1","BSS_0_3_1PSI_1","M_0_1_1PSI_1","M_0_2_1PSI_1","M_0_3_1PSI_1")
colnames(PSII_mean_2)<-c("GENEID","BSS_0_1_2PSII_1","BSS_0_2_2PSII_1","BSS_0_3_2PSII_1","M_0_1_2PSII_1","M_0_2_2PSII_1","M_0_3_2PSII_1")



normbyPSI <- read_csv("counts_normalised by PSI.CSV")
normbyPSII <- read_csv("counts_normalised by PSII.CSV")


colnames(normbyPSI)<-c("GENEID","BSS_0_1_3normbyPSI_2","BSS_0_2_3normbyPSI_2","BSS_0_3_3normbyPSI_2","M_0_1_3normbyPSI_2","M_0_2_3normbyPSI_2","M_0_3_3normbyPSI_2")
colnames(normbyPSII)<-c("GENEID","BSS_0_1_4normbyPSII_2","BSS_0_2_4normbyPSII_2","BSS_0_3_4normbyPSII_2","M_0_1_4normbyPSII_2","M_0_2_4normbyPSII_2","M_0_3_4normbyPSII_2")


combine_all<-counts0 %>%
  #left_join(PSII_mean_2, by="GENEID")%>%
  #left_join(PSI_mean_2, by="GENEID")%>%
  left_join(normbyPSII, by="GENEID")%>%
  left_join(normbyPSI, by="GENEID")
 
head(combine_all)

write.csv(combine_all,file="combine_all.csv")

combine_all<- read_csv("combine_all_basemean over 10.csv")

PS<-read_csv("S_list.csv")
head(PS)

PS<-PS[,c(1:3)]

head(PS)

PS_combine_all<-inner_join(PS,combine_all,by="GENEID")


head(PS_combine_all)



PS_counts<-PS_combine_all[,c(1,2,5:22)]

head(PS_counts)

counts.melt<- melt(PS_counts, id=c("Family","GENEID"))
head(counts.melt)
summary(counts.melt)
counts.melt<-counts.melt%>%
  separate(variable, c("cell","time","rep","type1","type2"),sep="[_]")
counts.melt$Family<-factor(counts.melt$Family,levels = c("PSII",
                                                         "PSI_0",
                                                         "calvin_cycle",
                                                         "cytochrome_b6f",
                                                         "cyclic_electron_transport",
                                                         "ATP_synthase",
                                                         "cytochrome_c6",
                                                         "Rubisco",
                                                         "photorespiration",
                                                         "NPQ"
))
counts.melt$type1<-factor(counts.melt$type1,levels = c("0counts",
                                                       "2PSII",
                                                       "1PSI",
                                                       "4normbyPSII",
                                                       "3normbyPSI"))

head(counts.melt)
str(counts.melt)

#sum all family members
sum_all<-aggregate(x=counts.melt$value,
                      by=list(counts.melt$Family, counts.melt$cell,counts.melt$type1,counts.melt$rep),
                      FUN=sum)
colnames(sum_all)<-c("Family","cell","type1","rep","value")
str(sum_all)

##Averadge replicates of each gene
tgc_rep_ave<-summarySE(counts.melt, measurevar="value", groupvars=c("Family","GENEID","cell", "type1"))
write.csv(tgc_rep_ave,file="tgc_rep_ave.csv")
##Averadge replicates first,then gene family
tgc_ave_rep<-summarySE(tgc_rep_ave, measurevar="value", groupvars=c("Family","cell", "type1"))

#Averadge gene replicates of sum in each family
tgc_sum <- summarySE(sum_all, measurevar="value", groupvars=c("Family","cell", "type1"))


#Averadge gene family

tgc_ave_fam<- summarySE(counts.melt, measurevar="value", groupvars=c("Family", "cell", "rep","type1"))
head(tgc0)

tgc00<-tgc0[,c(1,2,3,4,6)]
head(tgc00)

str(tgc_sum)
tgc_ave_fam_rep <- summarySE(tgc_ave_fam, measurevar="value", groupvars=c("Family","cell", "type1"))

#Rplot05_Rplots_mean value.pdf wi8,h8
#pdf("Rplot03_Rplots_PS_with family sum first and replicates averadged later_dotplot.pdf",width=6.5,height=21)
p1<-ggplot(tgc_sum, aes(x=cell, y=value, label=value,group=cell,fill=type1)) + 
  geom_bar(stat='identity', aes(fill=type1),  position=position_dodge(width=0.5),width=.5)  +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),width=.3,position=position_dodge(width=0.5)) +
  #geom_dotplot(data=sum_all,aes(x=cell, y=value),binaxis='y', stackdir='center',stackratio=1, alpha=1,
  #dotsize=2.5,position=position_dodge(0.6),color="black",fill="grey")+
  scale_fill_manual(values=c("deeppink","blue","turquoise","grey","lightgreen"))+
  facet_wrap(Family~type1, scale="free",ncol=3)+
  labs(title="PS-mean value") +
  theme_bw()+
  theme(plot.title = element_text(size=22),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x =element_text(size = 14,color = "black"),
        axis.text.y =element_text(size = 14,color = "black"),
        aspect.ratio = 1.2,
        strip.background = element_blank(),
        #strip.text = element_blank(),
        legend.position='none',
        panel.background = element_rect(fill = "white", colour = "black"))
p1+ geom_point(data=sum_all,colour = "black",  size =1.5,alpha=0.6, position=position_dodge(0.5))+
  geom_point(data=sum_all, colour= "red", size =1,alpha=0.5, position=position_dodge(0.5))

#dev.off()

# Pairwise comparisons
pwc <-sum_all %>%
  group_by(Family, type1) %>%
  pairwise_t_test(value ~ cell, paired = TRUE, p.adjust.method = "bonferroni") # Remove details
# Show comparison results for "diet:no,exercises:yes" groups
pwc
write.csv(pwc,file="unpairwise t test of PS related genes_compare sum value.csv")

# Visualization: box plots with p-values


