library(reshape2)
library(tidyverse)
library(plyr)
library(limma)
library("dplyr")
library(readr)
library("dplyr")
library("ggplot2")
library(DESeq2)
library(tximport)
library(readr)
library("vsn")
library("pheatmap")
library("RColorBrewer")
library(ComplexHeatmap)
require(gdata)
library(circlize)
library(colorspace)
library(GetoptLong)
library(VennDiagram)
library(Rmisc)
library(devtools)
library(WGCNA)
library("dplyr")
library("ggplot2")
library(readr)

At_counts<- read_csv("Sylvains_Supplementary Table 1.csv")
head(At_counts)

At<-At_counts[,c(1,4:9)]
head(At)

colnames(At)<-c("GENEID","M_1","M_2","M_3","BS_1","BS_2","BS_3")
head(At)

At$species<-c(rep("At",length(At$GENEID)))

Os<- read_csv("normalized_counts.csv")
head(Os_counts)


colnames(Os)<-c("GENEID","M_1","M_2","M_3","BS_1","BS_2","BS_3")
head(Os)
Os$species<-c(rep("Os",length(Os$GENEID)))
head(Os)
combined_counts<-bind_rows(At,Os)
head(combined_counts)

RBOH<- read_csv("RBOH genes in Arabidopsis&rice.csv")

RBOH_counts<-dplyr::inner_join(RBOH,combined_counts,by="GENEID")
head(RBOH_counts)

melt_counts<-melt(RBOH_counts,id=c("Name","GENEID","species"))
head(melt_counts)

melt_counts1<-melt_counts%>%
  separate(variable, c("cell","rep"),sep="[_]")
head(melt_counts1)

tgc<- summarySE(melt_counts1, measurevar="value", groupvars=c("Name", "GENEID", "species","cell"))
head(tgc)

tgc$cell<-factor(tgc$cell,levels = c("BS","M"))
melt_counts1$cell<-factor(melt_counts1$cell,levels = c("BS","M"))

ggplot(tgc, aes(x = cell, y = value,fill=species))+
  geom_bar(stat="identity", color="black",position=position_dodge(),width = 0.5)+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), width=.3, position=position_dodge(.5)) +
  geom_dotplot(data=melt_counts1,binaxis='y', stackdir='center',stackratio=1, alpha=0.5,dotsize=2,position=position_dodge(0.5),fill="red")+
  facet_wrap(~Name, scale="free",ncol=10,strip.position="top")+
  scale_fill_brewer(palette = "Blues")+
  xlab("Cell Type")+
  ylab("Relative expression level")+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text=  element_text(colour="black", size=12),
        axis.title = element_text(colour = "black", size=12),
        aspect.ratio =1,
        legend.position="right", 
        legend.box = "vertical",
        strip.background = element_rect(colour = "black",fill="white")        )
#pdf("Rplot_RNA-seq of RBOH genes in arabidopsis & rice.pdf",width=8,height=6)
#Compute simple simple main effect
# Effect of time at each diet X exercises cells

head(melt_counts1)

pwc <-melt_counts1 %>%
  group_by(GENEID, species) %>%
  pairwise_t_test(value ~ cell, paired = FALSE, p.adjust.method = "bonferroni") # Remove details
# Show comparison results for "diet:no,exercises:yes" groups
pwc
write.csv(pwc,file="unpairwise t test of AtRBOH and OsRBOH genes.csv")



