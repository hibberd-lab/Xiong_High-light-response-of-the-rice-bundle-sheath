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
require(xlsx)
require(gdata)
library(circlize)

#read the phenotype files
Fq_Fm <- read_csv("Fq'_Fm'.csv")
Fv_Fm <- read_csv("Fv_Fm or Fv'_Fm'.csv")
PQ_or_NPQ <- read_csv("PQ or NPQ.csv")

head(Fv_Fm)
head(Fq_Fm)
head(PQ_or_NPQ)

Fv_Fm$parameter<-as.factor(Fv_Fm$parameter)
Fq_Fm$parameter<-as.factor(Fq_Fm$parameter)
PQ_or_NPQ$parameter<-factor(PQ_or_NPQ$parameter,levels = c("Fq'/Fv' (PQ)","Fm/Fm'-1 (NPQ)"))


# read the position files
position_Fv_Fm <- read_csv("position_Fv_Fm.csv")
position_Fq_Fm <- read_csv("position_Fq_Fm .csv")
position_PQ_NPQ <- read_csv("position_PQ_NPQ.csv")

position_Fv_Fm$Light<-factor(position_Fv_Fm$Light,levels = c("Dark","LL","HL"))
position_Fq_Fm$Light<-factor(position_Fq_Fm$Light,levels = c("Dark","LL","HL"))
position_PQ_NPQ$Light<-factor(position_PQ_NPQ$Light,levels = c("Dark","LL","HL"))
position_PQ_NPQ$parameter<-factor(position_PQ_NPQ$parameter,levels = c("Fq'/Fv' (PQ)","Fm/Fm'-1 (NPQ)"))


#plot Fv/Fm or Fv'/Fm' (XE)
ggplot(data=Fv_Fm, aes(x=time, y=value,shape=parameter)) +
  geom_line(aes(colour=parameter),alpha=1, size=0.7)+
  geom_point(colour = "black", size = 2)+
  geom_point(aes(colour = parameter), size = 1.5) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                position=position_dodge(.5),size=0.3,color="black")+
  #scale_shape_manual(values = c(21, 17)) +
  scale_color_manual(values = c("#00ba38", "brown3", "#FC4E07"))+
  geom_rect(data=position_Fv_Fm, mapping=aes(NULL,NULL,xmin=start, xmax=end, ymin=-Inf, ymax=0.65, fill=Light),colour="black",size=0.2)+
  scale_fill_manual(values=alpha(c("black","gray","yellow"),1))+
  xlab("Time in light treatment (min)")+
  ylab("Fv/Fm or Fv'/Fm' (XE)")+
  scale_x_continuous(breaks=seq(-40,90,20))+
  ylim(0.65,0.85)+
  theme_bw() +
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text=  element_text(colour="black", size=10),
        axis.title = element_text(colour = "black", size=11),
        aspect.ratio = 0.8,
        legend.position="bottom", 
        legend.box = "vertical",
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))


#plot Fq¡¯/Fm¡¯(¦µPSII)
ggplot(data=Fq_Fm, aes(x=time, y=value,shape=parameter)) +
  geom_line(aes(colour=parameter),alpha=1, size=0.7)+
  geom_point(colour = "black", size = 2)+
  geom_point(aes(colour = parameter), size = 1.5) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                position=position_dodge(.5),size=0.3,color="black")+
  #scale_shape_manual(values = c(21, 17)) +
  scale_color_manual(values = c("#00ba38", "brown3", "#FC4E07"))+
  geom_rect(data=position_Fq_Fm , mapping=aes(NULL,NULL,xmin=start, xmax=end, ymin=-Inf, ymax=0.3, fill=Light),colour="black",size=0.2)+
  scale_fill_manual(values=alpha(c("black","gray","yellow"),1))+
  xlab("Time in light treatment (min)")+
  ylab("Fq'/Fm¡¯(¦µPSII)")+
  scale_x_continuous(breaks=seq(-40,90,20))+
  ylim(0.3,0.8)+
  theme_bw() +
  theme(strip.background = element_rect(colour = "black", fill = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text=  element_text(colour="black", size=10),
        axis.title = element_text(colour = "black", size=11),
        aspect.ratio = 0.8,
        legend.position="bottom", 
        legend.box = "vertical",
        panel.border = element_rect(colour = "black", fill=NA, size=0.5))

#plot PQ_or_NPQ
ggplot(data=PQ_or_NPQ, aes(x=time, y=value,shape=parameter)) +
  geom_line(aes(colour=parameter),alpha=1, size=0.7)+
  geom_point(colour = "black", size = 2)+
  geom_point(aes(colour = parameter), size = 1.5) +
  geom_errorbar(aes(ymin=value-se, ymax=value+se),
                position=position_dodge(.5),size=0.3,color="black")+
  #scale_shape_manual(values = c(21, 17)) +
  scale_color_manual(values = c("#00ba38", "brown3", "#FC4E07"))+
  geom_rect(data=position_PQ_NPQ , mapping=aes(NULL,NULL,xmin=start, xmax=end, ymin=-Inf, ymax=0, fill=Light),colour="black",size=0.2)+
  scale_fill_manual(values=alpha(c("black","gray","yellow"),1))+
  xlab("Time in light treatment (min)")+
  ylab("Fq¡¯/Fv¡¯(PQ) 
or Fm/Fm¡¯-1 (NPQ)")+
  scale_x_continuous(breaks=seq(-40,90,20))+
  ylim(0,1.8)+
  theme_bw() +
  theme(strip.background = element_rect(colour = "black", fill = "white"),
       panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        axis.text=  element_text(colour="black", size=10),
        axis.title = element_text(colour = "black", size=11),
        aspect.ratio = 0.8,
        legend.position="bottom", 
        legend.box = "vertical",
       panel.border = element_rect(colour = "black", fill=NA, size=0.5))



