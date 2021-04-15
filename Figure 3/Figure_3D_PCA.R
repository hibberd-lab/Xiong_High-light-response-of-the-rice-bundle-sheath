library(ggplot2)
library(ggalt)
library(ggfortify)
theme_set(theme_classic())
library(readr)


#making pca plot using DESeq2_rld transformed data
pcaData <- read_csv("pcaData_rld_0min.csv")
head(pcaData)
class(pcaData)

pcaData = as.data.frame(pcaData)
pcaData<-pcaData[c(1:6),]
head(pcaData)

str(pcaData)
summary(pcaData)

pcaData$Time_points<-factor(pcaData$Time_points, levels = c("0 min","5 min","10 min","30 min"))
pcaData$Cell_type<-factor(pcaData$Cell_type, levels = c("BSS","M"))

# select groups to draw circles
pca_0_BS <- pcaData[pcaData$group == "0_BSS", ]

pca_0_M <- pcaData[pcaData$group == "0_M", ]


#plot
ggplot(pcaData, aes(x = PC1, y = PC2,colour=group, shape=group)) +
  geom_point(size = 3) +
  ylim(-20,20)+
  xlab(paste0("PC1: ", "92", "% variance")) +
  ylab(paste0("PC2: ", "3", "% variance")) +
  coord_fixed()+
  theme_bw()+
  geom_encircle(data = pca_0_BS, aes(x=PC1, y=PC2)) + 
  geom_encircle(data = pca_0_M, aes(x=PC1, y=PC2))# draw circles
#Rpolt_PCA_0min