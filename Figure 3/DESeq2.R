library("dplyr")
library("ggplot2")
library(DESeq2)
library(tximport)
library(readr)
library("vsn")
library("pheatmap")
library("RColorBrewer")
library(ComplexHeatmap)
#
# make rice transcript database TxDb file
Os_tx2gene <- read.csv ("Os_tx2gene.csv", header = TRUE)
Os_tx2gene <- Os_tx2gene[,-1]

head (Os_tx2gene)

library(tximport)
library(readr)
files = list.files(pattern="*quant.sf")
files
txi <- tximport(files, type = "salmon", tx2gene = Os_tx2gene)


#make sample table

sampleTable <- data.frame(condition = c(rep("0_BSS",3), rep("0_M", 3)))


rownames(sampleTable) <- c("BSS_0_1","BSS_0_2","BSS_0_3","M_0_1","M_0_2","M_0_3")
sampleTable



# extract raw counts from txi object
rawcounts <- txi$counts

head(rawcounts)
class(rawcounts)
colnames(rawcounts)<- c("BSS_0_1","BSS_0_2","BSS_0_3","M_0_1","M_0_2","M_0_3")


head(rawcounts)

write.csv(rawcounts, file= "rawcounts.csv")

library(DESeq2)


dds0 <- DESeqDataSetFromTximport(txi, sampleTable, ~condition)
head(dds0)
colData(dds0)

head(dds0)

colnames(dds0)<- c("BSS_0_1","BSS_0_2","BSS_0_3","M_0_1","M_0_2","M_0_3")


# prefilter the data
nrow(dds0)
dds0 <- estimateSizeFactors(dds0)
idx <- rowMeans(counts(dds0, normalized=TRUE)) >= 10
dds <- dds0[idx,]

class(dds)
head(dds)
nrow(dds)
dim(dds)

save(dds,file="dds_basemean_10_0MIN.RData")

# get and save normalized counts
norm <- as.data.frame(counts(dds, normalized=TRUE ))
head (norm)


write.csv(norm, "hnormalized_counts.csv")
norm$GENEID<-rownames(norm)

# We plot the standard deviation of each row (genes) against the mean

lambda <- 10^seq(from = -1, to = 2, length = 1000)
cts <- matrix(rpois(1000*100, lambda), ncol = 100)
library("vsn")
meanSdPlot(cts, ranks = FALSE)


# for logarithm-transformed counts:
log.cts.one <- log2(cts + 1)
meanSdPlot(log.cts.one, ranks = FALSE)

rld <- rlog(dds, blind = FALSE)
head(assay(rld), 3)

vsd <- vst(dds, blind = FALSE)

head(assay(vsd), 3)

library("dplyr")
library("ggplot2")



df <- bind_rows(
  as_data_frame(log2(counts(dds, normalized=TRUE)[, 1:2]+1)) %>%
    mutate(transformation = "log2(x + 1)"),
  as_data_frame(assay(rld)[, 1:2]) %>% mutate(transformation = "rlog"),
  as_data_frame(assay(vsd)[, 1:2]) %>% mutate(transformation = "vst"))

colnames(df)[1:2] <- c("x", "y")  

ggplot(df, aes(x = x, y = y)) + geom_hex(bins = 80) +
  coord_fixed() + facet_grid( . ~ transformation)  

#
df <- bind_rows(
  as_data_frame(log2(counts(dds, normalized=TRUE)[, 1:2]+1)) %>%
    mutate(transformation = "log2(x + 1)"))

colnames(df)[1:2] <- c("x", "y")  

ggplot(df, aes(x = x, y = y)) + geom_hex(bins = 80) +
  coord_fixed() + facet_grid( . ~ transformation)  



sampleDists <- dist(t(assay(rld)))


sampleDists<-as.data.frame(sampleDists)

str(sampleDists)

dim(sampleDists)

write.csv(sampleDists,"sampleDists_0min.csv")
library("pheatmap")
library("RColorBrewer")



sampleDistMatrix <- as.matrix( sampleDists )

write.csv(sampleDistMatrix,file="heatmap_sampleDistance_0min.csv")


colors <- colorRampPalette( rev(brewer.pal(9, "Blues")) )(255)
pheatmap(sampleDistMatrix,
         #clustering_distance_rows = sampleDists,
         #clustering_distance_cols = sampleDists,
         col = colors)

#Rplot_hierachical clustering


# sample distance
library("PoiClaClu")
poisd <- PoissonDistance(t(counts(dds)))

samplePoisDistMatrix <- as.matrix( poisd$dd )

pheatmap(samplePoisDistMatrix,
         clustering_distance_rows = poisd$dd,
         clustering_distance_cols = poisd$dd,
         col = colors)
plotPCA(rld)

pcaData <- plotPCA(rld, intgroup = c("condition"), returnData = TRUE)

pcaData

write.csv(pcaData, "pcaData_rld_0min.csv")

percentVar <- round(100 * attr(pcaData, "percentVar"))

ggplot(pcaData, aes(x = PC1, y = PC2, shape = group, color = group)) +
  geom_point(size = 3) +
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  coord_fixed()


mds <- as.data.frame(colData(rld)) %>% cbind(cmdscale(sampleDistMatrix))

ggplot(mds, aes(x = `1`, y = `2`, color = condition, shape = condition  )) + 
  geom_point(size = 3) + 
  xlab(paste0("PC1: ", percentVar[1], "% variance")) +
  ylab(paste0("PC2: ", percentVar[2], "% variance")) +
  coord_fixed()

?coord_fixed()


#DE gene analysis

dds <- DESeq(dds, betaPrior=FALSE)
head(dds)


# pairwise comparision 
##no big change if use lfcShink function, but p-value is not consistent with pairwise comparasion##
res<-results(dds)

BSS_0_vs_M_0<-lfcShrink(dds, contrast=c("condition", "0_BSS", "0_M"))
#summary(BSS_0_vs_M_0)
#head(BSS_0_vs_M_0)

write.csv(BSS_0_vs_M_0,"BSS_0_vs_M_0_logFC.csv")

