
#BSS vs M at 0min with counts basemean >10
counts_0min0 <-read_csv("highlight_normalized_counts_basemean_10_0min.csv")

#with at least 1 replicates over 0 as expressed genes

m<-counts_0min[rowSums( counts_0min[,c(5,6,7)] != 0 ) >= 1,]
head(m)

bss<-counts_0min[rowSums(counts_0min [,c(2,3,4)]!= 0 ) >= 1,]

head(bss)

venn.plot <- venn.diagram(
  x = list(
    BSS= bss$X1,
    M= m$X1),
  filename = "BSS and M in 0 min with atleast 1 replicates over than 0.tiff",
  cex = 2.5,
  cat.cex = 2.5,
  cat.pos = c(-20, 20),
  ext.line.lty = "dotted",
  ext.line.lwd = 2,
  ext.pos = 12,
  ext.dist = -0.12,
  ext.length = 0.85)






