data<- read_csv("BSS_0_vs_M_0_logFC.csv")

head(data)

data$threshold = as.factor(abs(data$log2FoldChange) > 0.58 & data$padj < 0.05)

ggplot(subset(data, log2FoldChange > 0.58& padj < 0.05 |log2FoldChange < -0.58 & padj < 0.05),
       aes(x=log2FoldChange, y=-log10(padj), 
           colour=threshold,fill=threshold)) +
  scale_color_manual(values=c("black","blue"))+
  ylim(0,200)+
  xlim(-10,10)+
  geom_point(alpha=0.4, size=1.2) +
  theme(panel.background = element_rect(fill = "white", colour = "black"),
        axis.text=  element_text(colour="black", size=10))+
          geom_vline(xintercept=c(-0.58,0.58),linetype = "dashed",col="grey",lwd=1)+ 
          geom_hline(yintercept = -log10(0.05),linetype = "dashed",col="grey",lwd=1)+ 
            xlab("log2 fold change") + ylab("-log10 adj p-value") +
  ggtitle("Volcano picture of DEG of BSS vs M at 0MIN")

  