##### 立命の追加解析をする
library(gplots)

server <- "/home/rstudio/kankyo_220501"
setwd(server)
source("renv/activate.R")

source(file.path(server, "00data/data_forR4.1.R"))

setwd(.work <- file.path(server, sprintf("%sueno", .date)))

setwd(.work <- file.path(server, sprintf("%sueno", "221227")))

## sever/Microarrray/00data/ExpressionSet/に保存してあるexpsetファイルを読み込む。


######################################################
######################################################
## 発現量抽出
######################################################
######################################################
#正規表現
gene_heatmap <- function(fData, pData, exp, gene, save, exact=T, type="Gene.Symbol", sample=F, Colv=T, Rowv=T, cexRow=0.8){
  if (exact == T){
    pattern <- paste("^", gene, "$", sep = "")
  }else{
    pattern <- gene
  }
  # 情報取得
  x <- c()
  for (i in pattern){
    x <- c(x, rownames(fData)[grep(i, fData[, type], ignore.case = T)])
  }
  # x <- x[order(fData(a)[x, "Gene.Name"])]
  print(x)
  exp <- exp[x,]
  if (length(sample)>2){
    exp <- exp[,sample]
  }
  # 正規化
  #colnames(exp) <- pData(a)[colnames(exp),"sample"]
  zexp <- scale(t(exp))
  # 保存
  write.csv(cbind(fData[x,c("Gene.ID", "Gene.Name", "Gene.Symbol")], exp),paste(save,"exp.csv",sep="_"))
  write.csv(cbind(fData[x,c("Gene.ID", "Gene.Name", "Gene.Symbol")], t(zexp)), paste(save,"zscore.csv",sep="_"))
  # 作図
  colnames(zexp) <- fData[x,"Gene.Symbol"]
  pdf(paste(save, ".pdf", sep=""))
  heatmap.2(t(zexp),col="bluered",trace="none", Rowv=Rowv, Colv=Colv, margins=c(8, 5), cexRow=cexRow)
  dev.off()
}

tpm <- read.csv("gene_tpm.csv", header=T, row.names=1, check.names=F)
count <- read.csv("gene_count.csv", header=T, row.names=1, check.names=F)
fData <- read.csv("fData.csv", header=T, row.names=1, check.names=F)
pData <- read.csv("pData.csv", header=T, row.names=1, check.names=F)
tmm <- read.csv("tcc_all_tmm.csv", header=T, row.names=1, check.names=F)


save <- "HOX genes"
gene <- c("^hoxa1a", "^hoxa2b", "^hoxa3a", "^hoxa4a", "^hoxa5a,hoxa9a,hoxa9b", "^hoxa10b", "^hoxa11a", "^hoxa11b", "^hoxa13a", "^hoxa13b", "^hoxb1a", "^hoxb1b", "^hoxb2a", "^hoxb3a", "^hoxb4a", "^hoxb5a", "^hoxb5b", "^hoxb6a", "^hoxb6b", "^hoxb7a", "^hoxb8a", "^hoxb8b", "^hoxb9a", "^hoxb10a", "^hoxb13a", "^hoxc1a", "^hoxc3a", "^hoxc4a", "^hoxc5a", "^hoxc6a", "^hoxc6b", "^hoxc8a", "^hoxc9a", "^hoxc10a", "^hoxc11a", "^hoxc11b", "^hoxc12a", "^hoxc12b", "^hoxc13a", "^hoxc13b", "^hoxd3a", "^hoxd4a", "^hoxd9a", "^hoxd10a", "^hoxd11a", "^hoxd12a", "^hoxd13a"
)
gene_heatmap(fData, pData, tpm, gene, save, exact=F)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tpm, gene, save, exact=F, Colv=F, Rowv=F)
gene_all <- c(gene)


gene <- c("^otx1", "^otx2a", "^otx2b", "^egr2a", "^egr2b", "^meis1a", "^meis1b", "^meis2a", "^meis2b")
save <- "HOX related genes"
gene_heatmap(fData, pData, tpm, gene, save, exact=F)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tpm, gene, save, exact=F, Colv=F, Rowv=F)
gene_all <- c(gene_all, gene)

save <- "Retinoic acid related genes"
gene <- c("^aldh1", "^aldh2", "^cyp26a1", "^cyp26b1", "^cyp26c1", "^crabp1a", "^crabp1b", "^crabp2a", "^crabp2b", "^stra6", "^raraa", "^rarab", "^rarga", "^rargb", "^rxraa", "^rxrab", "^rxrba", "^rxrbb", "^rxrga", "^rxrgb")
gene_heatmap(fData, pData, tpm, gene, save, exact=F)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tpm, gene, save, exact=F, Colv=F, Rowv=F)
gene_all <- c(gene_all, gene)

save <- "Others"
gene <- c("^bmp4", "^wnt3a", "^fgf3", "^fgf8a", "^fgf8b")
gene_heatmap(fData, pData, tpm, gene, save, exact=F)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tpm, gene, save, exact=F, Colv=F, Rowv=F)
gene_all <- c(gene_all, gene)

save <- "All"
gene_heatmap(fData, pData, tpm, gene_all, save, exact=F, cexRow=0.3)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tpm, gene_all, save, exact=F, Colv=F, Rowv=F, cexRow=0.3)




save <- "No_cluster_HOX genes"
gene <- c("HOXA1", "HOXA2", "HOXA3", "HOXA4", "HOXA5", "HOXA6", "HOXA7", "HOXA9", "HOXA10", "HOXA11", "HOXA13", "HOXB1", "HOXB2", "HOXB3", "HOXB4", "HOXB5", "HOXB6", "HOXB7", "HOXB8", "HOXB9", "HOXB13", "HOXC4", "HOXC5", "HOXC6", "HOXC8", "HOXC9", "HOXC10", "HOXC11", "HOXC12", "HOXC13", "HOXD1", "HOXD3", "HOXD4", "HOXD8", "HOXD9", "HOXD10", "HOXD11", "HOXD12", "HOXD13")
# gene <- c("BMP1","BMP2","BMP4","BMP5","BMP6","BMP7","BMP8A","BMP8B","BMP10","BMP15","ID1","ID2","ID2B","ID3")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=F, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))
gene_all <- c(gene)

save <- "No_cluster_HOX related genes"
gene <- c("OTX1", "OTX2", "EGR2" ,"MEIS1", "MEIS2")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=F, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))
gene_all <- c(gene_all, gene)

save <- "No_cluster_Retinoic acid related genes"
gene <- c("AHDH1A1", "AHDH1A2", "AHDH1A3", "CYP26A1", "CYP26B1", "CYP26C1", "CRABP1", "CRABP2", "STRA6", "STRA8", "RARA", "RARB", "RARG", "RXRA", "RXRB", "RXRG")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=F, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))
gene_all <- c(gene_all, gene)

save <- "No_cluster_Others"
gene <- c("BMP4", "WNT3A", "FGF3", "FGF8")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=F, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))
gene_all <- c(gene_all, gene)

save <- "No_cluster_All"
gene_heatmap(afy[[1]], gene_all, save, Colv=F, Rowv=F, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))
