##### Zebra のヒートマップ作成
library(gplots)

server <- "/home/rstudio"
setwd(server)
source(file.path(server, "keisan/function/data_forR4.2.R"))


setwd(.work <- file.path(server, "keisan", "result", "zebra_ngs"))

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
  
  # colnames(exp) <- pData(a)[colnames(exp),"sample"]
  
  # 正規化
  zexp <- scale(t(exp))
  # 保存
  write.csv(cbind(fData[x,c("Gene.ID", "Gene.Name", "Gene.Symbol")], exp),paste(save,"exp.csv",sep="_"))
  write.csv(cbind(fData[x,c("Gene.ID", "Gene.Name", "Gene.Symbol")], t(zexp)), paste(save,"zscore.csv",sep="_"))
  # 作図
  colnames(zexp) <- fData[x,"Gene.Symbol"]
  pdf(paste(save, ".pdf", sep=""))
  heatmap.2(t(zexp),col="bluered",trace="none", Rowv=Rowv, Colv=Colv, margins=c(8, 5), cexRow=cexRow, distfun = function(x) dist(x, method="euclidean"),
  hclustfun = function(x) hclust(x, method="ward.D2"))
  dev.off()
}

setwd(.work)
tpm <- read.csv("gene_tpm.csv", header=T, row.names=1, check.names=F)
count <- read.csv("gene_count.csv", header=T, row.names=1, check.names=F)
fData <- read.csv("fData.csv", header=T, row.names=1, check.names=F)
pData <- read.csv("pData.csv", header=T, row.names=1, check.names=F)
tmm <- read.csv("tcc_all_tmm.csv", header=T, row.names=1, check.names=F)


dir.create("heatmap")
setwd("heatmap")

sample <- c(25,26,27,28,29,30,1,2,3,4,5,6,13,14,15,16,17,18,19,20,21,22,23,24,7,8,9,10,11,12)

save <- "HOX genes"
gene <- c("^hoxa1a", "^hoxa2b", "^hoxa3a", "^hoxa4a", "^hoxa5a" ,"^hoxa9a", "^hoxa9b", "^hoxa10b", "^hoxa11a", "^hoxa11b", "^hoxa13a", "^hoxa13b", "^hoxb1a", "^hoxb1b", "^hoxb2a", "^hoxb3a", "^hoxb4a", "^hoxb5a", "^hoxb5b", "^hoxb6a", "^hoxb6b", "^hoxb7a", "^hoxb8a", "^hoxb8b", "^hoxb9a", "^hoxb10a", "^hoxb13a", "^hoxc1a", "^hoxc3a", "^hoxc4a", "^hoxc5a", "^hoxc6a", "^hoxc6b", "^hoxc8a", "^hoxc9a", "^hoxc10a", "^hoxc11a", "^hoxc11b", "^hoxc12a", "^hoxc12b", "^hoxc13a", "^hoxc13b", "^hoxd3a", "^hoxd4a", "^hoxd9a", "^hoxd10a", "^hoxd11a", "^hoxd12a", "^hoxd13a"
)
# gene_heatmap(fData, pData, tpm, gene, save, exact=T, Colv=T,cexRow=0.5)
# control, BPA, RA_10, RA_12.5, RA_10+BPA の順番にする．手動の方が早い
# sample_tpm = c(7,8,9,10,11,12,1,2,3,4,5,6,19,20,21,22,23,24,25,26,27,28,29,30,13,14,15,16,17,18)

gene_heatmap(fData, pData, tmm, gene, paste(save, sep="_"), exact=T, Colv=T,cexRow=0.5)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tmm, gene, save, exact=F, Colv=F, Rowv=T,sample=sample, cexRow=0.5)
gene_all <- c(gene)


gene <- c("^otx1", "^otx2a", "^otx2b", "^egr2a", "^egr2b", "^meis1a", "^meis1b", "^meis2a", "^meis2b","^fgf8a", "^fgf8b", "^fgf3$")
save <- "HOX related genes"
gene_heatmap(fData, pData, tmm, gene, save, exact=F, Colv=T)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tmm, gene, save, exact=F, Colv=F, Rowv=T,sample=sample)
gene_all <- c(gene_all, gene)

# crabp1a, rargbは平均count数が10未満なので，遺伝子発現量（正規化済み）を出す前に除外 (raragb は rargbの間違いと仮定)
# gene_count.csv で確認可能
# cyp26c1 はタイプミスで前回送信時に修正済 ("cyp26c1", "crabp1a" を"cyp26c1 crabp1a"としていた)

save <- "Retinoic acid related genes"
gene <- c("aldh1a2", "aldh1a3", "cyp26a1", "cyp26b1", "cyp26c1", "crabp1b", "crabp2a", "crabp2b", "stra6", "raraa", "rarab", "rarga",  "rxraa", "rxrab", "rxrba", "rxrbb", "rxrga", "rxrgb")
gene_heatmap(fData, pData, tmm, gene, save, exact=T, Colv=T)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tmm, gene, save, exact=T, Colv=F, Rowv=T,sample=sample)
gene_all <- c(gene_all, gene)

