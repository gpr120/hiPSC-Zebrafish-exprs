##### 立命の追加解析
library(gplots)
server <- "/home/rstudio/kankyo_220501"


source(file.path(server, "function", "heatmap.R"))

setwd(server)
source("renv/activate.R")
source(file.path(server, "00data/data_forR4.1.R"))

setwd(.work <- file.path(server, sprintf("%sueno", .date)))
setwd(.work <- file.path(server, sprintf("%sueno", "230106")))

## sever/Microarrray/00data/ExpressionSet/に保存してあるexpsetファイルを読み込む。
setwd(file.path(server, sprintf("%sueno", "221225")))
tpm <- read.csv("gene_tpm.csv", header=T, row.names=1, check.names=F)
count <- read.csv("gene_count.csv", header=T, row.names=1, check.names=F)
fData <- read.csv("fData.csv", header=T, row.names=1, check.names=F)
pData <- read.csv("pData.csv", header=T, row.names=1, check.names=F)
tmm <- read.csv("tcc_all_tmm.csv", header=T, row.names=1, check.names=F)
setwd(.work)
setwd("zebra")

save <- "HOX genes"
gene <- c("^hoxa1a", "^hoxa2b", "^hoxa3a", "^hoxa4a", "^hoxa5a" ,"^hoxa9a", "^hoxa9b", "^hoxa10b", "^hoxa11a", "^hoxa11b", "^hoxa13a", "^hoxa13b", "^hoxb1a", "^hoxb1b", "^hoxb2a", "^hoxb3a", "^hoxb4a", "^hoxb5a", "^hoxb5b", "^hoxb6a", "^hoxb6b", "^hoxb7a", "^hoxb8a", "^hoxb8b", "^hoxb9a", "^hoxb10a", "^hoxb13a", "^hoxc1a", "^hoxc3a", "^hoxc4a", "^hoxc5a", "^hoxc6a", "^hoxc6b", "^hoxc8a", "^hoxc9a", "^hoxc10a", "^hoxc11a", "^hoxc11b", "^hoxc12a", "^hoxc12b", "^hoxc13a", "^hoxc13b", "^hoxd3a", "^hoxd4a", "^hoxd9a", "^hoxd10a", "^hoxd11a", "^hoxd12a", "^hoxd13a"
)
gene_heatmap(fData, pData, tpm, gene, save, exact=F, cexRow=0.5)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tpm, gene, save, exact=F, Colv=F, Rowv=F,sample=c(7,8,9,10,11,12,1,2,3,4,5,6,19,20,21,22,23,24,25,26,27,28,29,30,13,14,15,16,17,18), cexRow=0.5)
gene_all <- c(gene)


gene <- c("^otx1", "^otx2a", "^otx2b", "^egr2a", "^egr2b", "^meis1a", "^meis1b", "^meis2a", "^meis2b")
save <- "HOX related genes"
gene_heatmap(fData, pData, tpm, gene, save, exact=F)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tpm, gene, save, exact=F, Colv=F, Rowv=F,sample=c(7,8,9,10,11,12,1,2,3,4,5,6,19,20,21,22,23,24,25,26,27,28,29,30,13,14,15,16,17,18))
gene_all <- c(gene_all, gene)

save <- "Retinoic acid related genes"
gene <- c("aldh1a2", "aldh1a3", "cyp26a1", "cyp26b1", "cyp26c1 crabp1a", "crabp1b", "crabp2a", "crabp2b", "stra6", "raraa", "rarab", "rarga", "rargb", "rxraa", "rxrab", "rxrba", "rxrbb", "rxrga", "rxrgb")
gene_heatmap(fData, pData, tpm, gene, save, exact=T)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tpm, gene, save, exact=T, Colv=F, Rowv=F,sample=c(7,8,9,10,11,12,1,2,3,4,5,6,19,20,21,22,23,24,25,26,27,28,29,30,13,14,15,16,17,18))
gene_all <- c(gene_all, gene)

save <- "Others"
gene <- c("^bmp4", "^wnt3a", "^fgf3", "^fgf8a", "^fgf8b")
gene_heatmap(fData, pData, tpm, gene, save, exact=F)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tpm, gene, save, exact=F, Colv=F, Rowv=F,sample=c(7,8,9,10,11,12,1,2,3,4,5,6,19,20,21,22,23,24,25,26,27,28,29,30,13,14,15,16,17,18))
gene_all <- c(gene_all, gene)

save <- "All"
gene_heatmap(fData, pData, tpm, gene_all, save, exact=F, cexRow=0.3)
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(fData, pData, tpm, gene_all, save, exact=F, Colv=F, Rowv=F,sample=c(7,8,9,10,11,12,1,2,3,4,5,6,19,20,21,22,23,24,25,26,27,28,29,30,13,14,15,16,17,18), cexRow=0.3)
