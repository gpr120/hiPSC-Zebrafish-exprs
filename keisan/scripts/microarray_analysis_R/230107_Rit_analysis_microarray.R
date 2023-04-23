##### human のヒートマップ作成
library(gplots)

server <- "/home/rstudio"
setwd(server)
source(file.path(server, "keisan/function/data_forR4.2.R"))

## sever/Microarrray/00data/ExpressionSet/に保存してあるexpsetファイルを読み込む。
eset <- c("Ritsumei")

setwd(.work <- file.path(server, "keisan", "result", "human_microarray"))
# 解析したいGSEの番号を並べる。自前のデータを使いたい場合は"GPR120_EpiAdip"などと入れる。
# どんな名前で保存されているかは/Volumes/keisan/Documents/Calc/Microarray/00data/ExpressionSetを参照。

# 続いて以下をコピペして実行。
afy <- list() # ここから
for(i in eset){
  read <- list.files(.exp_path <- file.path(server, "keisan", "result", "ExpressionSet"))
  read <- read[grep(paste("^", i, ".*.expset", sep = ""), read)]
  for(j in read){
    message(sprintf("read %s ...", j))
    base::load(file.path(.exp_path, j))
    eval(parse(text = paste("afy[['", gsub(".expset", "", j), "']] <- afy.", gsub(".expset", "", j), sep = "")))
    eval(parse(text = paste("rm(afy.", gsub(".expset", "", j), ")", sep = "")))
  }} # ここまで
# これでexpsetファイルからExpressionSetを読み込める。



# 最後に作業ディレクトリを変更してStep1終了。
dir.create(file.path(.work, notes(afy[[1]])$tag), showWarnings = FALSE)
# 読み込んだデータに関する解析結果を入れるフォルダを作成する。esetで1番目に選んだExpressionSetの名前のフォルダが作成される。
setwd(file.path(.work, notes(afy[[1]])$tag))
# ディレクトリ移動。




######################################################
######################################################
### Step2: 解析対象の選択
######################################################
######################################################

## 解析するExpressionSetについて、解析対象とする比較の組み合わせを選択する。
lapply(afy, function(i) names(notes(i)$pair))
# 解析するExpressionSetの組み合わせの一覧。ここから実際に使う組み合わせを番号で指定する。
id = list(
  "Ritsumei_HT2.0" = c(1,2,3,4,5,6,7,8,9,10,11)
)
# GPR120_Liverの1,2,3,6番目とGSE24031の1,2,3,4番目と...といった形で指定する。GSE自体を解析対象から外す場合はc()とする。
# リストの名前(GSE番号などの部分)はnames(afy)と対応させること。

######################################################
######################################################
## heatmap
######################################################
######################################################
#正規表現
gene_heatmap <- function(a, gene, save, exact=T, type="Gene.Symbol", sample=F, Colv=T, Rowv=T, cexRow=0.8){
  if (exact == T) {
    pattern <- paste("^", gene, "$", sep = "")
  }
  # 情報取得
  x <- c()
  for (i in pattern){
    x <- c(x, featureNames(a)[grep(i, fData(a)[, type], ignore.case = T)])
  }
  # x <- x[order(fData(a)[x, "Gene.Name"])]
  print(x)
  exp <- Biobase::exprs(a)
  exp <- if (max(exp, na.rm = TRUE) > 30){
    exp} else{2^exp}
  exp <- exp[x,]
  if (length(sample)>2){
    exp <- exp[,sample]
  }
  
  # 重複の除去
  gene_symbol_list <- fData(a)[x, "Gene.Symbol"]
  new_rowname <- c()
  new_exp <- c()
  for (gene_symbol_i in unique(gene_symbol_list)){
    probe_i <- rownames(exp)[gene_symbol_list==gene_symbol_i][1]
    exp_i <- exp[gene_symbol_list==gene_symbol_i, ]
    if (!is.null(dim(exp_i))){
      exp_i <- apply(exp_i, 2, mean)
      print(exp_i)
    }
    new_rowname <- c(new_rowname, probe_i)
    new_exp <- rbind(new_exp, exp_i)
  }
  rownames(new_exp) <- new_rowname
  exp <- new_exp
  
  x <- new_rowname
  
  # 正規化
  colnames(exp) <- pData(a)[colnames(exp),"sample"]
  zexp <- scale(t(exp))
  # 保存
  write.csv(cbind(fData(a)[x,c("Gene.ID", "Gene.Name", "Gene.Symbol")], exp),paste(save,"exp.csv",sep="_"))
  write.csv(cbind(fData(a)[x,c("Gene.ID", "Gene.Name", "Gene.Symbol")], t(zexp)), paste(save,"zscore.csv",sep="_"))
  # 作図
  colnames(zexp) <- fData(a)[x,"Gene.Symbol"]
  pdf(paste(save, ".pdf", sep=""))
  heatmap.2(t(zexp),col="bluered",trace="none", Rowv=Rowv, Colv=Colv, margins=c(8, 5), cexRow=cexRow)
  dev.off()
}

save <- "HOX genes"
gene <- c("HOXA1", "HOXA2", "HOXA3", "HOXA4", "HOXA5", "HOXA6", "HOXA7", "HOXA9", "HOXA10", "HOXA11", "HOXA13", "HOXB1", "HOXB2", "HOXB3", "HOXB4", "HOXB5", "HOXB6", "HOXB7", "HOXB8", "HOXB9", "HOXB13", "HOXC4", "HOXC5", "HOXC6", "HOXC8", "HOXC9", "HOXC10", "HOXC11", "HOXC12", "HOXC13", "HOXD1", "HOXD3", "HOXD4", "HOXD8", "HOXD9", "HOXD10", "HOXD11", "HOXD12", "HOXD13")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=T, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=F, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))

save <- "HOX related genes"
gene <- c("OTX1", "OTX2", "EGR2" ,"MEIS1", "MEIS2")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=T, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))
save <- paste(save, "_No_cluster", sep="")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=F, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))

save <- "Retinoic acid related genes"
gene <- c("ALDH1A1", "ALDH1A2", "ALDH1A3", "CYP26A1", "CYP26B1", "CYP26C1", "CRABP1", "CRABP2", "STRA6", "STRA8", "RARA", "RARB", "RARG", "RXRA", "RXRB", "RXRG")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=T, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))
save <- paste(save, "_No_cluster")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=F, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))

save <- "Others"
gene <- c("BMP4", "WNT3A", "FGF3", "FGF8")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=T, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))
save <- paste(save, "_No_cluster")
gene_heatmap(afy[[1]], gene, save, Colv=F, Rowv=F, sample=c(16,17,18,4,5,6,1,2,3,7,8,9,13,14,15,10,11,12))
