# 遺伝子のZ-scoreでヒートマップ作成．

gene_heatmap <- function(fData, pData, exp, gene, save, exact=T, type="Gene.Symbol", sample=F, Colv=T, Rowv=T, cexRow=0.8){
    # Heatmap with z-score normalized by gene.
  # fData (DataFrame): Gene information
  # pData (DataFrame): ??
  # exp (DataFrame): gene expression
  # gene (List): List of genes used for plotting
  # save (str): file name
  # exact (bool): regular expression
  # type (str): Specified from fData column name.
  # sample (list): Specify the sample to be used for plotting.
  # colv (bool): Clustering or not.
  # Rowv (bool): Clustering or not.
  # cexRow (float): font size

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
  heatmap.2(t(zexp),col="bluered",trace="none", Rowv=Rowv, Colv=Colv, margins=c(8, 5), cexRow=cexRow)
  dev.off()
}
