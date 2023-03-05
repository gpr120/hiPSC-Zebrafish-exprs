##### TCCまでを行う．

server <- "/home/rstudio"
setwd(server)
source(file.path(server, "keisan/function/data_forR4.2.R"))

library("gtools")
library("TCC")


# setwd(.work <- file.path(server, sprintf("%sueno", .date)))

read_rsem_result <- function(rsem_result_f_list){
    # rsemの結果のファイルリストを入れると，CountとTPMを返す関数
    rsem_result_list = lapply(rsem_result_f_list,function(x){
        read.table(file=x, sep="\t", header=T, row.names=1)
    })
    rsem_result_count = sapply(rsem_result_list,function(x){x$expected_count})
    rsem_result_tpm = sapply(rsem_result_list,function(x){x$TPM})

    sample_name_list <- gsub(".*/", "", rsem_result_f_list)
    sample_name_list <- gsub("\\..*", "", sample_name_list)
    row_name <- rownames(rsem_result_list[[1]])

    rownames(rsem_result_count) <- row_name
    rownames(rsem_result_tpm) <- row_name
    colnames(rsem_result_count) <- sample_name_list
    colnames(rsem_result_tpm) <- sample_name_list
    return(list(rsem_result_count, rsem_result_tpm))
}


dir.create(.work <- file.path(server, "keisan", "result", "zebra_ngs"), recursive=T)
setwd(.work)

rsem_result_f = file.path(server, "RNAseq", "221211_Rit_zebra", "rsem_results_221216")
# fileのリスト
rsem_result_f_list = list.files(rsem_result_f, pattern="genes.results", full.names=T)

gene_count_tpm <- read_rsem_result(rsem_result_f_list)
gene_count <- gene_count_tpm[[1]]
gene_tpm <- gene_count_tpm[[2]]


# GTFから必要な情報を抜き出す．今回はGeneIDだけ．
gtf_f <- "~/RNAseq/database/Zebrafish/GCF_000002035.6_GRCz11_genomic.gtf"
gtf_info <- read.table(file=gtf_f, sep="\t", header=F)
tmp <- (gtf_info[,9])
tmp <- gsub("^gene_id.", "", tmp)
gene_id = gsub(";.*", "", tmp)
tmp <- gsub(".*db_xref.GeneID.", "", tmp)
GeneID <- gsub(";.*", "", tmp)

gtf_info_mini <- as.data.frame(cbind(gene_id, GeneID))
# アルファベットがGeneIDに含まれるわけがないのでソートすることで数字が頭に来るようにする
gtf_info_mini <- gtf_info_mini[order(gtf_info_mini$GeneID), ]
gtf_info_mini <- gtf_info_mini[!duplicated(gtf_info_mini$gene_id),]

# ncbiからGeneの情報を入手する．頻繁に更新されるようなので毎回ダウンロードした方がよさそう
curl <- "https://ftp.ncbi.nlm.nih.gov/gene/DATA/GENE_INFO/Non-mammalian_vertebrates/Danio_rerio.gene_info.gz"
download.file(curl, "Danio_rerio.gene_info.gz")
gene_info <- read.table(file="Danio_rerio.gene_info.gz", sep="\t", header=T, comment.char = "@", quote = "\"")

gene_info <- gene_info[,c("GeneID", "Symbol", "description")]

# gtfのデータと gene_info のデータをGeneIDをキーに結合する．

fData <- merge(gtf_info_mini, gene_info, by="GeneID", all.x=T)
rownames(fData) <- fData$gene_id
fData <- fData[rownames(gene_tpm), ]
colnames(fData) <- c("Gene.ID", "gene_id", "Gene.Symbol", "Gene.Name")


# 重複を削除
new_fData <- c()
new_gene_count <- c()
new_gene_tpm <- c()
unique_geneID <- unique(fData$Gene.ID) 
for(gene_id in unique_geneID){
  idx = fData$Gene.ID == gene_id
  fData_i <- fData[idx, ]
  gene_count_i <- gene_count[idx, ]
  gene_tpm_i <- gene_tpm[idx, ]
  if(length(fData_i$Gene.Name)>1){
    fData_i <- fData_i[1, ]
    gene_count_i <- apply(gene_count_i, 2, sum)
    gene_tpm_i <- apply(gene_tpm_i, 2, sum)
  }
  new_fData <- rbind(new_fData, fData_i)
  new_gene_count <- rbind(new_gene_count, gene_count_i)
  new_gene_tpm <- rbind(new_gene_tpm, gene_tpm_i)
}

rownames(new_gene_count) <- rownames(new_fData)
rownames(new_gene_tpm) <- rownames(new_fData)

# 諸々保存する．
write.csv(new_fData, "fData.csv")
write.csv(new_gene_tpm, "gene_tpm.csv")
write.csv(new_gene_count, "gene_count.csv")





# 以下統計解析

tpm <- read.csv("gene_tpm.csv", header=T, row.names=1, check.names=F)
count <- read.csv("gene_count.csv", header=T, row.names=1, check.names=F)
fData <- read.csv("fData.csv", header=T, row.names=1, check.names=F)
pData <- read.csv("pData.csv", header=T, row.names=1, check.names=F)

exprs_set <- list(count=count, tpm=tpm, fData=fData, pData=pData)


make_pair <- function(pData, set="All"){
  # pairは行名が vsにしたもの。
  condition <- unique(pData$condition)
  pair <- as.data.frame(permutations(n=length(condition), r=2, v=condition))
  colnames(pair) <- c("left", "right")
  
  rownames(pair) <- apply(pair, 1, function(i)(paste(i, collapse=" vs ")))
  if (set!="All" && set!="ALL"){
    pair <- pair[set,]
  }
  print(rownames(pair))
  print(pair)
  return(invisible(pair))
}

names(make_pair(pData))

exprs_set$pairs <- make_pair(pData, set=c(1, 9, 10, 11, 14, 18, 20))


statistic <- function (exprs_set, filter=10, data.path="."){
  tcc_analysis <- function(exprs_set, cond, filter=10, plot=FALSE){
    # exprs_set: 全部
    # cond: このconditionについて比較する
    # filter いずれかの条件で平均がこの値以上になっているデータを利用する。
    ### 優位な遺伝子、門田さんをリスペクトして
    # http://www.iu.a.u-tokyo.ac.jp/~kadota/r_seq.html#about_analysis_deg_RNAseq
    # 解析 | 発現変動 | 2群間 | 対応なし | 複製あり | TCC(Sun_2013)
    pData <- exprs_set$pData # いちいち打つのが長いので
    
    filter_idx = apply(exprs_set$mean_count[, cond], 1, max)
    # filter_idx = names(filter_idx[filter_idx >= filter])
    
    filter_idx2 = apply(exprs_set$count, 1, min)
    filter_idx = names(filter_idx[(filter_idx >= filter) & (filter_idx2 > -1)])

    group <- c()
    i <- 1
    for (cond_i in cond){
      cond_i_idx = pData$condition==cond_i
      group <- append(group, rep(i, sum(cond_i_idx)))
      if (i == 1){
        count_d <- exprs_set$count[filter_idx, rownames(pData[cond_i_idx, ])]}
      else{
        count_d <- cbind(count_d, exprs_set$count[filter_idx, rownames(pData[cond_i_idx, ])])
      }
      i = i + 1
    }
    tcc <- new("TCC", count_d, group) 
    #本番(正規化)
    # tcc <- filterLowCountGenes(tcc, low.count=low_count) # フィルタリング, low_countの仕様がよくわらかないので上でやる
    print("tcc")
    tcc <- calcNormFactors(tcc, norm.method="tmm", test.method="edger",iteration=3, FDR=0.1, floorPDEG=0.05)#正規化を実行した結果をtccに格納
    print("tcc")
    tcc <- estimateDE(tcc, test.method="edger", FDR=0.1)#DEG検出を実行した結果をtccに格納
    print("tcc")

    normalized_count <- getNormalizedData(tcc)
    result <- getResult(tcc, sort=FALSE)
    rownames(result) <- result$gene_id
    if (plot!=FALSE){
      pdf(paste(plot, "_maPlot.pdf", sep=""))
      for(i in c(0.1, 0.2, 0.3, 0.4, 0.5)){
        plot(tcc, FDR = i, main=i)
      }
      plot(density(cpm(normalized_count, log=TRUE)), main="filtering", xlab="log CPM")
      dev.off()
      write.csv(cbind(result, exprs_set$fData[result$gene_id,]), paste(save, ".csv", sep=""))
      write.csv(normalized_count, paste(save, "_tmm.csv", sep=""))
    }
    return (invisible(result))
  }
  
  ############ 全部を含んで統計量を出す。
  cond <- unique(unlist(exprs_set$pairs))
  pData <- exprs_set$pData
  pairs <- exprs_set$pairs
  mean <- sapply(cond, function(i)(apply(exprs_set$tpm[, rownames(pData[pData$condition==i, ])], 1, mean)))
  exprs_set$mean <- mean
  mean_count <- sapply(cond, function(i)(apply(exprs_set$count[, rownames(pData[pData$condition==i, ])], 1, mean)))
  exprs_set$mean_count <- mean_count
  div <- sapply(rownames(pairs), function(i){apply(mean[, unlist(pairs[i, ])], 1, function(j){j[1]/j[2]})})
  
  print("all")
  # 全部tccを回す用にGroupの作成とデータの並び替え
  save <- paste(data.path, "tcc_all", sep="/")
  tcc_by_all <- tcc_analysis(exprs_set, cond, filter=filter, plot=save)
  
  # それぞれの
  for (pair_i in rownames(pairs)){
    cond <- unname(unlist(pairs[pair_i, ]))
    save <- paste(data.path, paste("tcc", pair_i, sep="_"), sep="/")
    tcc_by_i <- tcc_analysis(exprs_set, cond, filter=filter, plot=save)
    }
}

filter <- 10
statistic(exprs_set, filter=filter)
