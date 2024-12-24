###################################
###### Microarrayの解析方法 #######
###################################

######################################################
######################################################
### Step1: データの読み込み
######################################################
######################################################

server <- "/home/rstudio/keisan"
setwd(server)
source(file.path(server, "00data/packages/data_forR4.2.R"))
setwd(.work <- file.path(server, "result", "zebra_ngs"))



eset <- c("Ritsumei_HT2.0","NGS_Ritsumei_zebra")
# 解析したいGSEの番号を並べる。自前のデータを使いたい場合は"GPR120_EpiAdip"などと入れる。
# どんな名前で保存されているかは/Volumes/keisan/Documents/Calc/Microarray/00data/ExpressionSetを参照。
.work <- file.path(server, "result")

# 続いて以下をコピペして実行。
afy <- list() # ここから
for(i in eset){
read <- list.files(file.path(server, "result", "ExpressionSet"))
read <- read[grep(paste("^", i, ".*.expset", sep = ""), read)]
for(j in read){
message(sprintf("read %s ...", j))
load(file.path(server, "result", "ExpressionSet", j))
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
id = list("Ritsumei_HT2.0" = c(1,3,4,11, 12), "NGS_Ritsumei_zebra" = c(1,2,3,4,5))
# GPR120_Liverの1,2,3,6番目とGSE24031の1,2,3,4番目と...といった形で指定する。GSE自体を解析対象から外す場合はc()とする。
# リストの名前(GSE番号などの部分)はnames(afy)と対応させること。
write.csv(cbind(fData(afy[[1]])[, 1:4], notes(afy[[1]])$test), file.path(.work, "ips_ttest.csv"))

### Step3: 解析


######################################################
######################################################
### Step3-A: GO解析
######################################################
######################################################

## afyに対してGO解析を行う。パラメータが多いが、これは関数の自由度を高くした結果であり、自分で設定する部分はそれほど多くない。
microarray.goplot(afy, cut = 1e-5, lmt = -10, tag = notes(afy[[1]])$tag, inside = -10, cluster = c("") , ind = id, barplot.length = 0, network.analysis = F, clusterKEGG = F, ver = 3, hccmethod = "-bind", firstpage = F)
microarray.goplot(afy, cut = 1e-5, lmt = -10, tag = notes(afy[[1]])$tag, inside = -10, cluster = c("1110","101111", "101011", "1001111", "10010") , notice = NULL, ind = id, barplot.length = 500, show.ontology = NULL, network.analysis = F, clusterKEGG = F, ver = 3, hccmethod = "-bind")


library(openxlsx)


getGO2GeneIDs <- function (afy, ont = c("BP", "MF", "CC")) {
	# ベクトル化。
	if (class(afy) == "list") return(mapply(microarray.gotest, afy, ont))
	# パッケージの読み込み。
	load.packages("GO.db")
	# 引数のチェック。
	ont <- match.arg(ont)
	# 遺伝子IDを取り出す。
	ids <- lapply(Biobase::notes(afy)$subset, function(i) list(id = Biobase::fData(afy)[i, "Gene.ID"], diff = Biobase::notes(afy)$diff[i, , drop = FALSE]))
	ids <- sapply(names(ids), function(i) split(ids[[i]]$id, cut(ids[[i]]$diff[, i], c(0, 1, Inf), c("decrease", "increase"))), simplify = FALSE)
	ids <- lapply(ids, lapply, function(x) unique(unlist(strsplit(x, " ?/{2,} ?"))))
	# 動物分類に対応するパッケージを読み込む。
	ann <- if (grepl("^GPL", Biobase::annotation(afy))) geoquery.convert(Biobase::annotation(afy)) else Biobase::annotation(afy)
	org <- if (grepl("^GPL", ann)) microarray.organism(afy) else ann
	org <- switch(org, "Mus musculus" = "org.Mm.eg", "Homo sapiens" = "org.Hs.eg", "Rattus norvegicus" = "org.Rn.eg", "Caenorhabditis elegans" = "org.Ce.eg", "Macaca mulatta" = "org.Mmu.eg", org)
	message(sprintf("Use Package: %s.db", org))
	load.packages(sprintf("%s.db", org))
	#detach(sprintf("package:%s.db", org), unload = TRUE, character.only = TRUE)
	#library(sprintf("%s.db", org), character.only = TRUE)
	con <- do.call(sprintf("%s_dbconn", org), list())
	# 全体の遺伝子ID。
	uni <- unique(unlist(strsplit(Biobase::fData(afy)[, "Gene.ID"], " ?/{2,} ?")))
	if (all(!grepl("^GPL", ann), !grepl("org.*eg", ann))) {
		oid <- unique(unlist(DBI::dbGetQuery(GO.db::GO_dbconn(), sprintf("SELECT go_id\nFROM go_term INNER JOIN go_%s_parents USING (_id)", ont))))
		pid <- DBI::dbGetQuery(con, sprintf("SELECT gene_id, go_id\nFROM genes INNER JOIN go_%s_all USING (_id)", ont))
		pid <- unique(pid$gene_id[pid$go_id %in% oid])
		pid <- intersect(pid, sort(DBI::dbGetQuery(con, "SELECT gene_id\nFROM probes\nWHERE is_multiple == 0")$gene_id))
		uni <- intersect(pid, uni)
	}
	# オントロジーと遺伝子IDの対応リストを作成する。
	message("Get mapping GO and EntrezID ...")
	ent <- paste("'", uni, "'", sep = "", collapse = ",")
	gid <- sprintf("SELECT DISTINCT gene_id, go_id\nFROM genes INNER JOIN go_%s_all USING (_id)\nWHERE gene_id IN (%s)", ont, ent)
	gid <- DBI::dbGetQuery(con, gid)
	cat <- split(gid$gene_id, gid$go_id)
    return (cat)
}


export_GO2GeneIDs_to_excel <- function(afy, go_ids_list, tmm=NULL, seq_by_pairs = TRUE) {
  
  # getGO2GeneIDs関数を使ってGOとGeneIDの対応を取得
  go_geneIDs <- getGO2GeneIDs(afy, ont = "BP")
  
  for(go_id in go_ids_list) {
    
    # go_id と一致するGeneIDを取得
    geneIDs_for_go_id <- go_geneIDs[[go_id]]
    
    # fData(afy)$Gene.ID に含まれる行を抽出
    filtered_fData <- fData(afy)[fData(afy)$Gene.ID %in% geneIDs_for_go_id, ]
    
	# tmmを指定した場合
	if (is.null(tmm)) {
        expr_data <- exprs(afy)[rownames(filtered_fData), ]
    }else{
		expr_data <- tmm[rownames(filtered_fData), ]
	}

	filtered_fData <- cbind(filtered_fData, expr_data)
	if (seq_by_pairs) {
		# (notes(afy)$test)のデータをfiltered_fDataに結合
		test_data <- notes(afy)$test[rownames(filtered_fData), ]
		filtered_fData <- cbind(filtered_fData, test_data)

		# notes(afy)$subset の names でループし、マッチングを実施
		matching_names_list <- list()
		for(name in names(notes(afy)$subset)) {
			matched_rows <- rownames(filtered_fData)[rownames(filtered_fData) %in% notes(afy)$subset[[name]]]
			if(length(matched_rows) > 0) {
			matching_names_list[[name]] <- matched_rows
			}
		}
    }
    
    # Excel に書き出し

    if (seq_by_pairs) {
		wb <- createWorkbook()
		for(name in names(matching_names_list)) {
		classification <- ifelse(notes(afy)$diff[matching_names_list[[name]], name] > 1, "increase", "decrease")
		for(cls in unique(classification)) {
			sheet_name <- paste(name, cls, sep = "_")
			if (nchar(sheet_name) > 31) {sheet_name <- substr(sheet_name, 1, 30)}
			filtered_rows <- filtered_fData[rownames(filtered_fData) %in% matching_names_list[[name]][classification == cls], ]
			addWorksheet(wb, sheet_name)
			writeData(wb, sheet_name, filtered_rows)
		}
		saveWorkbook(wb, paste0(notes(afy)$tag, "_", gsub(":", "-",go_id), ".xlsx"), overwrite = TRUE)
		}
	}else {
		write.csv(filtered_fData, paste0(notes(afy)$tag, "_", gsub(":", "-",go_id), ".csv"), row.names = FALSE)
	}
  }
}



# ファイルの読み込み（タブ区切りテキストとして）
file_tsv <- "231118_Microarray_GOanalysis_Ritsumei_HT2.0-5.xls"
file_data <- read.table(file_tsv, sep = "\t", header = TRUE, check.names = FALSE)

# 一列目から "GO:xxxxxxx" 形式の文字列を抽出
go_ids_list <- grep("GO:\\d+", names(file_data), value = TRUE)


# 例えば以下のようなリストで関数を呼び出す
# go_ids_list <- c("GO:0034754", "GO:0042445", "GO:0006721", "GO:0001523", "GO:0016101", "GO:0042572", "GO:0120254", "GO:0007389", "GO:0048384", "GO:0003002", "GO:0009952", "GO:0060322", "GO:0007420")

current_date <- format(Sys.Date(), format = "%y%m%d")
# フォルダ名を組み立て
folder_name <- paste0(current_date, "_GO_geneID")
# フォルダパスを作成
folder_path <- file.path(.work, folder_name)
if (!dir.exists(folder_path)) {
dir.create(folder_path, recursive = TRUE)
}
setwd(file.path(folder_path))

export_GO2GeneIDs_to_excel(afy[[1]], go_ids_list)
tmm <- read.csv(file.path(.work, "zebra_ngs", "tcc_all_tmm.csv"), header = T, row.names = 1, check.names = F)
export_GO2GeneIDs_to_excel(afy[[2]], go_ids_list, tmm=tmm)
setwd(.work)


current_date <- format(Sys.Date(), format = "%y%m%d")
# フォルダ名を組み立て
folder_name <- paste0(current_date, "_GO_geneID_combined")
# フォルダパスを作成
folder_path <- file.path(.work, folder_name)
if (!dir.exists(folder_path)) {
dir.create(folder_path, recursive = TRUE)
}
setwd(file.path(folder_path))

export_GO2GeneIDs_to_excel(afy[[1]], go_ids_list, seq_by_pairs = FALSE)
tmm <- read.csv(file.path(.work, "zebra_ngs", "tcc_all_tmm.csv"), header = T, row.names = 1, check.names = F)
export_GO2GeneIDs_to_excel(afy[[2]], go_ids_list, tmm=tmm, seq_by_pairs = FALSE)
setwd(.work)




getGeneIDs2GO <- function (afy, gene_symbols, go_ids_list, ont = c("BP", "MF", "CC")) {	
	ann <- if (grepl("^GPL", Biobase::annotation(afy))) geoquery.convert(Biobase::annotation(afy)) else Biobase::annotation(afy)
	org <- if (grepl("^GPL", ann)) microarray.organism(afy) else ann
	org <- switch(org, "Mus musculus" = "org.Mm.eg", "Homo sapiens" = "org.Hs.eg", "Rattus norvegicus" = "org.Rn.eg", "Caenorhabditis elegans" = "org.Ce.eg", "Macaca mulatta" = "org.Mmu.eg", org)
	message(sprintf("Use Package: %s.db", org))
	load.packages(sprintf("%s.db", org))
	library(AnnotationDbi)
	go_list <- lapply(gene_symbols, function(symbol){
		genes <- select(sprintf("%s.db", org), keys = symbol, columns = c("GO"), keytype = "SYMBOL")
		return(genes)
	})
	return(go_list)
}





getGeneSymbolToGO <- function(fData, gene_symbols, org) {
    # 必要なパッケージをロード
    db <- get(paste(org, "db", sep = "."), envir = asNamespace("AnnotationDbi"))
    
    # org.Hs.egGO データをリストとして取得
    GO <- as.list(get(paste(org, "GO", sep =""), envir = db))

    # fData から Gene.Symbol を取得し、指定されたパターンに一致する遺伝子IDを見つける
    matching_genes <- fData[grepl(paste(gene_symbols, collapse="|"), fData$Gene.Symbol, ignore.case = TRUE), ]


    # Gene.ID から GO 項目を検索し、結果を Gene.Symbol と紐付ける
    gene_to_go <- setNames(lapply(matching_genes$Gene.ID, function(gene_id) {
        if (!is.na(gene_id) && gene_id %in% names(GO)) {
            return(names(GO[[gene_id]]))
        } else {
            return(NA)
        }
    }), matching_genes$Gene.Symbol)
    
    return(gene_to_go)
}

saveGoGeneToCSV <- function(go_gene, filename) {
    # 各遺伝子のGO項目の最大数を見つける
    max_length <- max(sapply(go_gene, length))

    # リストの各要素を同じ長さにする
    go_gene_padded <- lapply(go_gene, function(x) {
        c(x, rep(NA, max_length - length(x)))
    })

    # データフレームに変換
    go_gene_df <- do.call(rbind, go_gene_padded)

    # 行名を設定
    rownames(go_gene_df) <- names(go_gene)

    # CSVファイルに保存
    write.csv(go_gene_df, filename, quote = FALSE, na = "", row.names = TRUE)
}


genes_human <- c("HOXA1", "HOXA2", "HOXA3", "HOXA4", "HOXA5", "HOXA6", "HOXA7", "HOXA11", "HOXA13", "HOXB1", "HOXB2", "HOXB3", "HOXB4", "HOXB5", "HOXB6", "HOXB7", "HOXB8", "HOXB9", "HOXB13", "HOXC8", "HOXC9", "HOXC10", "HOXC11", "HOXC12", "HOXC13", "HOXD4", "HOXD8", "HOXD9", "HOXD10", "HOXD11", "HOXD12", "HOXD13"
)
genes_zebra <- c("hoxa1a", "hoxa2b", "hoxa3a", "hoxa4a", "hoxa5a" ,"hoxa9a", "hoxa9b", "hoxa10b", "hoxa11a", "hoxa11b", "hoxa13a", "hoxa13b", "hoxb1a", "hoxb1b", "hoxb2a", "hoxb3a", "hoxb4a", "hoxb5a", "hoxb5b", "hoxb6a", "hoxb6b", "hoxb7a", "hoxb8a", "hoxb8b", "hoxb9a", "hoxb10a", "hoxb13a", "hoxc1a", "hoxc3a", "hoxc4a", "hoxc5a", "hoxc6a", "hoxc6b", "hoxc8a", "hoxc9a", "hoxc10a", "hoxc11a", "hoxc11b", "hoxc12a", "hoxc12b", "hoxc13a", "hoxc13b", "hoxd3a", "hoxd4a", "hoxd9a", "hoxd10a", "hoxd11a", "hoxd12a", "hoxd13a"
)

go_gene <- getGeneSymbolToGO(fData=fData(afy[[1]]), gene_symbols=genes_human, org=annotation(afy[[1]]))
saveGoGeneToCSV(go_gene, paste(notes(afy[[1]])$tag, "_GO_geneID.csv", sep=""))

go_gene <- getGeneSymbolToGO(fData=fData(afy[[2]]), gene_symbols=genes_zebra, org=annotation(afy[[2]]))
saveGoGeneToCSV(go_gene, paste(notes(afy[[2]])$tag, "_GO_geneID.csv", sep=""))