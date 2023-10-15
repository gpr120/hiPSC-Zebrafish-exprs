# GeneIDとSymbolをつけなおす．
# 2023/10/14 HT2.0RA100+BPA20 vs BPA20 の追加

# Step0: 初期設定

# CRANのパッケージを読み込むミラーサイトの設定、プロキシの設定、自作関数シリーズやその他必要なパッケージのロードなど、Rの解析を行う準備を行う。

library(gplots)

server <- "/home/rstudio/keisan"
setwd(server)
source(file.path(server, "00data/packages/data_forR4.2.R"))



## sever/00data/ExpressionSet/に保存してあるexpsetファイルを読み込む。
eset <- c("Ritsumei_HT")

# 続いて以下をコピペして実行。
afy <- list() # ここから
for(i in eset){
  read <- list.files(file.path(server, "result", "ExpressionSet"))
  read <- read[grep(paste("^", i, ".*.expset", sep = ""), read)]
  for(j in read){
    message(sprintf("read %s ...", j))
    base::load(file.path(server, "result", "ExpressionSet", j))
    eval(parse(text = paste("afy[['", gsub(".expset", "", j), "']] <- afy.", gsub(".expset", "", j), sep = "")))
    eval(parse(text = paste("rm(afy.", gsub(".expset", "", j), ")", sep = "")))
  }} # ここまで
# これでexpsetファイルからExpressionSetを読み込める。






#############################################
########## fDataを書き換える ################
#############################################

read <- afy[[1]]

pkg <- microarray.organism(read) # 機能しない
pkg <- "Hs"
# eval(parse(text = paste("load.packages('", pkg, ".eg.db')", sep = "")))

# 以下、probeを用いてfDataを作り直す。

load.packages("hugene20sttranscriptcluster.db")
## Bimap interface:
x <- hugene20sttranscriptclusterENTREZID
# Get the probe identifiers that are mapped to an ENTREZ Gene ID
mapped_probes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_probes])

entr <- fData(read)$Probe
geneid <- lapply(entr, function(i) unlist(xx[i]))


message("construct fData: Gene.Name...")
x <- hugene20sttranscriptclusterGENENAME
# Get the gene names that are mapped to an entrez gene identifier
mapped_genes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_genes])
genename <- lapply(entr, function(i) unlist(xx[i]))


message("construct fData: Gene.Symbol...")
x <- hugene20sttranscriptclusterSYMBOL
# Get the gene symbol that are mapped to an entrez gene identifiers
mapped_genes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_genes])
symb <- lapply(entr, function(i) unlist(xx[i]))


message("construct fData: Gene.Ontology.ID...")
x <- hugene20sttranscriptclusterGO
# Get the entrez gene identifiers that are mapped to a GO ID
mapped_genes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_genes])
# org.Mm.egGOが「Gene.ID -> その遺伝子が属するGO一覧」の橋渡し以下略
goid <- lapply(xx, lapply, function(i) if(i$Ontology == "BP") i$GOID)
# 複数あるが、必要なのはBiological Proccessの部分。
goid <- lapply(goid, unlist)
# Gene.IDが複数あったりするためにリスト化されているのでcharacterに変更。
goid <- lapply(entr, function(i) unique(unlist(goid[i])))
# Gene.IDが複数ある場合に重複が生じるのでuniqueにしておく。
message("construct fData: Gene.Ontology.Term...")
term <- DBI::dbGetQuery(GO_dbconn(), "SELECT go_id, term\nFROM go_term")
# DBIからGOIDに対応するGOtermを入手。おまじない。
term <- setNames(term[, "term"], term[, "go_id"])
# GOIDとtermの対応表を使いやすい形に。
term <- lapply(goid, function(i) term[i])
# ProbeのGOIDに対応するGOtermをリストアップ。


temp <- data.frame(
  Probe = fData(read)[, "Probe"], # ProbeのID
  Gene.ID = unlist(lapply(geneid, function(i) paste(i, collapse = "///"))), # 遺伝子のENTREZID
  Gene.Name = unlist(lapply(genename, function(i) paste(i, collapse = "///"))), # 遺伝子名
  Gene.Symbol = unlist(lapply(symb, function(i) paste(i, collapse = "///"))), # Symbol名
  Gene.Ontology.ID = unlist(lapply(goid, function(i) paste(i, collapse = "///"))), # GOID
  Gene.Ontology.Term = unlist(lapply(term, function(i) paste(i, collapse = "///")))) # GOTerm
# これがfDataの中身。matrixの形にしたいのでリストをCharacterに変換する。複数あるものは後でわかるように///でつないでおく。
rownames(temp) <- temp[, "Probe"]
# 行名がないのでProbe名を行の名前にする。
fData(read) <- temp
# fDataに代入。これでfData作成完了。
annotation(read) <- "org.Hs.eg"


############################################################################################################
############################################################################################################

if (0) { # pair の追加，23/10/14
  read <- afy[[1]]
}

# 以下 pair の追加で実行，23/10/14
# read <- microarray.pair(read, ind = c(1,29,23,26,9,5,12,14,15,19,28))
read <- microarray.pair(read, ind = c(1,29,23,26,9,5,12,14,15,19, 28,27))

[1] "BPA20 vs DMSO"      "RA100 vs BPA20"    
 [3] "RA500 vs DMSO"      "RA100BPA20 vs DMSO"
 [5] "DMSO vs IPS"        "BPA20 vs IPS"      
 [7] "RA100 vs IPS"       "RA100BPA20 vs IPS" 
 [9] "RA500 vs IPS"       "IPS vs DMSO"       
[11] "RA100 vs DMSO"  
# Step5: 統計量の計算~GO解析

# ここから下、if文が終わるところまでコピペして実行。Errorが出たらその旨を記載して次へ。Warningは無視して問題ない。
# Step3でエラーが起きてスルーした場合はここでもエラーが起きるはず。その場合はここもスルーしてStep6へ。
if(1){ # ここから
  read <- microarray.statistics(read, flt = "t-test")
  # 統計量の計算。サンプル数によってt-testかWADかで使い分けるが、その辺は自動で行うのですべてt-testとしておけばよい。
  read <- microarray.subset(read, switch(Biobase::notes(read)$fltr, `t-test` = 0.05, `WAD` = 0.5))
  # Testの結果が有意だった遺伝子を抽出する。有意基準はt-testの場合は0.05,WADの場合は0.5とする。
  # 以下では、発現量が小さい遺伝子を有意とみなさない処理をする。
  exp <- Biobase::exprs(read)
  exp <- if (max(exp, na.rm = TRUE) > 30) exp else 2^exp
  # thretholdを超える発現量を持つ遺伝子のみ有意とみなす
  threthold <- 20
  exp <- unlist(apply(exp, 1, max)) > threthold
  exp[is.na(exp)] <- FALSE
  notes(read)$subset <- lapply(notes(read)$subset, function(n) n[n%in%featureNames(read)[exp]])
  # ここまでが「発現量が小さい遺伝子を省く」処理。
  read <- microarray.gotest(read, pvl = Inf, ont = "BP")
  # GO解析を行う。ちょっと時間がかかる。
  read <- microarray.gotable(read)
  # GO解析の結果をTableの形にまとめておく。解析に使う。
} # ここまで
# ここでエラーが起きた場合、大抵は次の3通り: 
# 1. Step3でエラーが起きている。microarray.gotestでfDataを参照するのでこれは仕方が無い。あきらめてStep6へ。
# 2. GO解析に使うパッケージがインストールされていない(~_dbconnがどうのこうのと言われる)
#     -> この場合は「~_dbconn」の部分を「~.db」としたパッケージをインストールすればよい(例: mouse4302_dbconnの場合: install.packages("mouse4302.db") をRで実行し、Step5を再度最初から実行)
# 3. 統計量がそもそも解析できない。
#     -> この場合はStep2を再度確認する。まともなデータでない場合がほとんど。
#        まともなデータだった場合は原因を調べる必要がある。Rの知識がいるので無理な場合は備考にコメントを残してStep6へ。


# Step6: 保存

# 準備が完了したので保存。ここから先もif文が終わるところまでコピペして実行。
if(1){ # ここから
  afy <- c(read)
  afy <- unlist(afy)
  afy <- setNames(afy, rapply(afy, function(i) notes(i)$tag))
  # 汎用性のためリスト化。
  for(i in names(afy)){
    if(any(grepl(paste("^", notes(afy[[i]])$tag, "(_GPL.*)*.expset", sep = ""), list.files(file.path(server,"result", "ExpressionSet"))))){
      message("Warning: This ExpressionSet is already exist.")
      if(!any(grepl(paste("^", .date, "_", notes(afy[[i]])$tag, "(_GPL.*)*.expset", sep = ""), list.files(file.path(server, "result", "ExpressionSet", "Archives"))))){
        message(sprintf("Save old file: %s_%s.expset in archives", .date, notes(afy[[i]])$tag))
        afyold <- paste(notes(afy[[i]])$tag, ".expset", sep = "")
        base::load(file.path(server, "result", "ExpressionSet", afyold))
        eval(parse(text = paste("save(afy.", notes(afy[[i]])$tag, ", file = '", file.path(server, "result", "ExpressionSet", "Archives", paste(.date, notes(afy[[i]])$tag, sep = "_")), ".expset')", sep= "")))
      } # Archivesフォルダに前回のバックアップが作成されていない場合は作成する(その日の日付で前回作成したものを保存する。同じ日に作成したバックアップは上書きしない)
    } # 今作ったGSEの番号のExpressionSetがまだ作成されていない場合，ここまでスキップ
    message(sprintf("Save: %s.expset", notes(afy[[i]])$tag))
    eval(parse(text = paste("afy.", notes(afy[[i]])$tag, "<-", "afy[[i]]", sep= "")))
    eval(parse(text = paste("save(afy.", notes(afy[[i]])$tag, ", file = '", file.path(server, "result", "ExpressionSet", notes(afy[[i]])$tag), ".expset')", sep= "")))
  }
  # 作ったExpressionSetを保存する。上書きするが、バックアップはArchiveフォルダに作成する。
} # ここまで
# 再現性を確保するため、上書きする場合は前回解析時のものをArchiveフォルダに保存する。
# 再現したい場合はArchiveフォルダの中から対応するファイルを探して名前を変えてExpressionSetフォルダに入れればOK。

