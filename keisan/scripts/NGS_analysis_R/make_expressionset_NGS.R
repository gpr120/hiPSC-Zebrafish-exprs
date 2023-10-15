###################################
##### ExpressionSetの作成方法 #####
###################################


#####################################################
################### 関連付け ##########################
#####################################################
# Step0: 初期設定

# CRANのパッケージを読み込むミラーサイトの設定、プロキシの設定、自作関数シリーズやその他必要なパッケージのロードなど、Rの解析を行う準備を行う。

library(gplots)

server <- "/home/rstudio/keisan"
setwd(server)
source(file.path(server, "00data/packages/data_forR4.2.R"))


##### 立命の解析
setwd(.work <- file.path(server, "result", "zebra_ngs"))


tpm <-
  read.csv(
    "gene_tpm.csv",
    header = T,
    row.names = 1,
    check.names = F
  )
tmm <-
  read.csv(
    "gene_tmm.csv",
    header = T,
    row.names = 1,
    check.names = F
  )
count <-
  read.csv(
    "gene_count.csv",
    header = T,
    row.names = 1,
    check.names = F
  )
fData <- read.csv(
  "fData.csv",
  header = T,
  row.names = 1,
  check.names = F,
  colClasses = "character"
)
pData <- read.csv(
  "pData.csv",
  header = T,
  row.names = 1,
  check.names = F
)


# mean caliculate
# mn <- lapply(unique(pData$condition), function(i) apply(tpm[,grep(paste(i,".",sep=""), colnames(tpm)), drop=F], 1, mean))
# 条件ごとにデータを分割

# 条件ごとに平均を計算する関数
compute_mean <- function(cond) {
  cols <- grep(paste0("^", cond, ".*$"), colnames(tpm))
  sub_data <- tpm[, cols, drop = FALSE]
  apply(sub_data, 1, mean)
}

# 各条件に対して計算を適用
mn <- sapply(unique(pData$condition), compute_mean)

# mn <- do.call(mn, what = cbind)
# mn <- apply(mn, 1, mean)
# exp <- cbind(exp, "Mean" = mn)

# PhenoData
phn <- pData
phn <- cbind(rownames(phn), phn)
colnames(phn) <- c("sample", "condition", "species")

phn <- as.data.frame(phn)

phn <- new("AnnotatedDataFrame", data = phn)
# FeatureData
library("org.Dr.eg.db")

pkg <- "Dr"
# パッケージの読み込み。

# 以下、GeneIDを抽出する作業。
# Ensembltrans ID からGENEIDを探す

# Gene.ID -> Gene.Ontology.ID
message("fData: Gene.ID -> Gene.Ontology.ID...")
x <-
  eval(parse(text = sprintf("org.%s.eg.db::org.%s.egGO", pkg, pkg)))
# Get the entrez gene identifiers that are mapped to a GO ID
mapped_genes <- mappedkeys(x)
# Convert to a list
xx <- as.list(x[mapped_genes])
# 複数あるが、必要なのはBiological Proccessの部分。
goid <- lapply(xx, lapply, function(i)
  if (i$Ontology == "BP")
    i$GOID)
# Gene.IDが複数あったりするためにリスト化されているのでcharacterに変更。
goid <- lapply(goid, unlist)
# Gene.IDが複数ある場合に重複が生じるのでuniqueにしておく。
# goid <- lapply(entr, function(i) unique(unlist(goid[unlist(strsplit(i, "///"))])))

goid <-
  lapply(as.character(fData$Gene.ID), function(i)
    unique(unlist(goid[unlist(strsplit(i, "///"))])))

tmp <- sum(sapply(goid, function(x)
  ! is.null(x)))



# DBIからGOIDに対応するGOtermを入手。おまじない。
# Gene.Ontology.ID -> Gene.Ontologuy.Term
library(GO.db)
message("fData: Gene.Ontology.ID -> Gene.Ontology.Term...")
term <-
  DBI::dbGetQuery(GO_dbconn(), "SELECT go_id, term\nFROM go_term")
# GOIDとtermの対応表を使いやすい形に。
term <- setNames(term[, "term"], term[, "go_id"])
# ProbeのGOIDに対応するGOtermをリストアップ。
term <- lapply(goid, function(i)
  term[i])

# 最後にこれらの情報からfDataのtableを作成する
fea <- data.frame(
  Probe = rownames(fData),
  # ProbeのID
  Gene.ID = fData$Gene.ID,
  # 遺伝子のENTREZID
  Gene.Name = fData$Gene.Name,
  # 遺伝子名
  Gene.Symbol = fData$Gene.Symbol,
  # Symbol名
  Gene.Ontology.ID = unlist(lapply(goid, function(i)
    paste(i, collapse = "///"))),
  # GOID
  Gene.Ontology.Term = unlist(lapply(term, function(i)
    paste(i, collapse = "///"))) # GOTerm
)
rownames(fea) <- fea[, "Probe"]
read <- new(
  "ExpressionSet",
  assayData = Biobase::assayDataNew(exprs = tpm),
  phenoData = phn,
  featureData = new("AnnotatedDataFrame", data = fea),
  annotation = paste("org.", pkg, ".eg", sep = "")
)

notes(read)$tag <- paste("NGS", "Ritsumei", "zebra", sep = "_")

read <- microarray.pair(read, ind = c(1, 13, 16, 18, 19))


# Step5: 統計量の計算~GO解析

microarray.statistics <-
  function (afy, flt = c("t-test", "WAD", "None", "tcc")) {
    # ベクトル化。
    if (class(afy) == "list")
      return(mapply(microarray.statistics, afy, flt))
    # パッケージの読みこみ。
    load.packages("genefilter")
    # メッセージ。
    message("Calculating statistics ...")
    # 引数のチェック。
    flt <- match.arg(flt)
    grp <- Biobase::notes(afy)$group
    par <- Biobase::notes(afy)$pair
    # サンプル数が３未満の群がある場合、ｔ検定は行えない。
    flt <- if (any(table(grp) < 3) && flt == "t-test")
      "WAD"
    else
      flt
    # 発現量。２を底とする対数に変換されている場合は元に戻す。
    exp <- Biobase::exprs(afy)
    exp <-
      if (max(exp, na.rm = TRUE) > 30 || flt == "tcc")
        exp
    else
      2 ^ exp
    # 検定用の関数を選ぶ。
    fun <-
      switch(flt,
             "t-test" = microarray.rowFtests,
             "WAD" = microarray.rowWtests,
             "None" = invisible)
    # 計算を行う。
    dat <- list(fltr = flt)
    dat <-
      within(dat, mean <-
               sapply(unique(grp), function(i)
                 rowMeans(exp[, is.element(grp, i), drop = FALSE], na.rm = TRUE)))
    dat <-
      within(dat, stdd <-
               sapply(unique(grp), function(i)
                 genefilter::rowSds(exp[, is.element(grp, i), drop = FALSE], na.rm = TRUE)))
    dat <-
      within(dat, maxi <-
               stats::setNames(apply(mean, 1, max), rownames(mean)))
    dat <-
      within(dat, diff <-
               sapply(par, function(i)
                 mean[, i[1]] / mean[, i[2]]))
    if (flt != "tcc") {
      dat <-
        within(dat, test <-
                 sapply(par, function(i)
                   fun(exp, as.factor(match(
                     grp, i
                   )))))
    } else{
      # 関数定義
      process_file <- function(item) {
        # ファイル名生成
        file_name <-
          paste0("tcc_", item[1], " vs ", item[2], ".csv")
        print(file_name)
        # ファイル読み込み
        data <-
          read.csv(file_name,
                   row.names = 1,
                   stringsAsFactors = FALSE)
        # 行名をexpに合わせて並べ替え
        # TODO:: めっちゃ時間かかる。改善したい
        data_ordered <-
          merge(exp,
                data,
                by = "row.names",
                all.x = TRUE,
                sort = TRUE)
        # "q.value"列を取り出す
        return(setNames(data_ordered$q.value, data_ordered$Row.names))
      }
      dat <-
        within(dat, test <-
                 sapply(par, function(i)
                   (process_file(i)))[row.names(exp),])
    }
    # データをくっつけて返す。
    Biobase::notes(afy) <-
      replace(Biobase::notes(afy), names(dat), dat)
    afy
  }

microarray.subset <- function (afy, pvl)
{
  if (class(afy) == "list")
    return(mapply(microarray.subset, afy, pvl))
  load.packages("Biobase")
  pvl <-
    switch(
      Biobase::notes(afy)$fltr,
      `tcc` = c(pvl[1], 1),
      `t-test` = c(pvl[1], 1),
      WAD = if (length(pvl) == 1)
        sort(c(pvl, -pvl))
      else
        pvl[1:2]
    )
  msg <-
    switch(
      Biobase::notes(afy)$fltr,
      `t-test` = sprintf("p-value %s", pvl[1]),
      `tcc` = sprintf("q-value %s", pvl[1]),
      WAD = sprintf("lower cutoff %s and upper cutoff %s",
                    pvl[1], pvl[2])
    )
  message(sprintf(
    "Subsetting probes with %s for %s ...",
    msg,
    Biobase::notes(afy)$tag
  ))
  sub <-
    Biobase::notes(afy)$test < pvl[1] | Biobase::notes(afy)$test >
    pvl[2]
  sub <-
    sapply(names(Biobase::notes(afy)$pair), function(i)
      Biobase::featureNames(afy)[sub[, i]], simplify = FALSE)
  sub <- lapply(sub, function(x) x[!is.na(x)])
  Biobase::notes(afy) <- replace(Biobase::notes(afy), "subset", list(sub))
  afy
}


microarray.gotest <- function (afy, pvl = Inf, ont = c("BP", "MF", "CC")) 
{
  if (class(afy) == "list") 
    return(mapply(microarray.gotest, afy, pvl, ont))
  load.packages("GO.db", "GOstats", "annotate")
  message(sprintf("Ontology Test for %s ...", Biobase::notes(afy)$tag))
  ont <- match.arg(ont)
  ids <- lapply(Biobase::notes(afy)$subset, function(i) list(id = Biobase::fData(afy)[i, "Gene.ID"], diff = Biobase::notes(afy)$diff[i, , drop = FALSE]))
  ids <- sapply(names(ids), function(i) split(ids[[i]]$id, 
    cut(ids[[i]]$diff[, i], c(0, 1, Inf), c("decrease", 
      "increase"))), simplify = FALSE)
  ids <- lapply(ids, lapply, function(x) unique(unlist(strsplit(as.character(x), 
    " ?/{2,} ?"))))
  ann <- if (grepl("^GPL", Biobase::annotation(afy))) 
    geoquery.convert(Biobase::annotation(afy))
  else Biobase::annotation(afy)
  org <- if (grepl("^GPL", ann)) 
    microarray.organism(afy)
  else ann
  print(org)
  org <- switch(org, `Mus musculus` = "org.Mm.eg", `Homo sapiens` = "org.Hs.eg", 
    `Rattus norvegicus` = "org.Rn.eg", `Caenorhabditis elegans` = "org.Ce.eg", 
    `Macaca mulatta` = "org.Mmu.eg", `Danio rerio` = "org.Dr.eg", org)
  message(sprintf("Use Package: %s.db", org))
  load.packages(sprintf("%s.db", org))
  con <- do.call(sprintf("%s_dbconn", org), list())
  uni <- unique(unlist(strsplit(Biobase::fData(afy)[, "Gene.ID"], 
    " ?/{2,} ?")))
  if (all(!grepl("^GPL", ann), !grepl("org.*eg", ann))) {
    oid <- unique(unlist(DBI::dbGetQuery(GO.db::GO_dbconn(), 
      sprintf("SELECT go_id\nFROM go_term INNER JOIN go_%s_parents USING (_id)", 
        ont))))
    pid <- DBI::dbGetQuery(con, sprintf("SELECT gene_id, go_id\nFROM genes INNER JOIN go_%s_all USING (_id)", 
      ont))
    pid <- unique(pid$gene_id[pid$go_id %in% oid])
    pid <- intersect(pid, sort(DBI::dbGetQuery(con, "SELECT gene_id\nFROM probes\nWHERE is_multiple == 0")$gene_id))
    uni <- intersect(pid, uni)
  }
  message("Get mapping GO and EntrezID ...")
  ent <- paste("'", uni, "'", sep = "", collapse = ",")
  gid <- sprintf("SELECT DISTINCT gene_id, go_id\nFROM genes INNER JOIN go_%s_all USING (_id)\nWHERE gene_id IN (%s)", 
    ont, ent)
  gid <- DBI::dbGetQuery(con, gid)
  cat <- split(gid$gene_id, gid$go_id)
  siz <- sapply(cat, length)
  num <- length(intersect(gid$gene_id, uni)) - siz
  message("Calculating ...")
  res <- sapply(names(ids), function(i) sapply(names(ids[[i]]), 
    function(j) {
      message(sprintf("  Calculate: %s %s", i, j))
      sub <- ids[[i]][[j]]
      cnt <- sapply(cat, function(x) sum(sub %in% x))
      res <- list(over = TRUE, under = FALSE)
      res <- lapply(res, function(x) phyper(cnt - x, siz, 
        num, length(intersect(gid$gene_id, sub)), lower.tail = !x))
      res <- lapply(res, function(x) data.frame(GOID = names(cat), 
        Term = AnnotationDbi::Term(names(cat)), Pvalue = x, 
        Count = cnt, Size = siz)[order(x), ])
      res <- lapply(res, function(x) x[x$Pvalue < pvl, 
        ])
      res
    }, simplify = FALSE), simplify = FALSE)
  Biobase::notes(afy) <- replace(Biobase::notes(afy), "gotest", 
    list(res))
  afy
}




# ここから下、if文が終わるところまでコピペして実行。Errorが出たらその旨を記載して次へ。Warningは無視して問題ない。
# Step3でエラーが起きてスルーした場合はここでもエラーが起きるはず。その場合はここもスルーしてStep6へ。
if (1) {
  # ここから
  read <- microarray.statistics(read, flt = "tcc")
  # 統計量の計算。サンプル数によってt-testかWADかで使い分けるが、その辺は自動で行うのですべてt-testとしておけばよい。
  read <-
    microarray.subset(read, switch(
      Biobase::notes(read)$fltr,
      `t-test` = 0.05,
      `WAD` = 0.5,
      `tcc` = 0.1
    ))
  # Testの結果が有意だった遺伝子を抽出する。有意基準はt-testの場合は0.05,WADの場合は0.5とする。
  # 以下では、発現量が小さい遺伝子を有意とみなさない処理をする。
  if (0) {
    # NGSの場合は、検定の前にこの処理をしている
    exp <- Biobase::exprs(read)
    exp <- if (max(exp, na.rm = TRUE) > 30)
      exp
    else
      2 ^ exp
    # thretholdを超える発現量を持つ遺伝子のみ有意とみなす
    threthold <- 1
    exp <- unlist(apply(exp, 1, max)) > threthold
    exp[is.na(exp)] <- FALSE
  notes(read)$subset <-
    lapply(notes(read)$subset, function(n)
      n[n %in% featureNames(read)[exp]])
  }
  # ここまでが「発現量が小さい遺伝子を省く」処理。
  read <- microarray.gotest(read, pvl = Inf, ont = "BP")
  # GO解析を行う。ちょっと時間がかかる。
  read <- microarray.gotable(read)
  # GO解析の結果をTableの形にまとめておく。解析に使う。
} # ここまで


# Step6: 保存

# 準備が完了したので保存。ここから先もif文が終わるところまでコピペして実行。
if (1) {
  # ここから
  afy <- c(read)
  afy <- unlist(afy)
  afy <- setNames(afy, rapply(afy, function(i)
    notes(i)$tag))
  # 汎用性のためリスト化。
  for (i in names(afy)) {
    if (any(grepl(
      paste("^", notes(afy[[i]])$tag, "(_GPL.*)*.expset", sep = ""),
      list.files(file.path(server, "result", "ExpressionSet"))
    ))) {
      message("Warning: This ExpressionSet is already exist.")
      if (!any(grepl(
        paste(
          "^",
          .date,
          "_",
          notes(afy[[i]])$tag,
          "(_GPL.*)*.expset",
          sep = ""
        ),
        list.files(file.path(
          server, "result", "ExpressionSet", "Archives"
        ))
      ))) {
        message(sprintf(
          "Save old file: %s_%s.expset in archives",
          .date,
          notes(afy[[i]])$tag
        ))
        afyold <- paste(notes(afy[[i]])$tag, ".expset", sep = "")
        load(file.path(server, "result", "ExpressionSet", afyold))
        eval(parse(
          text = paste(
            "save(afy.",
            notes(afy[[i]])$tag,
            ", file = '",
            file.path(
              server,
              "result",
              "ExpressionSet",
              "Archives",
              paste(.date, notes(afy[[i]])$tag, sep = "_")
            ),
            ".expset')",
            sep = ""
          )
        ))
      } # Archivesフォルダに前回のバックアップが作成されていない場合は作成する(その日の日付で前回作成したものを保存する。同じ日に作成したバックアップは上書きしない)
    } # 今作ったGSEの番号のExpressionSetがまだ作成されていない場合，ここまでスキップ
    message(sprintf("Save: %s.expset", notes(afy[[i]])$tag))
    eval(parse(text = paste(
      "afy.", notes(afy[[i]])$tag, "<-", "afy[[i]]", sep = ""
    )))
    eval(parse(
      text = paste(
        "save(afy.",
        notes(afy[[i]])$tag,
        ", file = '",
        file.path(server, "result", "ExpressionSet", notes(afy[[i]])$tag),
        ".expset')",
        sep = ""
      )
    ))
  }
  # 作ったExpressionSetを保存する。上書きするが、バックアップはArchiveフォルダに作成する。
} # ここまで

# ここまででExpressionSetの作成は完了。