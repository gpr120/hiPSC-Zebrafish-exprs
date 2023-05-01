# by naoi okada
# by modified ueno
# charset=utf-8

# ロケールエラーを防ぐ。
# system("defaults write org.R-project.R 'force.LANG' 'en_US.UTF-8'")
# 履歴ファイルのパスを設定する。
# system("defaults write org.R-project.R 'History file path used for R type history files' '\\~/.Rhistory'")
# ~/Library/R/*.*/libraryをライブラリのパスに追加する。
# system(sprintf("mkdir -p %s", Sys.getenv("R_LIBS_USER"))); .libPaths(Sys.getenv("R_LIBS_USER"))

# RENVの起動
setwd("keisan")
source(".Rprofile")

# オプションの設定。
options(stringsAsFactors = FALSE)
#options(repos = c(CRAN = "http://cran.md.tsukuba.ac.jp/"))
#options(repos = c(CRAN = "http://cran.ism.ac.jp/"))
options(repos = c(CRAN = "https://healthstat.snu.ac.kr/CRAN"))
utils::setRepositories(ind = c(1, 3:6))
# プロキシの設定。
#Sys.setenv(http_proxy = "http://proxy.kuins.net:8080")
#Sys.setenv(ftp_proxy = "http://proxy.kuins.net:8080")
Sys.setenv(ALL_PROXY = "") # ALL_PROXYは効かなくなっているらしい
Sys.setenv(http_proxy = "")
Sys.setenv(ftp_proxy = "")

# 作業ディレクトリ。
# setwd(if(file.exists("~/R_keisan")) "~/R_keisan" else "~/Desktop")
.work <- getwd()
.date <- strftime(Sys.time() - 6 * 3600, "%y%m%d")

# ファイルを保管している場所の基点となるパス。
# server <- "/home/rstudio"
# # setwd(server)
# インストールしたパッケージの保管場所の設定。 renvに任せる
# .libPaths(file.path(server, "00data", "library"))
if (!requireNamespace("renv", quietly = TRUE))
  (install.packages("renv"))
# # インストールしたパッケージの保管場所の設定。この後はRenv
# library(renv)
# # パッケージのインストール。
# if (!requireNamespace("nbase", quietly = TRUE))
#   (utils::install.packages(file.path(server, "00data", "packages", "nbase"), lib = .libPaths()[1], repos = NULL, type = ""))
# library("nbase")
# if (!requireNamespace("narray", quietly = TRUE))
#   (utils::install.packages(file.path(server, "00data", "packages", "narray"), lib = .libPaths()[1], repos = NULL, type = ""))
# library("narray")
# if (!requireNamespace("ntool", quietly = TRUE))
#   (utils::install.packages(file.path(server, "00data", "packages", "ntool"), lib = .libPaths()[1], repos = NULL, type = ""))
# library("ntool")
# if (!requireNamespace("nmisc", quietly = TRUE))
#   (utils::install.packages(file.path(server, "00data", "packages", "nmisc"), lib = .libPaths()[1], repos = NULL, type = ""))
# library("nmisc")


# renv使ってるからいらない気がするがとりあえず．
check.installs.packages <- function(packages)
  for(package_i in packages){
  {if (!requireNamespace(package_i, quietly = TRUE))
    install.packages(package_i)
  }}
check.installs.Biocpackages <- function(packages)
  for(package_i in packages){
    {if (!requireNamespace(package_i, quietly = TRUE))
      BiocManager::install(package_i)
    }}
packages <- c("BiocManager", "gplots", "gdata", "dataframes2xls", "amap", "gtools")
check.installs.packages(packages)
# ‘, gmodels, , ’

biopackages <- c("BiocManager", "affy", "oligo", "GOstats", "GEOquery", "hugene20sttranscriptcluster.db", "affycoretools", "GO.db", "TCC")
check.installs.Biocpackages(biopackages)

library("affy")
library("gtools")
library("TCC")

# load.packages("nbase","narray", "affy", "gplots", "gdata", "gtools", "grid", "GOstats", "hugene20sttranscriptcluster.db", "affycoretools", "GEOquery", "amap", "GO.db")# "GEOmetadb")

