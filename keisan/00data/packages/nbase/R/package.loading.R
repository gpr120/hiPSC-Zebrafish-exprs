# by naoi okada
# charset=utf-8
# 
# title:
#   [Package] Load Packages
# description:
#   パッケージをロードする。
# arguments:
#   ... :: ロードするパッケージ名。
#   pth :: ソフトウェアのインストール先。
# details:
#   特殊な処理が必要な場合は個別に処理する。
package.loading <- function (...) {
	# デバッグ用メッセージ。
	if (identical(TRUE, options("verbose")[[1]])) cat("package.loading...", fill = TRUE)
	# パッケージ名を取り出す。
	pkg <- intersect(c(...), utils::installed.packages()[, "Package"])
	# JGR.app上でしか動かないパッケージとpdf2を除いてロードする。
	lapply(setdiff(pkg, c("JGR", "iplots", "pdf2")), library, character.only = TRUE)
	# 個別に必要な処理を行う。
	lapply(pkg, switch, 
		# jarファイルをパスに追加する。
		rJava = rJava::.jinit(dir(Sys.getenv("CLASSPATH"), pattern = "\\.jar$", full.names = TRUE, recursive = TRUE)),
		# pdf関数が干渉するのでマスクしないように最後に持ってくる。
		pdf2 = library(pdf2, pos = max(which(sapply(search(), exists, x = "pdf", inherits = FALSE))) + 1),
		# デフォルトでは何もしない。
		NULL)
	# コンソールへの出力を抑える。
	invisible()
}
