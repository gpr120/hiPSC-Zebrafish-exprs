# by naoi okada
# charset=utf-8
# modified by umeda
# 
# title:
#   [Package] Set Pptions and Environment Variables
# description:
#   オプションと環境変数にパスとソフトウェアのバージョン、日付を設定する。
# arguments:
#   ... :: ロードするパッケージ名のベクトル。
#   pth :: サーバのパス。
# details:
#   オプションに基点となるサーバ、ローカルリポジトリ、ソフトウェアのインストール先、セッション情報の保存先のパスを設定する。\cr
#   また、ソフトウェアのバージョンと日付も記録する。
#   環境変数にもソフトウェアのインストール先のパスを設定する。
package.options <- function (..., pth = NULL) {
	# pthが指すディレクトリが存在するとき。
	if (identical(TRUE, file.info(as.character(pth))$isdir)) {
		# デバッグ用メッセージ。
		if (identical(TRUE, options("verbose")[[1]])) cat("package.options...", fill = TRUE)
		# バージョン情報とアーキテクチャをハイフンで繋ぐ。
		ver <- paste(getRversion(), system("(sw_vers -productVersion || echo 10.x) | cut -d. -f1,2", intern = TRUE), version$arch, sep = "-")
		# コンピュータ名を取得する。。
		cnm <- tolower(system("scutil --get ComputerName || echo _", intern = TRUE))
		# パスを生成する。順に、サーバ、ローカルリポジトリ、ソフトウェアのインストール先、セッション情報の保存先のパス。
		dir <- gsub("/+(/|$)", "\\1", file.path(pth, c("", "00data", "00data", "00data"), c("", "packages", "binary", "session"), c("", "", ver, cnm)))
		# 名前を付ける。
		dir <- do.call("names<-", list(as.list(dir), c("path.server", "path.repos", "path.prefix", "path.session")))
		# kameyama (2) など同じ名前が複数ある場合に、同一の機体として扱う
		dir <- lapply(dir, function(i) gsub(" \\(.*\\)", "", i))
		# ディレクトリを作成する。
		dir <- lapply(lapply(dir, sprintf, fmt = "mkdir -p %1$s && echo %1$s"), system, intern = TRUE)
		# オプションにパスを設定する。
		dir <- replicate(2, options(dir), simplify = FALSE)[[2]]$path.prefix
		# 環境変数に設定する用のパスのリストを生成して名前を付ける。
		dir <- do.call("names<-", list(as.list(file.path(dir, c("bin", "java", "python"))), c("PATH", "CLASSPATH", "PYTHONPATH")))
		# バイナリパスを初期化した後、先頭に専用パスを追加するコマンド文字列に置き換える。
		dir <- within(dir, PATH <- sprintf(". /etc/profile; echo %s:$PATH | tr : '\n' | nl | sort -uk2 | sort | cut -f2 | paste -sd: -", PATH))
		# コマンドを実行してパスを取得する。
		dir <- within(dir, PATH <- system(PATH, intern = TRUE))
		# 環境変数にパスを設定する。
		dir <- do.call(Sys.setenv, dir)
	}
	# コンソールへの出力を抑える。
	invisible()
}
