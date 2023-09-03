# by naoi okada
# charset=utf-8
# 
# title:
#   [Package] Update Packages on Local Repository
# description:
#   ローカルリポジトリ上にあるパッケージを更新する。
# arguments:
#   ... :: ロードするパッケージ名。
#   pth :: ローカルリポジトリのパス。
# details:
#   ローカルリポジトリには各パッケージに対して１つのディレクトリを用意しておく。\cr
#   パッケージ化されたファイルの更新日時より後に更新されたファイルがあれば、
#   そのパッケージのバージョンを実行した日付に変更してからパッケージ化する。\cr
#   パッケージに含まれる関数を使用したり，明示的にロードしたパッケージは一度アンロードしないと更新が反映されない。
#   そのため，関数の最後でパッケージを一度アンロードした後ローカルリポジトリ上にあるパッケージを全てロードしている。
package.updates <- function (..., pth = NULL) {
	# pthが指すディレクトリが存在し、グローバル環境でload.packagesが実行されたとき。
	if (identical(TRUE, file.info(as.character(pth))$isdir) && sys.parent(n = 2) == 0) {
		# デバッグ用メッセージ。
		if (identical(TRUE, options("verbose")[[1]])) cat("package.updates...", fill = TRUE)
		# ローカルリポジトリ上にあるパッケージのDESCRIPTIONファイルのパスを取得する。
		pkg <- dir(pth, pattern = "DESCRIPTION", recursive = TRUE, full.names = TRUE)
		# DESCRIPTIONファイルをPACKAGESとして複製して、複製に成功したものだけ取り出す。
		pkg <- subset(dirname(pkg), file.copy(pkg, file.path(dirname(pkg), "PACKAGES"), overwrite = TRUE))
		# 利用可能なパッケージだけ取り出す。
		pkg <- data.frame(utils::available.packages(sprintf("file://%s", pkg), fields = "Packaged"), Path = pkg)
		# PACKAGESファイルを削除して、成功したものだけ取り出す。
		pkg <- subset(pkg, file.remove(file.path(Path, "PACKAGES")))
		# パッケージをインストールした日時としてpackage.rdsの更新日時を使う。
		pkg <- within(pkg, Install <- file.info(file.path(.libPaths()[1], Package, "Meta", "package.rds"))$mtime)
		# 各パッケージ内のスクリプトで一番新しい更新日時を取り出す。リストを解除するときにunlistを使うと秒数に変換されてしまうためdo.callを使う。
		pkg <- within(pkg, Modify <- do.call(c, lapply(Path, function(x) max(file.info(dir(file.path(x, "R"), full.names = TRUE))$mtime))))
		# 作成すべきアーカイブのパスを追加する。
		pkg <- within(pkg, Archive <- sprintf("%s/00archive/%s_%s.tar.gz", dirname(Path), Package, strftime(options("date")[[1]], "%y.%m.%d")))
		# アーカイブが存在しない，または，既にあるアーカイブより新しいファイルがある，かつ，今日の日付のアーカイブがないパッケージを取り出す。
		new <- subset(pkg, (is.na(Packaged) | Packaged < Modify) & !file.exists(Archive))
		# DESCRIPTIONファイルを読み込む。
		new <- sapply(file.path(new$Path, "DESCRIPTION"), read.dcf, all = TRUE, simplify = FALSE)
		# バージョン、日付、アーカイブ化した日付を書き換える。
		new <- lapply(new, replace, c("Version", "Date", "Packaged"), lapply(c("%y.%m.%d", "%F", "%F %H:%M:%S %Z"), strftime, x = options("date")[[1]]))
		# DESCRIPTIONファイルに書き込む。
		new <- dirname(as.character(names(mapply(write.dcf, file = names(new), new))))
		# アーカイブを作成するコマンドを実行する。
		new <- lapply(new, function(x) system(sprintf("cd %s/00archive && %s/R CMD BUILD %s", pth, R.home("bin"), x)))
		# インストールされていない，または，インストールされているものより新しいファイルがあるパッケージを選び出す。
		new <- subset(pkg, is.na(Install) | Install < Modify)
		# 古いヘルプファイルを削除する。
		man <- file.remove(dir(file.path(new$Path, "man"), pattern = "\\.Rd$", full.names = TRUE))
		# ソースファイルのパスを取得する。
		man <- dir(file.path(new$Path, "R"), pattern = "\\.R$", full.names = TRUE)
		# ソースファイルを読み込む。
		man <- sapply(man, function(x) try(parse(x)), simplify = FALSE)
		# 正しく読み込めたものだけ残す。
		man <- subset(man, as.logical(sapply(man, is.expression)))
		# 関数定義の文字列を取得する。
		man <- lapply(man, grep, pattern = "^((\\w|\\.)*)[[:blank:]]*<-[[:blank:]]*function", value = TRUE)
		# 関数定義を書き換える。
		man <- lapply(man, gsub, pattern = "^((\\w|\\.)*)[[:blank:]]*<-[[:blank:]]*function[[:blank:]]*(\\(.*?\\))[[:blank:]]*\\{.*", replacement = "# \\1 \\3")
		# 関数定義の前にusageブロックを追加する。
		man <- lapply(man, append, "# usage:", 0)
		# 元のソーステキストを追加する。
		man <- mapply(c, man, rep("#;", length(man)), lapply(names(man), readLines))
		# ファイルの先頭にあるコメント部分と関数定義の部分を取り出す。
		man <- lapply(man, function(x) utils::head(x, grep("^#", x, invert = TRUE)[1] - 1))
		# aliasブロックを追加する。
		man <- lapply(man, function(x) c(grep("^#;", x, value = TRUE, invert = TRUE), gsub("^# (.*?) .*$", "# alias:\n# \\1", x[2:(grep("^#;", x) - 1)])))
		# nameブロックにファイル名を追加する。
		man <- mapply(append, man, gsub("^.*/(.*)\\.R$", "# name:\n# \\1", names(man)), SIMPLIFY = FALSE)
		# 空行を削除する。
		man <- lapply(man, grep, pattern = "#[[:blank:]]*$", value = TRUE, invert = TRUE)
		# コメントのまま残す部分をヘルプファイル用に変換する。
		man <- lapply(man, gsub, pattern = "^# (by .*?|charset=utf-8)$", replacement = "% \\1")
		# 改行コードで繋げる。
		man <- lapply(man, paste, collapse = "\n")
		# 各ブロックを変換する。"# title:" => "\\title{"。先頭にバックスラッシュが２つあるのは次の置換のため。
		man <- lapply(man, gsub, pattern = "#[[:blank:]]*([^#:]*?):[[:blank:]]*\n", replacement = "\\\\\\\\\\1{\n")
		# ブロックの終わりに}を補完する。"\title"から次の"\"までを置換する。置換範囲が被ると置換されないので上でバックスラッシュを２つにしてある。
		man <- lapply(man, gsub, pattern = "[\\](.*?)\\{(.*?)(\n[\\]|$)", replacement = "\\\\\\1{\\2\n}\n")
		# ２つ続いているバックスラッシュを１つにする。
		man <- lapply(man, gsub, pattern = "[\\]{2}", replacement = "\\\\")
		# argumentsブロックの各項目を変換する。
		man <- lapply(man, gsub, pattern = "#[[:blank:]]*([[:alnum:].]*?) :: (.*?)\n", replacement = "\\\\item{\\1}{\\2}\n")
		# コメントマークを削除する。
		man <- lapply(man, gsub, pattern = "#[[:blank:]]*", replacement = "")
		# titleブロックの前にencodingブロックを追加する。
		man <- lapply(man, gsub, pattern = "(\n[\\]title)", replacement = "\n\\\\encoding{\nutf-8\n}\\1")
		# 改行コードで分割する。
		man <- sapply(man, strsplit, "\n")
		# ブロック毎に分割する。
		man <- lapply(man, function(x) split(x, cut(seq(x), c(1, grep("^\\}$", x)), make.unique(grep("^\\\\.*\\{$", x, value = TRUE)), include.lowest = TRUE)))
		# titleブロックとdescriptionブロックのあるものだけ残す。
		man <- subset(man, as.logical(sapply(man, function(x) all(is.element(c("\\title{", "\\description{"), names(x))))))
		# 並び替える。
		man <- lapply(man, function(x) x[unique(unlist(sapply(c("encoding", "name", "alias", "title", "description", "usage", ""), grep, names(x))))])
		# リストを解除する。
		man <- lapply(man, unlist, use.names = FALSE)
		# ...を\dotsに置換する。
		man <- lapply(man, gsub, pattern = "[.]{3}", replacement = "\\\\dots")
		# %を\%に置換する。
		man <- lapply(man, gsub, pattern = "([^%]+)\\%", replacement = "\\1\\\\%")
		# コメントを前に持ってくる。
		man <- lapply(man, function(x) x[c(grep("^%", x), grep("^%", x, invert = TRUE))])
		# 繋げる
		man <- lapply(man, paste, collapse = "\n")
		# encoding, name, aliasブロックの改行コードを消す。
		man <- lapply(man, gsub, pattern = "(\\\\(encoding|name|alias))\\{\n(.*?)\n\\}", replacement = "\\1{\\3}")
		# ファイルに書き出す。
		man <- mapply(writeLines, man, gsub("/R/([^/]*?)\\.R$", "/man/\\1.Rd", names(man)))
		# パッケージをインストールする。repos = NULL, type = ""を指定することでディレクトリからインストールできる。
		res <- lapply(new$Path, utils::install.packages, lib = .libPaths()[1], repos = NULL, type = "")
		# ローカルリポジトリ上のパッケージでロードされているものをスコープから取り除く。
		res <- lapply(intersect(new$Package, loadedNamespaces()), unloadNamespace)
		# ローカルリポジトリ上にあるパッケージをロードする。
		res <- lapply(intersect(setdiff(pkg$Package, .packages()), utils::installed.packages()[, "Package"]), library, character.only = TRUE)
	}
	# コンソールへの出力を抑える。
	invisible()
}
