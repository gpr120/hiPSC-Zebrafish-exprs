# by naoi okada
# charset=utf-8
# 
# title:
#   [Package] Session Informations
# description:
#   セッション情報を"日付.txt"に出力する。
# arguments:
#   ... :: ロードするパッケージ名。
#   pth :: セッション情報を保存するディレクトリのパス。
package.session <- function (..., pth = NULL) {
	# pthが指すディレクトリが存在するとき。
	if (identical(TRUE, file.info(as.character(pth))$isdir)) {
		# デバッグ用メッセージ。
		if (identical(TRUE, options("verbose")[[1]])) cat("package.session...", fill = TRUE)
		# 利用しているパッケージ名を取り出す。ロードされているだけのものも取り出しておく。
		pkg <- union(rev(.packages()), loadedNamespaces())
		# パッケージ名、バージョン、重要度を取り出す。
		pkg <- sapply(pkg, utils::packageDescription, fields = c("Package", "Version", "Priority"), simplify = FALSE)
		# アンダーバーで繋げる。"base_2.10.1_base"、"package_1.0_NA"。
		pkg <- sapply(pkg, paste, collapse = "_")
		# 重要度を取り除く。基幹パッケージはバージョンも取り除く。
		pkg <- gsub("_([0-9.]+_base|recommended|NA)$", "", pkg)
		# Rのバージョン情報の文字列。
		str <- R.version.string
		# 日付。"2012-01-01"。
		str <- c(str, "Date:", strftime(options("date")[[1]], "%F"))
		# コンピュータ名。"haruna"。
		str <- c(str, "Computer name:", tolower(system("scutil --get ComputerName || echo _", intern = TRUE)))
		# システム情報。"Mac OS X 10.6.8 10K549"。
		str <- c(str, "System:", system("(sw_vers || echo unknown) | cut -f2 | tr '\n' ' '", intern = TRUE))
		# プラットフォーム情報。"x86_64-apple-darwin9.8.0/x86_64 (64-bit)"。
		str <- c(str, "Platform:", sprintf("%s/%s (%d-bit)", version$platform, version$arch, 8 * .Machine$sizeof.pointer))
		# ロケール情報。"en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8"。
		str <- c(str, "Locale:", Sys.getlocale())
		# 検索パスに追加されている基幹パッケージ。"base"。
		str <- c(str, "Attached base packages:", grep("_", pkg, value = TRUE, invert = TRUE))
		# 検索パスに追加されているその他のパッケージ。"gplots_2.10.1"。
		str <- c(str, "Other attached packages:", grep("_", subset(pkg, is.element(names(pkg), .packages())), value = TRUE))
		# 検索パスに追加されていないがネームスペースがロードされているパッケージ。"tools_2.14.2"。
		str <- c(str, "Loaded via a namespace (and not attached):", grep("_", subset(pkg, !is.element(names(pkg), .packages())), value = TRUE))
		# 実行されたスクリプトのバージョン。"python_2.7.1"。
		str <- c(str, "Executed scripts:", gsub(" ", "_", system("cat $(dirname $(echo $PATH | cut -d: -f1))/VERSIONS || :", intern = TRUE)))
		# 項目毎に分割して、空白で繋げる。
		str <- unlist(tapply(str, cut(seq(str), c(1, grep(":$", str), Inf), right = FALSE), paste, collapse = "  "))
		# 改行を追加する。
		str <- gsub("^([^:]*:)", "\n\\1\n", str)
		# 改行コードで分割する。
		str <- unlist(strsplit(str, "\n"))
		# ８０文字毎に改行する。
		str <- unlist(ifelse(regexpr("^  ", str) > 0, strwrap(str, 80, prefix = "  ", simplify = FALSE), str))
		# ファイルに書き込む。
		writeLines(str, con = sprintf("%s/%s.txt", pth, strftime(options("date")[[1]], "%y%m%d")))
	}
	# コンソールへの出力を抑える。
	invisible()
}
