# by naoi okada
# charset=utf-8
#
# title:
#   [Package] Install Packages which are not already Installed
# description:
#   リポジトリにないパッケージやバイナリがないパッケージも含めてインストールする。
# arguments:
#   ... :: ロードするパッケージ名のベクトル。
package.install <- function (...) {
	# デバッグ用メッセージ。
	if (identical(TRUE, options("verbose")[[1]])) cat("package.install...", fill = TRUE)
	# 利用可能なパッケージのリストを取得しておく。
	avi <- utils::available.packages(contriburl = append(utils::contrib.url(options("repos")[[1]]), sprintf("file://%s", tempdir())), fields = "Repository")
	# データフレームに変換する。
	avi <- data.frame(avi, stringsAsFactors = FALSE)
	# リポジトリを書き換える。
	avi <- within(avi, Repository <- ifelse(is.na(Repository), Repository.1, Repository))
	# インストールしたいパッケージの種類に応じて拡張子を決める。
	avi <- within(avi, Extension <- switch(substring(options("pkgType")[[1]], 1, 10), source = "tar.gz", mac.binary = "tgz", win.binary = "zip"))
	# ダウンロードするファイル名を生成しておく。
	avi <- within(avi, File <- ifelse(is.na(File), sprintf("%s_%s.%s", Package, Version, Extension), File))
	# 行列に戻す。
        avi2 <- avi                     ###応急処置: 131123 matsubara
	avi <- sapply(avi, as.character)
	# インストールされていないもしくは新しいバージョンがあるパッケージ名を取得しておく。
	new <- utils::new.packages(available = avi)
	# インストールしたいパッケージ名。
	pkg <- intersect(grep("^[[:alnum:].]*$", c(...), value = TRUE), new)
	# インストールしたいものがある場合にインストールする。
	## pkg <- if (length(pkg)) utils::install.packages(pkg, contriburl = unique(avi$Repository), available = avi, type = "source")
	pkg <- if (length(pkg)) utils::install.packages(pkg, contriburl = unique(avi2$Repository), available = avi, type = "source") ### 131123 matsubara
	# インストールされているパッケージを更新する。
	pkg <- utils::update.packages(lib = .libPaths()[1], ask = FALSE, available = avi)
	# インストールされたもしくは更新されたパッケージを取り出す。
	pkg <- intersect(utils::installed.packages()[, "Package"], new)
	# ライブラリへのパスを取得する。
	lib <- sapply(pkg, sprintf, fmt = "%2$s/%1$s/libs/%3$s/%1$s%4$s", .libPaths()[1], .Platform$r_arch, .Platform$dynlib.ext, USE.NAMES = FALSE)
	# ライブラリが存在するものだけを残す。
	lib <- subset(unname(lib), file.exists(as.character(lib)))
	# それぞれのライブラリがリンクしている共有ライブラリへのパスを取得する。
	lib <- lapply(sapply(lib, sprintf, fmt = "otool -XL %s | cut -f2 | sed -e 's/ .*//g'"), system, intern = TRUE)
	# 置換前と置換後、ライブラリのパス、sprintf用のフォーマットをリストにする。
	lib <- mapply(list, old = lib, new = lib, pth = names(lib), MoreArgs = list(fmt = "install_name_tool -change %s %s %s"), SIMPLIFY = FALSE)
	# パスを置換する。"@rpath/R.framework/Resources"、"/Library/Frameworks/R.framework/Versions/2.14/Resources"をR.home()に書き換える。
	lib <- lapply(lib, within, new <- gsub("^(@rpath|/.*)/R.framework/.*Resources", R.home(), new))
	# パスを置換する。"/usr/local"をgfortranがインストールされている場所に書き換える。
	lib <- lapply(lib, within, new <- gsub("/usr/local", gsub("^$", "/usr/local", gsub("/bin/gfortran", "", Sys.which("gfortran"))), new))
	# リンクを修正するコマンドを作成しつつ、パスが書き換えられるものだけを残す。
	lib <- mapply(subset, lapply(lib, do.call, what = sprintf), lapply(lib, with, old != new), SIMPLIFY = FALSE)
	# コマンドを表示してから実行する。
	lib <- lapply(sprintf("echo '%1$s' && %1$s", unlist(lib)), system)
	# コンソールへの出力を抑える。
	invisible()
}
