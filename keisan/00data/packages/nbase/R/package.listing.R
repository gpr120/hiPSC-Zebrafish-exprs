# by naoi okada
# charset=utf-8
# 
# title:
#   [Package] Create Repository of External Packages
# description:
#   一時ディレクトリ上に通常の方法ではインストールできないパッケージのリポジトリを生成する。
# arguments:
#   ... :: ロードするパッケージ名のベクトル。
# details:
#   パッケージを追加する時はpackage.listing.Rを直接編集する。\cr
#   Package:    パッケージ名。\cr
#   Version:    バージョン。\cr
#   Depends:    依存関係。\cr
#   Archs:      対応しているアーキテクチャ。ppc，i386，x86_64のいずれか。いずれでもかまわない場合かソースの場合はNAを指定する。\cr
#   Repository: パッケージファイルのアドレス。'%1$s'，'%2$s', '%3$s'がそれぞれPackage, Version, Archsに置き換えられる。\cr
package.listing <- function (...) {
	# デバッグ用メッセージ。
	if (identical(TRUE, options("verbose")[[1]])) cat("package.listing...", fill = TRUE)
	# パッケージリストの文字列を作成する。
	pkg <- "
	Package Version  Depends                 Archs  Repository
	NCBI2R  1.3.2    R(>=2.10.0),R(<=2.11.1) NA     http://cran.r-project.org/src/contrib/Archive/%1$s/%1$s_%2$s.tar.gz
	RImageJ 0.3-144  R(>=2.11.0),rJava       NA     http://cran.r-project.org/src/contrib/Archive/%1$s/%1$s_%2$s.tar.gz
	pdf2    2.10.0.6 R(>=2.10.0),R(<=2.11.0) NA     http://%1$s.r-forge.r-project.org/packages/%1$s_%2$s.tar.gz
	pdf2    2.11.1.1 R(>=2.11.1),R(<=2.12.2) NA     http://%1$s.r-forge.r-project.org/packages/%1$s_%2$s.tar.gz
	RMeCab  0.86     R(>=2.10.0)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.89     R(>=2.11.0)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s_R_%3$s-apple-darwin9.8.0.tar.gz
	RMeCab  0.91     R(>=2.11.1)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.92     R(>=2.12.0)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.i386.tgz
	RMeCab  0.93     R(>=2.12.1)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s_i386_.tgz
	RMeCab  0.94     R(>=2.12.2)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.i386Mac.tgz
	RMeCab  0.95     R(>=2.13.0)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.i386Mac.tgz
	RMeCab  0.96     R(>=2.13.1)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s_i386Mac.tgz
	RMeCab  0.97     R(>=2.13.2)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s_i386Mac.tgz
	RMeCab  0.98     R(>=2.14.0)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s_i386Mac.tgz
	RMeCab  0.99     R(>=2.14.1)             i386   http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s_i386Mac.tgz
	RMeCab  0.95     R(>=2.10.0)             ppc    http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s_PPC_Mac.tgz
	RMeCab  0.86     R(>=2.10.0)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s_R_%3$s-apple-darwin9.8.0.tar.gz
	RMeCab  0.89     R(>=2.11.0)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s_R_%3$s-apple-darwin9.8.0.tar.gz
	RMeCab  0.91     R(>=2.11.1)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s_R_%3$s-apple-darwin9.8.0.tar.gz
	RMeCab  0.92     R(>=2.12.0)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.93     R(>=2.12.1)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.94     R(>=2.12.2)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.95     R(>=2.13.0)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.96     R(>=2.13.1)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.97     R(>=2.13.2)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.98     R(>=2.14.0)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.990    R(>=2.14.1)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.992    R(>=2.14.2)             x86_64 http://rmecab.jp/wiki/?plugin=attach&refer=SoftArchive&openfile=%1$s_%2$s.tgz
	RMeCab  0.994    R(>=2.15.0)             x86_64 http://web.ias.tokushima-u.ac.jp/linguistik/%1$s_%2$s.tgz # OS X Lion
	RMeCab  0.995    R(>=2.15.1)             x86_64 http://web.ias.tokushima-u.ac.jp/linguistik/%1$s_%2$s.tgz # OS X Lion
	bio3d   1.1-3    NA                      NA     http://mccammon.ucsd.edu/~bgrant/%1$s/%1$s_%2$s.tar.gz
	bio3d   1.1-4    NA                      NA     http://mccammon.ucsd.edu/~bgrant/%1$s/%1$s_%2$s.tar.gz
	"
	# 一時ファイルに書き出す。
	pkg <- names(mapply(writeLines, con = tempfile(), pkg))
	# スペース区切りテキストとして読み込む。
	pkg <- utils::read.table(file = pkg, header = TRUE, stringsAsFactors = FALSE)
	# Depends列にスペースを入れる。
	pkg <- within(pkg, Depends <- gsub("R\\(([<>=!]+)([0-9.]+)\\)", "R (\\1 \\2)", Depends))
	# Archs列のNAをRのアーキテクチャに書き換える。
	pkg <- within(pkg, Archs <- ifelse(is.na(Archs), version$arch, Archs))
	# File列を生成する。
	pkg <- within(pkg, File <- sprintf(basename(Repository), Package, Version, Archs))
	# Repository列のパッケージ名、バージョン、アーキテクチャを書き換える。
	pkg <- within(pkg, Repository <- sprintf(dirname(Repository), Package, Version, Archs))
	# 一時ディレクトリのPACKAGESファイルに書き出す。
	pkg <- write.dcf(pkg, file.path(tempdir(), "PACKAGES"))
	# コンソールへの出力を抑える。
	invisible()
}
