# by naoi okada
# charset=utf-8
# 
# title:
#   [Package] Execute Scripts
# description:
#   スクリプトを実行する。
# arguments:
#   ... :: ロードするパッケージ名。。
#   pth :: ソフトウェアのインストール先。
package.formula <- function (..., pth = NULL) {
	# pthが指すディレクトリが存在するとき。
	if (identical(TRUE, file.info(as.character(pth))$isdir)) {
		# デバッグ用メッセージ。
		if (identical(TRUE, options("verbose")[[1]])) cat("package.formula...", fill = TRUE)
		## 実行するスクリプト名を生成する。VERSIONSに記された実行済みのものは除外する。
		# 引数に指定されたスクリプト名を取り出す。
		keg <- tolower(substring(grep("^:[[:alnum:].]*$", c(...), value = TRUE), 2))
		# 指定されたパッケージ名からスクリプト名を追加する。
		keg <- c(keg, sapply(as.character(c(...)), switch, JGR = "jgr", RMeCab = "mecab"))
		# デフォルトで実行しておきたいスクリプト名を追加する。
		# keg <- c(keg, "java", "python", "ruby", "svn", "gfortran", "tcltk", "curl")
		# 新しい環境で何が必要になるかわからないのでとりあえずコメントアウト
		# 実行済みのものを除外する。
		keg <- setdiff(keg, system(paste(c("touch", "cut -d' ' -f1"), file.path(pth, "VERSIONS"), collapse = " && "), intern = TRUE))
		## Java：プログラミング言語実行環境。バージョン情報をファイルに書き出す。
		if (is.element("java", keg)) {
			message("==> Execute scripts for java ...")
			cab <- "(java -version 2>&1 || echo java unknown) | head -n1 | sed -e 's/version //g' -e 's/\"//g' | tee -a '%1$s/VERSIONS' >&2"
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## Python：プログラミング言語。
		if (is.element("python", keg)) {
			message("==> Execute scripts for python ...")
			cab <- "(python -V 2>&1 || echo python unknown) | tr [:upper:] [:lower:] | tee -a '%1$s/VERSIONS' >&2"
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## Ruby：プログラミング言語。
		if (is.element("ruby", keg)) {
			message("==> Execute scripts for ruby ...")
			cab <- "(ruby --version 2>&1 || echo ruby unknown) | cut -d' ' -f1,2 | tee -a '%1$s/VERSIONS' >&2"
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## Subversion：バージョン管理システム。OS X 10.4までは標準でインストールされていないのでインストールする。
		if (is.element("svn", keg)) {
			message("==> Execute scripts for subversion ...")
			cab <- "http://archive.apache.org/dist/subversion/subversion-1.3.2.tar.bz2"
			cab <- sprintf(c("curl -f#LO '%1$s'", "cd $(tar jxvf $(basename %1$s) 2>&1 | head -n1 | sed -e 's/^x //g')"), cab)
			cab <- c("cd '%2$s'", cab, "./configure --prefix='%1$s'", "make", "make install")
			cab <- ifelse(nzchar(Sys.which("svn")), ":", cab)
			cab <- c(cab, "(svn --version --quiet || echo unknown) | sed -e 's/^/svn /g' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## GFortran：ライブラリ。
		if (is.element("gfortran", keg)) {
			message(sprintf("==> Install gfortran to %s ...", pth))
			# cab <- "http://r.research.att.com/gfortran-4.2.3.dmg"
			# 上のリンク先が"File not found"になっているので下のものを用いる
			cab <- "http://cran.r-project.org/bin/macosx/tools/gfortran-4.2.3.dmg"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("hdiutil attach -nobrowse -noverify '%s' | grep -oe '/Volumes/.*' | sed -e 's/ /\\\\ /g'", cab), intern = TRUE)
			cab <- sprintf("gunzip -c %1$s/gfortran.pkg/Contents/Archive.pax.gz | pax -r; hdiutil detach %1$s", cab)
			cab <- c("cd '%2$s'", cab, "rsync -av ./usr/local/ '%1$s'", "rm -rf '%2$s/usr'")
			cab <- c(cab, "gfortran --version | head -n1 | sed -e 's/GNU Fortran (GCC)/gfortran/g' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## Tcl/Tk：スクリプト言語とそのGUIツールキット。
		if (is.element("tcltk", keg)) {
			message(sprintf("==> Install tcltk to %s ...", pth))
			cab <- "http://cran.r-project.org/bin/macosx/tools/tcltk-8.5.5-x11.dmg"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("hdiutil attach -nobrowse -noverify '%s' | grep -oe '/Volumes/.*' | sed -e 's/ /\\\\ /g'", cab), intern = TRUE)
			cab <- c("cd '%2$s'", sprintf("gunzip -c %1$s/tcltk.pkg/Contents/Archive.pax.gz | pax -r; hdiutil detach %1$s", cab))
			cab <- c(cab, "cd '%2$s/usr/local/bin'", "find ./* | sed -e 's/\\([^0-9]*\\)\\([0-9.]*\\)/mv \\1\\2 \\1/g' | sh")
			cab <- c(cab, "cd '%2$s/usr/local'", "rsync -av man share/", "rsync -avf '- /man/' ./ '%1$s'", "rm -rf '%2$s/usr'")
			cab <- c(cab, "echo 'puts $tcl_version; exit 0' | tclsh | sed -e 's/^/tcltk /g' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## cURL：ダウンローダー。10.4ではRCurlがエラーを吐くのでバージョンアップが必要。
		if (is.element("curl", keg)) {
			message(sprintf("==> Install curl to %s ...", pth))
			cab <- "http://curl.haxx.se/download/curl-7.22.0.tar.bz2"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar jxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", gsub("^x |/$", "", cab[1]))
			cab <- c("cd '%2$s'", cab, "./configure --prefix='%1$s' --disable-dependency-tracking --disable-debug", "make", "make install")
			cab <- c(cab, "curl --version | grep -oe 'curl [0-9.]*' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## WGet：ダウンローダー。cURLでプロキシを超えられないときに使う。
		if (is.element("wget", keg)) {
			message(sprintf("==> Install wget to %s ...", pth))
			cab <- switch(version$arch, ppc = "1.10.2", "1.13.4")
			cab <- sprintf("http://ftpmirror.gnu.org/wget/wget-%s.tar.bz2", cab)
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar jxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", gsub("^x |/$", "", cab[1]))
			cab <- c("cd '%2$s'", cab, "./configure --prefix='%1$s' --with-ssl=openssl --disable-dependency-tracking --disable-debug", "make", "make install")
			cab <- c(cab, "wget -V | grep -oe 'Wget [0-9.]*' | tr [:upper:] [:lower:] | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## PDFTk：PDFツール。
		if (is.element("pdftk", keg)) {
			message(sprintf("==> Install pdftk to %s ...", pth))
			cab <- ifelse(package_version(options("osx")[[1]]) < "10.6", "pdftk1.12_OSX10.3.dmg.gz", "pdftk-1.44-osx10.6.dmg")
			cab <- sprintf("http://www.pdflabs.com/tools/pdftk-the-pdf-toolkit/%s", cab)
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("hdiutil attach -nobrowse -noverify %s | grep -oe '/Volumes/.*' | sed -e 's/.*\\///g'", cab), intern = TRUE)
			cab <- sprintf("gunzip -c '/Volumes/%1$s/%1$s.mpkg/Contents/Packages/pdftk.pkg/Contents/Archive.pax.gz' | pax -r; hdiutil detach '/Volumes/%1$s'", cab)
			cab <- c("cd '%2$s'", cab, "rsync -av ./{bin,lib} '%1$s'", "rm -rf %2$s/{bin,lib}")
			cab <- c(cab, "pdftk --version | grep -oe 'pdftk [0-9.]*' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## JGR：JGR.app。バージョン情報は書き出さない。
		if (is.element("jgr", keg) && !file.exists("~/.JGRprefsrc") && is.element("JGR", utils::installed.packages()[, "Package"])) {
			message("==> Install JGR.app to ~/Applications ...")
			cab <- utils::download.packages("JGR", tempdir(), type = "source")[2]
			cab <- system(sprintf("tar zxvf %1$s -C %2$s 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s/launcher/macosx'", gsub("^x |/$", "", cab[1]))
			# XCodeを使用してビルドする。OSのバージョンに合わせてSDKを切り替える。
			cab <- c("cd '%2$s'", cab, "xcodebuild -alltargets -sdk macosx$(sw_vers -productVersion | sed 's/\\.[0-9]*$//')")
			# ホームディレクトリにアプリケーションをコピーする。
			cab <- c(cab, "mkdir -p ~/Applications", "cp -a ./build/Default/JGR.app ~/Applications")
			## 設定ファイルを書き出す。
			## InitialRLibrarPathに指定されたパスにJGR等のパッケージが無い場合、
			## /Library/Frameworks/R.framework/Resources/libraryにインストールしようとして、
			## 管理者権限が無い場合に強制終了してしまう。
			cab <- c(cab, "echo $'<?xml version=\"1.0\" encoding=\"UTF-8\"?>' > ~/.JGRprefsrc")
			cab <- c(cab, "echo $'<!DOCTYPE preferences SYSTEM \"http://java.sun.com/dtd/preferences.dtd\">' >> ~/.JGRprefsrc")
			cab <- c(cab, "echo $'<preferences EXTERNAL_XML_VERSION=\"1.0\">' >> ~/.JGRprefsrc")
			cab <- c(cab, "echo $'<root type=\"user\">' >> ~/.JGRprefsrc")
			cab <- c(cab, "echo $' <map/>\n <node name=\"org\">' >> ~/.JGRprefsrc")
			cab <- c(cab, "echo $'  <map/>\n  <node name=\"rosuda\">' >> ~/.JGRprefsrc")
			cab <- c(cab, "echo $'   <map/>\n   <node name=\"JGR\">' >> ~/.JGRprefsrc")
			cab <- c(cab, "echo $'    <map>' >> ~/.JGRprefsrc")
			cab <- c(cab, "echo $'     <entry key=\"InitialRLibraryPath\" value=\"%3$s\"/>' >> ~/.JGRprefsrc")
			cab <- c(cab, "echo $'     <entry key=\"WorkingDirectory\" value=\"%4$s\"/>' >> ~/.JGRprefsrc")
			cab <- c(cab, "echo $'    </map>' >> ~/.JGRprefsrc")
			cab <- c(cab, "echo $'   </node>\n  </node>\n </node>\n</root>\n</preferences>' >> ~/.JGRprefsrc")
			cab <- system(paste(sprintf(cab, pth, tempdir(), .libPaths()[1], getwd()), collapse = " && "))
		}
		## MeCab：形態素解析。
		if (is.element("mecab", keg)) {
			message(sprintf("==> Install mecab to %s ...", pth))
			cab <- ifelse(package_version(version) <= "2.14.1", "0.98", "0.993")
			cab <- sprintf("http://mecab.googlecode.com/files/mecab-%s.tar.gz", cab)
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar zxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", gsub("^x |/$", "", cab[1]))
			cab <- c("cd '%2$s'", cab, "./configure --prefix='%1$s' --with-charset=utf8 --disable-dependency-tracking", "make install")
			cab <- c(cab, "mecab --version | head -n1 | sed -e 's/of //g' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## MeCab-IPADic：形態素解析用辞書。
		if (is.element("mecab", keg)) {
			message(sprintf("==> Install mecab-ipadic to %s ...", pth))
			cab <- "http://mecab.googlecode.com/files/mecab-ipadic-2.7.0-20070801.tar.gz"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar zxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", gsub("^x |/$", "", cab[1]))
			cab <- c("cd '%2$s'", cab, "./configure --prefix='%1$s' --with-charset=utf8 --disable-dependency-tracking", "make install")
			cab <- c(cab, "grep '^VERSION=' ./configure | sed -e 's/VERSION=/mecab-ipadic /g' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## NKF：文字コード変換ツール。
		if (is.element("nkf", keg)) {
			message(sprintf("==> Install nkf to %s ...", pth))
			cab <- "http://dl.sourceforge.jp/nkf/53171/nkf-2.1.2.tar.gz"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar zxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", gsub("^x |/$", "", cab[1]))
			cab <- c("cd '%2$s'", cab, "sed -i '' -e 's,prefix = /usr/local,prefix = %1$s,g' -e's,[)]/man,)/share/man,g' -e 's,mkdir,& -p,g' Makefile")
			cab <- c(cab, "make", "make install")
			cab <- c(cab, "nkf --version | grep -oe 'Version [0-9.]*' | sed -e 's/Version/nkf/g' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## ClustalW：アライメントツール。
		if (is.element("clustalw2", keg)) {
			message(sprintf("==> Install clustalw2 to %s ...", pth))
			cab <- "http://www.clustal.org/download/2.0.12/clustalw-2.0.12.tar.gz"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar zxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", gsub("^x |/$", "", cab[1]))
			cab <- c("cd '%2$s'", cab, "./configure --prefix='%1$s'", "make", "make install")
			cab <- c(cab, "clustalw2 -help | grep -oe 'CLUSTAL [0-9.]*' | tr [:upper:] [:lower:] | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## Muscle：アライメントツール。
		if (is.element("muscle", keg)) {
			message(sprintf("==> Install muscle to %s ...", pth))
			cab <- switch(version$arch, i386 = "i86darwin32", ppc = "macppc", x86_64 = "i86darwin64")
			cab <- sprintf("http://www.drive5.com/muscle/downloads3.8.31/muscle3.8.31_%s.tar.gz", cab)
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar zxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cp '%1$s' '%%1$s/bin/muscle'", gsub("^x |/$", "", cab[1]))
			cab <- c("cd '%2$s'", cab)
			cab <- c(cab, "muscle -version | cut -d' ' -f1,2 | sed -e 's/ v/ /' | tr [:upper:] [:lower:] | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## Mafft：アライメントツール。
		if (is.element("mafft", keg)) {
			message(sprintf("==> Install mafft to %s ...", pth))
			cab <- "http://mafft.cbrc.jp/alignment/software/source.html"
			cab <- system(sprintf("curl -s '%s' | grep -oe 'mafft-[0-9.]*-with-extensions-src\\.tgz' | head -n1", cab), intern = TRUE)
			cab <- sprintf("http://mafft.cbrc.jp/alignment/software/%s", cab)
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar zxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", gsub("^x |/$", "", cab[1]))
			cab <- c("cd '%2$s'", cab, "sed -i '' -e 's,PREFIX = /usr/local,PREFIX = %1$s,g' -e 's,\\(MULTITHREAD =\\) .*,\\1,g' ./{core,extensions}/Makefile")
			cab <- c(cab, "( cd ./core && make && make install )", "( cd ./extensions && make && make install )")
			cab <- c(cab, "echo mafft $('%1$s/libexec/mafft/version') | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## CMake：ビルドツール。
		if (is.element("obabel", keg)) {
			message(sprintf("==> Install cmake to %s ...", pth))
			cab <- "http://www.cmake.org/files/v2.8/cmake-2.8.8.tar.gz"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar zxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", gsub("^x |/.*$", "", cab[1]))
			cab <- c("cd '%2$s'", cab, "./configure --prefix='%1$s'", "make", "make install")
			cab <- c(cab, "cmake --version | cut -d' ' -f1,3 | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## OpenBabel：化学構造ファイルフォーマット変換。
		if (is.element("obabel", keg)) {
			message(sprintf("==> Install openbabel to %s ...", pth))
			cab <- "http://downloads.sourceforge.net/project/openbabel/openbabel/2.3.1/openbabel-2.3.1.tar.gz"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar zxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", gsub("^x |/$", "", cab[1]))
			cab <- c("cd '%2$s'", cab, "cmake -DCMAKE_INSTALL_PREFIX='%1$s' .", "make -j2", "make install")
			cab <- c(cab, "obabel -V | grep -oe 'Babel [0-9.]*' | sed -e 's/^Babel/obabel/g' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## Poi：エクセルファイルの書き込み用ライブラリ。
		if (is.element("poi", keg)) {
			message(sprintf("==> Install poi to %s ...", pth))
			cab <- "http://archive.apache.org/dist/poi/release/bin/poi-bin-3.8-20120326.tar.gz"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar zxvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", dirname(gsub("^x ", "", cab[1])))
			cab <- c("cd '%2$s'", cab, "mkdir -p '%1$s/java'", "rsync -av --exclude '/docs/' . '%1$s/java/poi'")
			cab <- c(cab, "find '%1$s/java/poi' -regex .*poi-[0-9.-]*.jar | sed -e 's/.*\\///g' -e 's/.jar//g' -e 's/poi-/poi /g' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
		}
		## Modeller：ホモロジーモデリングモジュール。
		if (is.element("modeller", keg)) {
			message(sprintf("==> Install Modeller to %s ...", pth))
			cab <- "http://salilab.org/modeller/9.10/modeller-9.10.dmg"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("hdiutil attach -nobrowse -noverify %s | grep -oe '/Volumes/.*' | sed -e 's/.*\\///g'", cab), intern = TRUE)
			cab <- sprintf("gunzip -c '/Volumes/%1$s/%1$s.pkg/Contents/Archive.pax.gz' | pax -r; hdiutil detach '/Volumes/%1$s'", cab)
			cab <- c("cd '%2$s'", cab, "mkdir -p '%1$s/python'", "rsync -av '%2$s/Library/modeller-9.10/' '%1$s/python/modeller'")
			cab <- c(cab, "printf '%%s\n' %1$s/python/modeller/{modlib,lib/mac10v4} > '%1$s/python/modeller.pth'")
			cab <- c(cab, "echo install_dir = r\\'%1$s/modeller\\'$'\n'license = r\\'MODELIRANJE\\' > '%1$s/python/modeller/modlib/modeller/config.py'")
			cab <- c(cab, "cd %1$s/%4$s", "find $(pwd) -type f | xargs -I%% find * -name '*dylib' -exec install_name_tool -change %3$s/{} %1$s/%4$s/{} %% \\;")
			cab <- c(cab, "grep -oe '^Modeller [^ ]*' '%1$s/python/modeller/ChangeLog' | head -n1 | tr [:upper:] [:lower:] | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir(), "/Library/modeller-9.10/lib/mac10v4", "/python/modeller/lib/mac10v4"), collapse = " && "))
		}
		## ImageJ：画像処理ソフトウェア。
		if (is.element("imagej", keg)) {
			message(sprintf("==> Install ImageJ to %s ...", pth))
			cab <- "http://rsbweb.nih.gov/ij/download/zips/ij146.zip"
			cab <- names(mapply(utils::download.file, destfile = file.path(tempdir(), basename(cab)), cab))
			cab <- system(sprintf("tar xvf '%1$s' -C '%2$s' 2>&1", cab, tempdir()), intern = TRUE)
			cab <- sprintf("cd '%1$s'", dirname(gsub("^x ", "", grep("ij.jar$", cab, value = TRUE))))
			cab <- c("cd '%2$s'", cab, "mkdir -p '%1$s/java'", "rsync -av ij.jar '%1$s/java/imagej/'")
			cab <- c(cab, "java -jar '%1$s/java/imagej/ij.jar' -eval 'return getVersion();' -run 'Quit' | sed -e 's/^/imagej /g' | tee -a '%1$s/VERSIONS' >&2")
			cab <- system(paste(sprintf(cab, pth, tempdir()), collapse = " && "))
			
		}
	}
	# コンソールへの出力を抑える。
	invisible()
}
