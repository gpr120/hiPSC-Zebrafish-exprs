% by okada_n 111018, 111025, 111230, 120328, 120522
% charset=utf-8
\encoding{utf-8}
\name{xpdf}
\alias{pdf}
\title{
Extended PDF Graphics Device
}
\description{
pdf関数の拡張。１枚目の右上にファイル名、左上にタイトルを表示する。
}
\usage{
pdf (file = sprintf("\%s_Rplots.pdf", load.packages("@date")), width, height, onefile, family, title = "", \dots, tag = TRUE)
}
\arguments{
\item{file}{\code{\link[grDevices]{pdf}}を参照すること。１枚目の右上に表示される。}
\item{title}{\code{\link[grDevices]{pdf}}を参照すること。１枚目の左上に表示される。}
\item{\dots}{grDevicesパッケージのpdf関数に渡される引数。}
\item{tag}{右上と左上にタグを表示するかどうか。}
}
