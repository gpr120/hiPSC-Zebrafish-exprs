% by okada_n 120410, 120508
% charset=utf-8
\encoding{utf-8}
\name{adipocytesize.theory}
\alias{adipocytesize.theory.area}
\alias{adipocytesize.theory.diameter}
\title{
[AdipocyteSize] Theory for Cell Distribution
}
\description{
細胞サイズの分布について理論的な考察を行う。
}
\usage{
adipocytesize.theory.area (dis, cnt = 1000, rep = 50000, plot = TRUE, ver = "120410")
adipocytesize.theory.diameter (dis, cnt = 1000, rep = 50000, plot = TRUE, ver = "120410")
}
\arguments{
\item{dis}{真の面積分布。}
\item{cnt}{計算回数。}
\item{rep}{乱数を使う場合の繰り返し回数。}
\item{plot}{描画を行うかどうか。}
\item{ver}{関数のバージョン。}
}
\details{
\code{adipocytesize.theory.area}は細胞断面積分布について、\code{adipocytesize.theory.diameter}は細胞径についての理論的考察。
}
