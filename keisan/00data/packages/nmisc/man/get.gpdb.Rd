% by okada_n 111020, 111105, 111110, 111230, 120329
% charset=utf-8
\encoding{utf-8}
\name{get.gpdb}
\alias{get.gpdb}
\title{
Get Data from GlycoProtDB
}
\description{
糖タンパク質データベースからデータを取得する。
}
\usage{
get.gpdb (ids = NULL, tax = "Caenorhabditis elegans")
}
\arguments{
\item{ids}{GPDBでのIDを指定できる。省略された場合はすべてのデータをダウンロードする。}
\item{tax}{分類名。今のところ線虫のみ。}
}
\details{
RESTメソッドを使用してXMLファイルを読み込む。
RefSeq等で検索できないので通常はすべてのデータをダウンロードして作成したデータフレームを使う。
10分程度かかる。
}
\references{
http://riodb.ibase.aist.go.jp/rcmg/glycodb/Glc_ResultSearch\cr
2008 Research Center for Medical Glycoscience, National Institute of Advanced Industrial Science and Technology (AIST)
}
