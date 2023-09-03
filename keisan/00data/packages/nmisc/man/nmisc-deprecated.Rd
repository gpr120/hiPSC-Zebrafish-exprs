% by okada_n 111018
% charset=utf-8
\encoding{utf-8}
\name{nmisc-deprecated}
\alias{load.function}
\alias{is.wholenumber}
\alias{is.naturalnumber}
\alias{exec}
\alias{adipocytesize.plot.histgram}
\alias{geoquery.get.data}
\alias{microarray.ontology}
\alias{geoquery.pair.generate}
\alias{geoquery.pair.select}
\alias{geoquery.annotate}
\alias{geoquery.annotate.gds}
\alias{geoquery.annotate.gse}
\alias{microarray.pair.generate}
\alias{microarray.pair.select}
\title{
Deprecated Functions
}
\description{
廃止予定の関数。
}
\usage{
load.function (proj, \dots)
is.wholenumber (x, tol = .Machine$double.eps^0.5)
is.naturalnumber (x, tol = .Machine$double.eps^0.5)
exec (fmt, \dots)
adipocytesize.plot.histgram (\dots, ver = 1)
geoquery.get.data (\dots)
microarray.ontology (\dots)
geoquery.pair.generate (afy, ext = "")
geoquery.pair.select (afy, ind = "")
geoquery.annotate (afy)
geoquery.annotate.gds (afy)
geoquery.annotate.gse (afy)
microarray.pair.generate (afy)
microarray.pair.select (afy, ind = "")
}
\details{
名前の変更等で使用しなくなった，または，使用しないことにした関数群。
過去のスクリプトから全部置換し終わったら削除していく予定。
}
