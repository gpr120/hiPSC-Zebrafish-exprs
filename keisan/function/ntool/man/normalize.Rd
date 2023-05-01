% by naoi okada
% charset=utf-8
\encoding{utf-8}
\name{normalize}
\alias{normalize}
\title{
Normalization
}
\description{
数値ベクトルを正規化する。
}
\usage{
normalize (vec, fea = c("range", "sd"))
}
\arguments{
\item{vec}{数値ベクトル。}
\item{fea}{正規化に使用する特徴量の種類。range -> 最小値を０、最大値を１に正規化する。sd -> 平均を０、標準偏差を１に正規化する。}
}
\value{
正規化された数値ベクトル。
}
