% by naoi okada
% charset=utf-8
\encoding{utf-8}
\name{package.options}
\alias{package.options}
\title{
[Package] Set Pptions and Environment Variables
}
\description{
オプションと環境変数にパスとソフトウェアのバージョン、日付を設定する。
}
\usage{
package.options (\dots, pth = NULL)
modified by umeda
}
\arguments{
\item{\dots}{ロードするパッケージ名のベクトル。}
\item{pth}{サーバのパス。}
}
\details{
オプションに基点となるサーバ、ローカルリポジトリ、ソフトウェアのインストール先、セッション情報の保存先のパスを設定する。\cr
また、ソフトウェアのバージョンと日付も記録する。
環境変数にもソフトウェアのインストール先のパスを設定する。
}
