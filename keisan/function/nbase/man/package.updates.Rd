% by naoi okada
% charset=utf-8
\encoding{utf-8}
\name{package.updates}
\alias{package.updates}
\title{
[Package] Update Packages on Local Repository
}
\description{
ローカルリポジトリ上にあるパッケージを更新する。
}
\usage{
package.updates (\dots, pth = NULL)
}
\arguments{
\item{\dots}{ロードするパッケージ名。}
\item{pth}{ローカルリポジトリのパス。}
}
\details{
ローカルリポジトリには各パッケージに対して１つのディレクトリを用意しておく。\cr
パッケージ化されたファイルの更新日時より後に更新されたファイルがあれば、
そのパッケージのバージョンを実行した日付に変更してからパッケージ化する。\cr
パッケージに含まれる関数を使用したり，明示的にロードしたパッケージは一度アンロードしないと更新が反映されない。
そのため，関数の最後でパッケージを一度アンロードした後ローカルリポジトリ上にあるパッケージを全てロードしている。
}
