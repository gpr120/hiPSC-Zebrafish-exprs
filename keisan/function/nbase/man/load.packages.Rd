% by naoi okada
% charset=utf-8
\encoding{utf-8}
\name{load.packages}
\alias{load.packages}
\title{
Install and Load Packages
}
\description{
パッケージをロードする。指定されたパッケージがインストールされていない場合は自動的にインストールする。
}
\usage{
load.packages (\dots)
}
\arguments{
\item{\dots}{ロードするパッケージ名のベクトル。}
}
\value{
ロードされているパッケージのリスト。"@path", "@date"が指定されている場合はオプションを返す。
}
\details{
引数には下記の条件を満たす文字列も指定できる。\cr
^~?/[^ :]+$ : サーバのパス。スペースとコロンを含まないこと。\cr
^:[[:alnum:].]*$ : 実行したいスクリプト名。\cr
^@[[:alnum:].]*$ : "path", "date"のいずれか。パッケージのリストの代わりにオプションを返す。\cr
}
