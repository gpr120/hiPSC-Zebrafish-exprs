% by naoi okada
% charset=utf-8
\encoding{utf-8}
\name{package.listing}
\alias{package.listing}
\title{
[Package] Create Repository of External Packages
}
\description{
一時ディレクトリ上に通常の方法ではインストールできないパッケージのリポジトリを生成する。
}
\usage{
package.listing (\dots)
}
\arguments{
\item{\dots}{ロードするパッケージ名のベクトル。}
}
\details{
パッケージを追加する時はpackage.listing.Rを直接編集する。\cr
Package:    パッケージ名。\cr
Version:    バージョン。\cr
Depends:    依存関係。\cr
Archs:      対応しているアーキテクチャ。ppc，i386，x86_64のいずれか。いずれでもかまわない場合かソースの場合はNAを指定する。\cr
Repository: パッケージファイルのアドレス。'\%1$s'，'\%2$s', '\%3$s'がそれぞれPackage, Version, Archsに置き換えられる。\cr
}
