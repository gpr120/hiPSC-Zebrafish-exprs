# by naoi okada
# charset=utf-8
# 
# title:
#   Install and Load Packages
# description:
#   パッケージをロードする。指定されたパッケージがインストールされていない場合は自動的にインストールする。
# arguments:
#   ... :: ロードするパッケージ名のベクトル。
# value:
#   ロードされているパッケージのリスト。"@path", "@date"が指定されている場合はオプションを返す。
# details:
#   引数には下記の条件を満たす文字列も指定できる。\cr
#     ^~?/[^ :]+$ : サーバのパス。スペースとコロンを含まないこと。\cr
#     ^:[[:alnum:].]*$ : 実行したいスクリプト名。\cr
#     ^@[[:alnum:].]*$ : "path", "date"のいずれか。パッケージのリストの代わりにオプションを返す。\cr
load.packages <- function (...) {
	# オプションに日付を設定する。一度設定したら変更されない。
	options(date = getOption("date", Sys.time() - 6 * 3600))
	# オプションを設定する。
	package.options(..., pth = grep("^/[^ :]+$", path.expand(as.character(c(...))), value = TRUE)[1])
	# ローカルリポジトリ上のパッケージを更新する。
	package.updates(..., pth = options("path.repos")[[1]])
	# スクリプトを実行する。
	package.formula(..., pth = options("path.prefix")[[1]])
	# 一時ディレクトリ上に通常の方法ではインストールできないパッケージのリポジトリを生成する。
	package.listing(...)
	# インストールされていないパッケージをインストールする。
	package.install(...)
	# パッケージをロードする。各パッケージで必要な処理があれば行う。
	package.loading(...)
	# セッション情報を出力する。
	package.session(..., pth = options("path.session")[[1]])
	# @から始まる文字列を指定した場合は対応するオプションを、そうでない場合はロードされているパッケージのリストを返す。
	switch(substring(grep("^@", c(...), value = TRUE)[1], 2), path = options("path.server")[[1]], date = options("date")[[1]], (.packages()))
}
