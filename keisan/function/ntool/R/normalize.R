# by naoi okada
# charset=utf-8
# 
# title:
#   Normalization
# description:
#   数値ベクトルを正規化する。
# arguments:
#   vec :: 数値ベクトル。
#   fea :: 正規化に使用する特徴量の種類。range -> 最小値を０、最大値を１に正規化する。sd -> 平均を０、標準偏差を１に正規化する。
# value:
#   正規化された数値ベクトル。
normalize <- function (vec, fea = c("range", "sd")) {
	# 引数が数値ベクトルのときのみ処理を行う。
	stopifnot(is.numeric(vec))
	# 指定された特徴量の種類によって場合分け。
	switch(match.arg(fea),
		# 最小最大正規化：(x - min) / (max - min)
		range = (vec - min(vec, na.rm = TRUE)) / diff(range(vec, na.rm = TRUE)),
		# 標準化：(x - mean) / sd
		sd = (vec - mean(vec, na.rm = TRUE)) / stats::sd(vec, na.rm = TRUE),
		# 上記のもの以外を指定した場合はエラーを返す。
		stop("'fea' should be one of 'range', 'sd'"))
}
