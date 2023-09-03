# by okada_n 120223, 120328
# charset=utf-8
# 
# title:
#   String of Date Format
# description:
#   整形された日付文字列を返す。６時間前にずらすことで日付が変わっても次の日としないようにしてある。
# arguments:
#   dat :: NULLでない場合、指定された文字列を返すようにする。

xdate <- function(dat = NULL) {
	if (!is.null(dat))
		assign(".date", dat, envir = .GlobalEnv)
	if (!exists(".date", envir = .GlobalEnv))
		assign(".date", strftime(Sys.time() - 6 * 60 * 60, "%y%m%d"), envir = .GlobalEnv)
	get(".date", envir = .GlobalEnv)
}
