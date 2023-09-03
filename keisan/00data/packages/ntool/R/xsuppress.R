# by okada_n 120125, 120328
# charset=utf-8
#
# title:
#   Suppress Any Outputs
# description:
#   コンソールへの出力を抑止する。ただそれだけの関数。
# arguments:
#   exp :: 実行する文。

suppress <- function(exp) {
	sink("/dev/null"); on.exit(sink())
	options(warn = -1) -> ops; on.exit(options(ops), add = TRUE)
	msg <- function(c) invokeRestart("muffleMessage")
	wrn <- function(w) invokeRestart("muffleWarning")
	ret <- try(withCallingHandlers(exp, message = msg, warning = wrn), silent = TRUE)
	if (class(ret) == "try-error") {
		stop(ret)
	}
	ret
}
