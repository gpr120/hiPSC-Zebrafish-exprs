# by okada_n 111018, 111025, 111230, 120328, 120522
# charset=utf-8
#
# title:
#   Extended PDF Graphics Device
# description:
#   pdf関数の拡張。１枚目の右上にファイル名、左上にタイトルを表示する。
# arguments:
#   file :: \code{\link[grDevices]{pdf}}を参照すること。１枚目の右上に表示される。
#   title :: \code{\link[grDevices]{pdf}}を参照すること。１枚目の左上に表示される。
#   ... :: grDevicesパッケージのpdf関数に渡される引数。
#   tag :: 右上と左上にタグを表示するかどうか。

pdf <- function (file = sprintf("%s_Rplots.pdf", load.packages("@date")), width, height, onefile, family, title = "", ..., tag = TRUE) {
	# 通常のpdf関数に引数を渡す。
	grDevices::pdf(file, width, height, onefile, family, title, ...)
	# プロット用の関数を作成する。par("fig")の値から右上or左上を含む描画かどうかを判断する。一度描画したら自分自身を削除する。
	if (tag) {
		fun <- "function () {
			if (!(par('fig')[%3$d + 1] == %3$d & par('fig')[4] == 1)) return()
			setHook('plot.new', getHook('plot.new')[!names(getHook('plot.new')) %%in%% '%1$s'], 'replace')
			par(mar = numeric(4) + 1, mex = 1, xpd = TRUE, new = TRUE) -> op; on.exit(par(op))
			plot.window(xlim = 0:1, ylim = 0:1)
			mtext('%2$s', adj = %3$d, padj = 1, las = 1)
		}"
		fun <- mapply(sprintf, c("title", "file"), c(title, file), 0:1, MoreArgs = list(fmt = fun))
		fun <- lapply(fun, function(x) eval(parse(text = x)))
		# セットする。
		setHook("plot.new", fun, "replace")
	}
}
