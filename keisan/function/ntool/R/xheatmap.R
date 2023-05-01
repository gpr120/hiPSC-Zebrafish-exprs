# by okada_n 111025
# charset=utf-8

ex.heatmap <- function (
	x,
	rowv = TRUE,
	colv = TRUE,
	rowd = TRUE,
	cold = TRUE,
	method = "euclidean",
	link = "complete",
	key = TRUE,
	scale = c("none", "row", "column"),
	lhei = c(1.5, 4),
	lwid = c(1.5, 4),
	lmat = rbind(4:3, 2:1),
	margin = c(5, 5),
	...) {
	
	load.packages("amap", "gplots")
	
	stopifnot(length(dim(x)) == 2, is.numeric(x), nrow(x) > 1, ncol(x) > 1)
	
	scale <- match.arg(scale)
	
	dndr <- if(rowv) as.dendrogram(amap::hcluster(x, method = method, link = link)) else NULL
	dndc <- if(colv) as.dendrogram(amap::hcluster(t(x), method = method, link = link)) else NULL
	indr <- if(rowv) order.dendrogram(ddr, rowMeans(x, na.rm = TRUE)) else nrow(x):1
	indc <- if(colv) order.dendrogram(ddc, colMeans(x, na.rm = TRUE)) else ncol(x):1
	brks <- seq(-max(abs(x), na.rm = TRUE), max(abs(x), na.rm = TRUE), length = 16)
	cols <- gplots::greenred(15)
	
	## データを補正。
	x <- x[indr, indc]
	# 並び替える。
	x <- if(scale == "row") sweep(sweep(x, 1, rowMeans(x), na.rm = TRUE), 1, apply(x, 1, sd, na.rm = TRUE), "/") else x
	# 行方向のスケーリング。
	x <- if(scale == "column") sweep(sweep(x, 2, colMeans(x), na.rm = TRUE), 2, apply(x, 2, sd, na.rm = TRUE), "/") else x
	# 列方向のスケーリング。
	x[x < min(brks)] <- min(brks)
	# 小さすぎる値を最小値に。
	x[x > max(brks)] <- max(brks)
	# 大きすぎる値を最大値に。
	x <- t(x)
	# 転置する。
	
	## プロット。
	op <- par(no.readonly = TRUE); on.exit(par(op))
	# 終了時にグラフィックスパラメータを元に戻す。
	layout(lmat, widths = lwid, heights = lhei, respect = FALSE)
	# ウィンドウを分割。デフォルトでは４つに分割して右下、左下、右上、左上の順にプロットする。
	par(mar = c(margin[1], 0, 0, margin[2]))
	# 余白。下と右だけ。
	image(1:nrow(x), 1:ncol(x), x, xlim = c(0, nrow(x)) + 0.5, ylim = c(0, ncol(x)) + 0.5, axes = FALSE, xlab = "", ylab = "", col = cols, breaks = brks, ...)
	# ヒートマップを描く。
	if(any(is.na(x))) image(1:nrow(x), 1:ncol(x), ifelse(is.na(x), 1, NA), axes = FALSE, xlab = "", ylab = "", col = par("bg"), add = TRUE)
	# NAの部分に色を付ける。
	if(labr) axis(1, 1:nrow(x), labels = colnames(x), las = 2, line = -0.5, tick = 0, cex.axis = min(0.2 + 1/log10(nrow(x)), 1))
	# 列名。下。
	if(labt) axis(3, 1:nrow(x), labels = colnames(x), las = 2, line = -0.5, tick = 0, cex.axis = min(0.2 + 1/log10(nrow(x)), 1))
	# 列名。上。
	if(labc) axis(4, 1:ncol(x), labels = rownames(x), las = 2, line = -0.5, tick = 0, cex.axis = min(0.2 + 1/log10(ncol(x)), 1))
	# 行名。
	
	## 樹形図。
	par(mar = c(margin[1], 0, 0, 0))
	# 余白。下だけ。
	if(rowd) plot(dndr, horiz = TRUE, axes = FALSE, yaxs = "i", leaflab = "none") else plot.new()
	# 行方向の樹形図を描く。
	par(mar = c(0, 0, 0, margin[2]))
	# 余白。下だけ。
	if(cold) plot(dndc, axes = FALSE, xaxs = "i", leaflab = "none") else plot.new()
	# 行方向の樹形図を描く。
	
	## カラーテーブル。
	if(key){
	# カラーテーブルを表示するとき
		par(mar = c(5, 4, 2, 1), cex = 0.75, las = 1)
		# 余白と文字サイズ。
		image(z = matrix(brks, ncol = 1), col = cols, brks = brks, xaxt = "n", yaxt = "n")
		# プロット。
		par(usr = c(0, 1, 0, 1))
		# 作図領域を最大に。
		labs <- pretty(brks)
		# 切りのいい数字を取り出す。
		axis(1, at = nmisc::Rscale(as.numeric(labs)), labels = labs)
		# ラベル。
		mtext(1, if(scale == "row") "Row Z-Score" else if(scale == "column") "Column Z-Score" else "Value")
		# スケーリング情報。
		title("Color Key")
		# タイトル。
	} else plot.new()
	# カラーテーブルを書かないときは空白。
	invisible()
	# 表示しない。
}
