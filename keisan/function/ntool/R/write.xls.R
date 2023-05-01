# by naoi okada
# charset=utf-8
# 
# title:
#   Write List of data.frame to Excel Workbook
# description:
#   データフレームをエクセルファイルに書き込む。
# arguments:
#   dat :: 書き込むデータ。行列、データフレーム、またはそれらのリスト。リストの場合、名前がシート名に使用される。
#   cab :: 書き込むファイル名。
#   add :: 既存のエクセルファイルに追加するかどうか。ファイルが存在しない場合は常に新規作成する。
#   hdr :: 列名を追加するかどうか。
#   col :: 色データ。datを同じ構造のデータフレームのリスト。行名，列名の色は省略された場合noneが指定される。
# details:
#   処理は時間がかかる。
#   
#   色は次から指定する。\cr
#   none, black, white, red, bright_green, blue, yellow, pink, turquoise, 
#   dark_red, green, dark_blue, dark_yellow, violet, teal, grey_25_percent, 
#   grey_50_percent, cornflower_blue, maroon, lemon_chiffon, orchid, coral, 
#   royal_blue, light_cornflower_blue, sky_blue, light_turquoise, light_green, 
#   light_yellow, pale_blue, rose, lavender, tan, light_blue, aqua, lime, gold, 
#   light_orange, orange, blue_grey, grey_40_percent, dark_teal, sea_green, 
#   dark_green, olive_green, brown, plum, indigo, grey_80_percent, automatic
# examples:
#   write.xls(matrix(1:100, 10), "test1.xls")
#   write.xls(iris, "test2.xls", clr = matrix(ifelse(sapply(iris, class) == "numeric", "none", "red"), nrow(iris), ncol(iris), byrow = TRUE))
#   write.xls(list(Loblolly = Loblolly, Orange = Orange), "test3.xls")
write.xls <- function (dat, cab, add = FALSE, hdr = TRUE, clr = NULL) {
	# パッケージを読み込む。poi（Apache POI）はエクセルファイルを扱うためのJavaライブラリ。
	load.packages("rJava", ":poi")
	# デバッグ用。
	if (!sys.parent(0)) { dat <- iris; cab <- "test.xls"; add = FALSE; hdr = TRUE; clr <- cbind(ifelse(dat[, 1:4] > 3, "red", "blue"), "none"); }
	# データをデータフレームのリスト形式にする。
	dat <- lapply(if (is.data.frame(dat) || is.matrix(dat)) list(dat) else if (is.list(dat)) dat, as.data.frame, stringsAsFactors = FALSE)
	# 変換できていなかったらエラーを返す。
	dat <- if (is.list(dat) && length(dat) && all(sapply(dat, is.data.frame))) dat else stop("'dat' must be matrix, data.frame, or list of ones.")
	# 各要素を文字列に変換する。
	dat <- lapply(dat, sapply, as.character)
	# NAを"-"に変換する。
	dat <- mapply(ifelse, lapply(dat, is.na), "-", dat, SIMPLIFY = FALSE)
	# 再度データフレームに変換する。
	dat <- lapply(dat, as.data.frame, stringsAsFactors = FALSE)
	# 名前が無ければ、"Sheet 1"の形式で名前を追加する。
	dat <- if (is.null(names(dat))) get("names<-")(dat, sprintf("Sheet %d", seq(dat))) else dat
	# 列名を追加する。
	dat <- if (identical(TRUE, hdr)) mapply(rbind, lapply(dat, names), dat, SIMPLIFY = FALSE) else dat
	# ファイル名のチェック。
	cab <- if (is.element(gsub(".*\\.", "", cab), c("xls", "xlsx"))) path.expand(cab) else stop("'cab' must end with an .xls or .xlsx file extension.")
	# 色データがなければnoneで統一する。
	clr <- if (is.null(clr)) lapply(dat, function(x) matrix("none", nrow(x), ncol(x))) else clr
	# 色データをリスト形式にする。
	clr <- lapply(if (is.data.frame(clr) || is.matrix(clr)) list(clr) else if (is.list(clr)) clr, as.data.frame, stringsAsFactors = FALSE)
	# 変換できていなかったらエラーを返す。
	clr <- if (is.list(clr) && length(clr) && all(sapply(clr, is.data.frame))) clr else stop("'clr' must be matrix, data.frame, or list of ones.")
	# 色データの長さが１のときはデータの長さ分繰り返す。長さが一致しないときはエラーを返す。
	clr <- if (length(clr) == length(dat)) clr else if (length(clr) == 1) rep(clr, length(dat)) else stop("'clr' must have the same length as 'dat'.")
	# 各要素を文字列に変換する。
	clr <- lapply(clr, sapply, as.character)
	# NAを"none"に変換する。
	clr <- mapply(ifelse, lapply(clr, is.na), "none", clr, SIMPLIFY = FALSE)
	# 名前が無ければ，datの名前を追加する。
	clr <- if (is.null(names(clr))) get("names<-")(clr, names(dat)) else clr
	# 列名の分が足りない場合はnoneを追加する。
	clr <- ifelse(identical(TRUE, hdr) & sapply(clr, nrow) < sapply(dat, nrow), lapply(clr, function(x) rbind("none", x)), clr)
	# データフレームに変換する。
	clr <- lapply(clr, as.data.frame, stringsAsFactors = FALSE)
	# データと色データの大きさを比較して異なる場合はエラーを返す。
	clr <- if (all(sapply(clr, dim) == sapply(dat, dim))) clr else stop("The corresponding elements must have the same dimension in 'clr' and 'dat'.")
	# 読み込むクラスの名前。
	wbs <- c("java.io", "org.apache.poi.ss.usermodel", "org.apache.poi.hssf.usermodel", "org.apache.poi.xssf.usermodel")
	# クラスインポートしたあと検索パスに追加する。
	wbs <- lapply(wbs, function(x) attach(rJava::javaImport(x), name = sprintf("java:%s", x)))
	# ファイル名の拡張子に応じてクラスを選ぶ。
	wbs <- switch(gsub(".*\\.", "", cab), xls = get("HSSFWorkbook"), xlsx = get("XSSFWorkbook"))
	# ファイルに追記する場合はファイルを開く。そうでない場合は新しくクラスを生成する。
	wbs <- if (identical(TRUE, add) && file.exists(cab)) rJava::new(wbs, rJava::new(get("FileInputStream"), cab)) else rJava::new(wbs)
	# フォントオブジェクトを生成する。
	fnt <- rJava::J(wbs, "createFont")
	# フォント名をヒラギノに設定する。
	nul <- rJava::J(fnt, "setFontName", "Hiragino Kaku Gothic Pro W3")
	# フォントサイズを12ポイントに設定する。
	nul <- rJava::J(fnt, "setFontHeightInPoints", rJava::new("jshort", 12L))
	# 使用する色の名前を取り出す。
	ind <- setdiff(unique(toupper(as.vector(unlist(clr)))), "NONE")
	# 色の名前とインデックスの対応リストを作る。
	ind <- sapply(ind, function(x) rJava::J(rJava::J("IndexedColors", "valueOf", x), "getIndex"))
	# パターンのインデックスを追加する。
	ind <- lapply(ind, function(x) list(color = rJava::new("jshort", x), pattern = rJava::new("jshort", 1L)))
	# NONEに対応するリストを追加する。
	ind <- c(ind, list(NONE = list(color = rJava::new("jshort", 64L), pattern = rJava::new("jshort", 0L))))
	# スタイルオブジェクトを色の種類分生成する。
	sty <- lapply(ind, with, rJava::J(wbs, "createCellStyle"))
	# フォントを設定する。
	nul <- lapply(sty, rJava::J, "setFont", fnt)
	# 色を設定する。
	nul <- mapply(rJava::J, sty, "setFillForegroundColor", lapply(ind, "[[", "color"))
	# 色のパターンを設定する。
	nul <- mapply(rJava::J, sty, "setFillForegroundColor", lapply(ind, "[[", "pattern"))
	# datの各要素に対応するシートを作成する。
	for (tag in names(dat)) {
		# シートの作成。
		sht <- rJava::J(wbs, "createSheet", tag)
		# 列方向にループ。
		for (i in seq(nrow(dat[[tag]]))) {
			# 行の作成。
			row <- rJava::J(sht, "createRow", as.integer(i - 1))
			# 行方向にループ。
			for (j in seq(ncol(dat[[tag]]))) {
				# セルの作成。
				cel <- rJava::J(row, "createCell", as.integer(j - 1))
				# 値を入力する。
				rJava::J(cel, "setCellValue", (if (grepl("^-?[0-9]+(\\.[0-9]+)?(e-[0-9]*)?$", dat[[tag]][i, j])) as.numeric else c)(dat[[tag]][i, j]))
				# スタイルを設定する。
				rJava::J(cel, "setCellStyle", sty[[toupper(clr[[tag]][i, j])]])
			}
		}
		# 行方向にループ。
		for (j in seq(ncol(dat[[tag]]))) {
			# 列幅を設定する。
			rJava::J(sht, "setColumnWidth", as.integer(j - 1), as.integer(min(30, max(nchar(dat[[tag]][[j]], type = "width")) + 2) * 256))
		}
	}
	# ファイルに書き込む。
	rJava::J(wbs, "write", rJava::new("FileOutputStream", cab))
	# ファイル名を返す。
	invisible(cab)
}
