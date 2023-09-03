% by naoi okada
% charset=utf-8
\encoding{utf-8}
\name{write.xls}
\alias{write.xls}
\title{
Write List of data.frame to Excel Workbook
}
\description{
データフレームをエクセルファイルに書き込む。
}
\usage{
write.xls (dat, cab, add = FALSE, hdr = TRUE, clr = NULL)
}
\arguments{
\item{dat}{書き込むデータ。行列、データフレーム、またはそれらのリスト。リストの場合、名前がシート名に使用される。}
\item{cab}{書き込むファイル名。}
\item{add}{既存のエクセルファイルに追加するかどうか。ファイルが存在しない場合は常に新規作成する。}
\item{hdr}{列名を追加するかどうか。}
\item{col}{色データ。datを同じ構造のデータフレームのリスト。行名，列名の色は省略された場合noneが指定される。}
}
\details{
処理は時間がかかる。
色は次から指定する。\cr
none, black, white, red, bright_green, blue, yellow, pink, turquoise, 
dark_red, green, dark_blue, dark_yellow, violet, teal, grey_25_percent, 
grey_50_percent, cornflower_blue, maroon, lemon_chiffon, orchid, coral, 
royal_blue, light_cornflower_blue, sky_blue, light_turquoise, light_green, 
light_yellow, pale_blue, rose, lavender, tan, light_blue, aqua, lime, gold, 
light_orange, orange, blue_grey, grey_40_percent, dark_teal, sea_green, 
dark_green, olive_green, brown, plum, indigo, grey_80_percent, automatic
}
\examples{
write.xls(matrix(1:100, 10), "test1.xls")
write.xls(iris, "test2.xls", clr = matrix(ifelse(sapply(iris, class) == "numeric", "none", "red"), nrow(iris), ncol(iris), byrow = TRUE))
write.xls(list(Loblolly = Loblolly, Orange = Orange), "test3.xls")
}
