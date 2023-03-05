##### 立命の追加解析をする
library(gplots)

server <- "/home/rstudio/kankyo_220501"
setwd(server)
source("renv/activate.R")

source(file.path(server, "00data/data_forR4.1.R"))

# setwd(.work <- file.path(server, sprintf("%sueno", .date)))

setwd(.work <- file.path(server, sprintf("%sueno", "221227")))
# 221221はいずれかの組み合わせで10以上のCountの遺伝子を用いて解析している．


tpm <- read.csv("gene_tpm.csv", header=T, row.names=1, check.names=F)
count <- read.csv("gene_count.csv", header=T, row.names=1, check.names=F)
fData <- read.csv("fData.csv", header=T, row.names=1, check.names=F)
pData <- read.csv("pData.csv", header=T, row.names=1, check.names=F)
tmm <- read.csv("tcc_all_tmm.csv", header=T, row.names=1, check.names=F)






######################################################
######################################################
## Step3-D: PCA解析
######################################################
######################################################
### 内容
## 発現量(あるいはその平均値)について、PCA(主成分解析)を行い、
## 各PC毎にfactor loadingの高い遺伝子(発現量の大小がその軸の方向によく関連する遺伝子)
## を抽出してxlsにまとめる。さらに、各PCごとのproportion of variance
## (PCいくつまででどのくらい説明できるか)を棒グラフでまとめ、
## PC1-2平面に射影した場合の各sampleの座標をプロットする。
### 変数
### 具体例(161101)から急いで抽出したので適宜注意しながら実行すること。
## dat: いつものafy等に格納されている発現量のテーブル
## colnames(dat): わかりやすい名前にしておく
## pca_thr: factor loading(遺伝子の発現量方向の軸とPCとの角度)の絶対値のthrethold

# datに発現量のmatrixを代入 
dat <- tmm # tmm使うとき
dat <- tpm[rownames(tmm),] # tpm使うとき

# 名前が最後まで残るので見やすい形に変えておく
# colnames(dat) <- notes(afy[[1]])$group
# PCAを行う方向に合わせて行列を転置する
dat <- t(dat)
# サンプルについてPCAを行う
pc <- prcomp(dat, scale. = T)


# factor loading(後述)の絶対値がpca_thr以上のもののみをxlsにまとめる
pca_thr <- 0.9


# 因子負荷量[factor loading](どの成分にどれだけ寄与しているか。[-1,+1]の範囲で値をとり、±1に近いほどその主成分への寄与が大きい)
fl <- sweep(pc$rotation, MARGIN=2, pc$sdev, FUN="*")

## xls出力用にテーブルを整理する
res <- sapply(colnames(fl), function(i){
	sapply(c("plus", "minus"), function(j){
		# PC1に対する因子負荷量(あるいはその逆符号)が0.9を超えるもののみ抽出
		if(j == "plus") {
			res <- fl[fl[,i] > pca_thr, ,drop = F]
			# 0.9を超えるものが少ない場合は上位10個を取り出す
			if(nrow(res) < 10) res <- fl[order(fl[,i], decreasing = T),][1:10,]
			}
		if(j == "minus") {
			res <- fl[fl[,i] < -pca_thr, ,drop = F]
			# 0.9を超えるものが少ない場合は上位10個を取り出す
			if(nrow(res) < 10) res <- fl[order(fl[,i], decreasing = F),][1:10,]
			}
		# 因子負荷量の大きさで並び替える
		if(nrow(res) > 0) res <- res[order(abs(res[,i]), decreasing = T), i,drop = F]
		# 因子負荷量の情報に遺伝子の情報を加える
		res <- cbind(fData[rownames(res), c("Gene.ID", "Gene.Symbol", "Gene.Name")], res)
		colnames(res) <- gsub("PC", "factor loading: PC", colnames(res))
		res
	}, simplify = F)
}, simplify = F)
# 階層構造の消去
res <- unlist(res, recursive = F)
# 出力用に名前の整理
names(res) <- gsub("\\.", "_", names(res))
# 名前だけじゃなく内容も整理しないとエラーが出るので整理
res <- lapply(res, function(r) apply(r, 1:2, function(i) gsub(",", "_", i)))
# ProbeIDを追加
res <- lapply(res, function(r) cbind("ProbeID" = rownames(r), r))
# xlsに出力
microarray.write.xls(res, sprintf("PCA_factor_loading_%s.xls", pca_thr), sh.name = names(res), col.widths = 96)

# PCAの結果の、"各PCのProportion of Variance"をpdf形式で出力する
pdf("PCA_Proportion of Variance.pdf")
par(las = 1)
x <- barplot(summary(pc)$importance["Proportion of Variance",], main = "Importance: Proportion of Variance")
text(x,summary(pc)$importance["Proportion of Variance",],summary(pc)$importance["Proportion of Variance",], srt = 90, adj = 1)
x <- barplot(summary(pc)$importance["Cumulative Proportion",], main = "Importance: Cumulative Proportion")
text(x,summary(pc)$importance["Cumulative Proportion",],summary(pc)$importance["Cumulative Proportion",], srt = 90, adj = 1)
dev.off()


# 色の設定(自動)  ※手動で設定したい場合は適当に
color <- unlist(lapply(as.numeric(as.factor(gsub(".$", "", rownames(pc$x)))), function(i) rainbow(length(unique(gsub(".$", "", rownames(pc$x)))))[i]))
color <- lapply(strsplit(color, ""), function(i) i[2:7])
color <- lapply(color, lapply, function(i) which(i == c(0:9,LETTERS[1:6]))-1)
color <- lapply(color, function(i) c(16*i[[1]]+i[[2]], 16*i[[3]]+i[[4]], 16*i[[5]]+i[[6]]))
color <- lapply(color, function(i) floor(i/2))
color <- lapply(color, lapply, function(i) c(i%/%16, i%%16))
color <- lapply(color, lapply, function(i) c(0:9,LETTERS[1:6])[i+1])
color <- lapply(color, function(i) sprintf("#%s", paste(unlist(i), collapse = "")))
color <- unlist(color)

# PC1-PC2平面に射影した各sampleの座標をプロット
pdf(sprintf("%s_PCA.pdf", .date), width = 210/25.4*sqrt(2), height = 210/25.4,tag=F)
for(p in paste("PC", 2:ncol(pc$x), sep = "")){
par(cex = 1,mar=c(5,5,1,1),fig = c(0,1,0,1),lwd  = 3)
plot(pc$x[,c("PC1",p)], pch = 19,  
xlab = sprintf("PC1 (%.2g%%)", 100*summary(pc)$importance["Proportion of Variance","PC1"]), 
ylab = sprintf("%s (%.2g%%)", p, 100*summary(pc)$importance["Proportion of Variance",p]), 
xlim = c(min(pc$x[,1]), max(pc$x[,1])*1.1), 
col = color
)
segments(x0=min(pc$x[,"PC1"])*1.1, x1=max(max(pc$x[,"PC1"])*1.3), y0=0, y1=0, lty = 3)
segments(x0=0, x1=0, y0=min(pc$x[,"PC2"])*1.1, y1=max(max(pc$x[,p])*1.1), lty = 3)
for(i in unique(rownames(pc$x))){
# 文字を書く位置(適当に自動化してあるので気に入らない場合は手動で設定を変えること)
txtpos <- c(
mean(pc$x[sapply(unique(rownames(pc$x)), function(j) which(rownames(pc$x)%in%j), 
simplify = F)[[i]],"PC1"]), mean(pc$x[sapply(unique(rownames(pc$x)), function(j) which(rownames(pc$x)%in%j), simplify = F)[[i]],p])
)
text(
txtpos[[1]],
txtpos[[2]],
labels = i, col = color[which(rownames(pc$x)%in%i)[1]], pos=ifelse(txtpos[[1]]>0,2,4))
}
}

## PC2,3の描画
for(p in 2:ncol(pc$x)){
	p1 <- paste("PC", p-1, sep = "")
	p2 <- paste("PC", p, sep = "")
	par(cex = 1,mar=c(5,5,1,1),fig = c(0,1,0,1),lwd  = 3)
	plot(pc$x[,c(p-1, p)], pch = 19,  
	xlab = sprintf("%s (%.2g%%)", p1, 100*summary(pc)$importance["Proportion of Variance",p1]), 
	ylab = sprintf("%s (%.2g%%)", p2, 100*summary(pc)$importance["Proportion of Variance",p2]), 
	xlim = c(min(pc$x[,p1]), max(pc$x[,p1])*1.1), 
	col = color
	)
	segments(x0=min(pc$x[,p1])*1.1, x1=max(max(pc$x[,p1])*1.3), y0=0, y1=0, lty = 3)
	segments(x0=0, x1=0, y0=min(pc$x[,p2])*1.1, y1=max(max(pc$x[,p2])*1.1), lty = 3)
	for(i in unique(rownames(pc$x))){
	# 文字を書く位置(適当に自動化してあるので気に入らない場合は手動で設定を変えること)
	txtpos <- c(
	mean(pc$x[sapply(unique(rownames(pc$x)), function(j) which(rownames(pc$x)%in%j), 
	simplify = F)[[i]],p1]), mean(pc$x[sapply(unique(rownames(pc$x)), function(j) which(rownames(pc$x)%in%j), simplify = F)[[i]],p2])
	)
	text(
	txtpos[[1]],
	txtpos[[2]],
	labels = i, col = color[which(rownames(pc$x)%in%i)[1]], pos=ifelse(txtpos[[1]]>0,2,4))
	}
}

dev.off()

if(0){
	# PCAの結果の、PC1~3(Varianceの大きい順に3成分)を軸とする3次元空間に、各組み合わせの座標をplotする
	# ここから先はR3以上でないと動作しないので注意
	load.packages("rgl")
	plot3d(x = pc$x[,1], y = pc$x[,2], z = pc$x[,3], xlab="PC1", ylab="PC2", zlab="PC3", col = "red")
	text3d(x = pc$x[,1], y = pc$x[,2], z = pc$x[,3], text = rownames(pc$x), cex = 0.7)
	rglwidget()
	# htmlで保存
	# 保存する前に拡大、縮小、回転などを行っていた場合はその情報も記録される
	writeWebGL("expPCA.html")
	# Rstudio ではこれが機能しないので（多分クロームが入っていないから）Viewer からexportする
}