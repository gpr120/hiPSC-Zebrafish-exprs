# by okada_n 120328
# charset=utf-8
# 
# title:
#   [Plot] Barplot 3D
# description:
#   http://addictedtor.free.fr/graphiques/graphcode.php?graph=161

plot.barplot3d2 <- function(mat, row, size = 3, space = 0.5, col.lab = NULL, row.lab = NULL, z.lab = NULL, col = "#aaaaaa", grid = "white", ...) {
	col <- length(mat) / row
	
}

plot.barplot3d <- function(heights, rows, transp="f0", theta=25, phi=25, bar.size=3, bar.space=0.5,
    col.lab=NULL, row.lab=NULL, z.lab=NULL, col.bar=c("#44ff58","#5844ff","#ff5844"), grid="white", ..., ver = 1) {
    # Set parameters
    cols    = length(heights)/rows
    calkdl  = (bar.size + bar.space)
    slupki  = matrix(heights, nrow=cols, ncol=rows)
    zakres  = pretty(0:ceiling(max(heights, na.rm=T)*1.1))
    odstep  = bar.space/2 + bar.size/2
    colors  = paste(col.bar, transp, sep="")
    shcols  = colors
    for (i1 in 1:length(colors)) shcols[i1] = paste("#",
	as.hexmode(round(unclass(as.hexmode(substr(colors[i1],2,3)))*0.8,0)),
	as.hexmode(round(unclass(as.hexmode(substr(colors[i1],4,5)))*0.8,0)),
	as.hexmode(round(unclass(as.hexmode(substr(colors[i1],6,7)))*0.8,0)),
    substr(colors[i1],8,9), sep="")

    # Prepare the grid for bars
    y = x = 0
    for (i1 in (1:rows)-1) y = c(y, bar.space/2+i1*calkdl, bar.space/1.99+i1*calkdl,
        bar.space/2+bar.size+i1*calkdl, bar.space/1.99+bar.size+i1*calkdl,
        bar.space+bar.size+i1*calkdl)
    for (i1 in (1:cols)-1) x = c(x, bar.space/2+i1*calkdl, bar.space/1.99+i1*calkdl,
        bar.space/2+bar.size+i1*calkdl, bar.space/1.99+bar.size+i1*calkdl,
        bar.space+bar.size+i1*calkdl)

    # Prepare the z matrix of bar heights
    z = matrix(nrow=length(x), ncol=length(y))
    for (i1 in (1:cols)-1)  for (i2 in (1:rows)-1) z[c(2:5)+5*i1,c(2:5)+5*i2] = 0
    for (i1 in (1:cols)-1)  for (i2 in (1:rows)-1) z[c(3:4)+5*i1,c(3:4)+5*i2] = slupki[i1+1,i2+1]

    # Prepare colors matrix
    fill   = matrix(nrow=length(x)-1, ncol=length(y)-1)
    for (i1 in (1:rows)-1) {
        fill[,c(2:5)+5*i1] = colors[i1+1]
        for (i2 in (1:cols)-1)  fill[c(4:5)+5*i2,c(3:5)+5*i1] = shcols[i1+1]
        }

    # Prepare area for plotting
    rys = persp(x, y, matrix(nrow=length(x), ncol=length(y)), col=fill, scale=F, theta=theta,
        phi=phi, zlim = range(zakres), lphi=44, ltheta=-10, axes=F, ...)

    # Add walls
    polygon(rbind(trans3d(0,0,0,rys), trans3d(0,0,max(zakres),rys), trans3d(0,max(y),max(zakres),rys),
        trans3d(0,max(y),0,rys)), col="#aaaaaa66")
    polygon(rbind(trans3d(0,max(y),0,rys), trans3d(0,max(y),max(zakres),rys), trans3d(max(x),
        max(y),max(zakres),rys), trans3d(max(x),max(y),0,rys)), col="#aaaaaa50")
    polygon(rbind(trans3d(0,0,0,rys), trans3d(0,max(y),0,rys), trans3d(max(x),max(y),0,rys),
        trans3d(max(x),0,0,rys)), col="#aaaaba66")

    # Add grid lines & numbers
    for (i1 in zakres) {
        lines(rbind(trans3d(0,0,i1,rys), trans3d(0, max(y),i1,rys)), lwd=2, col=grid)
        lines(rbind(trans3d(0,max(y),i1,rys), trans3d(max(x), max(y),i1,rys)), lwd=2, col=grid)
        text(trans3d(-(calkdl*cols)*0.04,0,i1,rys), labels=i1, adj=1, cex=0.9)
        }
    text(trans3d(-(calkdl*cols)*0.04,0,max(zakres)*1.05,rys), labels=z.lab, adj=0.8, cex=0.9)

    # Add ticks & text
    for (i1 in (1:cols)-1) {
        lines(rbind(trans3d((odstep+calkdl*i1),0,0,rys), trans3d((odstep+calkdl*i1),
            -(calkdl*rows)*0.05,0,rys)))
        if (!is.null(col.lab)) text(trans3d((odstep+calkdl*i1),-(calkdl*rows)*0.1,0,rys),
            col.lab[i1+1], adj=1, cex=0.9)
        }
    for (i1 in (1:rows)-1) {
        lines(rbind(trans3d(max(x),(odstep+calkdl*i1),0,rys), trans3d(max(x)+(calkdl*cols)*0.03,
            (odstep+calkdl*i1),0,rys)))
        if (!is.null(col.lab)) text(trans3d(max(x)+(calkdl*cols)*0.05,(odstep+calkdl*i1),0,rys),
            row.lab[i1+1], adj=0, cex=0.9)
        }
	
    # Plot the bars!
    par(new = TRUE)
    persp(x, y, z, col=fill, scale=F, theta=theta, phi=phi, zlim = range(zakres), lphi=44, ltheta=-10, shade=0.4, axes=F)
    
    invisible(rys)
}

   