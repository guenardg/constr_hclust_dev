
library(constr.hclust)

plot.constr.hclust <- function(x, k, xlim, ylim, xlab, ylab, bg, col, lty, lwd,
                               col.links, links=FALSE, points=TRUE, pch=21L,
                               hybrids=c("change","single","none"), lty.hyb=1L,
                               lwd.hyb=1, col.hyb="black", plot=TRUE,
                               axes=TRUE, cex=1, lwd.pt=1, invert.axes=FALSE,
                               ...) {
  hybrids <- match.arg(hybrids)
  if(missing(lty)) lty <- par()$lty
  if(missing(lwd)) lwd <- par()$lwd
  
  if(!plot&&(dev.cur()==1L))
    stop("Use 'plot=FALSE' only for drawing over as existing plot!")
  if(is.null(x$coords)) {
    class(x) <- "hclust"
    plot(x, cex=cex, ...)
    return(invisible(NULL))
  }
  if(missing(bg)) {
    if(k > 10) {
      bg <- head(rainbow(1.2*k),k)
    } else
      bg <- head(
        c("blue", "gold", "grey70", "cadetblue2", "red", "orange3",
          "coral2", "green", "blueviolet", "grey30")
        ,k
      )
  } else {
    if(length(bg) < k) {
      if(length(bg) != 1L)
        warning("Argument 'bg' has length < k and had to be recycled")
      bg <- rep(bg, length.out=k)
    }
  }
  if(missing(col)) {
    col <- rep("black", k)
  } else
    if(length(col) < k) {
      if(length(col) != 1L)
        warning("Argument 'col' has length < k and had to be recycled")
      col <- rep(col, length.out=k)
    }
  if(any(nna <- is.na(x$height)))
    if(k < sum(nna) + 1L) {
      warning("Impossible to plot the cluster for k = ", k, " because ",
              "the graph provided involves ", sum(nna) + 1L, " non",
              "-connected clusters that cannot be linked at any ",
              "dissimilarity level. This cluster is plotted for k = ",
              sum(nna) + 1L)
      k <- sum(nna) + 1L
    }
  cl <- cutree(x,k)
  coords <- x$coords
  if(invert.axes) coords <- coords[,2L:1L]
  if(plot) {
    if(missing(xlim)) {
      xlim <- range(coords[,1L])
      if((xlim[2L]-xlim[1L])==0)
        xlim <- xlim + c(-0.5,0.5)
    }
    if(missing(ylim)) {
      ylim <- range(coords[,2L])
      if((ylim[2L]-ylim[1L])==0)
        ylim <- ylim + c(-0.5,0.5)
    }
    if(missing(xlab))
      xlab <- if(diff(range(coords[,1L]))==0) "" else "x"
    if(missing(ylab))
      ylab <- if(diff(range(coords[,2L]))==0) "" else "y"
    plot(NA, asp=1, xlim=xlim, ylim=ylim, xlab=xlab, ylab=ylab, axes=axes,
          type="n", cex=cex, ...)
  }
  if(links) {
    if(missing(col.links)) {
      col.links <- bg
    } else
      if(length(col.links) < k) {
        if(length(col.links) != 1L)
          warning("Argument 'col.links' has length < k and had to be recycled")
        col.links <- rep(col.links, length.out=k)
      }
  if(is.null(x$links))
    x$links <- cbind(1L:(nrow(coords)-1L),2L:nrow(coords))
    dev.hold()
    for(i in 1:nrow(x$links)) {
      ## i=1L
      clij <- cl[x$links[i,1L:2L]]
      if(clij[1L]==clij[2L]) {
        segments(coords[x$links[i,1L],1L],
                 coords[x$links[i,1L],2L],
                 coords[x$links[i,2L],1L],
                 coords[x$links[i,2L],2L],
                 col=col.links[clij[1L]], lty=lty, lwd=lwd)
      } else {
        ## The link is an hybrid
        if(hybrids=="change") {
          mid <- c(mean(coords[x$links[i,],1L]),
                   mean(coords[x$links[i,],2L]))
          segments(coords[x$links[i,1L],1L],
                   coords[x$links[i,1L],2L],
                   mid[1L], mid[2L],
                   col=col.links[clij[1L]],
                   lty=lty.hyb,
                   lwd=lwd*lwd.hyb)
          segments(mid[1L], mid[2L],
                   coords[x$links[i,2L],1L],
                   coords[x$links[i,2L],2L],
                   col=col.links[clij[2L]],
                   lty=lty.hyb,
                   lwd=lwd*lwd.hyb)
        } else if(hybrids=="single")
          segments(coords[x$links[i,1L],1L],
                   coords[x$links[i,1L],2L],
                   coords[x$links[i,2L],1L],
                   coords[x$links[i,2L],2L],
                   col=col.hyb,
                   lty=lty.hyb,
                   lwd=lwd*lwd.hyb)
      }
    }
    dev.flush()
  }
  if(points)
    points(x=coords[,1L], y=coords[,2L], bg=bg[cl], lty=lty,
           lwd=lwd.pt*lwd, cex=cex, pch=pch, col=col[cl], ...)
  return(invisible(NULL))
}
