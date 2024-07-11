##
### Projet de transcodage de la fonction constr.clust de Pierre Legendre
##
## rm(list=ls())
compile <- function() {
    try(dyn.unload("constr.hclust/src/constr.hclust.so"),silent=TRUE)
    system("R CMD SHLIB constr.hclust/src/constr.hclust.c")
    dyn.load("constr.hclust/src/constr.hclust.so")
    source("constr.hclust/R/constr.hclust.R")
    source("constr.hclust/R/constr.lshclust.R")
    source("constr.hclust/R/plot.constr.hclust.R")
}
## compile()
library(constr.hclust)
##
### Exemple du livre:
if(FALSE) {
  dat <- c(41,42,25,38,50,30,41,43,43,41,30,50,38,25,42,41)
  coord.dat <- matrix(c(1,3,5,7,2,4,6,8,1,3,5,7,2,4,6,8,
                        4.4,4.4,4.4,4.4,3.3,3.3,3.3,3.3,
                        2.2,2.2,2.2,2.2,1.1,1.1,1.1,1.1),16,2)
  listW <- spdep::nb2listw(spdep::tri2nb(coord.dat), style="B")  ## str(listW)
  links.mat.dat <- spdep::listw2mat(listW)
  D.dat <- dist(dat)
  neighbors <- spdep::listw2sn(listW)[,1:2]
  ##
  ## X11()
  {
      plot(coord.dat, type='n',asp=1)
      title("Delaunay triangulation")
      text(coord.dat, labels=as.character(as.matrix(dat)), pos=3)
      for(i in 1:nrow(neighbors))
          lines(rbind(coord.dat[neighbors[i,1],],
                      coord.dat[neighbors[i,2],]))
  }
  ##
  grpWD2_hclust <- hclust(D.dat, method="ward.D2")
  plot(grpWD2_hclust, hang=-1)
  ## constr.hclust::map.hclust(grpWD2_hclust, coord.dat, 3, 8)
  grpWD2_constr_hclust <- constr.hclust(D.dat, method="ward.D2")
  plot(grpWD2_constr_hclust, hang=-1)
  print.default(grpWD2_constr_hclust)
  ##
  ## str(grpWD2_constr_hclust)
  ##
  ## Sous-contrainte de contiguité spatiale telle que décrite par 'neighbors'
  grpWD2cst_constr_hclust <- constr.hclust(D.dat, method="ward.D2", neighbors,
                                           coord.dat)
  ## stats:::plot.hclust(grpWD2cst_constr_hclust, hang=-1)
  ##
  ### Plot the results on a map with k=3 clusters:
  plot(grpWD2cst_constr_hclust, k=3, links=TRUE, las=1, xlab="Eastings",
       ylab="Northings", cex=3, lwd=3)
  ## plot(grpWD2cst_constr_hclust, k=3, links=TRUE, las=1, xlab="Eastings",
  ##      ylab="Northings", cex=3, lwd=3, invert.axes=TRUE)
  plot(grpWD2cst_constr_hclust, k=3, links=TRUE, las=1, xlab="Eastings",
       ylab="Northings", cex=3, lwd=3, lwd.hyb=0.25, lty.hyb=1L)
  plot(grpWD2cst_constr_hclust, k=3, links=TRUE, hybrids="single", las=1,
       xlab="Eastings", ylab="Northings", cex=3, lwd=3, lwd.hyb=0.5)
  plot(grpWD2cst_constr_hclust, k=3, links=TRUE, hybrids="single", las=1,
       xlab="Eastings", ylab="Northings", cex=3, lwd=3, lwd.hyb=0.5,
       lty.hyb=3L)
  plot(grpWD2cst_constr_hclust, k=3, links=TRUE, hybrids="none", las=1,
       xlab="Eastings", ylab="Northings", cex=3, lwd=2)
  ##
  if(FALSE) {
    ## For testing the plot=FALSE functionality:
    plot(grpWD2cst_constr_hclust, k=3, links=TRUE, las=1, plot=FALSE,
         xlab="Eastings", ylab="Northings", cex=3, lwd=3)
    plot(NA,xlim=c(1,8),ylim=c(1,5),asp=1)
    plot(grpWD2cst_constr_hclust, k=3, links=TRUE, plot=FALSE,
         cex=3, lwd=3)
  }
  ##
  ### Generics from hclust can be used, for instance to obtain a list
  ### of members for each cluster:
  cutree(grpWD2cst_constr_hclust, k=3)
  ##
  ### Now with k=5 clusters.
  plot(grpWD2cst_constr_hclust, k=5, links=TRUE, las=1, xlab="Eastings",
       ylab="Northings", cex=3, lwd=4, lwd.pt=0.25)
  cutree(grpWD2cst_constr_hclust, k=5)
  ##
}
##
### Chronological case: Doubs river fishes
if(FALSE) {
  data(Doubs, package="codep")
  ## 
  ## Doubs.env
  ## Doubs.geo <-
  ##     SpatialPointsDataFrame(
  ##         coords=Doubs.geo[,c("Lon","Lat")],
  ##         data=data.frame(Doubs.geo[,c("DFS","Alt")],Doubs.env),
  ##         proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  ##     )
  ## Doubs.geo <-
  ##     spTransform(
  ##         Doubs.geo,
  ##         CRS("+proj=utm +zone=32 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  ##     )
  ## colnames(Doubs.geo@data)[c(1L,2L,11L)] <- c("dfs","alt","bod")
  ## Doubs <- list(geo=Doubs.geo, fish=Doubs.fish)
  ## rm(Doubs.geo, Doubs.fish)
  ##
  ## save(Doubs, file="./constr.hclust/data/Doubs.rda")
  ##
  ## load("./constr.hclust/data/Doubs.rda")
  ##
  ## The example begins here:
  data(doubs, package="ade4")
  Doubs.hel <- vegan::decostand(doubs$fish, "hellinger")
  Doubs.D <- dist(Doubs.hel)
  grpWD2cst_fish <- constr.hclust(Doubs.D, method="ward.D2", chron=TRUE,
                                  coords=as.matrix(doubs$xy))
  ## stats:::plot.hclust(grpWD2cst_fish, hang=-1)
  ## str(grpWD2cst_fish)
  plot(grpWD2cst_fish, k=10, las=1, xlab="Eastings (km)",
       ylab="Northings (km)", cex=3, lwd=3)
  plot(grpWD2cst_fish, k=10, las=1, xlab="Northings (km)",
       ylab="Eastings (km)", cex=3, lwd=3, invert.axes=TRUE)
  grpWD2cst_fish <- constr.hclust(Doubs.D, method="ward.D2", chron=TRUE,
                                  coords=as.matrix(doubs$xy))
  ##
  plot(grpWD2cst_fish, links=TRUE, k=10, las=1, xlab="Eastings (km)",
       ylab="Northings (km)", cex=2, lwd=5, lwd.pt=0.5,
       hybrids="single", lty.hyb=3, lwd.hyb=0.5, col="white")
}
##
### Another two-dimensional example: Scotch Whiskeys
if(FALSE) {
  data(ScotchWhiskey,package="constr.hclust")
  ##
  grpWD2cst_ScotchWhiskey <-
    lapply(
      ScotchWhiskey$dist,
      constr.hclust,
      links=ScotchWhiskey$neighbors@data,
      coords=ScotchWhiskey$geo@coords/1000
    )
  ##
  ### The four D matrices (nose, body, palate, finish), represented as vectors
  ### in the \code{scotchWiskey} data file, are combined as follows to produce
  ### a single distance matrix integrating all four types of tastes:
  norm <- ScotchWhiskey$dist
  ScotchWhiskey[["norm"]] <-
    sqrt(norm$nose^2 + norm$body^2 + norm$palate^2 + norm$finish^2)
  ##
  ### This example shows how to apply const.clust to a single D matrix when
  ### the data file contains several matrices.
  grpWD2cst_ScotchWhiskey[["norm"]] <-
    constr.hclust(
      d=ScotchWhiskey[["norm"]],method="ward.D2",
      ScotchWhiskey$neighbors@data,
      coords=ScotchWhiskey$geo@coords/1000
    )
  ##
  ### A fonction to plot the clustering results:
  plotWhiskey <- function(wh, k) {
    par(fig=c(0,1,0,1))
    plot(grpWD2cst_ScotchWhiskey[[wh]], k=k, links=TRUE, las=1,
         xlab="Eastings (km)", ylab="Northings (km)", cex=0.01, lwd=3,
         main=sprintf("Feature: %s",wh),hybrids="single",col.hyb="grey10",
         lwd.hyb=0.5,lty.hyb=3L)
    text(ScotchWhiskey$geo@coords/1000,labels=1:length(ScotchWhiskey$geo))
    legend(x=375, y=700, lty=1L, lwd=3, col=rainbow(1.2*k)[1L:k],
           legend=sprintf("Group %d",1:k), cex=1.25)
    SpeyZoom <- list(xlim=c(314.7,342.2), ylim=c(834.3,860.0))
    rect(xleft=SpeyZoom$xlim[1L], ybottom=SpeyZoom$ylim[1L],col="#E6E6E680",
         xright=SpeyZoom$xlim[2L], ytop=SpeyZoom$ylim[2L], lwd=2, lty=1L)
    par(fig=c(0.01,0.50,0.46,0.99), new=TRUE)
    plot(grpWD2cst_ScotchWhiskey[[wh]], xlim=SpeyZoom$xlim,
         ylim=SpeyZoom$ylim, k=k, links=TRUE, las=1, xlab="", ylab="",
         cex=0.01, lwd=3, axes=FALSE,hybrids="single",col.hyb="grey10",
         lwd.hyb=0.5,lty.hyb=3L)
    text(ScotchWhiskey$geo@coords/1000,labels=1:length(ScotchWhiskey$geo))
    rect(xleft=SpeyZoom$xlim[1L], ybottom=SpeyZoom$ylim[1L],
         xright=SpeyZoom$xlim[2L], ytop=SpeyZoom$ylim[2L], lwd=2, lty=1L)
  }
  ##
  ### Plot the clustering results on the map of Scotland for 5 groups.
  ### The inset map shows the Speyside distilleries in detail:
  plotWhiskey("nose", 5L)
  plotWhiskey("body", 5L)
  plotWhiskey("palate", 5L)
  plotWhiskey("finish", 5L)
  plotWhiskey("norm", 5L)
}
##
### Another chronological example: The Tiahura fish transect
##
if(FALSE) {
  data("Tiahura",package="adespatial")
  tiah.jac <- ade4::dist.binary(Tiahura$fish, method=1L)
  tiah.chclust <-
    constr.hclust(d=tiah.jac, coords=Tiahura$habitat[,"distance"], chron=TRUE)
  par(mar=c(5,7,2,2),mfrow=c(1L,1L))
  plot(NA,xlim=range(Tiahura$habitat[,"distance"]),ylim=c(0.5,4.5),yaxt="n",
       ylab="",xlab="Distance (m)")
  parts <- c(2,3,5,7)
  for(i in 1L:length(parts)) {
    ## i=1L
    tiah.chclust$coords[,"y"] <- i
    plot(tiah.chclust, parts[i], link=TRUE, lwd=3, hybrids="none",
         lwd.pt=0.5, cex=3, plot=FALSE)
  }
}
##
### Not retained:
if(FALSE) {
  m <- 5L
  col <- rainbow(m + 1L)
  for(i in c("ward.D", "ward.D2", "single", "complete", "average", "mcquitty",
             "centroid", "median", "flexible")) {
    ## i="ward.D"
    tmp <- constr.hclust(D.dat, method=i, neighbors)
    plot(x=coord.dat[,1L],y=coord.dat[,2L],main=i,xlab="x",ylab="y",asp=1,
         type="n")
    for(i in 1:nrow(neighbors))
      lines(rbind(coord.dat[neighbors[i,1],], coord.dat[neighbors[i,2],]))
    points(x=coord.dat[,1L],y=coord.dat[,2L],pch=21L,cex=3,
           bg=col[cutree(tmp,m)])
    if(is.null(locator(1L))) break
  }
  rm(col,i,tmp)
}
##
### A large simulated example:
if(FALSE) {
  library(magrittr)
  library(spdep)
  coord.big <-
    cbind(
      (50*cos(pi*2*(1:6)/6)) %>%
        sapply(
          function(mn,n,sd) rnorm(n,mn,sd),n=2000,sd=2
        ) %>%
        as.numeric,
      (50*sin(pi*2*(1:6)/6)) %>%
        sapply(
          function(mn,n,sd) rnorm(n,mn,sd),n=2000,sd=2
        ) %>%
        as.numeric
    )
  dat.big <-
    c(-15,-10,-5,0,5,10) %>%
    sapply(
      function(mn,n,sd) rnorm(n,mn,sd),n=2000,sd=0.1
    ) %>%
    as.numeric
  ## length(dat.big)
  D.big <- dat.big %>% dist
  neighbors.big <-
    (coord.big %>%
       tri2nb %>%
       nb2listw(style="B") %>%
       listw2sn)[,1:2]
  ##
  ## pryr::object_size(coord.big,dat.big,D.big,neighbors.big)
  ## X11()
  plot(coord.big, type='n', asp=1, main="Delaunay triangulation")
  {
    dev.hold()
    for(i in 1:nrow(neighbors.big))
      lines(rbind(coord.big[neighbors.big[i,1],],
                  coord.big[neighbors.big[i,2],]))
    dev.flush()
  }
  ##
  {
    start.time = Sys.time()
    res.big <- constr.hclust(D.big, method="ward.D2", neighbors.big)
    end.time = Sys.time()
    end.time - start.time
  }
  {
    start.time = Sys.time()
    part <- cutree(res.big,100L)
    end.time = Sys.time()
    end.time - start.time
  }
  ##
  k <- 6L
  col <- rainbow(k+5L)[cutree(res.big, k)]
  points(coord.big[,1], coord.big[,2], pch=21L, col=col, bg=col)
  ##
  rm(coord.big,D.big,dat.big,end.time,neighbors.big,res.big,start.time)
  gc()
}
##
if(FALSE) {
  wardsExample <- c(2,6,5,6,2,2,2,0,0,0)
  ## sum((wardsExample-mean(wardsExample))^2)
  hcl <- constr.hclust(dist(wardsExample),method="ward.D2")
  ## plot(hcl)
  ##
  n <- length(wardsExample)
  ## sum(wardsExample^2) - sum(wardsExample)^2/n
  ##
  ## En fait: pas besoin de calculer les moyennes, il faut accumuler les valeurs
  ## et les carrés des valeurs. Ça suffira. En fait, on a pas vraiment besoin des
  ## moyennes. Pour des grands jeux de données, il pourrait être nécessaire de
  ## downscaler les valeurs pour éviter de se retrouver barré par la précision
  ## des floating points.
  ##
  x <- wardsExample
  xx <- wardsExample^2
  membr <- rep(1L,n)
  flag <- rep(TRUE,n)
  merge <- matrix(NA,n-1L,2L)
  crit <- rep(NA,n-1L)
  ##
  for(k in 1L:(n-1L)) {
    ## k=2L
    ss <- Inf
    nni <- nnj <- NA
    for(i in 1L:(n-1L))   ## i=1L
      if(flag[i])
        for(j in (i+1L):n)  ## j=i+1L
          if(flag[j]) {
            ssi <- sum(xx[c(i,j)])-sum(x[c(i,j)])^2/sum(membr[c(i,j)])
            ## Pour plusieurs variables, il faudra accumuler...
            if(ssi<ss) {
              ss <- ssi
              nni <- i
              nnj <- j
            }
          }
    merge[k,] <- c(nni,nnj)
    crit[k] <- sqrt(ss)
    x[nni] <- x[nni] + x[nnj]
    xx[nni] <- xx[nni] + xx[nnj]
    membr[nni] <- membr[nni] + membr[nnj]
    flag[nnj] <- FALSE
  }
  ## cbind(merge,crit)
  ##
  hcl2 <- constr.lshclust(wardsExample)
  ## hcl2 <- constr.lshclust(wardsExample,output="S")
  plot(hcl2)
  ## cbind(merge,crit)
  ## cbind(hcl2$merge,hcl2$height)
  ## plot(hcl)
  links <- list(from=c(1,1,2,2,3,3,4,5,5,6,6,7,8,8,9),
                to=c(2,3,4,5,5,6,7,7,8,8,10,9,9,10,10))
  coords <- cbind(x=c(2,1,3,0,2,4,1,3,2,4),y=c(4,3,3,2,2,2,1,1,0,0))
  ##
  chcl <- constr.lshclust(x=wardsExample, links=links, coords=coords)
  plot(chcl,k=5,link=TRUE)
  ## chcl$merge
  ## chcl$height
  ## cutree(chcl,k=5)
  ##
  grpWD2cst_constr_lshclust <-
    constr.lshclust(dat, neighbors, coord.dat)
  ##
  plot(grpWD2cst_constr_lshclust, k=3, links=TRUE, hybrids="single", las=1,
       xlab="Eastings", ylab="Northings", cex=3, lwd=3, lwd.hyb=0.5,
       lty.hyb=3L)
  plot(grpWD2cst_constr_lshclust, k=4, links=TRUE, hybrids="single", las=1,
       xlab="Eastings", ylab="Northings", cex=3, lwd=3, lwd.hyb=0.5,
       lty.hyb=3L)
  plot(grpWD2cst_constr_lshclust, k=5, links=TRUE, hybrids="single", las=1,
       xlab="Eastings", ylab="Northings", cex=3, lwd=3, lwd.hyb=0.5,
       lty.hyb=3L)
  ##
  par(mfrow=c(1,2))
  stats:::plot.hclust(grpWD2cst_constr_hclust, hang=-1)
  stats:::plot.hclust(grpWD2cst_constr_lshclust, hang=-1)
}
##
if(FALSE) {
  data("Oribates",package="constr.hclust")
  ##
  mite.chclust <- constr.hclust(
    d=dist(vegan::decostand(Oribates$fau,method="hellinger")),
    links=Oribates$link,
    coords=as.matrix(Oribates$xy)
  )
  par(mfrow=c(4,1),mar=c(0.5,0,0.5,0))
  cols <- c("turquoise","orange","blue","violet","green","red","purple")
  parts <- c(2,3,5,7)
  for(i in 1L:length(parts)) {
    plot(NA, xlim=c(0,10), ylim=c(-0.1,2.5), xaxs="i", yaxs="i",
         asp=1, axes=FALSE)
    arrows(x0=0.15, x1=1.15, y0=0.1, y1=0.1, code=3, length=0.05,
           angle=90, lwd=2)
    text(x=0.65, y=0, labels="1m", cex=1.5)
    plot(mite.chclust, parts[i], links=TRUE, plot=FALSE,
         bg=cols[round(seq(1,length(cols),length.out=parts[i]))], lwd=4,
         cex=2.5, hybrids="single", lwd.hyb=0.25, lty.hyb=3)
    text(x=0.25, y=2.25, labels=LETTERS[i], cex=2.5)
  }
  ##
  mite.clshclust <- constr.lshclust(
    x=vegan::decostand(Oribates$fau,method="hellinger"),
    links=Oribates$link,
    coords=as.matrix(Oribates$xy)
  )
  for(i in 1L:length(parts)) {
    plot(NA, xlim=c(0,10), ylim=c(-0.1,2.5), xaxs="i", yaxs="i",
         asp=1, axes=FALSE)
    arrows(x0=0.15, x1=1.15, y0=0.1, y1=0.1, code=3, length=0.05,
           angle=90, lwd=2)
    text(x=0.65, y=0, labels="1m", cex=1.5)
    plot(mite.clshclust, parts[i], links=TRUE, plot=FALSE,
         bg=cols[round(seq(1,length(cols),length.out=parts[i]))], lwd=4,
         cex=2.5, hybrids="single", lwd.hyb=0.25, lty.hyb=3)
    text(x=0.25, y=2.25, labels=LETTERS[i], cex=2.5)
  }
  ##
  getSS <- function(x, cl, k) {
    f <- function(x,cl,k)
      tapply(x,cutree(cl,k),function(x) sum((x-mean(x))^2))
    rs <- rowSums(apply(x,2L,f,cl=cl, k=k))
    return(c(rs,total=sum(rs)))
  }
  res <- matrix(NA,69L,2L)
  for(i in 1L:69L) {
    res[i,1L] <- tail(
      getSS(vegan::decostand(Oribates$fau,method="hellinger"),
            mite.clshclust,i+1L),1L)
    res[i,2L] <- tail(
      getSS(vegan::decostand(Oribates$fau,method="hellinger"),
            mite.chclust,i+1L),1L)
  }
  cbind(res,res[,1L]-res[,2L],2:70)
  par(mfrow=c(1,2),mar=c(0,4,4,0))
  stats:::plot.hclust(mite.chclust)
  stats:::plot.hclust(mite.clshclust)
  ##
}
##
### Large scale example for constr.lshclust:
if(FALSE) {
  library(magrittr)
  library(spdep)
  coord.big <-
    cbind(
      (50*cos(pi*2*(1:6)/6)) %>%
        sapply(
          function(mn,n,sd) rnorm(n,mn,sd),n=10000,sd=2
        ) %>%
        as.numeric,
      (50*sin(pi*2*(1:6)/6)) %>%
        sapply(
          function(mn,n,sd) rnorm(n,mn,sd),n=10000,sd=2
        ) %>%
        as.numeric
    )
  dat.big <-
    c(-15,-10,-5,0,5,10) %>%
    sapply(
      function(mn,n,sd) rnorm(n,mn,sd),n=10000,sd=0.1
    ) %>%
    as.numeric
  ## length(dat.big)
  neighbors.big <-
    (coord.big %>%
       tri2nb %>%
       nb2listw(style="B") %>%
       listw2sn)[,1:2]
  ##
  ## pryr::object_size(coord.big,dat.big,D.big,neighbors.big)
  ## X11()
  par(mfrow=c(1L,1L))
  plot(coord.big, type='n', asp=1, main="Delaunay triangulation")
  {
    dev.hold()
    for(i in 1:nrow(neighbors.big))
      lines(rbind(coord.big[neighbors.big[i,1],],
                  coord.big[neighbors.big[i,2],]))
    dev.flush()
  }
  ##
  {
    start.time = Sys.time()
    res.big <- constr.lshclust(dat.big, neighbors.big)
    end.time = Sys.time()
    end.time - start.time
  }
  {
    start.time = Sys.time()
    part <- cutree(res.big,100L)
    end.time = Sys.time()
    end.time - start.time
  }
  ##
  k <- 6L
  col <- rainbow(k+5L)[cutree(res.big, k)]
  points(coord.big[,1], coord.big[,2], pch=21L, col=col, bg=col)
  ##
  rm(coord.big,dat.big,end.time,neighbors.big,res.big,start.time)
  gc()
}
##
### constr.lshclust examples
##
### First example: Artificial map data from Legendre & Legendre
###                (2012, Fig. 13.26): n = 16
##
dat <- c(41,42,25,38,50,30,41,43,43,41,30,50,38,25,42,41)
coord.dat <- matrix(c(1,3,5,7,2,4,6,8,1,3,5,7,2,4,6,8,
                      4.4,4.4,4.4,4.4,3.3,3.3,3.3,3.3,
                      2.2,2.2,2.2,2.2,1.1,1.1,1.1,1.1),16,2)
##
### Obtaining a list of neighbours:
listW <- nb2listw(tri2nb(coord.dat), style="B")
links.mat.dat <- listw2mat(listW)
neighbors <- listw2sn(listW)[,1:2]
##
### Display the points:
par(mfrow=c(1L,1L))
plot(coord.dat, type='n',asp=1)
title("Delaunay triangulation")
text(coord.dat, labels=as.character(as.matrix(dat)), pos=3)
for(i in 1:nrow(neighbors))
    lines(rbind(coord.dat[neighbors[i,1],],
                coord.dat[neighbors[i,2],]))
##
### Clustering without a contiguity constraint;
### the result is represented as a dendrogram:
grpWD2_constr_lshclust <- constr.lshclust(x=dat, output="RSS")
plot(grpWD2_constr_lshclust, hang=-1)
##
### Clustering with a contiguity constraint described by a list of
### links:
grpWD2cst_constr_lshclust <-
    constr.lshclust(
        dat, neighbors,
        coord.dat, output="RSS")
##
### Plot the results on a map with k=3 clusters:
plot(grpWD2cst_constr_lshclust, k=3, links=TRUE, las=1, xlab="Eastings",
     ylab="Northings", cex=3, lwd=3)
##
### Generic functions from hclust can be used, for instance to obtain
### a list of members of each cluster:
cutree(grpWD2cst_constr_lshclust, k=3)
##
### Now with k=5 clusters:
plot(grpWD2cst_constr_lshclust, k=5, links=TRUE, las=1, xlab="Eastings",
     ylab="Northings", cex=3, lwd=3)
cutree(grpWD2cst_constr_lshclust, k=5)
##
## End of the artificial map example
##
### Second example: Fish community composition along the Doubs River,
### France. The sequence is analyzed as a case of chronological
### clustering, substituting space for time.
##
data(doubs, package="ade4")
Doubs.hel <- sqrt(doubs$fish / rowSums(doubs$fish))
Doubs.hel[rowSums(doubs$fish)==0,] <- 0
grpWD2cst_fish <- constr.lshclust(x=Doubs.hel, chron=TRUE,
                                  coords=as.matrix(doubs$xy))
##
plot(grpWD2cst_fish, k=5, las=1, xlab="Eastings (km)",
     ylab="Northings (km)", cex=3, lwd=3)
##
### Repeat the plot with other values of k (number of groups)
##
## End of the Doubs River fish assemblages example
##
### Third example: Scotch Whiskey distilleries clustered using four tasting
### scores (nose, body, palate, and finish) constrained with respect to the
### distillery locations in Scotland.
##
## Documentation file about the Scotch Whiskey data: ?ScotchWhiskey
##
data(ScotchWhiskey, package="constr.hclust")
### Cluster analyses for the colour, nose, body, palate, and finish using
### least squares on the basis of the Mahalanobis metric.
##
### Combining the data matrices:
cols <- contr.treatment(
    n = nlevels(ScotchWhiskey$colour)
)[ScotchWhiskey$colour,]
dimnames(cols) <- list(
    rownames(ScotchWhiskey$geo[,"Distillery"]),
    levels(ScotchWhiskey$colour)[-1L]
)
WhiskeyDat <- cbind(
    cols,
    ScotchWhiskey$body,
    ScotchWhiskey$palate,
    ScotchWhiskey$finish
)
rm(cols)
##
### Transforming WhiskeyDat into an orthonormal matrix using the Cholesky
### factorization: the least squares will relate to the Mahalanobis metric.
WhiskeyTr <- WhiskeyDat %*% solve(chol(cov(WhiskeyDat)))
grpWD2cst_ScotchWhiskey <-
    constr.lshclust(
        x=WhiskeyTr,
        links=ScotchWhiskey$neighbors@data,
        coords=ScotchWhiskey$geo@coords/1000
    )
##
### A fonction to plot the Whiskey clustering results
plotWhiskey <- function(k) {
    par(fig=c(0,1,0,1))
    plot(grpWD2cst_ScotchWhiskey, k=k, links=TRUE, las=1,
         xlab="Eastings (km)", ylab="Northings (km)", cex=0.1, lwd=3)
    text(ScotchWhiskey$geo@coords/1000,labels=1:length(ScotchWhiskey$geo))
    legend(x=375, y=700, lty=1L, lwd=3, col=rainbow(1.2*k)[1L:k],
           legend=sprintf("Group %d",1:k), cex=1.25)
    SpeyZoom <- list(xlim=c(314.7,342.2), ylim=c(834.3,860.0))
    rect(xleft=SpeyZoom$xlim[1L], ybottom=SpeyZoom$ylim[1L], col="#E6E6E680",
         xright=SpeyZoom$xlim[2L], ytop=SpeyZoom$ylim[2L], lwd=2, lty=1L)
    par(fig=c(0.01,0.50,0.46,0.99), new=TRUE)
    plot(grpWD2cst_ScotchWhiskey, xlim=SpeyZoom$xlim,
         ylim=SpeyZoom$ylim, k=k, links=TRUE, las=1, xlab="", ylab="",
         cex=0.1, lwd=3, axes=FALSE)
    text(ScotchWhiskey$geo@coords/1000,labels=1:length(ScotchWhiskey$geo))
    rect(xleft=SpeyZoom$xlim[1L], ybottom=SpeyZoom$ylim[1L],
         xright=SpeyZoom$xlim[2L], ytop=SpeyZoom$ylim[2L], lwd=2, lty=1L)
}
##
### Plot the clustering results on the map of Scotland for 5 groups.
### The inset map shows the Speyside distilleries in detail:
plotWhiskey(5L)
##
## End of the Scotch Whiskey tasting data example
##
## \dontrun{
### Benchmarking example
### Benchmarking can be used to estimate computation time for different
### values of N. 
### Computing time grows with N at roughly the same speed as the memory
### storage requirements to store the dissimilarity matrices.
##
require(magrittr)
require(pryr)
##
benchmark <- function(N, M) {
  res <- matrix(NA,length(N)*length(M),4L) %>% as.data.frame
  colnames(res) <- c("N(obs)","M(var)","Storage (MiB)","Time (sec)")
  res[[1L]] <- rep(N,length(M))
  res[[2L]] <- rep(M,each=length(N))
  ##
  for(i in 1:nrow(res)) {
    ## i=1L
    cat("N:",res[i,1L]," M:",res[i,2L],"\n")
    coords.mem <- cbind(x=runif(res[i,1L],-1,1),y=runif(res[i,1L],-1,1))
    if(i>1L) rm(dat.mem) ; gc()
    dat.mem <- try(matrix(runif(res[i,1L]*res[i,2L],0,1),res[i,1L],res[i,2L]))
    if(any(class(dat.mem)=="try-error"))
      break
    neighbors.mem <-
      (coords.mem %>%
         tri2nb %>%
         nb2listw(style="B") %>%
         listw2sn)[,1:2]
    {
      start.time = Sys.time()
      res.mem <- try(constr.lshclust(dat.mem, neighbors.mem))
      end.time = Sys.time()
    }
    if(any(class(res.mem)=="try-error"))
      break
    res[i,3L] <- (3*object_size(dat.mem) + object_size(neighbors.mem) +
                    object_size(res.mem))/1048576  # n. bytes per MiB
    res[i,4L] <- as.numeric(end.time)-as.numeric(start.time)
  }
  res[["N(obs)"]] <- as.integer(res[["N(obs)"]])
  res[["M(var)"]] <- as.integer(res[["M(var)"]])
  res
}
##
N <- c(1000,2000,5000,10000,20000,50000)
M <- c(1,2,5,10,20,50)
res <- benchmark(N, M)
##
### Plotting the results:
par(mar=c(3,6,2,2),mfrow=c(2L,1L))
barplot(height = matrix(res[,"Time (sec)"],length(N),length(M)),
        names.arg = N, ylab = "Time (seconds)\n", xlab = "",
        las = 1L, log = "y", beside=TRUE)
par(mar=c(5,6,0,2))
barplot(height = matrix(res[,"Storage (MiB)"],length(N),length(M)),
        names.arg = N, ylab = "Total storage (MB)\n",
        xlab = "Number of observations", las = 1L, log = "y", beside=TRUE)
##
### Examine the output file
res
##
### Examine how computing time increases with increasing number of observations
### and variables. 
lm(log(`Time (sec)`)~log(`N(obs)`)+log(`M(var)`), data=res)
lm(log(`Storage (MiB)`)~log(`N(obs)`)+log(`M(var)`), data=res)
## } ## End dontrun
##
### End of the benchmarking example
##
### End of examples
##
