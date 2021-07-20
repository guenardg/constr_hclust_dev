##
### Runs the examples: must be copied on updates
##
## rm(list=ls())
library(constr.hclust)
## help.start()
##
### constr.hclust
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
library(spdep)
listW <- nb2listw(tri2nb(coord.dat), style="B")
links.mat.dat <- listw2mat(listW)
neighbors <- listw2sn(listW)[,1:2]
##
### Calculating the (Euclidean) distance between points:
D.dat <- dist(dat)
##
### Display the points:
plot(coord.dat, type='n',asp=1)
title("Delaunay triangulation")
text(coord.dat, labels=as.character(as.matrix(dat)), pos=3)
for(i in 1:nrow(neighbors))
  lines(rbind(coord.dat[neighbors[i,1],],
              coord.dat[neighbors[i,2],]))
##
### Unconstrained clustring by hclust:
grpWD2_hclust <- hclust(D.dat, method="ward.D2")
plot(grpWD2_hclust, hang=-1)
##
### Clustering without a contiguity constraint;
### the result is represented as a dendrogram:
grpWD2_constr_hclust <- constr.hclust(D.dat, method="ward.D2")
plot(grpWD2_constr_hclust, hang=-1)
##
### Clustering with a contiguity constraint described by a list of
### links:
grpWD2cst_constr_hclust <-
  constr.hclust(
    D.dat, method="ward.D2",
    neighbors, coord.dat)
##
### To visualize using hclust's plotting method:
### stats:::plot.hclust(grpWD2cst_constr_hclust, hang=-1)
##
### Plot the results on a map with k=3 clusters:
plot(grpWD2cst_constr_hclust, k=3, links=TRUE, las=1, xlab="Eastings",
     ylab="Northings", pch=21L, cex=3, lwd=3)
##
### Generic functions from hclust can be used, for instance to obtain
### a list of members of each cluster:
cutree(grpWD2cst_constr_hclust, k=3)
##
### Now with k=5 clusters:
plot(grpWD2cst_constr_hclust, k=5, links=TRUE, las=1, xlab="Eastings",
     ylab="Northings", pch=21L, cex=3, lwd=3)
cutree(grpWD2cst_constr_hclust, k=5)
##
## End of the artificial map example


### Second example: Fish community composition along the Doubs River,
### France. The sequence is analyzed as a case of chronological
### clustering, substituting space for time.
##
data(doubs, package="ade4")
Doubs.D <- dist.ldc(doubs$fish, method="hellinger")
grpWD2cst_fish <- constr.hclust(Doubs.D, method="ward.D2", chron=TRUE,
                                coords=as.matrix(doubs$xy))
plot(grpWD2cst_fish, k=5, las=1, xlab="Eastings (km)",
     ylab="Northings (km)", pch=21L, cex=3, lwd=3)
##
### Repeat the plot with other values of k (number of groups)
##
## End of the Doubs River fish assemblages example


### Third example: Scotch Whiskey distilleries clustered using tasting scores
### (nose, body, palate, finish, and the four distances combined) constrained
### with respect to the distillery locations in Scotland.
##
## Documentation file about the Scotch Whiskey data: ?ScotchWhiskey
##
data(ScotchWhiskey)
### Cluster analyses for the nose, body, palate, and finish D
### matrices:
grpWD2cst_ScotchWhiskey <-
  lapply(
    ScotchWhiskey$dist,    ## A list of distance matrices
    constr.hclust,         ## The function called by function lapply
    links=ScotchWhiskey$neighbors@data,         ## The list of links
    coords=ScotchWhiskey$geo@coords/1000
  )
##
### The four D matrices (nose, body, palate, finish), represented as
### vectors in the ScotchWiskey data file, are combined as follows to
### produce a single distance matrix integrating all four types of
### tastes:
Dmat <- ScotchWhiskey$dist
ScotchWhiskey[["norm"]] <-
  sqrt(Dmat$nose^2 + Dmat$body^2 + Dmat$palate^2 + Dmat$finish^2)
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
### A fonction to plot the Whiskey clustering results
plotWhiskey <- function(wh, k) {
  par(fig=c(0,1,0,1))
  plot(grpWD2cst_ScotchWhiskey[[wh]], k=k, links=TRUE, las=1,
       xlab="Eastings (km)", ylab="Northings (km)", cex=0.1, lwd=3,
       main=sprintf("Feature: %s",wh))
  text(ScotchWhiskey$geo@coords/1000,labels=1:length(ScotchWhiskey$geo))
  legend(x=375, y=700, lty=1L, lwd=3, col=rainbow(1.2*k)[1L:k],
         legend=sprintf("Group %d",1:k), cex=1.25)
  SpeyZoom <- list(xlim=c(314.7,342.2), ylim=c(834.3,860.0))
  rect(xleft=SpeyZoom$xlim[1L], ybottom=SpeyZoom$ylim[1L],col="#E6E6E680",
       xright=SpeyZoom$xlim[2L], ytop=SpeyZoom$ylim[2L], lwd=2, lty=1L)
  par(fig=c(0.01,0.50,0.46,0.99), new=TRUE)
  plot(grpWD2cst_ScotchWhiskey[[wh]], xlim=SpeyZoom$xlim,
       ylim=SpeyZoom$ylim, k=k, links=TRUE, las=1, xlab="", ylab="",
       cex=0.1, lwd=3, axes=FALSE)
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
##
## End of the Scotch Whiskey tasting data example
## Not run: 
### Benchmarking example
### Benchmarking can be used to estimate computation time for different
### values of N. 
### Computing time grows with N at roughly the same speed as the memory
### storage requirements to store the dissimilarity matrices.
##
require(magrittr)
require(pryr)
##
benchmark <- function(nobj) {
  # Argument -
  # nobj : Number of objects in simulation runs
  res <- matrix(NA,length(nobj),3) %>% as.data.frame
  colnames(res) <- c("N.objects","Storage (MiB)","Time (sec)")
  res[,1L] <- nobj
  ## resources <- list()
  for(i in 1:length(nobj)) { 
    N <- nobj[i]
    coords.mem <- cbind(x=runif(N,-1,1),y=runif(N,-1,1))
    dat.mem <- runif(N,0,1)
    if(i>1L) rm(D.mem) ; gc()
    D.mem <- try(dat.mem %>% dist)  #; gc()
    if(any(class(D.mem)=="try-error"))
      break
    neighbors.mem <-
      (coords.mem %>%
         tri2nb %>%
         nb2listw(style="B") %>%
         listw2sn)[,1:2]
    {start.time = Sys.time()
      res.mem <- try(constr.hclust(D.mem, method="ward.D2",
                                   neighbors.mem))
      end.time = Sys.time()}
    if(any(class(res.mem)=="try-error"))
      break
    res[i,2L] <- (2*object_size(D.mem) + object_size(neighbors.mem) +
                    object_size(res.mem))/1048576  # n. bytes per MiB
    res[i,3L] <- end.time-start.time
  }
  res[["N.objects"]] <- as.integer(res[["N.objects"]])
  res
}
res <- benchmark(nobj=c(1000,2000,5000,10000,20000,50000,100000))
##
### Plotting the results:
ok <- res %>% apply(1L, function(x) !x %>% is.na %>% any)
par(mar=c(3,6,2,2),mfrow=c(2L,1L))
barplot(height = res[ok,"Time (sec)"], names.arg= res[ok,"N.objects"],
        ylab="Time (seconds)\n",xlab="",las=1L,log="y")
par(mar=c(5,6,0,2))
barplot(height = res[ok,"Storage (MiB)"], names.arg= res[ok,"N.objects"],
        ylab="Total storage (MB)\n",xlab="Number of observations",
        las=1L,log="y")
##
### Examine the output file
res

## End(Not run)
## End of the benchmarking example
##
### End of examples
##
### constr.lshclust
##
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
library(spdep)
listW <- nb2listw(tri2nb(coord.dat), style="B")
links.mat.dat <- listw2mat(listW)
neighbors <- listw2sn(listW)[,1:2]
##
### Display the points:
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
     ylab="Northings", pch=21L, cex=3, lwd=3)
##
### Generic functions from hclust can be used, for instance to obtain
### a list of members of each cluster:
cutree(grpWD2cst_constr_lshclust, k=3)
##
### Now with k=5 clusters:
plot(grpWD2cst_constr_lshclust, k=5, links=TRUE, las=1, xlab="Eastings",
     ylab="Northings", pch=21L, cex=3, lwd=3)
cutree(grpWD2cst_constr_lshclust, k=5)
##
## End of the artificial map example
##
### Second example: Fish community composition along the Doubs River,
### France. The sequence is analyzed as a case of chronological
### clustering, substituting space for time.
##
data(doubs, package="ade4")
##
### Using the Hellinger metric on the species abundances:
Doubs.hel <- sqrt(doubs$fish / rowSums(doubs$fish))
Doubs.hel[rowSums(doubs$fish)==0,] <- 0
grpWD2cst_fish <- constr.lshclust(x=Doubs.hel, chron=TRUE,
                                  coords=as.matrix(doubs$xy))
##
plot(grpWD2cst_fish, k=5, las=1, xlab="Eastings (km)",
     ylab="Northings (km)", pch=21L, cex=3, lwd=3)
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
data(ScotchWhiskey)
### Cluster analyses for the colour, nose, body, palate, and finish using
### least squares on the basis of the Mahalanobis metric.
##
### Combining the data matrices:
cols <-
  contr.treatment(
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
plotWhiskey(k=5L)
##
## End of the Scotch Whiskey tasting data example
##
## Not run: 
### Benchmarking example
### Benchmarking can be used to estimate computation time for different
### values of N (number of sites) and M (number of variables).
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
    dat.mem <- try(matrix(runif(res[i,1L]*res[i,2L],0,1),
                          res[i,1L],res[i,2L]))
    if(any(class(dat.mem)=="try-error"))
      break
    neighbors.mem <-
      (coords.mem %>%
         tri2nb %>%
         nb2listw(style="B") %>%
         listw2sn)[,1:2]
    {start.time = Sys.time()
      res.mem <- try(constr.lshclust(dat.mem, neighbors.mem))
      end.time = Sys.time()}
    if(any(class(res.mem)=="try-error"))
      break
    res[i,3L] <- (3*object_size(dat.mem) + object_size(neighbors.mem) +
                    object_size(res.mem))/1048576  # n. bytes per MiB
    res[i,4L] <- as.numeric(end.time) - as.numeric(start.time)
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
### Analyze how computing time and storage scales up with increasing number
### of observations and variables.
lm(log(`Time (sec)`)~log(`N(obs)`)+log(`M(var)`), data=res)
lm(log(`Storage (MiB)`)~log(`N(obs)`)+log(`M(var)`), data=res)

## End(Not run)
##
### End of the benchmarking example
##
### End of examples
##
### plot.constr.hclust
##
##
### Artificial map data from Legendre & Legendre (2012, Fig. 13.26)
### n = 16
##
dat <- c(41,42,25,38,50,30,41,43,43,41,30,50,38,25,42,41)
coord.dat <- matrix(c(1,3,5,7,2,4,6,8,1,3,5,7,2,4,6,8,
                      4.4,4.4,4.4,4.4,3.3,3.3,3.3,3.3,
                      2.2,2.2,2.2,2.2,1.1,1.1,1.1,1.1),16,2)
##
### Obtaining a list of neighbours:
library(spdep)
listW <- nb2listw(tri2nb(coord.dat), style="B")
links.mat.dat <- listw2mat(listW)
neighbors <- listw2sn(listW)[,1:2]
##
### Calculating the (Euclidean) distance between points:
D.dat <- dist(dat)
##
### Display the points:
plot(coord.dat, type='n',asp=1)
title("Delaunay triangulation")
text(coord.dat, labels=as.character(as.matrix(dat)), pos=3)
for(i in 1:nrow(neighbors))
  lines(rbind(coord.dat[neighbors[i,1],],
              coord.dat[neighbors[i,2],]))
##
### Clustering with a contiguity constraint described by a list of
### links:
grpWD2cst_constr_hclust <-
  constr.hclust(
    D.dat, method="ward.D2",
    neighbors, coord.dat)
##
### Plot the results with k=5 clusters on a map:
plot(grpWD2cst_constr_hclust, k=5, links=TRUE, las=1,
     xlab="Eastings", ylab="Northings", pch=21L, cex=3, lwd=3)
##
### Repeat the plot with other values of k (number of groups)
##
### ScotchWhiskey
##
data(ScotchWhiskey)
lapply(ScotchWhiskey,ncol)
ScotchWhiskey$nbChar
ScotchWhiskey$listW  ## attr(ScotchWhiskey$listW,"class")
names(ScotchWhiskey)
names(ScotchWhiskey$dist)
##
plotWhiskey <- function(main) {
  plot(x=ScotchWhiskey$geo@coords[,1L]/1000,
       xlab="Eastings (km)",
       y=ScotchWhiskey$geo@coords[,2L]/1000,
       ylab="Northings (km)",
       main=main,
       type="n",asp=1)
  apply(
    ScotchWhiskey$neighbor@data,1L,
    function(X,coords) {
      segments(
        coords[X[1L],1L]/1000,
        coords[X[1L],2L]/1000,
        coords[X[2L],1L]/1000,
        coords[X[2L],2L]/1000
      )
    },
    coords=ScotchWhiskey$geo@coords
  )
  invisible(NULL)
}
##
plotWhiskey("Scotch whiskey: peat nose")
cols <- c("blue","orange")
points(ScotchWhiskey$geo@coords/1000,pch=21L,
       bg=cols[ScotchWhiskey$nose[,"peat"]+1L])
legend(x=50,y=1000,legend=c("Has a peat nose","Has no peat nose"),
       pch=21L,pt.bg=rev(cols))
##
plotWhiskey("Scotch whiskey: soft body")
cols <- c("red","green")
points(ScotchWhiskey$geo@coords/1000,pch=21L,
       bg=cols[ScotchWhiskey$body[,"soft"]+1L])
legend(x=50,y=1000,legend=c("Has a soft body","Has no soft body"),
       pch=21L,pt.bg=rev(cols))
##
plotWhiskey("Scotch whiskey: spicy palate")
cols <- c("red","green")
points(ScotchWhiskey$geo@coords/1000,pch=21L,
       bg=cols[ScotchWhiskey$palate[,"spice"]+1L])
legend(x=50,y=1000,legend=c("Has a spicy palate","Has no spicy palate"),
       pch=21L,pt.bg=rev(cols))
##
plotWhiskey("Scotch whiskey: sweet finish")
cols <- c("red","green")
points(ScotchWhiskey$geo@coords/1000,pch=21L,
       bg=cols[ScotchWhiskey$finish[,"sweet"]+1L])
legend(x=50,y=1000,legend=c("Has a sweet finish","Has no sweet finish"),
       pch=21L,pt.bg=rev(cols))
##
### To visualize (part of) the distance matrices:
as.matrix(ScotchWhiskey$dist$nose)[1:5,1:5]
as.matrix(ScotchWhiskey$dist$body)[1:5,1:5]
as.matrix(ScotchWhiskey$dist$palate)[1:5,1:5]
as.matrix(ScotchWhiskey$dist$finish)[1:5,1:5]
##
### The data tables:
ScotchWhiskey$colour
head(ScotchWhiskey$nose)
head(ScotchWhiskey$body)
head(ScotchWhiskey$palate)
head(ScotchWhiskey$finish)
##
### The end
##

