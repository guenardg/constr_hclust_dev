pkgname <- "constr.hclust"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "constr.hclust-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('constr.hclust')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("Faithful")
### * Faithful

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Faithful
### Title: Old Faithful Erruption Interval Data Set
### Aliases: Faithful
### Keywords: Geyser

### ** Examples

data(Faithful)

## Distance matrix (Euclidean) of the mean intervals:

dst <- dist(Faithful$Interval)

## Segmenting the series with respect to eruption time intervals using
## chronological clustering (time-constrained Lance-Williams hierarchical
## agglomerative clustering):

chcl <- constr.hclust(dst, coords = Faithful$Date, chron = TRUE)

## Plotting the results:

### Partition sizes and the colors to display them:
parts <- c(2,3,4,5,6,7)
cols <- list(
  c("red","purple"),
  c("red","blue","purple"),
  c("red","orange","blue","purple"),
  c("red","orange","yellow","blue","purple"),
  c("red","orange","yellow","green","blue","purple"),
  c("red","orange","yellow","green","aquamarine","blue","purple")
)

### Plotting partitions with 2-7 segments:
par(mar=c(5,6.5,2,2))
plot(x=range(Faithful$Date), y=c(0.5,6.5), type="n", xlab="Time",
     ylab="Partitions\n\n", yaxt="n")
for(i in 1L:length(parts)) {
  chcl$coords[,"y"] <- i
  plot(chcl, parts[i], link=TRUE, lwd=25, hybrids="none",
       lwd.pt=0.5, cex=0, pch=21, plot=FALSE, lend=2, col=cols[[i]])
}
axis(2, at=1:length(parts), labels=sprintf("%d groups",parts), las=1)

tmp <- which(!!diff(cutree(chcl,parts[i])))
data.frame(From=Faithful$Date[tmp],
           To=Faithful$Date[tmp+1])




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Faithful", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("Oribates")
### * Oribates

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: Oribates
### Title: Borcard's Obitatid Mite Data Set
### Aliases: Oribates
### Keywords: mite

### ** Examples

data("Oribates",package="constr.hclust")

## A map of the study area with the links.
par(mar=rep(0,4L))
plot(NA,xlim=c(0,12),ylim=c(-0.1,2.5),yaxs="i",asp=1,axes=FALSE)
rasterImage(Oribates$map, 0, -0.1, 10, 2.5, interpolate=FALSE)
arrows(x0=0.15,x1=1.15,y0=0.1,y1=0.1,code=3,length=0.05,angle=90,lwd=2)
text(x=0.65,y=0.025,labels="1m")
invisible(
  apply(Oribates$link,1L,
        function(x,xy,labels) {
          segments(x0=xy[x[1L],1L],x1=xy[x[2L],1L],
                   y0=xy[x[1L],2L],y1=xy[x[2L],2L])
        },xy=Oribates$xy,labels=FALSE)
)
points(Oribates$xy,cex=1.25,pch=21,bg="black")
legend(10.1,2.5,legend=Oribates$topo[["Type"]],pt.bg=Oribates$topo[["RGB"]],
       pch=22L,pt.cex=2.5)

## Hellinger distance on the species composition matrix.
Oribates.hel <- dist(sqrt(Oribates$fau/rowSums(Oribates$fau)))

## Constrained clustering of the sites on the basis of their species
## composition.
Oribates.chclust <- constr.hclust(d=Oribates.hel, links=Oribates$link,
                                  coords=Oribates$xy)

## Plotting with different numbers of clusters.
par(mfrow=c(4,1),mar=c(0.5,0,0.5,0))
cols <- c("turquoise", "orange", "blue", "violet", "green", "red", "purple")
parts <- c(2,3,5,7)
for(i in 1L:length(parts)) {
  plot(NA, xlim=c(0,10), ylim=c(-0.1,2.5), xaxs="i", yaxs="i", asp=1,
       axes=FALSE)
  arrows(x0=0.15, x1=1.15, y0=0.1, y1=0.1, code=3, length=0.05, angle=90,
         lwd=2)
  text(x=0.65, y=0, labels="1m", cex=1.5)
  plot(Oribates.chclust, parts[i], links=TRUE, plot=FALSE,
       col=cols[round(seq(1,length(cols),length.out=parts[i]))], lwd=4,
       cex=2.5, pch=21, hybrids="single", lwd.hyb=0.25, lty.hyb=3)
  text(x=0.25, y=2.25, labels=LETTERS[i], cex=2.5)
}




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("Oribates", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("ScotchWhiskey")
### * ScotchWhiskey

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: ScotchWhiskey
### Title: Scotch Whiskey Data Set
### Aliases: ScotchWhiskey
### Keywords: Scotch Whiskey

### ** Examples

data(ScotchWhiskey)
lapply(ScotchWhiskey,ncol)
ScotchWhiskey$nbChar
ScotchWhiskey$listW  ## attr(ScotchWhiskey$listW,"class")
names(ScotchWhiskey)
names(ScotchWhiskey$dist)

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

plotWhiskey("Scotch whiskey: peat nose")
cols <- c("blue","orange")
points(ScotchWhiskey$geo@coords/1000,pch=21L,
       bg=cols[ScotchWhiskey$nose[,"peat"]+1L])
legend(x=50,y=1000,legend=c("Has a peat nose","Has no peat nose"),
       pch=21L,pt.bg=rev(cols))

plotWhiskey("Scotch whiskey: soft body")
cols <- c("red","green")
points(ScotchWhiskey$geo@coords/1000,pch=21L,
       bg=cols[ScotchWhiskey$body[,"soft"]+1L])
legend(x=50,y=1000,legend=c("Has a soft body","Has no soft body"),
       pch=21L,pt.bg=rev(cols))

plotWhiskey("Scotch whiskey: spicy palate")
cols <- c("red","green")
points(ScotchWhiskey$geo@coords/1000,pch=21L,
       bg=cols[ScotchWhiskey$palate[,"spice"]+1L])
legend(x=50,y=1000,legend=c("Has a spicy palate","Has no spicy palate"),
       pch=21L,pt.bg=rev(cols))

plotWhiskey("Scotch whiskey: sweet finish")
cols <- c("red","green")
points(ScotchWhiskey$geo@coords/1000,pch=21L,
       bg=cols[ScotchWhiskey$finish[,"sweet"]+1L])
legend(x=50,y=1000,legend=c("Has a sweet finish","Has no sweet finish"),
       pch=21L,pt.bg=rev(cols))

## To visualize (part of) the distance matrices:
as.matrix(ScotchWhiskey$dist$nose)[1:5,1:5]
as.matrix(ScotchWhiskey$dist$body)[1:5,1:5]
as.matrix(ScotchWhiskey$dist$palate)[1:5,1:5]
as.matrix(ScotchWhiskey$dist$finish)[1:5,1:5]

## The data tables:
ScotchWhiskey$colour
head(ScotchWhiskey$nose)
head(ScotchWhiskey$body)
head(ScotchWhiskey$palate)
head(ScotchWhiskey$finish)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("ScotchWhiskey", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("constr.hclust")
### * constr.hclust

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: constr.hclust
### Title: Space- And Time-Constrained Clustering
### Aliases: constr.hclust

### ** Examples

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
     ylab="Northings", cex=3, lwd=3)
##
### Generic functions from hclust can be used, for instance to obtain
### a list of members of each cluster:
cutree(grpWD2cst_constr_hclust, k=3)
##
### Now with k=5 clusters:
plot(grpWD2cst_constr_hclust, k=5, links=TRUE, las=1, xlab="Eastings",
     ylab="Northings", cex=3, lwd=3)
cutree(grpWD2cst_constr_hclust, k=5)
##
## End of the artificial map example

##
### Second example: Scotch Whiskey distilleries clustered using tasting
### scores (nose, body, palate, finish, and the four distances combined)
### constrained with respect to the distillery locations in Scotland.
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
##D ##
##D ### Third example: Fish community composition along the Doubs River,
##D ### France. The sequence is analyzed as a case of chronological
##D ### clustering, substituting space for time.
##D ##
##D library(ade4)
##D library(adespatial)
##D data(doubs, package="ade4")
##D Doubs.D <- dist.ldc(doubs$fish, method="hellinger")
##D grpWD2cst_fish <- constr.hclust(Doubs.D, method="ward.D2", chron=TRUE,
##D                                 coords=as.matrix(doubs$xy))
##D plot(grpWD2cst_fish, k=5, las=1, xlab="Eastings (km)",
##D      ylab="Northings (km)", cex=3, lwd=3)
##D ##
##D ### Repeat the plot with other values of k (number of groups)
##D ##
##D ## End of the Doubs River fish assemblages example
##D 
##D ##
##D ### Example with 6 connected points, shown in Fig. 2 of Guénard & Legendre paper 
##D ##
##D var = c(1.5, 0.2, 5.1, 3.0, 2.1, 1.4)
##D ex.Y = data.frame(var)
##D ##
##D ## Site coordinates, matrix xy
##D x.coo = c(-1, -2, -0.5, 0.5, 2, 1)
##D y.coo = c(-2, -1, 0, 0, 1, 2)
##D ex.xy = data.frame(x.coo, y.coo)
##D ##
##D ## Matrix of connecting edges E
##D from = c(1,1,2,3,4,3,4)
##D to = c(2,3,3,4,5,6,6)
##D ex.E = data.frame(from, to)
##D ##
##D ## Carry out constrained clustering analysis
##D test.out <-
##D     constr.hclust(
##D         dist(ex.Y),       # Response dissimilarity matrix
##D         method="ward.D2", # Clustering method
##D         links=ex.E,       # File of link edges (constraint) E
##D         coords=ex.xy      # File of geographic coordinates
##D     )
##D ##
##D par(mfrow=c(1,2))
##D ## Plot the map of the results for k = 3
##D plot(test.out, k=3)
##D ## Plot the dendrogram
##D stats:::plot.hclust(test.out, hang=-1)
##D ##
##D 
##D ### Same example modified: disjoint clusters
##D ##  Same ex.Y and ex.xy as in the previous example
##D var = c(1.5, 0.2, 5.1, 3.0, 2.1, 1.4)
##D ex.Y = data.frame(var)
##D ##
##D ## Site coordinates, matrix xy
##D x.coo = c(-1, -2, -0.5, 0.5, 2, 1)
##D y.coo = c(-2, -1, 0, 0, 1, 2)
##D ex.xy = data.frame(x.coo, y.coo)
##D ##
##D ## Matrix of connecting edges E2
##D from = c(1,1,2,4,4)
##D to = c(2,3,3,5,6)
##D ex.E2 = data.frame(from, to)
##D ##
##D ## Carry out constrained clustering analysis
##D test.out2 <-
##D     constr.hclust(
##D         dist(ex.Y),       # Response dissimilarity matrix
##D         method="ward.D2", # Clustering method
##D         links=ex.E2,      # File of link edges (constraint) E
##D         coords=ex.xy      # File of geographic coordinates
##D     )
##D cutree(test.out2, k=2)
##D ##
##D par(mfrow=c(1,2))
##D ## Plot the map of the results for k = 3
##D plot(test.out2, k=3)
##D ## Plot the dendrogram showing the disconnected groups
##D stats:::plot.hclust(test.out2, hang=-1)
##D axis(2,at=0:ceiling(max(test.out2$height,na.rm=TRUE)))
##D ##
##D ## End of the disjoint clusters example
##D ##
##D ### Benchmarking example
##D ### Benchmarking can be used to estimate computation time for different
##D ### values of N. 
##D ### Computing time grows with N at roughly the same speed as the memory
##D ### storage requirements to store the dissimilarity matrices.
##D ##
##D require(magrittr)
##D require(pryr)
##D ##
##D benchmark <- function(nobj) {
##D     # Argument -
##D     # nobj : Number of objects in simulation runs
##D     res <- matrix(NA,length(nobj),3) %>% as.data.frame
##D     colnames(res) <- c("N.objects","Storage (MiB)","Time (sec)")
##D     res[,1L] <- nobj
##D     ## resources <- list()
##D     for(i in 1:length(nobj)) { 
##D         N <- nobj[i]
##D         coords.mem <- cbind(x=runif(N,-1,1),y=runif(N,-1,1))
##D         dat.mem <- runif(N,0,1)
##D         if(i>1L) rm(D.mem) ; gc()
##D         D.mem <- try(dat.mem %>% dist)  #; gc()
##D         if(any(class(D.mem)=="try-error"))
##D             break
##D         neighbors.mem <-
##D             (coords.mem %>%
##D                  tri2nb %>%
##D                  nb2listw(style="B") %>%
##D                  listw2sn)[,1:2]
##D         {start.time = Sys.time()
##D             res.mem <- try(constr.hclust(D.mem, method="ward.D2",
##D                                          neighbors.mem))
##D             end.time = Sys.time()}
##D         if(any(class(res.mem)=="try-error"))
##D             break
##D         res[i,2L] <- (2*object_size(D.mem) + object_size(neighbors.mem) +
##D                           object_size(res.mem))/1048576  # n. bytes per MiB
##D         res[i,3L] <- end.time-start.time
##D     }
##D     res[["N.objects"]] <- as.integer(res[["N.objects"]])
##D     res
##D }
##D res <- benchmark(nobj=c(1000,2000,5000,10000,20000,50000,100000))
##D ##
##D ### Plotting the results:
##D ok <- res %>% apply(1L, function(x) !x %>% is.na %>% any)
##D par(mar=c(3,6,2,2),mfrow=c(2L,1L))
##D barplot(height = res[ok,"Time (sec)"], names.arg= res[ok,"N.objects"],
##D         ylab="Time (seconds)\n",xlab="",las=1L,log="y")
##D par(mar=c(5,6,0,2))
##D barplot(height = res[ok,"Storage (MiB)"], names.arg= res[ok,"N.objects"],
##D         ylab="Total storage (MB)\n",xlab="Number of observations",
##D         las=1L,log="y")
##D ##
##D ### Examine the output file
##D res
##D ##
##D ## End of the benchmarking example
## End(Not run)
### End of examples




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("constr.hclust", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("constr.lshclust")
### * constr.lshclust

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: constr.lshclust
### Title: Space- And Time-Constrained Least Squares Clustering
###   (Experimental)
### Aliases: constr.lshclust

### ** Examples


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
##D ##
##D ### Third example: Fish community composition along the Doubs River,
##D ### France. The sequence is analyzed as a case of chronological
##D ### clustering, substituting space for time.
##D ##
##D library(ade4)
##D data(doubs, package="ade4")
##D ##
##D ### Using the Hellinger metric on the species abundances:
##D Doubs.hel <- sqrt(doubs$fish / rowSums(doubs$fish))
##D Doubs.hel[rowSums(doubs$fish)==0,] <- 0
##D grpWD2cst_fish <- constr.lshclust(x=Doubs.hel, chron=TRUE,
##D                                   coords=as.matrix(doubs$xy))
##D ##
##D plot(grpWD2cst_fish, k=5, las=1, xlab="Eastings (km)",
##D      ylab="Northings (km)", cex=3, lwd=3)
##D ##
##D ### Repeat the plot with other values of k (number of groups)
##D ##
##D ## End of the Doubs River fish assemblages example
##D 
##D ##
##D ### Benchmarking example
##D ### Benchmarking can be used to estimate computation time for different
##D ### values of N (number of sites) and M (number of variables).
##D ##
##D require(magrittr)
##D require(pryr)
##D ##
##D benchmark <- function(N, M) {
##D     res <- matrix(NA,length(N)*length(M),4L) %>% as.data.frame
##D     colnames(res) <- c("N(obs)","M(var)","Storage (MiB)","Time (sec)")
##D     res[[1L]] <- rep(N,length(M))
##D     res[[2L]] <- rep(M,each=length(N))
##D     ##
##D     for(i in 1:nrow(res)) {
##D         ## i=1L
##D         cat("N:",res[i,1L]," M:",res[i,2L],"\n")
##D         coords.mem <- cbind(x=runif(res[i,1L],-1,1),y=runif(res[i,1L],-1,1))
##D         if(i>1L) rm(dat.mem) ; gc()
##D         dat.mem <- try(matrix(runif(res[i,1L]*res[i,2L],0,1),
##D                               res[i,1L],res[i,2L]))
##D         if(any(class(dat.mem)=="try-error"))
##D             break
##D         neighbors.mem <-
##D             (coords.mem %>%
##D              tri2nb %>%
##D              nb2listw(style="B") %>%
##D              listw2sn)[,1:2]
##D         {start.time = Sys.time()
##D          res.mem <- try(constr.lshclust(dat.mem, neighbors.mem))
##D          end.time = Sys.time()}
##D         if(any(class(res.mem)=="try-error"))
##D             break
##D         res[i,3L] <- (3*object_size(dat.mem) + object_size(neighbors.mem) +
##D                       object_size(res.mem))/1048576  # n. bytes per MiB
##D         res[i,4L] <- as.numeric(end.time) - as.numeric(start.time)
##D     }
##D     res[["N(obs)"]] <- as.integer(res[["N(obs)"]])
##D     res[["M(var)"]] <- as.integer(res[["M(var)"]])
##D     res
##D }
##D ##
##D N <- c(1000,2000,5000,10000,20000,50000)
##D M <- c(1,2,5,10,20,50)
##D res <- benchmark(N, M)
##D ##
##D ##
##D ### Plotting the results:
##D par(mar=c(3,6,2,2),mfrow=c(2L,1L))
##D barplot(height = matrix(res[,"Time (sec)"],length(N),length(M)),
##D         names.arg = N, ylab = "Time (seconds)\n", xlab = "",
##D         las = 1L, log = "y", beside=TRUE)
##D par(mar=c(5,6,0,2))
##D barplot(height = matrix(res[,"Storage (MiB)"],length(N),length(M)),
##D         names.arg = N, ylab = "Total storage (MB)\n",
##D         xlab = "Number of observations", las = 1L, log = "y", beside=TRUE)
##D ##
##D ### Examine the output file
##D res
##D ##
##D ### Analyze how computing time and storage scales up with increasing number
##D ### of observations and variables.
##D lm(log(`Time (sec)`)~log(`N(obs)`)+log(`M(var)`), data=res)
##D lm(log(`Storage (MiB)`)~log(`N(obs)`)+log(`M(var)`), data=res)
## End(Not run)
##
### End of the benchmarking example
##
### End of examples
##




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("constr.lshclust", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
graphics::par(get("par.postscript", pos = 'CheckExEnv'))
cleanEx()
nameEx("plot.constr.hclust")
### * plot.constr.hclust

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: plot.constr.hclust
### Title: Plotting Method For Space- And Time-Constrained Clustering
### Aliases: plot.constr.hclust

### ** Examples

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
     xlab="Eastings", ylab="Northings", cex=3, lwd=3)
##
### Repeat the plot with other values of k (number of groups)




base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("plot.constr.hclust", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
