##
### Oribates data
##
## rm(list=ls())
##
library(png)
library(grid)
library(magrittr)
library(spdep)
##
if(FALSE) {
  map <-
    readPNG("GIS/mite.png")%>%
    as.raster
  ## str(map)
  ##
  map <- map[rep(c(TRUE,FALSE,FALSE),length.out=nrow(map)),
             rep(c(TRUE,FALSE,FALSE),length.out=ncol(map))]
  ##
  mite <-
    list(
      fau = read.table("mite.txt"),
      env = read.table("mite_env.txt"),
      xy = read.table("mite_xy.txt")
    )
  ##
  mite$xy <- mite$xy[,2L:1L]
  mite$xy[,1L] <- 10-mite$xy[,1L]
  ##
  mite$link <-
    mite$xy %>%
    tri2nb %>%
    nb2listw(style="B") %>%
    listw2sn %>%
    {.[,1L:2L]} %>%
    {.[.[,1L]<.[,2L],]}%>%
    cbind(Id=1L:nrow(.),keep=rep(TRUE,nrow(.)))
  mite$link[c(4,7,15,33,67,100:102,165,189,148,125,59:61,21),4L] <- FALSE
  ##
  X11(width=10,height=2.6)
  par(mar=rep(0,4L))
  plot(NA,xlim=c(0,10),ylim=c(-0.1,2.5),asp=1)
  rasterImage(map2, 0, -0.1, 10, 2.5, interpolate=FALSE)
  arrows(x0=0.15,x1=1.15,y0=0.1,y1=0.1,code=3,length=0.05,angle=90,lwd=2)
  text(x=0.65,y=0.025,labels="1m")
  ## points(mite$xy,cex=1.25,pch=21,bg="red")
  apply(mite$link,1L,
        function(x,xy,labels) {
          if(x[4L]) {
            segments(x0=xy[x[1L],1L],x1=xy[x[2L],1L],
                     y0=xy[x[1L],2L],y1=xy[x[2L],2L])
            if(labels)
              text(x=0.5*xy[x[1L],1L]+0.5*xy[x[2L],1L],
                   y=0.5*xy[x[1L],2L]+0.5*xy[x[2L],2L],
                   labels=x[3L])
          }
        },xy=mite$xy,labels=FALSE)
  points(mite$xy,cex=1.25,pch=21,bg="black")
  ##
  mite$link <- mite$link[mite$link[,4L],1L:2L]
  rownames(mite$link) <- NULL
  ##
  mite$topo <- data.frame(Type=c("Sphagn. 1","Sphagn. 2","Sphagn. 3","Sphagn. 4",
                                 "Lign. litter","Bare peat"),
                          R=c(0,255,126,238,167,226),G=c(144,237,83,131,0,0),
                          B=c(54,0,25,0,125,26))
  mite$topo[["RGB"]] <- rgb(mite$topo[,"R"]/255,mite$topo[,"G"]/255,
                            mite$topo[,"B"]/255)
  ##
  save(mite,file="mite.rds")
  Oribates <- mite
  Oribates$map <- map
  save(Oribates,file="Oribates.rda")
} else load(file="Oribates.rda")
system("cp -avf Oribates.rda ../../constr.hclust/data/")
rm(list=ls())
##
### Colors:
##
### Type           R   G   B
### Sphagn.1     0   144  54
### Sphagn.2     255 237   0
### Sphagn.3     126  83  25
### Sphagn.4     238 131   0
### Lign. litter 167   0 125
### Bare peat    226   0  26
##
### Examples
##
### 1
data("Oribates",package="constr.hclust")
##
### miteMap
par(mar=rep(0,4L))
plot(NA,xlim=c(0,12),ylim=c(-0.1,2.5),yaxs="i",asp=1,axes=FALSE)
rasterImage(Oribates$map, 0, -0.1, 10, 2.5, interpolate=FALSE)
arrows(x0=0.15,x1=1.15,y0=0.1,y1=0.1,code=3,length=0.05,angle=90,lwd=2)
text(x=0.65,y=0.025,labels="1m")
## points(Oribates$xy,cex=1.25,pch=21,bg="red")
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
##
### 2
Oribates.hel <- dist(sqrt(Oribates$fau/rowSums(Oribates$fau)))
## Oribates.hel <- Oribates$fau %>% dist.ldc("hellinger")
##
### 3
Oribates.chclust <- constr.hclust(d=Oribates.hel, links=Oribates$link,
                                  coords=Oribates$xy)
##
### 4
par(mfrow=c(4,1),mar=c(0.5,0,0.5,0))
cols <- c("turquoise", "orange", "blue", "violet", "green", "red",
          "purple")
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
##
