##
### The disjoint sets problem in constrained hierarchical clustering
##
## rm(list=ls())
##
library(spdep)
library(constr.hclust)
##
coords <- cbind(x=c(1,1,1,2,2,4,5,5,5,7,7,8),y=c(1,2,3,1,2,3,4,3,2,1,2,1))
plot(coords,pch=LETTERS[1:nrow(coords)])
##
listW <- nb2listw(tri2nb(coords), style="B")
links.mat.dat <- listw2mat(listW)
links.mat.dat[1:5,6:12] <- 0
links.mat.dat[6:12,1:5] <- 0
links.mat.dat[6:9,10:12] <- 0
links.mat.dat[10:12,6:9] <- 0
##
neighbors <- matrix(NA,0,2)
for(i in 1:(nrow(links.mat.dat)-1))
  for(j in (i+1):ncol(links.mat.dat))
    if(links.mat.dat[i,j])
      neighbors <- rbind(neighbors,c(i,j))
##
D.dat <- dist(runif(1:nrow(coords)))
##
plot(coords, type='n',asp=1)
title("Delaunay triangulation")
text(coords, labels=LETTERS[1:nrow(coords)], pos=3)
for(i in 1:nrow(neighbors))
  lines(rbind(coords[neighbors[i,1],],
              coords[neighbors[i,2],]))
##
grpWD2cst_constr_hclust <-
  constr.hclust(
    D.dat, method="ward.D2",
    neighbors, coords)
## stats:::plot.hclust(grpWD2cst_constr_hclust, hang=-1)
##
plot(grpWD2cst_constr_hclust, k=2, links=TRUE, las=1, xlab="Eastings",
     ylab="Northings", pch=21L, cex=3, lwd=3)
##
grpWD2cst_constr_hclust$height
grpWD2cst_constr_hclust$merge
## cutree(grpWD2cst_constr_hclust,12)
##


