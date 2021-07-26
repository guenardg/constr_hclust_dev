##
### The disjoint sets problem in constrained hierarchical clustering
##
## rm(list=ls())
##
## Pierre's example
##
library(constr.hclust)
##
## For simplicity of the example, matrix Y only contains one response variable
var = c(1.5, 0.2, 5.1, 3.0, 2.1, 1.4)
ex.Y = data.frame(var)
# Site coordinates, matrix xy
x.coo = c(-1, -2, -0.5, 0.5, 2, 1)
y.coo = c(-2, -1, 0, 0, 1, 2)
ex.xy = data.frame(x.coo, y.coo)
# Matrix of connecting edges E
from = c(1,1,2,3,4,3,4)
to = c(2,3,3,4,5,6,6)
ex.E = data.frame(from, to)
##
## Carry out constrained clustering analysis
test.out <-
  constr.hclust(
    dist(ex.Y),       # Response dissimilarity matrix
    method="ward.D2", # Clustering method
    links=ex.E,       # File of link edges (constraint) E
    coords=ex.xy      # File of geographic coordinates
  )
##
## Plot the results for k = 3
par(mfrow=c(1,2))
plot(test.out, k=3)
stats:::plot.hclust(test.out, hang=-1)
##
test.out$merge
test.out$height
test.out$order
##
from = c(1,1,2,4,4)
to = c(2,3,3,5,6)
ex.E2 = data.frame(from, to)
##
test.out2 <-
  constr.hclust(
    dist(ex.Y), # Response dissimilarity matrix
    method="ward.D2", # Clustering method
    links=ex.E2, # File of link edges (constraint) E
    coords=ex.xy # File of geographic coordinates
  )
##
par(mfrow=c(1,2))
plot(test.out2, k=3) # Même graphique que test.out (OK)
stats:::plot.hclust(test.out2, hang=-1)
##
str(test.out2)
test.out2$merge
test.out2$height
test.out2$order
##
test.out_mod <- test.out2
test.out_mod$height[5] <- NA
test.out_mod$merge[5,] <- c(3,4)
test.out_mod$order <- c(1,2,3,6,4,5)
##
par(mfrow=c(1,2))
plot(test.out_mod, k=3) # Même graphique que test.out (OK)
stats:::plot.hclust(test.out_mod, hang=-1)
##
##
library(spdep)
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
plot(grpWD2cst_constr_hclust, k=3, links=TRUE, las=1, xlab="Eastings",
     ylab="Northings", pch=21L, cex=3, lwd=3)
##
grpWD2cst_constr_hclust$height[10:11] <- NA
grpWD2cst_constr_hclust$merge[10,] <- c(8,9)  ## Fusion acceptable par hclust.
grpWD2cst_constr_hclust$merge[11,] <- c(7,10) ## Idem
grpWD2cst_constr_hclust$order <- 1:length(grpWD2cst_constr_hclust$order)
## stats:::plot.hclust(grpWD2cst_constr_hclust, hang=-1, ylim=c(0,1))
##
grpWD2cst_constr_hclust2 <- grpWD2cst_constr_hclust
grpWD2cst_constr_hclust2$height[-(10:11)] <- 0
grpWD2cst_constr_hclust2$merge[-(10:11),] <- 0
cutree(grpWD2cst_constr_hclust2,3)




