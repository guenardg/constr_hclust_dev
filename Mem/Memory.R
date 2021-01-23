##
### Combien de points, combien de mémoire vive?
##
## rm(list=ls()) ; gc()
## dyn.load("../constr.hclust/src/constr.hclust.so")
## source("../constr.hclust/R/constr.hclust.R")
## 
library(spdep)
##
library(constr.hclust)
### Benchmarking:
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
      res.mem <- try(constr.hclust(D.mem, method="ward.D2", neighbors.mem))
      end.time = Sys.time()}
    if(any(class(res.mem)=="try-error"))
      break
    res[i,2L] <- (2*object_size(D.mem) + object_size(neighbors.mem) +
                   object_size(res.mem))/1048576  # n. bytes in a MiM
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
res
##
### Comparison between hclust, flashClust, and constr.hclust on non-constrained
### cases:
##
library(flashClust)
##
dd <- dist(rnorm(5000L,0,1))
res.nc <- list()
{start.time = Sys.time()
  res.nc[["hclust"]] <- try(stats::hclust(dd, method="centroid"))
  end.time = Sys.time()}
end.time-start.time
##
{start.time = Sys.time()
  res.nc[["FlashClust"]] <- try(flashClust::hclust(dd, method="centroid"))
  end.time = Sys.time()}
end.time-start.time
##
{start.time = Sys.time()
  res.nc[["constr.hclust"]] <- try(constr.hclust(dd, method="centroid"))
  end.time = Sys.time()}
end.time-start.time
rm(dd)
##
### The new implementation compares advantageously with either hclust or
### flashClust.
##
