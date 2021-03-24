##
### That code is a numeric experiment for assessing the occurence frequency of
### inversion in a population constrained clustering trees obtained by random
### means. Each clustering is obtained by drawing a random normal deviate with
### mean = 0 and sigma = 1 from which the euclidean distance is calculated (the
### dissimilarity matrix) and a two-dimensional plot by drawing x and y
### coordinates as random uniform deviates with min = -1 and max = +1. A
### Delauney triangulation is performed on the coordinates to obtain the edge
### list (i.e., the neighbouring constraint) and a constraint clustering is
### performed.
##
### A total of 1000 simulations is performed, each with encompassing seven data
### sets with sample sizes 10, 20, 50, 100, 200, 500, and 1000; and clusterings
### were done with all nine methods available and with seven values of beta
### (-0.9, -0.6, -0.3, 0, 0.3, 0.6, and 0.9) for the flexible clustering, for a
### total of 15 different methods. On each calculation 1000 * 7 * 15 = 105000,
### the proportion of edges with negative lengths is reported.
##
### I (the first author) judged the results below are outside of the scope of
### the presentation paper (JSS); they were not included in the latter but kept
### here for the benefit of the most inquiring readers.
##
if(FALSE) {
  ##
  ### Data generation
  if(FALSE) {
    nsim <- 1000L
    N <- c(10L,20L,50L,100L,200L,500L,1000L)
    simDat <- list()
    rnseed <- .Random.seed
    for(i in 1L:nsim) {
      ## i=1L
      simDat[[i]] <- list()
      for(j in 1L:length(N)) {
        ## j=1L
        simDat[[i]][[j]] <-
          list(
            coords = runif(2*N[j],-1,1) %>% matrix(N[j]),
            values = rnorm(N[j],0,1) ## Distances would have taken too much space.
          )
        simDat[[i]][[j]]$lnk <-
          simDat[[i]][[j]]$coords %>%
          tri2nb %>%
          nb2listw(style="B") %>%
          listw2sn %>%
          {.[,1:2]}
      }
    }
    rm(N,i,j)
    save(simDat,rnseed,file="Inversions_simulations.rda")
  } else load(file="Inversions_simulations.rda")
  ## rm(simDat,rnseed)
  ##
  if(FALSE) {
    conditions <- list()
    methods <- c("ward.D", "ward.D2", "single", "complete", "average", 
                 "mcquitty", "centroid", "median", "flexible")
    for(i in 1L:8L)
      conditions[[i]] <-list(methods=methods[i],beta=-0.25)
    beta <- seq(-0.9,0.9,0.3)
    for(i in 1L:length(beta))
      conditions[[8L+i]] <-list(methods=methods[9L],beta=beta[i])
    rm(methods,i,beta)
    ## x <- simDat[[1L]]
    analyzeDat <- function(x, conditions, ...) {
      out <- list()
      for(i in 1L:length(x)) {
        ## i=1L
        out[[i]] <- list()
        d <- x[[i]]$values %>% dist
        for(j in 1L:length(conditions)) {
          ## j=1L
          out[[i]][[j]] <-
            constr.hclust(d = d, method=conditions[[j]]$methods,
                          links=x[[i]]$lnk, coords=x[[i]]$coords,
                          beta=conditions[[j]]$beta) %>%
            as.phylo %>% {mean(.$edge.length<0)}
        }
      }
      out
    }
    ##
    if(any(
      !require(magrittr),
      !require(constr.hclust),
      !require(ape)
    )) stop("Misssing dependency!")
    ## analyzeDat(simDat[[1L]],conditions)
    ##
    library(parallel)
    cl <- makeForkCluster(detectCores())
    simRes <- parLapply(cl,simDat,analyzeDat,conditions=conditions)
    save(conditions,simRes,file="Inversions_simulations_results.rda")
    stopCluster(cl)
    ## rm(simDat,rnseed,conditions,simRes)
  } else {
    ## load(file="Inversions_simulations.rda")
    load(file="Inversions_simulations_results.rda")
  }
  ##
  ## str(conditions)
  ## length(simRes)
  ## length(simRes[[1L]])
  ## length(simRes[[1L]][[1L]])
  nsim <- 1000L
  N <- c(10L,20L,50L,100L,200L,500L,1000L)
  ## length(conditions)
  ## 1000*7*15
}
##
