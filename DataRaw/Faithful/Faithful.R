##
### Old Faithful geyser data 2000 - 2011
### Yellow Stone National Park, WY, USA
### Analyzed by Guillaume Guénard - Université de Montréal 2019-2020
##
## rm(list=ls())
library(magrittr)
##
if(FALSE) {
  url <- paste("http://www.geyserstudy.org/geysers/OLDFAITHFUL/eruptions/",
               "Old%%20Faithful%%20eruptions%%20for%%2020%02d.TXT",sep="")
  years <- 0L:11L
  Access_time <- date()
  dat <- list()
  for(i in 1L:length(years)) {
    con <- file(description = sprintf(url,years[i]))
    tmp <- con %>%
      scan(what=character(),sep="\n", quiet=TRUE) %>%
      {.[(which(.=="  ")+1L):(length(.)-1L)]} %>%
      strsplit(",") %>%
      lapply(function(x) x[1L]) %>%
      unlist %>%
      strptime("%m/%d/%y %H:%M:%S","GMT") %>%
      as.POSIXct
    dat[[i]] <- tmp
    close(con)
  }
  rm(i,con,tmp)
  ## str(dat)
  merge_segments <- function(x, y) {
    idx <- length(x)+(1L:length(y))
    x[idx] <- y
    x
  }
  ##
  raw <- dat[[1L]] %>% as.character
  for(i in 2L:length(dat)) {
    ## i=2L
    raw[length(raw)+1L] <- "NEXT SEGMENT"
    raw %<>%
      merge_segments(dat[[i]]%>%as.character)
  }
  raw %>%
    as.data.frame %>%
    write.table(file="raw.txt",quote=FALSE,row.names=FALSE,col.names=FALSE)
  rm(i)
  ##
  save(url,years,Access_time,dat,merge_segments,raw,file="raw.rda")
} else load(file="raw.rda")
##
### Manual curation using a spreadsheet software :-(
if(FALSE) {
  reworked <-
    read.table("reworked.csv",sep=";",dec=",",header=TRUE,
               stringsAsFactors=FALSE)
  ## head(reworked)
  Segment <- list()
  for(i in reworked[["Segment"]]%>%unique%>%na.omit) {
    ## i=1L
    sel <- which(reworked[["Segment"]]==i)
    dd <- reworked[["Date"]][sel] %>% as.POSIXct("MST5MDT")
    Segment[[i]] <-
      data.frame(Date=dd, Period=c(NA,diff(dd%>%as.numeric)/3600))
    ## summary(df)
  }
  rm(i,sel,dd)
  ## Segment%>%str
  ## Segment%>%length
  if(FALSE) {
    for(i in 1L:(Segment%>%length)) {
      if((Segment[[i]]%>%nrow)>1L) {
        plot(x=Segment[[i]]$Date,y=Segment[[i]]$Period,
             type="l",xlab="Time",ylab="Period (h)") %>% try
        abline(h=Segment[[i]]$Period%>%mean(na.rm=TRUE),lty=3L)
        if(locator(1L)%>%is.null) break
      }
    }
    rm(i)
  }
  ##
  Meta <- list()
  Meta[["n"]] <- Segment %>% lapply(nrow) %>% unlist
  Meta[["from"]] <- Segment %>%
    lapply(function(x) x$Date%>%head(1L)%>%as.character) %>%
    unlist %>% as.POSIXct("MST5MDT")
  Meta[["to"]] <- Segment %>%
    lapply(function(x) x$Date%>%tail(1L)%>%as.character) %>%
    unlist %>% as.POSIXct("MST5MDT")
  Meta[["min"]] <- Segment %>%
    lapply(function(x) x$Period%>%min(na.rm=TRUE)) %>% unlist
  Meta[["qt025"]] <- Segment %>%
    lapply(function(x) x$Period%>%quantile(0.025,na.rm=TRUE)) %>% unlist
  Meta[["mean"]] <- Segment %>%
    lapply(function(x) x$Period%>%mean(na.rm=TRUE)) %>% unlist
  Meta[["qt975"]] <- Segment %>%
    lapply(function(x) x$Period%>%quantile(0.975,na.rm=TRUE)) %>% unlist
  Meta[["max"]] <- Segment %>%
    lapply(function(x) x$Period%>%max(na.rm=TRUE)) %>% unlist
  Meta[["ylim"]] <- Segment %>% lapply(function(x) x$Period) %>%
    unlist %>% range(na.rm=TRUE)
  ## Segment[[1L]][["Date"]]%>%head(1L)
  ## Segment[[20L]][["Date"]]%>%tail(1L)
  Meta[["xlim"]] <- c("2000-10-11 13:56:55","2011-08-16 04:50:48") %>%
      as.POSIXct("MST5MDT")
  Meta[["N"]] <- Segment %>% length
  ##
  if(FALSE) {
    cols <- rainbow(1.2*Meta[["N"]])[1L:Meta[["N"]]]
    plot(x=Meta[["xlim"]], y=Meta[["ylim"]],
         type="n", xlab="Time", ylab="Period (h)")
    for(i in 1L:(Segment%>%length))
      if(Meta[["n"]][i]>2L) {
        lines(x=Segment[[i]]$Date,
              y=Segment[[i]]$Period,
              col=cols[i])
        segments(x0=Meta[["from"]][i],
                 x1=Meta[["to"]][i],
                 y0=Meta[["qt025"]][i],
                 y1=Meta[["qt025"]][i],
                 lty=3L)
        segments(x0=Meta[["from"]][i],
                 x1=Meta[["to"]][i],
                 y0=Meta[["mean"]][i],
                 y1=Meta[["mean"]][i],
                 lty=1L)
        segments(x0=Meta[["from"]][i],
                 x1=Meta[["to"]][i],
                 y0=Meta[["qt975"]][i],
                 y1=Meta[["qt975"]][i],
                 lty=3L)
      }
    rm(cols,i)
  }
  ##
  Processed <- list()
  Processed$samples <-
    Meta$n %>%
    lapply(
      function(x, a)
        if(x>a) round(seq(a+1L,x-a,length.out=floor((x-1L)/a)-1L)) else integer(),
      a=50)
  ##
  hann <- function(a) 1 - sin(pi*(-(a - 1):a)/(2*a - 1))^2
  ## plot(y=hann(50),x=1:(2*50),type="l")
  win <- hann(150)
  swin <- win%>%sum
  ##
  Processed$wavg <-
    Processed$samples%>%sapply(length)%>%sum%>%numeric
  Processed$Date <-
    Processed$samples %>%
    sapply(length)%>%sum%>%
    rep("2000-10-11 13:56:55"%>%as.POSIXct("MST5MDT"),.)
  ##
  k <- 1L
  for(i in 1L:length(Processed$samples))
    if(length(Processed$samples[[i]]))
      for(j in 1L:length(Processed$samples[[i]])) {
        ## i=1L ; j=1L
        Processed$wavg[k] <-
          Processed$samples[[i]][j] %>%
          {Segment[[i]][.+(-49:50),"Period"]*win} %>%
          sum %>% {./swin}
        Processed$Date[k] <-
          Segment[[i]][Processed$samples[[i]][j],"Date"]
        k <- k + 1L
      }
  rm(win,swin,k,i,j)
  ## Processed$dst <-
  ##   Processed$wavg %>% dist
  ##
  save(reworked,Segment,Meta,Processed,file="reworked.rda")
} else load(file="reworked.rda")
##
## Demonstration:
Faithful <- data.frame(
  Interval = Processed$wavg,
  Date = Processed$Date
)
save(Faithful,file="Faithful.rda")
system("cp -avf Faithful.rda ../../constr.hclust/data/")
##
### Trying clustering
dst <- dist(Faithful$Interval)
##
library(constr.hclust)
chcl <- constr.hclust(
  dst,
  coords = Faithful$Date,
  chron = TRUE)
## chcl%>%str
##
X11()
par(mar=c(5,6.5,2,2))
plot(x=range(Faithful$Date), y=c(0.5,6.5), type="n", xlab="Time",
     ylab="Partitions\n\n", yaxt="n")
##
parts <- c(2,3,4,5,6,7)
cols <- list(
  c("red","purple"),
  c("red","blue","purple"),
  c("red","orange","blue","purple"),
  c("red","orange","yellow","blue","purple"),
  c("red","orange","yellow","green","blue","purple"),
  c("red","orange","yellow","green","aquamarine","blue","purple")
)
for(i in 1L:length(parts)) {
  chcl$coords[,"y"] <- i
  plot(chcl, parts[i], link=TRUE, lwd=25, hybrids="none",
       lwd.pt=0.5, cex=0, pch=21, plot=FALSE, lend=2, col=cols[[i]])
}
axis(2, at=1:length(parts), labels=sprintf("%d groups",parts), las=1)
##
tmp <- which(!!diff(cutree(chcl,parts[i])))
data.frame(From=Faithful$Date[tmp],
           To=Faithful$Date[tmp+1])
rm(i,tmp)
##
