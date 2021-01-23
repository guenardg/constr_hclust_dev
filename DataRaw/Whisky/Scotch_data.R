##
### 
##
## rm(list=ls())
## list.files(".")
library(sp)
library(magrittr)
library(rgdal)
library(spdep)
library(ade4)
## library(mapdata)
datTypes <- c("geo","colour","nose","body","palate","finish")
ScotchWhiskey <-
    datTypes %>%
    lapply(
        function(x) read.csv(sprintf("scotch_%s.csv",x),sep=";")
    )
names(ScotchWhiskey) <- datTypes
ScotchWhiskey$geo$Lon %<>% {-.}
##
ScotchWhiskey$geo[,c("Lon","Lat")] %>%
    apply(1L,paste,collapse=";") %>%
    match(
        ScotchWhiskey$geo[,c("Lon","Lat")] %>%
        apply(.,1L,paste,collapse=";") %>%
        unique
    )
ScotchWhiskey$geo[c(43L,47L),c("Lon","Lat")] <-
    rbind(c(-3.118249,57.447974),
          c(-3.127094,57.454495))
ScotchWhiskey$geo[c(62L,98L,99L),c("Lon","Lat")] <-
    rbind(c(-5.606559,55.430155),
          c(-5.607635,55.425260),
          c(-5.608081,55.425855))
##
ScotchWhiskey$geo <-
    SpatialPointsDataFrame(
        coords=as.matrix(ScotchWhiskey$geo[,c("Lon","Lat")]),
        data=ScotchWhiskey$geo[-(2L:3L)],
        proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    ) %>%
    spTransform(
        CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
    )
##
UK <-
    lapply(0:2,
           function(x) {
               sprintf("./GIS/GBR_adm%d.shp",x) %>%
                   readOGR %>%
                   spTransform(
                       CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
                   )
           }
           )
##
## plot(UK[[1L]])
plot(UK[[2L]])
## plot(UK[[3L]])
points(ScotchWhiskey$geo,pch=21,bg="red")
##
ScotchWhiskey$colour %<>%
    {colnames(.)[apply(!!.,1L,which)]} %>% factor
ScotchWhiskey$nose %<>%
    apply(.,2L,as.logical)
colnames(ScotchWhiskey$nose) <-
    colnames(ScotchWhiskey$nose) %>% tolower
ScotchWhiskey$body %<>%
    apply(.,2L,as.logical)
ScotchWhiskey$palate %<>%
    apply(.,2L,as.logical)
ScotchWhiskey$finish %<>%
    apply(.,2L,as.logical)
ScotchWhiskey$nbChar <-
    ScotchWhiskey[c("nose","body","palate","finish")] %>%
    lapply(
        rowSums
    )
##
ScotchWhiskey$listW <-
    ScotchWhiskey$geo@coords %>%
    tri2nb %>%
    nb2listw(
        style="B"
    )
##
ScotchWhiskey$links.mat <-
    ScotchWhiskey$listW %>%
    listw2mat
##
ScotchWhiskey$neighbors <-
    ScotchWhiskey$listW %>%
    listw2sn %>%
    {.[,1:2]} %>%
    {.[.[,1L]<.[,2L],]}
##
ScotchWhiskey$geo %>%
    writeOGR(
        "./GIS/Scotch_Distillery.shp",
        layer="Distillery",
        driver="ESRI Shapefile"
    )
##
pts <- ScotchWhiskey$geo@coords
LinesList <- list()
for(i in 1L:nrow(ScotchWhiskey$neighbors))
    LinesList[[i]] <-
        ScotchWhiskey$neighbors[i,1L:2] %>%
        unlist %>%
        {pts[.,]} %>%
        Line %>%
        Lines(ID=i)
ScotchWhiskey$neighbors <-
    LinesList %>%
    SpatialLines(
        CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")
    ) %>%
    SpatialLinesDataFrame(
        data=data.frame(
            ScotchWhiskey$neighbors,
            row.names=1L:nrow(ScotchWhiskey$neighbors))
    )
##
ScotchWhiskey$neighbors %>%
    writeOGR(
        "./GIS/Scotch_links.shp",
        layer="Distillery",
        driver="ESRI Shapefile"
    )
##
### Édition manuelle des liens dans QGIS ici...
##
ScotchWhiskey$neighbors <-
    readOGR(
        "./GIS/Scotch_links_edited.shp"
    )
##
## ScotchWhiskey$neighbors@data%>%unlist%>%as.numeric%>%unique%>%sort%>%length
## ScotchWhiskey$neighbors%>%plot
##
## ScotchWhiskey$dist <-
##     ScotchWhiskey$colour %>%
##     as.data.frame %>%
##     model.matrix(~.-1,.) %>%
##     dist.binary(method=2L)
##
ScotchWhiskey$dist <- list()
##
for(i in c("nose","body","palate","finish"))
    ScotchWhiskey$dist[[i]] <-
        ScotchWhiskey[[i]] %>%
        as.numeric %>%
        matrix(
            nrow(ScotchWhiskey[[i]]),
            ncol(ScotchWhiskey[[i]])
        ) %>%
        dist.binary(method=2L)
##
## ScotchWhiskey$normDist <- ScotchWhiskey$dist[[1L]]
## ScotchWhiskey$normDist[] <- ScotchWhiskey$dist %>% sapply(as.numeric) %>% {rowSums(.^2)^0.5}
## range(ScotchWhiskey$normDist)
save(ScotchWhiskey, file="../constr.hclust/data/ScotchWhiskey.rda", version = 2L)
##
rm(i,pts,LinesList)
##
data(ScotchWhiskey)
lapply(ScotchWhiskey,ncol)
ScotchWhiskey$nbChar
ScotchWhiskey$listW  ## attr(ScotchWhiskey$listW,"class")
names(ScotchWhiskey$dist)
## ScotchWhiskey$dist$nose%>%str
## data(ScotchWhiskey)
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
plotWhiskey("Scotch wiskey: peat nose")
cols <- c("blue","orange")
points(ScotchWhiskey$geo@coords/1000,pch=21L,bg=cols[ScotchWhiskey$nose[,"peat"]+1L])
legend(x=50,y=1000,legend=c("Has a peat nose","Has no peat nose"),pch=21L,pt.bg=rev(cols))
##
plotWhiskey("Scotch wiskey: soft body")
cols <- c("red","green")
points(ScotchWhiskey$geo@coords/1000,pch=21L,bg=cols[ScotchWhiskey$body[,"soft"]+1L])
legend(x=50,y=1000,legend=c("Has a soft body","Has no soft body"),pch=21L,pt.bg=rev(cols))
##
plotWhiskey("Scotch wiskey: spicy palate")
cols <- c("red","green")
points(ScotchWhiskey$geo@coords/1000,pch=21L,bg=cols[ScotchWhiskey$palate[,"spice"]+1L])
legend(x=50,y=1000,legend=c("Has a spicy palate","Has no spicy palate"),pch=21L,pt.bg=rev(cols))
##
plotWhiskey("Scotch wiskey: sweet finish")
cols <- c("red","green")
points(ScotchWhiskey$geo@coords/1000,pch=21L,bg=cols[ScotchWhiskey$finish[,"sweet"]+1L])
legend(x=50,y=1000,legend=c("Has a sweet finish","Has no sweet finish"),pch=21L,pt.bg=rev(cols))
##
