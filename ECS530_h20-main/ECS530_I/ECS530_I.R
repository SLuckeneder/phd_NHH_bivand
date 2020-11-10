

## ---- echo=TRUE---------------------------------------------------------------
needed <- c("raster", "stars", "abind", "terra", "elevatr", "sp", "mapview", "sf", "osmdata", "wordcloud", "RColorBrewer")


## ---- echo = FALSE, eval=FALSE------------------------------------------------
## BCrepos <- BiocManager::repositories()
## bioc <- available.packages(repo = BCrepos[1])
## bioc_ann <- available.packages(repo = BCrepos[2])
## bioc_exp <- available.packages(repo = BCrepos[3])
## cran <- available.packages()
## saveRDS(cran, file="cran_201006.rds")
## pdb <- rbind(cran, bioc, bioc_ann, bioc_exp)
## saveRDS(pdb, file="pdb_201006.rds")


## ---- echo = FALSE, eval=FALSE------------------------------------------------
## pdb <- readRDS("pdb_201006.rds")
## suppressPackageStartupMessages(library(miniCRAN))
## suppressPackageStartupMessages(library(igraph))
## suppressPackageStartupMessages(library(magrittr))
## pg <- makeDepGraph(pdb[, "Package"], availPkgs = pdb, suggests=TRUE, enhances=TRUE, includeBasePkgs = FALSE)
## pr <- pg %>%
##   page.rank(directed = FALSE) %>%
##   use_series("vector") %>%
##   sort(decreasing = TRUE) %>%
##   as.matrix %>%
##   set_colnames("page.rank")
##   cutoff <- quantile(pr[, "page.rank"], probs = 0.2)
## popular <- pr[pr[, "page.rank"] >= cutoff, ]
## toKeep <- names(popular)
## vids <- V(pg)[toKeep]
## gs <- induced.subgraph(pg, vids = toKeep)
## cl <- walktrap.community(gs, steps = 3)
## topClusters <- table(cl$membership) %>%
##   sort(decreasing = TRUE) %>%
##   head(25)
## cluster <- function(i, clusters, pagerank, n=10){
##   group <- clusters$names[clusters$membership == i]
##   pagerank[group, ] %>% sort(decreasing = TRUE) %>% head(n)
## }
## z <- lapply(names(topClusters)[1:15], cluster, clusters=cl, pagerank=pr, n=50)
## saveRDS(z, file="all_z_201006.rds")


## ----plot4a, cache=TRUE, echo=FALSE, eval=TRUE--------------------------------
suppressPackageStartupMessages(library(wordcloud))
z <- readRDS("ECS530_h20-main/ECS530_I/all_z_201006.rds")
oopar <- par(mar=c(0,0,0,0)+0.1)
wordcloud(names(z[[5]]), freq=unname(z[[5]])) # sf 2 sp 5
par(oopar)


## ---- echo = TRUE-------------------------------------------------------------
suppressPackageStartupMessages(library(osmdata))
library(sf)


## ---- cache=TRUE, echo = TRUE-------------------------------------------------
bbox <- opq(bbox = 'bergen norway')
byb0 <- osmdata_sf(add_osm_feature(bbox, key = 'railway',
  value = 'light_rail'))$osm_lines
tram <- osmdata_sf(add_osm_feature(bbox, key = 'railway',
  value = 'tram'))$osm_lines
byb1 <- tram[!is.na(tram$name),]
o <- intersect(names(byb0), names(byb1))
byb <- rbind(byb0[,o], byb1[,o])
saveRDS(byb, file="byb.rds")


## ---- echo = TRUE-------------------------------------------------------------
byb <- readRDS("byb.rds")
library(mapview)
mapviewOptions(fgb = FALSE)
mapview(byb)

byb %>% ggplot2::ggplot()+
  ggplot2::geom_sf()


## ---- echo = TRUE, eval=FALSE, cache=TRUE-------------------------------------
## bike_fls <- list.files("bbs")
## trips0 <- NULL
## for (fl in bike_fls) trips0 <- rbind(trips0,
##   read.csv(file.path("bbs", fl), header=TRUE))
## trips0 <- trips0[trips0[, 8] < 6 & trips0[, 13] < 6,]
## trips <- cbind(trips0[,c(1, 4, 2, 9)], data.frame(count=1))
## from <- unique(trips0[,c(4,5,7,8)])
## names(from) <- substring(names(from), 7)
## to <- unique(trips0[,c(9,10,12,13)])
## names(to) <- substring(names(to), 5)
## stations0 <- st_as_sf(merge(from, to, all=TRUE),
##   coords=c("station_longitude", "station_latitude"))
## stations <- aggregate(stations0, list(stations0$station_id),
##   head, n=1)
## suppressWarnings(stations <- st_cast(stations, "POINT"))
## st_crs(stations) <- 4326
## od <- aggregate(trips[,-(1:4)], list(trips$start_station_id,
##   trips$end_station_id), sum)
## od <- od[-(which(od[,1] == od[,2])),]
## library(stplanr)
## od_lines <- od2line(flow=od, zones=stations, zone_code="Group.1",
##   origin_code="Group.1", dest_code="Group.2")
## saveRDS(od_lines, "od_lines.rds")
## Sys.setenv(CYCLESTREET="XxXxXxXxXxXxXx")
## od_routes <- line2route(od_lines, plan = "fastest")
## saveRDS(od_routes, "od_routes.rds")


## ----plot3, cache=TRUE, eval=TRUE---------------------------------------------
od_lines <- readRDS("ECS530_h20-main/ECS530_I/od_lines.rds")
mapviewOptions(fgb = FALSE)
mapview(od_lines, alpha=0.2, lwd=(od_lines$x/max(od_lines$x))*10)

od_lines %>% ggplot2::ggplot()+
  ggplot2::geom_sf(alpha=0.2)


## ----plot4, cache=TRUE, eval=TRUE---------------------------------------------
od_routes <- readRDS("ECS530_h20-main/ECS530_I/od_routes.rds")
mapviewOptions(fgb = FALSE)
mapview(od_routes, alpha=0.2, lwd=(od_lines$x/max(od_lines$x))*10)

od_routes %>% ggplot2::ggplot()+
  ggplot2::geom_sf()


## ---- echo = TRUE-------------------------------------------------------------
library(sp)
byb_sp <- as(byb, "Spatial")
str(byb_sp, max.level=2)


## ---- echo = TRUE-------------------------------------------------------------
str(slot(byb_sp, "lines")[[1]])


## ---- echo = TRUE-------------------------------------------------------------
library(terra)
(byb_sv <- as(byb, "SpatVector"))
str(byb_sv)

## ---- echo = TRUE-------------------------------------------------------------
geomtype(byb_sv)
str(geom(byb_sv))


## ---- echo = TRUE, eval=FALSE-------------------------------------------------
## library(elevatr)
## elevation <- get_elev_raster(byb_sp, z = 10)
## is.na(elevation) <- elevation < 1
## saveRDS(elevation, file="elevation.rds")


## ---- echo = TRUE-------------------------------------------------------------
library(raster)
(elevation <- readRDS("elevation.rds"))
str(elevation, max.level=2)


## ---- echo=TRUE---------------------------------------------------------------
str(slot(elevation, "data"))


## ---- echo=TRUE---------------------------------------------------------------
str(as(elevation, "SpatialGridDataFrame"), max.level=2)


## ---- echo = TRUE, eval=TRUE, cache=TRUE--------------------------------------
mapview(elevation, col=terrain.colors)


## ---- echo = TRUE-------------------------------------------------------------
(elevation_sr <- as(elevation, "SpatRaster"))
str(elevation_sr)


## ---- echo = TRUE-------------------------------------------------------------
str(values(elevation_sr))


## ---- echo = TRUE-------------------------------------------------------------
V1 <- 1:3
V2 <- letters[1:3]
V3 <- sqrt(V1)
V4 <- sqrt(as.complex(-V1))
L <- list(v1=V1, v2=V2, v3=V3, v4=V4)


## ---- echo = TRUE-------------------------------------------------------------
str(L)
L$v3[2]
L[[3]][2]


## ---- echo = TRUE-------------------------------------------------------------
DF <- as.data.frame(L)
str(DF)
DF <- as.data.frame(L, stringsAsFactors=FALSE)
str(DF)


## ---- echo = TRUE-------------------------------------------------------------
V2a <- letters[1:4]
V4a <- factor(V2a)
La <- list(v1=V1, v2=V2a, v3=V3, v4=V4a)
DFa <- try(as.data.frame(La, stringsAsFactors=FALSE), silent=TRUE)
message(DFa)


## ---- echo = TRUE-------------------------------------------------------------
DF$v3[2]
DF[[3]][2]
DF[["v3"]][2]


## ---- echo = TRUE-------------------------------------------------------------
DF[2, 3]
DF[2, "v3"]
str(DF[2, 3])
str(DF[2, 3, drop=FALSE])


## ---- echo = TRUE-------------------------------------------------------------
as.matrix(DF)
as.matrix(DF[,c(1,3)])


## ---- echo = TRUE-------------------------------------------------------------
length(L)
length(DF)
length(as.matrix(DF))


## ---- echo = TRUE-------------------------------------------------------------
dim(L)
dim(DF)
dim(as.matrix(DF))


## ---- echo = TRUE-------------------------------------------------------------
str(as.matrix(DF))


## ---- echo = TRUE-------------------------------------------------------------
row.names(DF)
names(DF)
names(DF) <- LETTERS[1:4]
names(DF)
str(dimnames(as.matrix(DF)))


## ---- echo = TRUE-------------------------------------------------------------
str(attributes(DF))
str(attributes(as.matrix(DF)))


## ---- echo = TRUE-------------------------------------------------------------
V1a <- c(V1, NA)
V3a <- sqrt(V1a)
La <- list(v1=V1a, v2=V2a, v3=V3a, v4=V4a)
DFa <- as.data.frame(La, stringsAsFactors=FALSE)
str(DFa)


## ---- echo = TRUE-------------------------------------------------------------
DF$E <- list(d=1, e="1", f=TRUE)
str(DF)


## ---- echo = TRUE-------------------------------------------------------------
pt1 <- st_point(c(1,3))
pt2 <- pt1 + 1
pt3 <- pt2 + 1
str(pt3)


## ---- echo = TRUE-------------------------------------------------------------
st_as_text(pt3)


## ---- echo = TRUE-------------------------------------------------------------
st_as_binary(pt3)


## ---- echo = TRUE-------------------------------------------------------------
pt_sfc <- st_as_sfc(list(pt1, pt2, pt3))
str(pt_sfc)


## ---- echo = TRUE-------------------------------------------------------------
st_geometry(DF) <- pt_sfc
(DF)


## ---- echo = TRUE-------------------------------------------------------------
(buf_DF <- st_buffer(DF, dist=0.3))


## ---- echo = TRUE-------------------------------------------------------------
library(stars)
fn <- system.file("tif/L7_ETMs.tif", package = "stars")
L7 <- read_stars(fn)
L7


## ---- echo = TRUE-------------------------------------------------------------
(L7_R <- as(L7, "Raster"))
(as(L7_R, "SpatRaster"))


## ---- echo = TRUE-------------------------------------------------------------
ndvi <- function(x) (x[4] - x[3])/(x[4] + x[3])
(s2.ndvi <- st_apply(L7, c("x", "y"), ndvi))


## ---- echo = TRUE-------------------------------------------------------------
L7p <- read_stars(fn, proxy=TRUE)
L7p


## ---- echo = TRUE-------------------------------------------------------------
(L7p.ndvi = st_apply(L7p, c("x", "y"), ndvi))


## ---- echo = TRUE-------------------------------------------------------------
(x6 <- split(L7, "band"))


## ---- echo = TRUE-------------------------------------------------------------
x6$mean <- (x6[[1]] + x6[[2]] + x6[[3]] + x6[[4]] + x6[[5]] +
              x6[[6]])/6
xm <- st_apply(L7, c("x", "y"), mean)
all.equal(xm[[1]], x6$mean)


## -----------------------------------------------------------------------------
sessionInfo()

