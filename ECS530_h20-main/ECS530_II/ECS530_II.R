

## -----------------------------------------------------------------------------
needed <- c("rgdal", "RSQLite", "units", "gdistance", "Matrix", "igraph", "raster", "sp", "spData", "mapview", "sf")


## -----------------------------------------------------------------------------
byb <- readRDS("byb.rds")
names(attributes(byb))


## -----------------------------------------------------------------------------
library(sf)
sf::st_agr(byb)


## -----------------------------------------------------------------------------
byb$length <- st_length(byb)
summary(byb$length)


## -----------------------------------------------------------------------------
str(byb$length)


## ---- echo = TRUE-------------------------------------------------------------
TASSIM <- st_read("TASSIM.gpkg")
library(mapview)
mapviewOptions(fgb = FALSE)
mapview(TASSIM)


## ---- echo = TRUE-------------------------------------------------------------
library(sf)
library(spData)
b506 <- st_read(system.file("shapes/boston_tracts.shp", package="spData")[1])

## -----------------------------------------------------------------------------
mapview(b506, zcol="censored")


## ---- echo = TRUE-------------------------------------------------------------
b489 <- b506[b506$censored == "no",]
t0 <- aggregate(b489, list(ids = b489$NOX_ID), head, n = 1)
b94 <- t0[, c("ids", attr(t0, "sf_column"))]


## -----------------------------------------------------------------------------
mapview(b489, zcol="NOX")


## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics('snowmap.png')


## ----echo=FALSE---------------------------------------------------------------
knitr::include_graphics('brodyetal00_fig1.png')


## ---- echo=TRUE---------------------------------------------------------------
library(sf)
bbo <- st_read("snow/bbo.gpkg")


## ---- echo=TRUE, warning=FALSE------------------------------------------------
buildings <- st_read("snow/buildings.gpkg", quiet=TRUE)
deaths <- st_read("snow/deaths.gpkg", quiet=TRUE)
sum(deaths$Num_Css)
b_pump <- st_read("snow/b_pump.gpkg", quiet=TRUE)
nb_pump <- st_read("snow/nb_pump.gpkg", quiet=TRUE)


## ---- echo=TRUE, warning=FALSE------------------------------------------------
library(sf)
st_crs(buildings) <- st_crs(bbo)
buildings1 <- st_intersection(buildings, bbo)
buildings2 <- st_buffer(buildings1, dist=-4)


## ---- echo=TRUE, warning=FALSE------------------------------------------------
plot(st_geometry(buildings2))


## ---- echo=TRUE---------------------------------------------------------------
library(raster)
resolution <- 1
r <- raster(extent(buildings2), resolution=resolution, crs=st_crs(bbo)$proj4string)
r[] <- resolution
summary(r)


## ---- echo=TRUE, cache=TRUE, warning=FALSE------------------------------------
buildings3 <- as(buildings2[!st_is_empty(buildings2),], "Spatial")
cfp <- cellFromPolygon(r, buildings3)
is.na(r[]) <- unlist(cfp)
summary(r)


## ---- echo=TRUE, warning=FALSE------------------------------------------------
plot(r)


## ---- echo=TRUE, warning=FALSE, message=FALSE---------------------------------
library(gdistance)


## ---- echo=TRUE, cache=TRUE---------------------------------------------------
tr1 <- transition(r, transitionFunction=function(x) 1/mean(x), directions=8, symm=TRUE)


## ---- echo=TRUE, cache=TRUE, warning=FALSE------------------------------------
sp_deaths <- as(deaths, "Spatial")
d_b_pump <- st_length(st_as_sfc(shortestPath(tr1, as(b_pump, "Spatial"), sp_deaths, output="SpatialLines")))


## ---- echo=TRUE, cache=TRUE, warning=FALSE------------------------------------
res <- matrix(NA, ncol=nrow(nb_pump), nrow=nrow(deaths))
sp_nb_pump <- as(nb_pump, "Spatial")
for (i in 1:nrow(nb_pump)) res[,i] <- st_length(st_as_sfc(shortestPath(tr1, sp_nb_pump[i,], sp_deaths, output="SpatialLines")))
d_nb_pump <- apply(res, 1, min)


## ---- echo=TRUE---------------------------------------------------------------
library(units)
units(d_nb_pump) <- "m"
deaths$b_nearer <- d_b_pump < d_nb_pump
by(deaths$Num_Css, deaths$b_nearer, sum)


## ---- echo=TRUE---------------------------------------------------------------
rgeos::version_GEOS0()


## ---- echo=TRUE, warning=FALSE------------------------------------------------
cV_old_default <- ifelse(rgeos::version_GEOS0() >= "3.7.2", 0L, FALSE)
yy <- rgeos::readWKT(readLines("invalid.wkt"))
rgeos::gIsValid(yy, byid=TRUE, reason=TRUE)


## ---- echo=TRUE---------------------------------------------------------------
sf::sf_extSoftVersion()


## ---- echo=TRUE---------------------------------------------------------------
sf::st_is_valid(sf::st_as_sf(yy), reason=TRUE)


## ---- echo=TRUE, warning=FALSE------------------------------------------------
ply <- rgeos::readWKT(readLines("ply.wkt"))
oo <- try(rgeos::gIntersection(yy, ply, byid=TRUE, checkValidity=cV_old_default), silent=TRUE)
print(attr(oo, "condition")$message)

## ---- echo=TRUE---------------------------------------------------------------
ooo <- try(sf::st_intersection(sf::st_as_sf(yy), sf::st_as_sf(ply)), silent=TRUE)
print(attr(oo, "condition")$message)


## ---- echo=TRUE---------------------------------------------------------------
cV_new_default <- ifelse(rgeos::version_GEOS0() >= "3.7.2", 1L, TRUE)
try(rgeos::gIntersection(yy, ply, byid=TRUE, checkValidity=cV_new_default), silent=TRUE)


## ---- echo=TRUE---------------------------------------------------------------
oo <- rgeos::gIntersection(yy, ply, byid=TRUE, checkValidity=2L)
rgeos::gIsValid(oo)


## ---- echo=TRUE---------------------------------------------------------------
oo <- rgeos::gIntersection(rgeos::gBuffer(yy, byid=TRUE, width=0), ply, byid=TRUE, checkValidity=1L)
rgeos::gIsValid(oo)


## ---- echo=TRUE---------------------------------------------------------------
ooo <- sf::st_intersection(sf::st_buffer(sf::st_as_sf(yy), dist=0), sf::st_as_sf(ply))
all(sf::st_is_valid(ooo))


## -----------------------------------------------------------------------------
sf_extSoftVersion()


## -----------------------------------------------------------------------------
sort(as.character(st_drivers("vector")$name))


## -----------------------------------------------------------------------------
sort(as.character(st_drivers("raster")$name))


## -----------------------------------------------------------------------------
library(RSQLite)
db = dbConnect(SQLite(), dbname="snow/b_pump.gpkg")
dbListTables(db)


## -----------------------------------------------------------------------------
str(dbReadTable(db, "gpkg_geometry_columns"))


## -----------------------------------------------------------------------------
str(dbReadTable(db, "b_pump")$geom)


## -----------------------------------------------------------------------------
dbDisconnect(db)


## -----------------------------------------------------------------------------
str(st_layers("snow/b_pump.gpkg"))


## -----------------------------------------------------------------------------
st_layers("snow/nb_pump.gpkg")


## -----------------------------------------------------------------------------
library(rgdal)
ogrInfo("snow/nb_pump.gpkg")


## -----------------------------------------------------------------------------
rgdal::GDALinfo(system.file("tif/L7_ETMs.tif", package = "stars"))


## -----------------------------------------------------------------------------
obj <- GDAL.open(system.file("tif/L7_ETMs.tif", package = "stars"))


## -----------------------------------------------------------------------------
dim(obj)


## -----------------------------------------------------------------------------
getDriverLongName(getDriver(obj))


## -----------------------------------------------------------------------------
image(getRasterData(obj, band=1, offset=c(100, 100), region.dim=c(200, 200)))


## -----------------------------------------------------------------------------
GDAL.close(obj)


## -----------------------------------------------------------------------------
library(raster)
(obj <- raster(system.file("tif/L7_ETMs.tif", package = "stars")))


## -----------------------------------------------------------------------------
sessionInfo()

