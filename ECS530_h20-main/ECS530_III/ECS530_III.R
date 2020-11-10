

## ----set-options-cdn, echo=FALSE, results='hide'-----------------------------
td <- tempfile()
dir.create(td)
Sys.setenv("PROJ_USER_WRITABLE_DIRECTORY"=td)


## ---- echo=TRUE--------------------------------------------------------------
needed <- c("RSQLite", "mapview", "sf", "rgdal", "sp")


## ----------------------------------------------------------------------------
library(sp)
library(rgdal)
projInfo("ellps")


## ----------------------------------------------------------------------------
data("GridsDatums")
GridsDatums[grep("Norway", GridsDatums$country),]


## ----------------------------------------------------------------------------
EPSG <- make_EPSG()
EPSG[grep("Oslo", EPSG$note), 1:2]


## ----------------------------------------------------------------------------
CRS("+init=epsg:4817")


## ----------------------------------------------------------------------------
list_coordOps("EPSG:4817", "EPSG:4326")


## ----------------------------------------------------------------------------
getClass("CRS")


## ----------------------------------------------------------------------------
library(sf)
st_crs(4326)


## ----echo=FALSE--------------------------------------------------------------
knitr::include_graphics('VEMOS_sum.png')


## ---- echo = TRUE------------------------------------------------------------
ellps <- sf_proj_info("ellps")
(clrk66 <- unlist(ellps[ellps$name=="clrk66",]))


## ---- echo = TRUE------------------------------------------------------------
eval(parse(text=clrk66["major"]))
eval(parse(text=clrk66["ell"]))
print(sqrt((a^2-b^2)/a^2), digits=10)


## ---- echo = TRUE------------------------------------------------------------
library(RSQLite)
DB0 <- strsplit(sf:::CPL_get_data_dir(), .Platform$path.sep)[[1]]
DB <- file.path(DB0[length(DB0)], "proj.db")
db <- dbConnect(SQLite(), dbname=DB)
cat(strwrap(paste(dbListTables(db), collapse=", ")), sep="\n")
dbDisconnect(db)


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
library(sf)
bp_file <- system.file("gpkg/b_pump.gpkg", package="sf")
b_pump_sf <- st_read(bp_file)


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
proj5 <- paste0("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717",
 " +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs")
legacy <- st_crs(proj5)
proj6 <- legacy$proj4string
proj5_parts <- unlist(strsplit(proj5, " "))
proj6_parts <- unlist(strsplit(proj6, " "))
proj5_parts[!is.element(proj5_parts, proj6_parts)]
proj6_parts[!is.element(proj6_parts, proj5_parts)]


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
b_pump_sf1 <- b_pump_sf
st_crs(b_pump_sf1) <- st_crs(st_crs(b_pump_sf1)$proj4string)


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
b_pump_sf_ll <- st_transform(b_pump_sf, "OGC:CRS84")
b_pump_sf1_ll <- st_transform(b_pump_sf1, "OGC:CRS84")
st_distance(b_pump_sf_ll, b_pump_sf1_ll)


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
library(mapview)
if (sf:::CPL_gdal_version() >= "3.1.0") mapviewOptions(fgb = FALSE)
pts <- rbind(b_pump_sf_ll, b_pump_sf1_ll)
pts$CRS <- c("original", "degraded")
mapview(pts, zcol="CRS", map.type="OpenStreetMap", col.regions=c("green", "red"), cex=18)


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
st_crs("EPSG:4326")


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
library(sp)
cat(wkt(CRS("EPSG:4326")), "\n")


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
sf_from_sp <- st_crs(CRS("EPSG:4326"))
o <- strsplit(sf_from_sp$wkt, "\n")[[1]]
cat(paste(o[grep("CS|AXIS|ORDER", o)], collapse="\n"))


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
sp_from_sf <- as(st_crs("EPSG:4326"), "CRS")
o <- strsplit(wkt(sp_from_sf), "\n")[[1]]
cat(paste(o[grep("CS|AXIS|ORDER", o)], collapse="\n"))


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
cat(st_crs("OGC:CRS:84")$wkt, "\n")


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
cat(wkt(CRS("OGC:CRS84")), "\n")


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
rgdal::set_prefer_proj(FALSE)
GDAL_SRS <- wkt(CRS("OGC:CRS84"))
all.equal(st_crs("OGC:CRS:84")$wkt, GDAL_SRS)
rgdal::set_prefer_proj(TRUE)


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
cat(GDAL_SRS, "\n")
rgdal::compare_CRS(CRS("OGC:CRS84"), as(st_crs("OGC:CRS:84"), "CRS"))


## ---- echo = TRUE, eval=TRUE, warning=FALSE, message=FALSE-------------------
b_pump_sp <- as(b_pump_sf, "Spatial")
b_pump_sp1 <- as(b_pump_sf1, "Spatial")


## ---- echo = TRUE, eval=FALSE------------------------------------------------
## td <- tempfile()
## dir.create(td)
## Sys.setenv("PROJ_USER_WRITABLE_DIRECTORY"=td)

## ---- echo = TRUE, eval=TRUE-------------------------------------------------
library(rgdal)


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
WKT <- wkt(b_pump_sp)
o <- list_coordOps(WKT, "EPSG:4326")
aoi0 <- project(t(unclass(bbox(b_pump_sp))), WKT, inv=TRUE)
aoi <- c(t(aoi0 + c(-0.1, +0.1)))
o_aoi <- list_coordOps(WKT, "EPSG:4326", area_of_interest=aoi)


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
b_pump_sp_ll <- spTransform(b_pump_sp, "OGC:CRS84")
cat(strwrap(get_last_coordOp()), sep="\n")


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
o <- list_coordOps(wkt(b_pump_sp1), "OGC:CRS84",
  area_of_interest=aoi)
b_pump_sp1_ll <- spTransform(b_pump_sp1, "OGC:CRS84")
cat(strwrap(get_last_coordOp()), sep="\n")


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
enable_proj_CDN()
list.files(td)


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
b_pump_sp_llg <- spTransform(b_pump_sp, "OGC:CRS84")
cat(strwrap(get_last_coordOp()), sep="\n")


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
(fls <- list.files(td))
file.size(file.path(td, fls[1]))
disable_proj_CDN()
ll <- st_as_sf(b_pump_sp_ll)
ll1 <- st_as_sf(b_pump_sp1_ll)
llg <- st_as_sf(b_pump_sp_llg)
sf_use_s2(FALSE)
c(st_distance(ll, ll1), st_distance(ll, llg))


## ---- echo = TRUE, eval=TRUE-------------------------------------------------
sf_use_s2(TRUE)
c(st_distance(ll, ll1), st_distance(ll, llg))
sf_use_s2(FALSE)

