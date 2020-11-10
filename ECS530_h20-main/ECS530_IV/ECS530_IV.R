
## ---- echo=TRUE--------------------------------------------------------------
needed <- c("mapview", "ggplot2", "cartography", "tmap", "colorspace", "RColorBrewer", "sf", "classInt")


## ---- echo=TRUE--------------------------------------------------------------
library(classInt)
args(classIntervals)


## ---- echo=TRUE--------------------------------------------------------------
library(sf)
olinda_sirgas2000 <- st_read("olinda_sirgas2000.gpkg")
(cI <- classIntervals(olinda_sirgas2000$DEPRIV, n=7, style="fisher"))


## ---- echo=TRUE--------------------------------------------------------------
library(RColorBrewer)
pal <- RColorBrewer::brewer.pal((length(cI$brks)-1), "Reds")
plot(cI, pal)


## ---- echo=TRUE--------------------------------------------------------------
display.brewer.all()


## ---- echo=TRUE--------------------------------------------------------------
library(colorspace)
hcl_palettes("sequential (single-hue)", n = 7, plot = TRUE)


## ---- echo=TRUE, eval=FALSE--------------------------------------------------
## pal <- hclwizard()
## pal(6)


## ---- echo=TRUE--------------------------------------------------------------
wheel <- function(col, radius = 1, ...)
  pie(rep(1, length(col)), col = col, radius = radius, ...) 
opar <- par(mfrow=c(1,2))
wheel(rainbow_hcl(12))
wheel(rainbow(12))
par(opar)


## ---- echo=TRUE--------------------------------------------------------------
plot(olinda_sirgas2000[,"DEPRIV"], breaks=cI$brks, pal=pal)


## ---- echo=TRUE--------------------------------------------------------------
plot(olinda_sirgas2000[,"DEPRIV"], nbreaks=7, breaks="fisher", pal=pal)


## ---- echo=TRUE--------------------------------------------------------------
library(tmap)
tmap_mode("plot")
o <- tm_shape(olinda_sirgas2000) + tm_fill("DEPRIV", style="fisher", n=7, palette="Reds")
class(o)


## ---- echo=TRUE--------------------------------------------------------------
o


## ---- echo=TRUE--------------------------------------------------------------
o + tm_borders(alpha=0.5, lwd=0.5)


## ---- echo=TRUE, eval=FALSE--------------------------------------------------
## tmaptools::palette_explorer()


## ---- echo=TRUE--------------------------------------------------------------
library(cartography)
display.carto.all()


## ---- echo=TRUE--------------------------------------------------------------
choroLayer(olinda_sirgas2000, var="DEPRIV", method="fisher-jenks", nclass=7, col=pal, legend.values.rnd=3)


## ---- echo=TRUE--------------------------------------------------------------
library(ggplot2)


## ---- echo=TRUE--------------------------------------------------------------
g <- ggplot(olinda_sirgas2000) + geom_sf(aes(fill=DEPRIV))
g


## ---- echo=TRUE--------------------------------------------------------------
g + theme_void()


## ---- echo=TRUE--------------------------------------------------------------
g + theme_void() + scale_fill_distiller(palette="Reds", direction=1)


## ---- echo=TRUE--------------------------------------------------------------
tmap_mode("view")


## ---- echo=TRUE--------------------------------------------------------------
o + tm_borders(alpha=0.5, lwd=0.5)


## ---- echo=TRUE--------------------------------------------------------------
tmap_mode("plot")


## ---- echo=TRUE, eval=FALSE--------------------------------------------------
## tmaptools::palette_explorer()


## ---- echo=TRUE--------------------------------------------------------------
library(mapview)
if (sf:::CPL_gdal_version() >= "3.1.0") mapviewOptions(fgb = FALSE)
mapview(olinda_sirgas2000, zcol="DEPRIV", col.regions=pal, at=cI$brks)


## ----------------------------------------------------------------------------
data("pol_pres15", package = "spDataLarge")
pol_pres15 <- st_buffer(pol_pres15, dist=0)


## ----------------------------------------------------------------------------
library(tmap)
o <- tm_shape(pol_pres15) + tm_facets(free.scales=FALSE) + tm_borders(lwd=0.5, alpha=0.4) + tm_layout(panel.labels=c("Duda", "Komorowski"))


## ---- cache=TRUE-------------------------------------------------------------
o + tm_fill(c("I_Duda_share", "I_Komorowski_share"), n=6, style="pretty", title="I round\nshare of votes")


## ---- cache=TRUE-------------------------------------------------------------
o + tm_fill(c("II_Duda_share", "II_Komorowski_share"), n=6, style="pretty", title="II round\nshare of votes")

