
## ---- echo=TRUE--------------------------------------------------------------
needed <- c("igraph", "tmap", "spdep", "spData", "sp", "gstat", "sf", "spatstat", "spatstat.data")


## ---- echo=TRUE--------------------------------------------------------------
suppressPackageStartupMessages(library(spatstat))
intenfun <- function(x, y) 200 * x
set.seed(1)
(ihpp <- rpoispp(intenfun, lmax = 200))


## ---- echo=TRUE, out.width='90%', fig.align='center'-------------------------
plot(density(ihpp), axes=TRUE)
points(ihpp, col="green2", pch=19)


## ---- echo=TRUE, out.width='90%', fig.align='center'-------------------------
opar <- par(mfrow=c(1,2))
plot(envelope(ihpp, Kest, verbose=FALSE), main="Homogeneous")
plot(envelope(ihpp, Kinhom, verbose=FALSE), main="Inhomogeneous")
par(opar)


## ---- echo=TRUE, out.width='90%', fig.align='center'-------------------------
library(sf)
sf_ihpp <- subset(st_as_sf(ihpp), label == "point")
#st_as_sf(ihpp) %>% dplyr::filter(label == "point") -> sf_ihpp
#st_as_sf(ihpp) ->.; .[.$label == "point",] -> sf_ihpp
crds <- st_coordinates(sf_ihpp)
sf_ihpp$x <- crds[,1]
sf_ihpp$y <- 100 + 50 * sf_ihpp$x + 20 * rnorm(nrow(sf_ihpp))
plot(sf_ihpp[,"y"], pch=19)


## ---- echo=TRUE, out.width='90%', fig.align='center'-------------------------
suppressPackageStartupMessages(library(gstat))
vg0 <- variogram(y ~ 1, sf_ihpp)
vg1 <- variogram(y ~ x, sf_ihpp)
library(ggplot2)
g0 <- ggplot(vg0, aes(x=dist, y=gamma)) + geom_point() + geom_smooth(span=1) + ylim(300, 575) + ggtitle("Trend ignored")
g1 <- ggplot(vg1, aes(x=dist, y=gamma)) + geom_point() + geom_smooth(span=1) + ylim(300, 575) + ggtitle("Trend included")
gridExtra::grid.arrange(g0, g1, ncol=2)


## ---- echo=TRUE, results='hide', message=FALSE-------------------------------
suppressPackageStartupMessages(library(spdep))
nb_tri <- tri2nb(crds)


## ---- echo=TRUE--------------------------------------------------------------
(nb_soi <- graph2nb(soi.graph(nb_tri, crds), sym=TRUE))


## ---- echo=TRUE, out.width='90%', fig.align='center'-------------------------
plot(nb_soi, crds)


## ---- echo=TRUE--------------------------------------------------------------
comps <- n.comp.nb(nb_soi)
sf_ihpp$comps <- comps$comp.id
comps$nc


## ---- echo=TRUE, warning=FALSE, message=FALSE--------------------------------
lwB <- nb2listw(nb_soi, style="B")
out <- broom::tidy(moran.test(sf_ihpp$y, listw=lwB, randomisation=FALSE, alternative="two.sided"))[1:5]
names(out)[1:3] <- c("Moran's I", "Expectation", "Variance"); out


## ---- echo=TRUE--------------------------------------------------------------
lm_obj <- lm(y ~ x, data=sf_ihpp)
out <- broom::tidy(lm.morantest(lm_obj, listw=lwB, alternative="two.sided"))[1:5]
names(out)[1:3] <- c("Moran's I", "Expectation", "Variance"); out


## ---- echo=TRUE--------------------------------------------------------------
lmor0 <- localmoran(sf_ihpp$y, listw=lwB, alternative="two.sided")
lmor1 <- as.data.frame(localmoran.sad(lm_obj, nb=nb_soi, style="B", alternative="two.sided"))
sf_ihpp$z_value <- lmor0[,4]
sf_ihpp$z_lmor1_N <- lmor1[,2]
sf_ihpp$z_lmor1_SAD <- lmor1[,4]


## ---- echo=TRUE, warning=FALSE, out.width='90%', fig.align='center', width=7, height=4----
suppressPackageStartupMessages(library(tmap))
tm_shape(sf_ihpp) + tm_symbols(col=c("z_value", "z_lmor1_N", "z_lmor1_SAD"), midpoint=0) + tm_facets(free.scales=FALSE, nrow=1) + tm_layout(panel.labels=c("No trend", "Trend, normal", "Trend, SAD"))


## ---- echo=TRUE--------------------------------------------------------------
library(sf)
nc <- st_read(system.file("shapes/sids.shp", package="spData")[1], quiet=TRUE)
st_crs(nc) <- "+proj=longlat +datum=NAD27"
row.names(nc) <- as.character(nc$FIPSNO)
head(nc)


## ---- echo=TRUE, warning=FALSE-----------------------------------------------
library(spdep)
gal_file <- system.file("weights/ncCR85.gal", package="spData")[1]
ncCR85 <- read.gal(gal_file, region.id=nc$FIPSNO)
ncCR85


## ---- echo=TRUE, warning=TRUE, out.width='90%', fig.align='center', width=7, height=4----
plot(st_geometry(nc), border="grey")
plot(ncCR85, st_centroid(st_geometry(nc), of_largest_polygon), add=TRUE, col="blue")


## ---- echo=TRUE--------------------------------------------------------------
set.seed(1)
nc$rand <- rnorm(nrow(nc))
lw <- nb2listw(ncCR85, style="B")
moran.test(nc$rand, listw=lw, alternative="two.sided")


## ---- echo=TRUE--------------------------------------------------------------
nc$LM <- as.numeric(interaction(nc$L_id, nc$M_id))
alpha <- 1
beta <- 0.5
sigma <- 2
nc$trend <- alpha + beta*nc$LM + sigma*nc$rand
moran.test(nc$trend, listw=lw, alternative="two.sided")


## ---- echo=TRUE--------------------------------------------------------------
lm.morantest(lm(trend ~ LM, nc), listw=lw, alternative="two.sided")


## ---- echo=TRUE--------------------------------------------------------------
aggLM <- aggregate(nc[,"LM"], list(nc$LM), head, n=1)
(aggnb <- poly2nb(aggLM))


## ---- echo=TRUE--------------------------------------------------------------
set.seed(1)
LMrand <- rnorm(nrow(aggLM))


## ---- echo=TRUE--------------------------------------------------------------
moran.test(LMrand, nb2listw(aggnb, style="B"))


## ---- echo=TRUE--------------------------------------------------------------
nc$LMrand <- LMrand[match(nc$LM, aggLM$LM)]
plot(nc[,"LMrand"])


## ---- echo=TRUE--------------------------------------------------------------
moran.test(nc$LMrand, listw=lw, alternative="two.sided")


## ---- echo=TRUE--------------------------------------------------------------
library(sf)
# if(!require("spData", quietly=TRUE)) install.packages("spData")
# if(!require("spDataLarge", quietly=TRUE)) install.packages("spDataLarge",
#   repos = "https://nowosad.github.io/drat/", type = "source")
data(pol_pres15, package="spDataLarge")
pol_pres15 <- st_buffer(pol_pres15, dist=0)
head(pol_pres15[, c(1, 4, 6)])


## ---- echo=TRUE, eval=FALSE--------------------------------------------------
## ?spDataLarge::pol_pres15


## ---- echo=TRUE--------------------------------------------------------------
suppressPackageStartupMessages(library(spdep))
system.time(nb_q <- poly2nb(pol_pres15, queen=TRUE))


## ---- echo=TRUE--------------------------------------------------------------
nb_q


## ---- echo=TRUE--------------------------------------------------------------
opar <- par(mar=c(0,0,0,0)+0.5)
plot(st_geometry(pol_pres15), border="grey", lwd=0.5)
coords <- st_centroid(st_geometry(pol_pres15),
  of_largest_polygon=TRUE)
plot(nb_q, coords=st_coordinates(coords), add=TRUE, points=FALSE, lwd=0.5)
par(opar)


## ---- echo=TRUE--------------------------------------------------------------
system.time({
  fB1 <- st_intersects(st_as_sfc(lapply(st_geometry(pol_pres15), function(x) {
    st_as_sfc(st_bbox(x))[[1]]
  })))
  fB1a <- lapply(seq_along(fB1), function(i) fB1[[i]][fB1[[i]] > i])
  fB1a <- fB1a[-length(fB1a)]
  nb_sf_q1 <- poly2nb(pol_pres15, queen=TRUE, foundInBox=fB1a)
})


## ---- echo=TRUE--------------------------------------------------------------
all.equal(nb_q, nb_sf_q1, check.attributes=FALSE)


## ---- echo=TRUE--------------------------------------------------------------
n.comp.nb(nb_q)$nc


## ---- echo=TRUE--------------------------------------------------------------
lw <- nb2listw(nb_q, style="B")


## ---- echo=TRUE, results='hide', message=FALSE-------------------------------
library(Matrix)
W <- as(lw, "CsparseMatrix")


## ---- echo=TRUE--------------------------------------------------------------
isTRUE(all.equal(W, t(W)))


## ---- echo=TRUE--------------------------------------------------------------
image(W)


## ---- echo=TRUE--------------------------------------------------------------
WW <- W %*% W
image(WW)


## ---- echo=TRUE--------------------------------------------------------------
W3 <- WW %*% W
image(W3)


## ---- echo=TRUE--------------------------------------------------------------
W4 <- W3 %*% W
image(W4)


## ---- echo=TRUE, message=FALSE-----------------------------------------------
library(igraph)
g1 <- graph.adjacency(W, mode="undirected")
class(g1)


## ---- echo=TRUE--------------------------------------------------------------
B1 <- get.adjacency(g1)
mat2listw(B1)$neighbours


## ---- echo=TRUE--------------------------------------------------------------
c1 <- clusters(g1)
c1$no


## ---- echo=TRUE--------------------------------------------------------------
is.connected(g1)


## ---- echo=TRUE--------------------------------------------------------------
dg1 <- diameter(g1)
dg1


## ---- echo=TRUE--------------------------------------------------------------
sp_mat <- shortest.paths(g1)
max(sp_mat)
str(sp_mat)


## ---- echo=TRUE--------------------------------------------------------------
gra <- R2BayesX::nb2gra(ncCR85)
str(gra)


## ---- echo=TRUE--------------------------------------------------------------
tf <- tempfile()
nb2INLA(tf, ncCR85)
file.size(tf)


## ---- echo=TRUE, warning=FALSE-----------------------------------------------
coords <- st_centroid(st_geometry(nc), of_largest_polygon)
(knn_5_nb <- knn2nb(knearneigh(st_coordinates(coords), k=5)))


## ---- echo=TRUE--------------------------------------------------------------
klw <- nb2listw(knn_5_nb, style="B")
kW <- as(klw, "CsparseMatrix")
isTRUE(all.equal(kW, t(kW)))


## ---- echo=TRUE--------------------------------------------------------------
image(kW)


## ---- echo=TRUE, messages=FALSE----------------------------------------------
library(igraph)
g1 <- graph.adjacency(kW, mode="directed")
B1 <- get.adjacency(g1)
mat2listw(B1)$neighbours


## ---- echo=TRUE--------------------------------------------------------------
diameter(g1)


## ---- echo=TRUE--------------------------------------------------------------
nc_sps <- shortest.paths(g1)
mr <- which.max(apply(nc_sps, 2, max))
nc$sps1 <- nc_sps[,mr]
plot(nc[,"sps1"], breaks=0:21)

## ---- echo=TRUE--------------------------------------------------------------
plot(nc$sps1, c(st_distance(coords[mr], coords))/1000, xlab="shortest path count", ylab="km distance")


## ---- echo=TRUE--------------------------------------------------------------
ncCR85a <- ncCR85
attr(ncCR85a, "region.id") <- as.character(nc$CRESS_ID)
nc_gra <- R2BayesX::nb2gra(ncCR85a)
nc_tf <- tempfile()
nb2INLA(nc_tf, ncCR85)
nc_lw <- nb2listw(ncCR85, style="B")
nc_W <- as(nc_lw, "CsparseMatrix")
nc_mat <- listw2mat(nc_lw)


## ---- echo=TRUE--------------------------------------------------------------
nc$ft.SID74 <- sqrt(1000)*(sqrt(nc$SID74/nc$BIR74) + sqrt((nc$SID74+1)/nc$BIR74))
nc$ft.NWBIR74 <- sqrt(1000)*(sqrt(nc$NWBIR74/nc$BIR74) + sqrt((nc$NWBIR74+1)/nc$BIR74))
tm_shape(nc) + tm_fill(c("ft.SID74", "ft.NWBIR74"))


## ---- echo=TRUE--------------------------------------------------------------
plot(ft.SID74 ~ ft.NWBIR74, nc)


## ---- echo=TRUE--------------------------------------------------------------
moran.test(nc$ft.SID74, nc_lw, alternative="two.sided", randomisation=FALSE)


## ---- echo=TRUE--------------------------------------------------------------
lm.morantest(lm(ft.SID74 ~ 1, weights=BIR74, data=nc), nc_lw, alternative="two.sided")


## ---- echo=TRUE--------------------------------------------------------------
lm.morantest(lm(ft.SID74 ~ ft.NWBIR74, data=nc), nc_lw, alternative="two.sided")


## ---- echo=TRUE--------------------------------------------------------------
lm.morantest(lm(ft.SID74 ~ ft.NWBIR74, weights=BIR74, data=nc), nc_lw, alternative="two.sided")

