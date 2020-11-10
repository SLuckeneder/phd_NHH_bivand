
## ---- echo=TRUE-----------------------------------------------------------------------
needed <- c("stars", "sf", "raster", "spatstat", "spatstat.data", "tmap",
            "automap", "ranger", "gstat", "RColorBrewer", "sp", "geoR")


## ---- echo = TRUE---------------------------------------------------------------------
library(geoR)
data(SIC)


## ---- echo = TRUE---------------------------------------------------------------------
plot(sic.100, borders=sic.borders, lowess=TRUE)


## ---- echo = TRUE---------------------------------------------------------------------
library(sf)
sic.100sf <- st_as_sf(cbind(as.data.frame(sic.100[[1]]), precip=sic.100[[2]], sic.100[[3]]), coords=1:2)
sic.allsf <- st_as_sf(cbind(as.data.frame(sic.all[[1]]), precip=sic.all[[2]], sic.all[[3]]), coords=1:2)


## ---- echo = TRUE---------------------------------------------------------------------
library(gstat)
hscat(precip ~ altitude, data=sic.100sf, seq(0,120,20))


## ---- echo = TRUE---------------------------------------------------------------------
g <- gstat(id="precip", formula=precip ~ altitude, data=sic.100sf)
evgm <- variogram(g, cutoff=100, width=5)
revgm <- variogram(g, cutoff=100, width=5, cressie=TRUE)
cevgm <- variogram(g, cutoff=100, width=5, cloud=TRUE)


## ---- echo = TRUE---------------------------------------------------------------------
oopar <- par(mfrow=c(1,2))
plot(gamma ~ dist, cevgm, pch=".", cex=2, col="grey65", ylab="semivariance", xlab="distance")
lines(gamma ~ dist, evgm, lwd=2)
lines(gamma ~ dist, revgm, lwd=2, lty=2)
abline(v=seq(0,100,5), lty=2, col="grey50")
plot(gamma ~ dist, evgm, ylab="semivariance", xlab="distance", type="b", lwd=2)
points(gamma ~ dist, revgm, pch=3)
lines(gamma ~ dist, revgm, lty=2, lwd=2)
abline(v=seq(0,100,5), lty=2, col="grey50")
legend("topleft", legend=c("classic", "robust"), pch=c(1,3), lty=c(1,2), bty="n", lwd=2)
par(oopar)


## ---- echo = TRUE---------------------------------------------------------------------
dbin <- findInterval(cevgm$dist, seq(0, 100, 5), all.inside=TRUE)
wid <- tapply(cevgm$gamma, dbin, length)
boxplot(cevgm$gamma ~ dbin, width=wid, ylab="semivariance", xlab="distance", axes=FALSE)
axis(2)
axis(1, at=c(0.5, 5.5, 10.5, 15.5, 20.5), labels=c(0, 25, 50, 75, 100)) 
box()
lines(gamma ~ I(dist/5), evgm, lwd=2, lty=2)


## ---- echo = TRUE---------------------------------------------------------------------
mevgm <- variogram(g, cutoff=100, width=5, map=TRUE)
aevgm <- variogram(g, cutoff=100, width=5, alpha=c(0, 45, 90, 135))


## ---- echo=TRUE-----------------------------------------------------------------------
library(RColorBrewer)
oopar <- par(mar=c(1,1,1,1))
image(mevgm$map, col=colorRampPalette(brewer.pal(7, "Blues")[-(6:7)])(20))
abline(v=0, lty=1)
abline(a=0, b=1, lty=2)
abline(h=0, lty=3)
abline(a=0, b=-1, lty=4)
par(oopar)


## ---- echo=TRUE-----------------------------------------------------------------------
library(lattice)
trellis.device(new=FALSE,color=FALSE)
plot(aevgm, multipanel=FALSE)


## ---- echo = TRUE---------------------------------------------------------------------
evg <- variog(sic.100, max.dist=200, trend=formula(~altitude), messages=FALSE)
fvg <- variofit(evg, messages=FALSE)
ml <- likfit(sic.100, ini.cov.pars=c(0.5, 0.5), trend=formula(~altitude), messages=FALSE)


## ---- echo=TRUE-----------------------------------------------------------------------
plot(evg)
lines(fvg)
lines(ml, lty=2)
legend("topleft", legend=c("WLS", "ML"), lty=1:2, bty="n")


## ---- echo = TRUE---------------------------------------------------------------------
evgm <- variogram(g, cutoff=200, width=5)
fevgm <- fit.variogram(evgm, vgm(psill=16000, model="Mat", range=30, nugget=1, kappa=0.5))
fevgm


## ---- echo=TRUE-----------------------------------------------------------------------
plot(evgm, model=fevgm)


## ---- echo = TRUE---------------------------------------------------------------------
UK_fit <- gstat(g, id="precip", model=fevgm)
z <- predict(UK_fit, newdata=sic.allsf, debug.level=0)
sic.367sf <- sic.allsf[which(z$precip.var > 0.0001),]
z <- predict(UK_fit, newdata=sic.367sf, debug.level=0)


## ---- echo = TRUE, cache=TRUE---------------------------------------------------------
kcwls <- krige.conv(sic.100, locations=st_coordinates(sic.367sf),
  krige=krige.control(obj.model=fvg, type.krige="OK",
  trend.d=formula(~altitude), trend.l=formula(~sic.367sf$altitude)))
kcml <- krige.conv(sic.100, locations=st_coordinates(sic.367sf),
 krige=krige.control(obj.model=ml, type.krige="OK",
 trend.d=formula(~altitude), trend.l=formula(~sic.367sf$altitude)))
kcB <- krige.bayes(sic.100, locations=st_coordinates(sic.367sf),
 model=model.control(trend.d=formula(~altitude),
 trend.l=formula(~sic.367sf$altitude)))


## ---- echo = TRUE---------------------------------------------------------------------
plot(density(kcB$predictive$simulations[1,]), ylim=c(0, 0.006))
abline(v=kcB$predictive$mean[1], col="red", lwd=2)
curve(dnorm(x, mean=kcB$predictive$mean[1],
 sd=sqrt(kcB$predictive$variance[1])), lty=2, lwd=2, from=-100, to=500, add=TRUE)
abline(v=sic.367sf$precip[1], col="blue", lwd=2)


## ---- echo = TRUE---------------------------------------------------------------------
library(automap)
aK <- autoKrige(formula=precip ~ altitude, input_data=as(sic.100sf, "Spatial"), new_data=as(sic.367sf, "Spatial"))
aK$var_model


## ---- echo=TRUE-----------------------------------------------------------------------
plot(aK)


## ---- echo = TRUE---------------------------------------------------------------------
#grid.dist0 <- GSIF::buffer.dist(sic.100SP["precip"], sic.367SP[1], as.factor(1:nrow(sic.100SP)))
dist0sf <- as.data.frame(st_distance(st_geometry(sic.100sf)))
names(dist0sf) <- paste("layer", names(dist0sf), sep=".")
dist1sf <- as.data.frame(st_distance(st_geometry(sic.367sf), st_geometry(sic.100sf)))
names(dist1sf) <- paste("layer", names(dist1sf), sep=".")


## ---- echo = TRUE---------------------------------------------------------------------
rm.precip <- cbind(data.frame(precip=sic.100sf$precip, altitude=sic.100sf$altitude), dist0sf)
rm.precip1 <- cbind(data.frame(altitude=sic.367sf$altitude), dist1sf)


## ---- echo = TRUE---------------------------------------------------------------------
dn0 <- paste(names(dist0sf), collapse="+")
fm0 <- as.formula(paste("precip ~ altitude +", dn0))
#fm0


## ---- echo = TRUE---------------------------------------------------------------------
library(ranger)
m.precip <- ranger(fm0, rm.precip, quantreg=TRUE, num.trees=150, seed=1)
m.precip


## ---- echo = TRUE---------------------------------------------------------------------
quantiles <- c(pnorm(1, lower.tail=FALSE), 0.5, pnorm(1))
precip.rfd <- as.data.frame(predict(m.precip, rm.precip1, type="quantiles",
                                     quantiles=quantiles)$predictions)


## ---- echo = TRUE---------------------------------------------------------------------
res <- cbind(sic.367sf[,"precip"], precip.rfd, as.data.frame(aK$krige_output)[,3:5])
res$rf_sd <- (res[[4]] - res[[2]])/2
names(res) <- make.names(names(res))
names(res)[c(2,4)] <- c("quantile= 0.159", "quantile= 0.841")


## ---- echo=TRUE-----------------------------------------------------------------------
library(tmap)
st_crs(res) <- 32662
tm_shape(res) + tm_symbols(col=c("precip", "var1.pred", "quantile..0.5"), pal="Blues", size=0.2) + tm_facets(free.scales=FALSE) + tm_layout(panel.labels=c("Preciptation", "Kriging predictons", "Random Forest predictions"))


## ---- echo=TRUE-----------------------------------------------------------------------
tm_shape(res) + tm_symbols(col=c("var1.stdev", "rf_sd"), pal="Reds", size=0.2) + tm_facets(free.scales=FALSE) + tm_layout(panel.labels=c("Kriging std. dev", "Random Forest 0.159-0.841 range"))


## ---- echo = TRUE---------------------------------------------------------------------
xy100 <- st_coordinates(sic.100sf)
xy367 <- st_coordinates(sic.367sf)
rm.precipa <- cbind(rm.precip, x=xy100[,1], y=xy100[,2])
rm.precipa1 <- cbind(rm.precip1, x=xy367[,1], y=xy367[,2])


## ---- echo = TRUE---------------------------------------------------------------------
fm1 <- update(fm0, . ~ . + x + y)


## ---- echo = TRUE---------------------------------------------------------------------
m.precipa <- ranger(fm1, rm.precipa, quantreg=TRUE, num.trees=150, seed=1)
m.precipa


## ---- echo = TRUE---------------------------------------------------------------------
quantiles <- c(pnorm(1, lower.tail=FALSE), 0.5, pnorm(1))
precipa.rfd <- as.data.frame(predict(m.precipa, rm.precipa1, type="quantiles",
                                     quantiles=quantiles)$predictions)


## ---- echo = TRUE---------------------------------------------------------------------
resa <- cbind(sic.367sf[,"precip"], precipa.rfd, as.data.frame(aK$krige_output)[,3:5])
resa$rf_sda <- (resa[[4]] - resa[[2]])/2
names(resa) <- make.names(names(resa))
names(resa)[c(2,4)] <- c("quantile= 0.159", "quantile= 0.841")


## ---- echo=TRUE-----------------------------------------------------------------------
st_crs(resa) <- 32662
tm_shape(resa) + tm_symbols(col=c("precip", "var1.pred", "quantile..0.5"), pal="Blues", size=0.2) + tm_facets(free.scales=FALSE) + tm_layout(panel.labels=c("Preciptation", "Kriging predictons", "Random Forest predictions"))


## ---- echo=TRUE-----------------------------------------------------------------------
tm_shape(resa) + tm_symbols(col=c("var1.stdev", "rf_sda"), pal="Reds", size=0.2) + tm_facets(free.scales=FALSE) + tm_layout(panel.labels=c("Kriging std. dev", "Random Forest 0.159-0.841 range"))


## ---- echo = TRUE---------------------------------------------------------------------
library(sf)
drumlins <- st_geometry(st_read("drumlins.shp"))


## ---- echo = TRUE---------------------------------------------------------------------
library(spatstat)
(drumlins_ppp <- as.ppp(drumlins))


## ---- echo = TRUE---------------------------------------------------------------------
bb <- boundingbox(drumlins_ppp)
ch <- convexhull.xy(drumlins_ppp)
rr <- ripras(drumlins_ppp)
drumlins_rr <- ppp(drumlins_ppp$x, drumlins_ppp$y, window=rr)


## ---- echo=TRUE-----------------------------------------------------------------------
plot(drumlins_ppp)
plot(bb, add=TRUE, border="darkgreen", lwd=2, lty=1)
plot(ch, add=TRUE, border="darkred", lwd=2, lty=3)
plot(rr, add=TRUE, border="orange", lwd=2, lty=2)


## ---- echo = TRUE---------------------------------------------------------------------
qc <- quadratcount(drumlins_ppp)


## ---- echo=TRUE-----------------------------------------------------------------------
plot(drumlins, cex=0.8)
t3 <- cbind(expand.grid(x=attr(qc, "xbreaks")[1:5] + diff(attr(qc, "xbreaks"))[1]/2, y=rev(attr(qc, "ybreaks")[1:5] + diff(attr(qc, "ybreaks"))[1]/2)), qc=c(t(qc)))
text(t3[,1], t3[,2], t3[,3], cex=1.2, font=2, col="darkred")
abline(h=attr(qc, "ybreaks"))
abline(v=attr(qc, "xbreaks"))


## ---- echo = TRUE---------------------------------------------------------------------
quadrat.test(drumlins_ppp)


## ---- echo = TRUE---------------------------------------------------------------------
quadrat.test(drumlins_ppp, nx=6)
quadrat.test(drumlins_rr)


## ---- echo = TRUE---------------------------------------------------------------------
crds <- crds <- st_coordinates(st_sample(st_as_sfc(rr), size=10000, type="regular"))
crds <- list(x=crds[,1], y=crds[,2])
library(raster)
k02 <- as(density(drumlins_rr, sigma=0.2, xy=crds), "RasterLayer")
k04 <- as(density(drumlins_rr, sigma=0.4, xy=crds), "RasterLayer")
k06 <- as(density(drumlins_rr, sigma=0.6, xy=crds), "RasterLayer")
k08 <- as(density(drumlins_rr, sigma=0.8, xy=crds), "RasterLayer")
rB <- brick(k02, k04, k06, k08)
library(stars)
rB_st <- st_as_stars(rB)


## ---- echo=TRUE-----------------------------------------------------------------------
library(tmap)
st_crs(rB_st) <- 32662
st_crs(drumlins) <- 32662
tm_shape(rB_st) + tm_raster(title="Density") + tm_layout(panel.labels=c("0.2", "0.4", "0.6", "0.8")) + tm_shape(drumlins) + tm_symbols(size=0.25, shape=4)


## ---- echo = TRUE---------------------------------------------------------------------
summary(rB)


## ---- echo = TRUE---------------------------------------------------------------------
boxplot(rB)


## ---- echo = TRUE---------------------------------------------------------------------
nns <- nndist(drumlins_rr)
summary(nns)


## ---- echo=TRUE-----------------------------------------------------------------------
plot(ecdf(nns))


## ---- echo = TRUE, eval=FALSE---------------------------------------------------------
## plot(ecdf(nns), xlim=c(0, 0.5))
## plot(Gest(drumlins_ppp), add=TRUE, lwd=3)


## ---- echo = TRUE---------------------------------------------------------------------
n <- drumlins_rr$n
set.seed(121122)
ex <- expression(runifpoint(n, win=rr))
res <- envelope(drumlins_rr, Gest, nsim=99, simulate=ex, 
        verbose=FALSE, savefuns=TRUE)


## ---- echo=TRUE-----------------------------------------------------------------------
plot(res, xlim=c(0,0.7))
for(i in 2:100) lines(attr(res, "simfuns")[[1]], attr(res, "simfuns")[[i]], col="grey")
plot(res, add=TRUE, lwd=3, xlim=c(0,0.7))


## ---- echo = TRUE---------------------------------------------------------------------
clarkevans(drumlins_ppp)
clarkevans(drumlins_rr, correction="none")
clarkevans(drumlins_rr, correction="guard", clipregion=erosion.owin(rr, r=1))


## ---- echo = TRUE, cache=TRUE---------------------------------------------------------
ex <- expression(rSSI(0.18, n, win=rr))
set.seed(121122)
res <- envelope(drumlins_rr, Gest, nsim=99, simulate=ex, 
                verbose=FALSE, savefuns=TRUE)


## ---- echo=TRUE-----------------------------------------------------------------------
null <- capture.output(plot(res, xlim=c(0,0.7)))
for(i in 2:100) lines(attr(res, "simfuns")[[1]], attr(res, "simfuns")[[i]], col="grey")
null <- capture.output(plot(res, add=TRUE, lwd=3, xlim=c(0,0.7)))


## ---- echo = TRUE, cache=TRUE---------------------------------------------------------
ex <- expression(runifpoint(n, win=rr))
set.seed(121122)
res <- envelope(drumlins_rr, Kest, nsim=99, simulate=ex, 
                verbose=FALSE, savefuns=TRUE)


## ---- echo=TRUE-----------------------------------------------------------------------
r <- res$r
Lhat <- function(k, r) { (sqrt(k/pi)) - r }
plot(r, Lhat(res$obs, r), type="n", ylab="L(r)", main="CSR simulation", ylim=c(-0.17, 0.1))
for(i in 2:100) lines(r, Lhat(attr(res, "simfuns")[[i]], r), col="grey")
lines(r, Lhat(res$obs, r), lwd=2, col="brown")
lines(r, Lhat(res$lo, r), lwd=2, col="black", lty=2)
lines(r, Lhat(res$hi, r), lwd=2, col="black", lty=2)


## ---- echo = TRUE, cache=TRUE---------------------------------------------------------
ex <- expression(rSSI(0.18, n, win=rr))
set.seed(121122)
res <- envelope(drumlins_rr, Kest, nsim=99, simulate=ex, 
                verbose=FALSE, savefuns=TRUE)


## ---- echo=TRUE-----------------------------------------------------------------------
r <- res$r
Lhat <- function(k, r) { (sqrt(k/pi)) - r }
plot(r, Lhat(res$obs, r), type="n", ylab="L(r)", main="SSI simulation", ylim=c(-0.17, 0.1))
for(i in 2:100) lines(r, Lhat(attr(res, "simfuns")[[i]], r), col="grey")
lines(r, Lhat(res$obs, r), lwd=2, col="brown")
lines(r, Lhat(res$lo, r), lwd=2, col="black", lty=2)
lines(r, Lhat(res$hi, r), lwd=2, col="black", lty=2)


## ---- echo = TRUE, cache=TRUE---------------------------------------------------------
ex <- expression(runifpoint(n, win=rr))
set.seed(121122)
res <- envelope(drumlins_rr, Kinhom, nsim=99, simulate=ex, 
                verbose=FALSE, savefuns=TRUE)


## ---- echo=TRUE-----------------------------------------------------------------------
r <- res$r
Lhat <- function(k, r) { (sqrt(k/pi)) - r }
plot(r, Lhat(res$obs, r), type="n", ylab="L(r)", main="CSR simulation", ylim=c(-0.17, 0.1))
for(i in 2:100) lines(r, Lhat(attr(res, "simfuns")[[i]], r), col="grey")
lines(r, Lhat(res$obs, r), lwd=2, col="brown")
lines(r, Lhat(res$lo, r), lwd=2, col="black", lty=2)
lines(r, Lhat(res$hi, r), lwd=2, col="black", lty=2)


## ---- echo = TRUE, cache=TRUE---------------------------------------------------------
ex <- expression(rSSI(0.18, n, win=rr))
set.seed(121122)
res <- envelope(drumlins_rr, Kinhom, nsim=99, simulate=ex, 
                verbose=FALSE, savefuns=TRUE)


## ---- echo=TRUE-----------------------------------------------------------------------
r <- res$r
Lhat <- function(k, r) { (sqrt(k/pi)) - r }
plot(r, Lhat(res$obs, r), type="n", ylab="L(r)", main="SSI simulation", ylim=c(-0.17, 0.1))
for(i in 2:100) lines(r, Lhat(attr(res, "simfuns")[[i]], r), col="grey")
lines(r, Lhat(res$obs, r), lwd=2, col="brown")
lines(r, Lhat(res$lo, r), lwd=2, col="black", lty=2)
lines(r, Lhat(res$hi, r), lwd=2, col="black", lty=2)


## ---- echo = TRUE---------------------------------------------------------------------
points <- st_read("points.shp")
rivers <- st_geometry(st_read("rivers.shp"))
poly <- st_geometry(st_read("poly.shp"))


## ---- echo=TRUE-----------------------------------------------------------------------
plot(poly)
plot(rivers, add=TRUE)
plot(st_geometry(points), pch=3:4, add=TRUE)


## ---- echo = TRUE---------------------------------------------------------------------
points$mark <- factor(points$mark)
points.ppp <- as.ppp(points)
points.ppp$window <- as.owin(poly)
summary(points.ppp)


## ---- echo=TRUE-----------------------------------------------------------------------
plot(split(points.ppp))


## ---- echo = TRUE---------------------------------------------------------------------
XY <- st_coordinates(st_sample(poly, size=10000, type="regular"))
XY <- list(x=XY[,1], y=XY[,2])
Z <- density(points.ppp, xy=XY)


## ---- echo=TRUE-----------------------------------------------------------------------
plot(Z)


## ---- echo = TRUE---------------------------------------------------------------------
Z <- density(split(points.ppp), xy=XY)


## ---- echo=TRUE-----------------------------------------------------------------------
plot(Z)


## ---- echo = TRUE---------------------------------------------------------------------
pCases <- with(Z, eval.im(case/(case + control)))


## ---- echo=TRUE-----------------------------------------------------------------------
plot(pCases)
plot(points.ppp, add=TRUE)

