
## ---- echo=TRUE--------------------------------------------------------------
needed <- c("hglm", "hglm.data", "tmap", "spatialreg", "spdep", "spData", "sp", "sf")


## ---- message=FALSE, warning=FALSE-------------------------------------------
library(sf)
nc <- st_read(system.file("shapes/sids.shp", package="spData")[1], quiet=TRUE)
st_crs(nc) <- "+proj=longlat +datum=NAD27"
row.names(nc) <- as.character(nc$FIPSNO)
nc$ft.SID74 <- sqrt(1000)*(sqrt(nc$SID74/nc$BIR74) + sqrt((nc$SID74+1)/nc$BIR74))
nc$ft.NWBIR74 <- sqrt(1000)*(sqrt(nc$NWBIR74/nc$BIR74) + sqrt((nc$NWBIR74+1)/nc$BIR74))
library(spdep)
gal_file <- system.file("weights/ncCR85.gal", package="spData")[1]
ncCR85 <- read.gal(gal_file, region.id=nc$FIPSNO)
nc_lw <- nb2listw(ncCR85, style="B")
nc_W <- as(nc_lw, "CsparseMatrix")


## ---- echo=TRUE, results='hide', message=FALSE, warning=FALSE----------------
library(spatialreg)
m1 <- spautolm(ft.SID74 ~ ft.NWBIR74, weights=BIR74, data=nc, listw=nc_lw, family="SAR")


## ---- echo=TRUE--------------------------------------------------------------
summary(m1)


## ---- echo=TRUE--------------------------------------------------------------
nc$SAR_ssre <- 2*as.vector((m1$lambda * nc_W) %*% m1$Y - (m1$lambda * nc_W) %*% (m1$X %*% m1$fit$coefficients))
library(tmap)
tm_shape(nc) + tm_fill(c("ft.SID74", "SAR_ssre"), midpoint=c(NA, 0))


## ---- echo=TRUE, message=FALSE, warning=FALSE--------------------------------
m1a <- errorsarlm(ft.SID74 ~ ft.NWBIR74, weights=BIR74, data=nc, listw=nc_lw)
summary(m1a, Hausman=TRUE)


## ---- echo=TRUE, message=FALSE, warning=FALSE--------------------------------
m1b <- errorsarlm(ft.SID74 ~ ft.NWBIR74, weights=BIR74, data=nc, listw=nc_lw, Durbin=TRUE)
summary(m1b, Hausman=TRUE)


## ---- echo=TRUE, message=FALSE, warning=FALSE--------------------------------
summary(impacts(m1b))


## ---- echo=TRUE, results='hide', message=FALSE, warning=FALSE----------------
library(hglm)
E <- nc$BIR74 * sum(nc$SID74)/sum(nc$BIR74)
HGLM_iid <- hglm(fixed=SID74 ~ ft.NWBIR74, random= ~ 1|CRESS_ID, offset=log(E), weights=BIR74,
                 data=nc, family=poisson(link=log))


## ---- echo=TRUE--------------------------------------------------------------
ranef_iid <- unname(summary(HGLM_iid, print.ranef=TRUE)$RandCoefMat)
metafor::forest(ranef_iid[,1], ranef_iid[,2], subset=order(ranef_iid[,1], decreasing=TRUE), 
        slab=NA, annotate=FALSE, lty=c("solid","blank"), pch=19, psize=2, cex.lab=1, cex.axis=1)


## ---- echo=TRUE, results='hide', message=FALSE, cache=TRUE, warning=FALSE----
HGLM_sar <- hglm(fixed=SID74 ~ ft.NWBIR74, random= ~ 1|CRESS_ID, offset=log(E), weights=BIR74, 
                 data=nc, family=poisson(link=log), rand.family=SAR(D=nc_W))
ranef_sar <- unname(summary(HGLM_sar, print.ranef=TRUE)$RandCoefMat)
metafor::forest(ranef_sar[,1], ranef_sar[,2], subset=order(ranef_sar[,1], decreasing=TRUE), 
        slab=NA, annotate=FALSE, lty=c("solid","blank"), pch=19, psize=2, cex.lab=1, cex.axis=1)


## ---- echo=TRUE--------------------------------------------------------------
nc$HGLM_re <- ranef_iid[,1]
nc$HGLM_ss_SAR <- ranef_sar[,1]
tm_shape(nc) + tm_fill(c("HGLM_re", "HGLM_ss_SAR"), midpoint=c(0), title="Poisson HGLM RE") +
  tm_facets(free.scales=FALSE) + tm_layout(panel.labels=c("IID", "SAR SSRE"))


## ---- echo=TRUE--------------------------------------------------------------
m1c <- spautolm(ft.SID74 ~ ft.NWBIR74, weights=BIR74, data=nc, listw=nc_lw, family="CAR")
summary(m1c)


## ---- echo=TRUE--------------------------------------------------------------
nc$CAR_ssre <- as.vector((m1c$lambda * nc_W) %*% m1c$Y - 
                           (m1c$lambda * nc_W) %*% (m1c$X %*% m1c$fit$coefficients))
tm_shape(nc) + tm_fill(c("SAR_ssre", "CAR_ssre"), midpoint=c(0), title="Gauss ML RE") +
  tm_facets(free.scales=FALSE) + tm_layout(panel.labels=c("SAR SSRE", "CAR SSRE"))


## ---- echo=TRUE, results='hide', message=FALSE, cache=TRUE, warning=FALSE----
HGLM_car <- hglm(fixed=SID74 ~ ft.NWBIR74, random= ~ 1|CRESS_ID, offset=log(E), weights=BIR74, 
                 data=nc, family=poisson(link=log), rand.family=CAR(D=nc_W))
ranef_car <- unname(summary(HGLM_car, print.ranef=TRUE)$RandCoefMat)
metafor::forest(ranef_car[,1], ranef_car[,2], subset=order(ranef_car[,1], decreasing=TRUE), 
        slab=NA, annotate=FALSE, lty=c("solid","blank"), pch=19, psize=2, cex.lab=1, cex.axis=1)


## ---- echo=TRUE--------------------------------------------------------------
nc$HGLM_ss_CAR <- ranef_car[,1]
tm_shape(nc) + tm_fill(c("HGLM_ss_CAR", "HGLM_ss_SAR"), midpoint=c(0), title="Poisson HGLM RE") +
  tm_facets(free.scales=FALSE) + tm_layout(panel.labels=c("CAR SSRE", "SAR SSRE"))


## ---- echo=TRUE, results='hide', message=FALSE, cache=TRUE, warning=FALSE----
nc$LM <- as.numeric(interaction(nc$L_id, nc$M_id))
aggLM <- aggregate(nc[,"LM"], list(nc$LM), head, n=1)
aggnb <- poly2nb(aggLM)
library(mgcv)
names(aggnb) <- as.character(aggLM$Group.1)
nc$LM <- as.factor(nc$LM)
GAM_mrf <- gam(SID74 ~ s(ft.NWBIR74) + s(LM, bs="mrf", xt=list(nb=aggnb)), offset=log(E), weights=BIR74, data=nc, family=poisson(link=log))
summary(GAM_mrf)


## ---- echo=TRUE--------------------------------------------------------------
library(mgcv)
plot(GAM_mrf)


## ---- echo=TRUE--------------------------------------------------------------
GAM_mrf_re <- predict(GAM_mrf, type="terms", se=TRUE)
metafor::forest(GAM_mrf_re$fit[,2], GAM_mrf_re$se.fit[,2], subset=order(GAM_mrf_re$fit[,1], decreasing=TRUE), 
        slab=NA, annotate=FALSE, lty=c("solid","blank"), pch=19, psize=2, cex.lab=1, cex.axis=1)


## ---- echo=TRUE--------------------------------------------------------------
nc$GAM_mrf_re <- GAM_mrf_re$fit[,2]
tm_shape(nc) + tm_fill(c("GAM_mrf_re"), midpoint=c(0), title="Poisson GAM MRF RE")


## ---- echo=TRUE--------------------------------------------------------------
NY8 <- st_read(system.file("shapes/NY8_utm18.shp", package="spData"))
NY8 <- st_buffer(NY8, dist=0)


## ---- echo=TRUE--------------------------------------------------------------
tm_shape(NY8) + tm_fill("Z", midpoint=0)


## ---- echo=TRUE--------------------------------------------------------------
NY_nb <- poly2nb(NY8)
NY_lw <- nb2listw(NY_nb, style="B")


## ---- echo=TRUE--------------------------------------------------------------
mod1 <- spautolm(Z ~ PEXPOSURE + PCTAGE65P + PCTOWNHOME, data=NY8, family="CAR", listw=NY_lw, weights=POP8)
summary(mod1)

