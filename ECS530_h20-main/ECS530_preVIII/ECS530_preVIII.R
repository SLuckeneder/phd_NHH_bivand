
## ---- echo=TRUE-----------------------------------------------------------------------
needed <- c("sphet", "spatialreg", "spdep", "spData", "sf", "HSAR", "sp")



## ----load_data, echo = TRUE-----------------------------------------------------------
library(sp)
library(HSAR)
data(landprice)
data(landSPDF)


## ----merge_data1, echo = TRUE---------------------------------------------------------
library(sf)
landprice1 <- st_as_sf(landSPDF)
landprice1 <- merge(landprice1, landprice, by="obs")
landprice2 <- landprice1[order(landprice1$district.id.x),]


## ----strip_data1, echo = TRUE---------------------------------------------------------
landprice2$fyear <- factor(landprice2$year + 2003)
landprice2$f_district.id.x <- factor(landprice2$district.id.x)


## ----strip_data2, echo = TRUE---------------------------------------------------------
landprice2$price <- exp(landprice2$lnprice)
landprice2$area <- exp(landprice2$lnarea)
landprice2$Dcbd <- exp(landprice2$lndcbd)
landprice2$Dsubway <- exp(landprice2$dsubway)
landprice2$Dpark <- exp(landprice2$dpark)
landprice2$Dele <- exp(landprice2$dele)


## ----W_weights, echo = TRUE-----------------------------------------------------------
suppressPackageStartupMessages(library(spdep))
dnb1.5 <- dnearneigh(landprice2, 0, 1500, row.names=row.names(landprice2))
dists <- nbdists(dnb1.5, st_coordinates(landprice2))
edists <- lapply(dists, function(x) exp((-((x/1000)^2))/(1.5^2)))
ozpo <- set.ZeroPolicyOption(TRUE)
lw <- nb2listw(dnb1.5, glist=edists, style="W")
library(spatialreg)
W <- as(lw, "CsparseMatrix")
trs <- trW(W, m=50)


## ----form_lm, echo = TRUE-------------------------------------------------------------
form <- log(price) ~ log(area) + log(Dcbd) + log(Dele) + log(Dpark) + log(Dsubway) + 
  crimerate + popden + fyear
OLS <- lm(form, data=landprice2)


## ----mI, echo = TRUE------------------------------------------------------------------
lm.morantest(OLS, lw)


## ----SLX, echo = TRUE-----------------------------------------------------------------

SLX <- lmSLX(form, data=landprice2, listw=lw, zero.policy=TRUE)


## ----mI_SLX, echo = TRUE--------------------------------------------------------------
class(SLX)
lm.morantest(SLX, lw)


## ----imp_SLX, echo = TRUE-------------------------------------------------------------
imps_SLX <- impacts(SLX)
imps_SLX


## ----SEM, echo = TRUE-----------------------------------------------------------------
e <- eigenw(lw)
int <- 1/range(e)
SEM <- errorsarlm(form, data=landprice2, listw=lw, interval=int, control=list(pre_eig=e), zero.policy=TRUE)
Hausman.test(SEM)


## ----GMM1, echo = TRUE----------------------------------------------------------------
library(sphet)
ozpo <- set.ZeroPolicyOption(TRUE)
GM_SEM <- spreg(form, data=landprice2, listw=lw, model="error")


## ----SDEM, echo = TRUE----------------------------------------------------------------
SDEM <- errorsarlm(form, data=landprice2, listw=lw, etype="emixed",
 interval=int, control=list(pre_eig=e), zero.policy=TRUE)
Hausman.test(SDEM)


## ----imp_SDEM, echo = TRUE------------------------------------------------------------
imps_SDEM <- spatialreg::impacts(SDEM)
imps_SDEM


## ----SAR, echo = TRUE-----------------------------------------------------------------
SAR <- lagsarlm(form, data=landprice2, listw=lw, type="lag", interval=int, control=list(pre_eig=e), zero.policy=TRUE)
digits <- max(5, .Options$digits - 3)
cat("test value: ", format(signif(SAR$LMtest, digits)), ", p-value: ",
 format.pval((1 - pchisq(SAR$LMtest, 1)), digits), "\n", sep="")


## ----imp_SDM, echo = TRUE-------------------------------------------------------------
spatialreg::impacts(SAR, tr=trs)


## ----GMM2, echo = TRUE----------------------------------------------------------------
library(sphet)
GM_SAR <- spreg(form, data=landprice2, listw=lw, model="lag")


## ----imp_GM_SAR, echo = TRUE----------------------------------------------------------
m <- nrow(GM_SAR$coefficients)
GM_SAR$coefficients <- drop(GM_SAR$coefficients)
GM_SAR$coefficients <- GM_SAR$coefficients[c(m, (1:(m-1)))]
GM_SAR$var <- GM_SAR$var[c(m, (1:(m-1))), c(m, (1:(m-1)))]
# impacts(GM_SAR, tr=trs)


## ----SDM, echo = TRUE-----------------------------------------------------------------
SDM <- lagsarlm(form, data=landprice2, listw=lw, type="mixed", interval=int, control=list(pre_eig=e), zero.policy=TRUE)
digits <- max(5, .Options$digits - 3)
cat("test value: ", format(signif(SDM$LMtest, digits)), ", p-value: ",
 format.pval((1 - pchisq(SDM$LMtest, 1)), digits), "\n", sep="")


## ----LR1, echo = TRUE-----------------------------------------------------------------
cat(capture.output(LR.sarlm(SAR, SDM))[c(7,8,5)], sep="\n")
cat(capture.output(LR.sarlm(SEM, SDM))[c(7,8,5)], sep="\n")
cat(capture.output(LR.sarlm(SEM, SDEM))[c(7,8,5)], sep="\n")


## ----LR2, echo = TRUE-----------------------------------------------------------------
cat(capture.output(LR.sarlm(SLX, SDM))[c(7,8,5)], sep="\n")
cat(capture.output(LR.sarlm(SLX, SDEM))[c(7,8,5)], sep="\n")


## ----imp_SDM0, echo = TRUE------------------------------------------------------------
set.seed(1)
imps <- spatialreg::impacts(SDM, tr=trs, R=2000)
imps


## ----imp_SDM1, echo = TRUE------------------------------------------------------------
IrW1 <- invIrW(lw, rho=coef(SDM)[1])
n <- nrow(landprice2)
S_area <- IrW1 %*% ((diag(n) * coef(SDM)[3]) + (coef(SDM)[16] * W))
sum(S_area)/n
sum(diag(S_area))/n


## ----imp_SDM2, echo = TRUE------------------------------------------------------------
newdata <- landprice2
suppressWarnings(p0 <- predict(SDM, newdata=newdata, listw=lw))
newdata$area <- exp(log(newdata$area)+1)
suppressWarnings(p1 <- predict(SDM, newdata=newdata, listw=lw))
mean(p1-p0)


## ----BSDM, echo = TRUE----------------------------------------------------------------
set.seed(1)
BSDM <- spBreg_lag(form, data=landprice2, listw=lw, type="Durbin")


## ----imp_BSDM, echo = TRUE------------------------------------------------------------
impsB <- spatialreg::impacts(BSDM, tr=trs)
impsB


## ----fig2, echo=TRUE, eval=TRUE-------------------------------------------------------
plot(density(impsB$sres$total[,1]), lty=4, col="orange", main="log(area) total impacts", ylim=c(0, 10))
lines(density(imps$sres$total[,1]), lty=3, col="red")
abline(v=imps$res$total[1], lty=3, col="red")
abline(v=impsB$res$total[1], lty=4, col="orange")
abline(v=mean(p1-p0), lty=2, col="blue")
abline(v=imps_SLX$impacts$total[1], lwd=2, col="green")
curve(dnorm(x, mean=imps_SLX$impacts$total[1], sd=imps_SLX$se$total[1]), col="green", lwd=2, add=TRUE, from=-0.2, to=0.4)
legend("topleft", legend=c("BSDM", "SDM (tr)", "SDM (pred)", "SLX"), lty=c(4,3,2,1), col=c("orange", "red", "blue", "green"), lwd=c(1,1,1,2), bty="n", cex=0.8)


## ----GNM, echo = TRUE-----------------------------------------------------------------
GNM <- sacsarlm(form, data=landprice2, listw=lw, type="sacmixed", interval1=int, interval2=int,
 control=list(pre_eig1=e, pre_eig2=e), zero.policy=TRUE)


## ----imp_GNM, echo = TRUE-------------------------------------------------------------
spatialreg::impacts(GNM, tr=trs)


## ----LR3, echo = TRUE-----------------------------------------------------------------
cat(capture.output(LR.sarlm(SDM, GNM))[c(7,8,5)], sep="\n")
cat(capture.output(LR.sarlm(SDEM, GNM))[c(7,8,5)], sep="\n")


## ----si, echo = FALSE-----------------------------------------------------------------
sessionInfo()

