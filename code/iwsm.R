# clean up ---------------------------------------------------------------------
rm(list = ls())
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(graphics.off(), silent = TRUE)
defpar <- par(no.readonly = TRUE)
resetpar <- function() par(defpar)


# wd and packages --------------------------------------------------------------
pathwd <- '/home/beng/work/xrr'
setwd(pathwd)

library(gamlss2)
library(mgcv)
library(nnet)
library(ranger)
library(partykit)
library(colorspace)

set.seed(42)


# source files -----------------------------------------------------------------
source("code/xrr.R")


# data -------------------------------------------------------------------------
miete <- haven::read_dta("data/rent99_stata9.dta") 
miete <- data.frame(miete) 
miete$location <- as.factor(miete$location)

## model list
mmiete <- list()


# gamlss2: bccg ----------------------------------------------------------------
mmiete$gamlss$bccg$full <-
  gamlss2(rent ~ s(area) + s(yearc) + location | . | ., data = miete, family = BCCG)
mmiete$gamlss$bccg$area <-
  gamlss2(rent ~ s(yearc) + location | . | ., data = miete, family = BCCG)
mmiete$gamlss$bccg$yearc <-
  gamlss2(rent ~ s(area) + location | . | ., data = miete, family = BCCG)
mmiete$gamlss$bccg$location <-
  gamlss2(rent ~ s(area) + s(yearc) | . | ., data = miete, family = BCCG)


# gamlss2: normal homoscedastic ------------------------------------------------
mmiete$gamlss$normalhomo$full <-
  gamlss2(rent ~ s(area) + s(yearc) + location, data = miete)
mmiete$gamlss$normalhomo$area <-
  gamlss2(rent ~ s(yearc) + location, data = miete)
mmiete$gamlss$normalhomo$yearc <-
  gamlss2(rent ~ s(area) + location, data = miete)
mmiete$gamlss$normalhomo$location <-
  gamlss2(rent ~ s(area) + s(yearc), data = miete)


# gamlss2: normal heteroscedastic ----------------------------------------------
mmiete$gamlss$normalhetero$full <-
  gamlss2(rent ~ s(area) + s(yearc) + location | ., data = miete)
mmiete$gamlss$normalhetero$area <-
  gamlss2(rent ~ s(yearc) + location | ., data = miete)
mmiete$gamlss$normalhetero$yearc <-
  gamlss2(rent ~ s(area) + location | ., data = miete)
mmiete$gamlss$normalhetero$location <-
  gamlss2(rent ~ s(area) + s(yearc) | ., data = miete)


# gam --------------------------------------------------------------------------
mmiete$gam$full <-
  gam(rent ~ s(area) + s(yearc) + location, data = miete)
mmiete$gam$area <-
  gam(rent ~ s(yearc) + location, data = miete)
mmiete$gam$yearc <-
  gam(rent ~ s(area) + location, data = miete)
mmiete$gam$location <-
  gam(rent ~ s(area) + s(yearc), data = miete)


# cforest ----------------------------------------------------------------------
mmiete$cforest$full <-
  cforest(rent ~ area + yearc + location, data = miete, ntree = 100)
mmiete$cforest$area <-
  cforest(rent ~ yearc + location, data = miete, ntree = 100)
mmiete$cforest$yearc <-
  cforest(rent ~ area + location, data = miete, ntree = 100)
mmiete$cforest$location <-
  cforest(rent ~ area + yearc, data = miete, ntree = 100)


# nnet -------------------------------------------------------------------------
decay <- 0.3

mmiete$nnet$full <-
  nnet(rent ~ area + yearc + location, data = miete, linout = TRUE,
       size = 150, decay = decay, maxit = 1000)
mmiete$nnet$area <-
  nnet(rent ~ yearc + location, data = miete, linout = TRUE,
       size = 150, decay = decay, maxit = 1000)
mmiete$nnet$yearc <-
  nnet(rent ~ area + location, data = miete, linout = TRUE,
       size = 150, decay = decay, maxit = 1000)
mmiete$nnet$location <-
  nnet(rent ~ area + yearc, data = miete, linout = TRUE,
       size = 150, decay = decay, maxit = 1000)


# model comparison -------------------------------------------------------------
fun3(list(
  gam             = mmiete$gam$full,
  gamlss_norm_het = mmiete$gamlss$normalhetero$full,
  gamlss_bccg     = mmiete$gamlss$bccg$full,
  cforest         = mmiete$cforest$full,
  nnet            = mmiete$nnet$full
))


## plot
png("plot/iwsm_rent01_area.png", width = 6, height = 3.6, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(0.1, 0.1, 1.1, 0.1), oma = c(2.5, 2.5, 0, 0), mgp = c(2.5, 0.5, 0))
ylim <- c(100, 1600)
fun1(mmiete$gam$full, mmiete$gam$area, ylim = ylim, main = "GAM", ystat = FALSE, xlab = NA, ylab = NA, xaxt = "n")
fun1(mmiete$gamlss$normalhetero$full, mmiete$gamlss$normalhetero$area, ylim = ylim, main = "GAMLSS|NHet", ystat = FALSE, legend = FALSE, xlab = NA, ylab = NA, xaxt = "n", yaxt = "n")
fun1(mmiete$cforest$full, mmiete$cforest$area, ylim = ylim, main = "CForest", ystat = FALSE, legend = FALSE, xlab = NA, ylab = NA)
fun1(mmiete$nnet$full, mmiete$nnet$area, ylim = ylim, main = "NNet", ystat = FALSE, legend = FALSE, xlab = NA, ylab = NA, yaxt = "n")
mtext(expression("Area in m"^2), 1, outer = TRUE, line = 1.5); mtext("Effect on mean of rent", 2, outer = TRUE, line = 1.5)
dev.off()


png("plot/iwsm_rent02_area.png", width = 8, height = 2.5, units = "in", res = 300)
par(mfrow = c(1, 2), mar = c(2.6, 2.6, 1.1, 0.1), mgp = c(1.5, 0.5, 0))
ylim <- c(100, 1600)
# fun1(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$area, ylim = ylim, main = "GAMLSS2: BCCG, Median", what = "quantile", quantile = 0.5, ystat = FALSE)
fun1(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$area, ylim = ylim, main = "GAMLSS|BCCG: Q90", ystat = FALSE, what = "quantile", quantile = 0.9, legend = FALSE, xlab = expression("Area in m"^2))
quantiles <- seq(0.01, 0.99, 0.01)
plot(quantiles, fun2(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$area, quantiles = quantiles), 
     type = "l", lwd = 2, 
     ylim = c(0, 280),
     xlab = "Quantile", ylab = "Relevance in euros",
     main = "GAMLSS|BCCG: Relevance")
lines(quantiles, 
      fun2(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$yearc, quantiles = quantiles), 
      lwd = 2, lty = 2)
lines(quantiles, 
      fun2(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$location, quantiles = quantiles), 
      lwd = 2, lty = 3)
legend("topleft", legend = c("area", "yearc", "location"), 
       lty = 1:3, lwd = 2, bty = "n")
dev.off()


## other idea
# x11()
# pred1 <- predictions(mmiete$gamlss$normalhetero$full)
# pred2 <- predictions(mmiete$gamlss$normalhetero$area)
# head(pred1)
# head(pred2)
# plot(pred1, pred2)
# 
# 
# d <- abs(pred1 - pred2)
# plot(miete$area, d)
# abline(h = mean(d), col = 2, lwd = 3)
# abline(h = median(d), col = 3, lwd = 3)
# rug(miete$area)
# 
# 
# 
# median(d)
# 
# 
# x <- density(miete$area)
# # plot(x)
# foo <- splinefun(x$x, x$y)
# w <- foo(miete$area)
# w <- w/sum(w)
# 
# sum(d*w)
# abline(h = sum(d*w), col = 4, lwd = 3)
# 
# 
# length(x$y)
# x11(); plot(density(miete$area))
# 
# mean(d)

