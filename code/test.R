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
source("code/simulate.R")


# nutrition --------------------------------------------------------------------
data("ZambiaNutrition", package = "R2BayesX")

mnutri <- list()

## gamlss2: sst
mnutri$gamlss$sst$full <- 
  gamlss2(stunting ~ mbmi + s(mbmi) + s(agechild) + te(mbmi,agechild) | . | . | ., data = ZambiaNutrition, family = SST)
mnutri$gamlss$sst$agechild <- 
  gamlss2(stunting ~ mbmi + s(mbmi) | . | . | ., data = ZambiaNutrition, family = SST)
mnutri$gamlss$sst$mbmi <- 
  gamlss2(stunting ~ s(agechild) | . | . | ., data = ZambiaNutrition, family = SST)

## gamlss2: normal homo
mnutri$gamlss$normalhomo$full <- 
  gamlss2(stunting ~ mbmi + s(mbmi) + s(agechild) + te(mbmi,agechild), data = ZambiaNutrition)
mnutri$gamlss$normalhomo$agechild <- 
  gamlss2(stunting ~ mbmi + s(mbmi), data = ZambiaNutrition)
mnutri$gamlss$normalhomo$mbmi <- 
  gamlss2(stunting ~ s(agechild), data = ZambiaNutrition)

## gamlss2: normal hetero
mnutri$gamlss$normalhetero$full <-
  gamlss2(stunting ~ mbmi + s(mbmi) + s(agechild) + te(mbmi,agechild) | ., data = ZambiaNutrition)
mnutri$gamlss$normalhetero$agechild <-
  gamlss2(stunting ~ mbmi + s(mbmi) | ., data = ZambiaNutrition)
mnutri$gamlss$normalhetero$mbmi <-
  gamlss2(stunting ~ s(agechild) | ., data = ZambiaNutrition)

## gam
mnutri$gam$full  <- 
  gam(stunting ~ mbmi + s(mbmi) + s(agechild) + te(mbmi,agechild), data = ZambiaNutrition)
mnutri$gam$agechild <- 
  gam(stunting ~ mbmi + s(mbmi), data = ZambiaNutrition)
mnutri$gam$mbmi <- 
  gam(stunting ~ s(agechild), data = ZambiaNutrition)

## nnet
mnutri$nnet$full <- 
  nnet(stunting ~ mbmi + agechild, data = ZambiaNutrition, linout = TRUE, size = 100, decay = 0.1, maxit = 1000)
mnutri$nnet$agechild <- 
  nnet(stunting ~ mbmi, data = ZambiaNutrition, linout = TRUE, size = 100, decay = 0.1, maxit = 1000)
mnutri$nnet$mbmi <- 
  nnet(stunting ~ agechild, data = ZambiaNutrition, linout = TRUE, size = 100, decay = 0.1, maxit = 1000)

## cforest
mnutri$cforest$full <- 
  cforest(stunting ~ mbmi + agechild, ntree = 100, data = ZambiaNutrition)
mnutri$cforest$agechild <- 
  cforest(stunting ~ mbmi, ntree = 100, data = ZambiaNutrition)
mnutri$cforest$mbmi <- 
  cforest(stunting ~ agechild, ntree = 100, data = ZambiaNutrition)

## plot
png("plot/nutri01.png", width = 8, height = 6, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
ylim <- c(-1, 2)
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, ylim = ylim, main = "gamlss2: sst")
fun1(mnutri$gam$full, mnutri$gam$agechild, ylim = ylim, ystat = FALSE, legend = FALSE, main = "gam")
fun1(mnutri$nnet$full, mnutri$nnet$agechild, ylim = ylim, ystat = FALSE, legend = FALSE, main = "nnet")
fun1(mnutri$cforest$full, mnutri$cforest$agechild, ylim = ylim, ystat = FALSE, legend = FALSE, main = "cforest")
dev.off()

png("plot/nutri02.png", width = 8, height = 6, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
ylim <- c(-1, 2)
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$mbmi, ylim = ylim, main = "gamlss2: sst")
fun1(mnutri$gam$full, mnutri$gam$mbmi, ylim = ylim, ystat = FALSE, legend = FALSE, main = "gam")
fun1(mnutri$nnet$full, mnutri$nnet$mbmi, ylim = ylim, ystat = FALSE, legend = FALSE, main = "nnet")
fun1(mnutri$cforest$full, mnutri$cforest$mbmi, ylim = ylim, ystat = FALSE, legend = FALSE, main = "cforest")
dev.off()

png("plot/nutri03.png", width = 8, height = 10, units = "in", res = 300)
par(mfrow = c(4, 3), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild)
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, ystat = FALSE)
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, main = "gamlss2", relevance = FALSE, rug = FALSE)
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, relevance = FALSE, fade = FALSE, legendpos = "topleft")
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, fadealpha = c(0.5, 1))
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, seqhcl = "Mint")
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, probsmarginal = c(0, 0.25, 0.75, 1))
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, allmarginal = FALSE, xlab = "asdf", ylab = "asdf")
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, allmarginal = FALSE, fade = FALSE)
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, allmarginal = FALSE, fade = FALSE, median = FALSE)
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, allmarginal = FALSE, fade = FALSE, median = FALSE, rug = FALSE)
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, allmarginal = FALSE, fade = FALSE, median = FALSE, rug = FALSE, legend = FALSE)
dev.off()

png("plot/nutri04.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1, 1), mar = c(3.1, 3.1, 2.1, 0.1), mgp = c(2, 0.5, 0))
quantiles <- seq(0.01, 0.99, 0.01)
plot(quantiles, fun2(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, quantiles = quantiles), 
     type = "l", lwd = 2, 
     ylim = c(0, 0.5),
     xlab = "Quantile", ylab = "Relevance",
     main = "gamlss: sst")
lines(quantiles, 
      fun2(mnutri$gamlss$sst$full, mnutri$gamlss$sst$mbmi, quantiles = quantiles), 
      lwd = 2, lty = 2)
legend("topleft", legend = c("agechild", "mbmi"), 
       lty = 1:2, lwd = 2, bty = "n")
dev.off()

png("plot/nutri05.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1, 1), mar = c(3.1, 3.1, 2.1, 0.1), mgp = c(2, 0.5, 0))
quantiles <- seq(0.01, 0.99, 0.01)
plot(quantiles, fun2(mnutri$gamlss$sst$full, mnutri$gamlss$normalhomo$full, quantiles = quantiles), 
     type = "l", lwd = 2, 
     ylim = c(0, 0.5),
     xlab = "Quantile", ylab = "Relevance",
     main = "gamlss distribution comparison")
lines(quantiles, 
      fun2(mnutri$gamlss$sst$full, mnutri$gamlss$normalhetero$full, quantiles = quantiles), 
      lwd = 2, lty = 2)
legend("topleft", legend = c("normal (homoscedastic)", "normal (heteroscedastic)"), 
       lty = 1:2, lwd = 2, bty = "n")
dev.off()

png("plot/nutri06.png", width = 8, height = 3.5, units = "in", res = 300)
par(mfrow = c(1, 2), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
ylim <- c(0.65, 1)
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$agechild, what = "variance", ylim = ylim, main = "gamlss2: sst")
fun1(mnutri$gamlss$sst$full, mnutri$gamlss$sst$mbmi, what = "variance", ylim = ylim, ystat = FALSE, legend = FALSE, main = "gamlss2: sst")
dev.off()


# orange juice -----------------------------------------------------------------
orange <- haven::read_dta("data/orangejuice.dta")
orange <- as.data.frame(orange)
orange$outlet <- as.factor(orange$outlet)

morange <- list()

## gamlss2: normal
morange$gamlss$normal$full <-
  gamlss2(y ~ s(logp) + s(logpc1) + s(logpc2) + s(logpc3) | ., data = orange, family = NO)
morange$gamlss$normal$logp <-
  gamlss2(y ~ s(logpc1) + s(logpc2) + s(logpc3) | ., data = orange, family = NO)
morange$gamlss$normal$logpc1 <-
  gamlss2(y ~ s(logp) + s(logpc2) + s(logpc3) | ., data = orange, family = NO)
morange$gamlss$normal$logpc2 <-
  gamlss2(y ~ s(logp) + s(logpc1) + s(logpc3) | ., data = orange, family = NO)
morange$gamlss$normal$logpc3 <-
  gamlss2(y ~ s(logp) + s(logpc1) + s(logpc2) | ., data = orange, family = NO)

## gam
morange$gam$full <-
  gam(y ~ s(logp) + s(logpc1) + s(logpc2) + s(logpc3), data = orange)
morange$gam$logp <-
  gam(y ~ s(logpc1) + s(logpc2) + s(logpc3), data = orange)
morange$gam$logpc1 <-
  gam(y ~ s(logp) + s(logpc2) + s(logpc3), data = orange)
morange$gam$logpc2 <-
  gam(y ~ s(logp) + s(logpc1) + s(logpc3), data = orange)
morange$gam$logpc3 <-
  gam(y ~ s(logp) + s(logpc1) + s(logpc2), data = orange)

## nnet
morange$nnet$full <-
  nnet(y ~ logp + logpc1 + logpc2 + logpc3, data = orange, linout = TRUE, size = 20, decay = 0.1, maxit = 1000)
morange$nnet$logp <-
  nnet(y ~ logpc1 + logpc2 + logpc3, data = orange, linout = TRUE, size = 20, decay = 0.1, maxit = 1000)
morange$nnet$logpc1 <-
  nnet(y ~ logp + logpc2 + logpc3, data = orange, linout = TRUE, size = 20, decay = 0.1, maxit = 1000)
morange$nnet$logpc2 <-
  nnet(y ~ logp + logpc1 + logpc3, data = orange, linout = TRUE, size = 20, decay = 0.1, maxit = 1000)

morange$nnet$logpc3 <-
  nnet(y ~ logp + logpc1 + logpc2,
       data = orange, linout = TRUE, size = 20, decay = 0.1, maxit = 1000)

## cforest
morange$cforest$full <-
  cforest(y ~ logp + logpc1 + logpc2 + logpc3, ntree = 100, data = orange)
morange$cforest$logp <-
  cforest(y ~ logpc1 + logpc2 + logpc3, ntree = 100, data = orange)
morange$cforest$logpc1 <-
  cforest(y ~ logp + logpc2 + logpc3, ntree = 100, data = orange)
morange$cforest$logpc2 <-
  cforest(y ~ logp + logpc1 + logpc3, ntree = 100, data = orange)
morange$cforest$logpc3 <-
  cforest(y ~ logp + logpc1 + logpc2, ntree = 100, data = orange)

## plot
png("plot/orange01.png", width = 8, height = 6, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
ylim <- c(2, 8)
fun1(morange$gamlss$normal$full, morange$gamlss$normal$logp, ylim = ylim, main = "gamlss2")
fun1(morange$gam$full, morange$gam$logp, ylim = ylim, ystat = FALSE, legend = FALSE, main = "gam")
fun1(morange$nnet$full, morange$nnet$logp, ylim = ylim, ystat = FALSE, legend = FALSE, main = "nnet")
fun1(morange$cforest$full, morange$cforest$logp, ylim = ylim, ystat = FALSE, legend = FALSE, main = "cforest")
dev.off()

png("plot/orange02.png", width = 8, height = 6, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
ylim <- c(2, 8)
fun1(morange$gamlss$normal$full, morange$gamlss$normal$logpc1, ylim = ylim, main = "gamlss2")
fun1(morange$gam$full, morange$gam$logpc1, ylim = ylim, ystat = FALSE, legend = FALSE, main = "gam")
fun1(morange$nnet$full, morange$nnet$logpc1, ylim = ylim, ystat = FALSE, legend = FALSE, main = "nnet")
fun1(morange$cforest$full, morange$cforest$logpc1, ylim = ylim, ystat = FALSE, legend = FALSE, main = "cforest")
dev.off()

png("plot/orange03.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1, 1), mar = c(3.1, 3.1, 2.1, 0.1), mgp = c(2, 0.5, 0))
quantiles <- seq(0.01, 0.99, 0.01)
plot(quantiles, fun2(morange$gamlss$normal$full, morange$gamlss$normal$logp, quantiles = quantiles),
     type = "l", lwd = 2,
     ylim = c(0, 0.5),
     xlab = "Quantile", ylab = "Relevance",
     main = "gamlss")
lines(quantiles, fun2(morange$gamlss$normal$full, morange$gamlss$normal$logpc1, quantiles = quantiles),
      lwd = 2, lty = 2)
lines(quantiles, fun2(morange$gamlss$normal$full, morange$gamlss$normal$logpc2, quantiles = quantiles),
      lwd = 2, lty = 3)
lines(quantiles, fun2(morange$gamlss$normal$full, morange$gamlss$normal$logpc3, quantiles = quantiles),
      lwd = 2, lty = 4)
legend("topleft", legend = c("logp", "logpc1", "logpc2", "logpc3"),
       lty = 1:4, lwd = 2, bty = "n")
dev.off()


# miete ------------------------------------------------------------------------
miete <- haven::read_dta("data/rent99_stata9.dta") 
miete <- data.frame(miete) 
miete$location <- as.factor(miete$location)

mmiete <- list()

## gamlss2: bccg
mmiete$gamlss$bccg$full <-
  gamlss2(rent ~ s(area) + s(yearc) | . | ., data = miete, family = BCCG)
mmiete$gamlss$bccg$area <-
  gamlss2(rent ~ s(yearc) | . | ., data = miete, family = BCCG)
mmiete$gamlss$bccg$yearc <-
  gamlss2(rent ~ s(area) | . | ., data = miete, family = BCCG)

## gamlss2: normal homoscedastic
mmiete$gamlss$normalhomo$full <-
  gamlss2(rent ~ s(area) + s(yearc), data = miete)
mmiete$gamlss$normalhomo$area <-
  gamlss2(rent ~ s(yearc), data = miete)
mmiete$gamlss$normalhomo$yearc <-
  gamlss2(rent ~ s(area), data = miete)

## gamlss2: normal heteroscedastic
mmiete$gamlss$normalhetero$full <-
  gamlss2(rent ~ s(area) + s(yearc) | ., data = miete)
mmiete$gamlss$normalhetero$area <-
  gamlss2(rent ~ s(yearc) | ., data = miete)
mmiete$gamlss$normalhetero$yearc <-
  gamlss2(rent ~ s(area) | ., data = miete)

## gam
mmiete$gam$full <-
  gam(rent ~ s(area) + s(yearc), data = miete)
mmiete$gam$area <-
  gam(rent ~ s(yearc), data = miete)
mmiete$gam$yearc <-
  gam(rent ~ s(area), data = miete)

## nnet
mmiete$nnet$full <-
  nnet(rent ~ area + yearc, data = miete, linout = TRUE,
       size = 150, decay = 0.1, maxit = 1000)
mmiete$nnet$area <-
  nnet(rent ~ yearc, data = miete, linout = TRUE,
       size = 150, decay = 0.1, maxit = 1000)
mmiete$nnet$yearc <-
  nnet(rent ~ area, data = miete, linout = TRUE,
       size = 150, decay = 0.1, maxit = 1000)

## cforest
mmiete$cforest$full <-
  cforest(rent ~ area + yearc, data = miete, ntree = 100)
mmiete$cforest$area <-
  cforest(rent ~ yearc, data = miete, ntree = 100)
mmiete$cforest$yearc <-
  cforest(rent ~ area, data = miete, ntree = 100)

## gamlss2: location
mmiete$gamlss$bccglocation$full <- 
  gamlss2(rent ~ s(area) + s(yearc) + location | . | ., data = miete, family = BCCG)
mmiete$gamlss$bccglocation$location <- 
  gamlss2(rent ~ s(area) + s(yearc) | . | ., data = miete, family = BCCG)

## plot
png("plot/miete01.png", width = 8, height = 6, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
ylim <- c(100, 1600)
fun1(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$area, ylim = ylim, main = "gamlss2: bccg", what = "quantile", quantile = 0.5)
fun1(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$yearc, ylim = ylim, main = "gamlss2: bccg", what = "quantile", quantile = 0.5, ystat = FALSE, legend = FALSE)
fun1(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$area, ylim = ylim, main = "gamlss2: bccg", what = "quantile", quantile = 0.9, ystat = FALSE, legend = FALSE)
fun1(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$yearc, ylim = ylim, main = "gamlss2: bccg", what = "quantile", quantile = 0.9, ystat = FALSE, legend = FALSE)
dev.off()

png("plot/miete02.png", width = 8, height = 6, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
ylim <- c(100, 1600)
fun1(mmiete$gamlss$normalhetero$full, mmiete$gamlss$normalhetero$area, ylim = ylim, main = "gamlss2: normal hetero")
fun1(mmiete$gam$full, mmiete$gam$area, ylim = ylim, ystat = FALSE, legend = FALSE, main = "gam")
fun1(mmiete$nnet$full, mmiete$nnet$area, ylim = ylim, ystat = FALSE, legend = FALSE, main = "nnet")
fun1(mmiete$cforest$full, mmiete$cforest$area, ylim = ylim, ystat = FALSE, legend = FALSE, main = "cforest")
dev.off()

png("plot/miete03.png", width = 8, height = 6, units = "in", res = 300)
par(mfrow = c(2, 2), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
ylim <- c(100, 1600)
fun1(mmiete$gamlss$normalhetero$full, mmiete$gamlss$normalhetero$yearc, ylim = ylim, main = "gamlss2: normal hetero")
fun1(mmiete$gam$full, mmiete$gam$yearc, ylim = ylim, ystat = FALSE, legend = FALSE, main = "gam")
fun1(mmiete$nnet$full, mmiete$nnet$yearc, ylim = ylim, ystat = FALSE, legend = FALSE, main = "nnet")
fun1(mmiete$cforest$full, mmiete$cforest$yearc, ylim = ylim, ystat = FALSE, legend = FALSE, main = "cforest")
dev.off()

png("plot/miete04.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1, 1), mar = c(3.1, 3.1, 2.1, 0.1), mgp = c(2, 0.5, 0))
quantiles <- seq(0.01, 0.99, 0.01)
plot(quantiles, fun2(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$area, quantiles = quantiles), 
     type = "l", lwd = 2, 
     ylim = c(0, 280),
     xlab = "Quantile", ylab = "Relevance",
     main = "gamlss: bccg")
lines(quantiles, 
      fun2(mmiete$gamlss$bccg$full, mmiete$gamlss$bccg$yearc, quantiles = quantiles), 
      lwd = 2, lty = 2)
legend("topleft", legend = c("area", "yearc"), 
       lty = 1:2, lwd = 2, bty = "n")
dev.off()

png("plot/miete05.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1, 1), mar = c(3.1, 3.1, 2.1, 0.1), mgp = c(2, 0.5, 0))
quantiles <- seq(0.01, 0.99, 0.01)
plot(quantiles, fun2(mmiete$gamlss$bccg$full, mmiete$gamlss$normalhomo$full, quantiles = quantiles), 
     type = "l", lwd = 2, 
     ylim = c(0, 120),
     xlab = "Quantile", ylab = "Relevance",
     main = "gamlss distribution comparison")
lines(quantiles, 
      fun2(mmiete$gamlss$bccg$full, mmiete$gamlss$normalhetero$full, quantiles = quantiles), 
      lwd = 2, lty = 2)
legend("topleft", legend = c("normal (homoscedastic)", "normal (heteroscedastic)"), 
       lty = 1:2, lwd = 2, bty = "n")
dev.off()


# spiro ------------------------------------------------------------------------
data("SpirometryUS")
spiro <- subset(SpirometryUS, gender == "Female")

mspiro <- list()

## gamlss2: bct
mspiro$gamlss$bct$full <-
  gamlss2(fev1 ~ s(age) + s(height) + s(weight) + te(age,height,weight) | . | . | ., data = spiro, family = BCT)
mspiro$gamlss$bct$age <-
  gamlss2(fev1 ~ s(height) + s(weight) + te(height,weight) | . | . | ., data = spiro, family = BCT)
mspiro$gamlss$bct$height <-
  gamlss2(fev1 ~ s(age) + s(weight) + te(age,weight) | . | . | ., data = spiro, family = BCT)
mspiro$gamlss$bct$weight <-
  gamlss2(fev1 ~ s(age) + s(height) + te(age,height) | . | . | ., data = spiro, family = BCT)
mspiro$gamlss$bct$interact <-
  gamlss2(fev1 ~ s(age) + s(height) + s(weight) | . | . | ., data = spiro, family = BCT)

## gamlss2: normal hetero
# mspiro$gamlss$normalhetero$full <-
#   gamlss2(fev1 ~ s(age) + s(height) + s(weight) + elm(~ age + height + weight, k = 200) | . , data = spiro, family = NO)
# mspiro$gamlss$normalhetero$age <-
#   gamlss2(fev1 ~ s(height) + s(weight) + elm(~ height + weight, k = 200) | ., data = spiro, family = NO)
# mspiro$gamlss$normalhetero$height <-
#   gamlss2(fev1 ~ s(age) + s(weight) + elm(~ age + weight, k = 200) | ., data = spiro, family = NO)
# mspiro$gamlss$normalhetero$weight <-
#   gamlss2(fev1 ~ s(age) + s(height) + elm(~ age + height, k = 200) | ., data = spiro, family = NO)

## plot
# png("plot/spiro01.png", width = 8, height = 6, units = "in", res = 300)
# par(mfrow = c(2, 3), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
# ylim <- c(0.5, 4)
# fun1(mspiro$gamlss$bct$full, mspiro$gamlss$bct$age, ylim = ylim, main = "gamlss2: bct", what = "quantile", quantile = 0.5)
# fun1(mspiro$gamlss$bct$full, mspiro$gamlss$bct$height, ylim = ylim, main = "gamlss2: bct", what = "quantile", quantile = 0.5, ystat = FALSE, legend = FALSE)
# fun1(mspiro$gamlss$bct$full, mspiro$gamlss$bct$weight, ylim = ylim, main = "gamlss2: bct", what = "quantile", quantile = 0.5, ystat = FALSE, legend = FALSE)
# fun1(mspiro$gamlss$bct$full, mspiro$gamlss$bct$age, ylim = ylim, main = "gamlss2: bct", what = "quantile", quantile = 0.025, ystat = FALSE, legend = FALSE)
# fun1(mspiro$gamlss$bct$full, mspiro$gamlss$bct$height, ylim = ylim, main = "gamlss2: bct", what = "quantile", quantile = 0.025, ystat = FALSE, legend = FALSE)
# fun1(mspiro$gamlss$bct$full, mspiro$gamlss$bct$weight, ylim = ylim, main = "gamlss2: bct", what = "quantile", quantile = 0.025, ystat = FALSE, legend = FALSE)
# dev.off()

png("plot/spiro02.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1, 1), mar = c(3.1, 3.1, 2.1, 0.1), mgp = c(2, 0.5, 0))
quantiles <- seq(0.01, 0.99, 0.01)
plot(quantiles, fun2(mspiro$gamlss$bct$full, mspiro$gamlss$bct$age, quantiles = quantiles), 
     type = "l", lwd = 2, 
     ylim = c(0, 0.6),
     xlab = "Quantile", ylab = "Relevance",
     main = "gamlss: bct")
lines(quantiles, 
      fun2(mspiro$gamlss$bct$full, mspiro$gamlss$bct$height, quantiles = quantiles), 
      lwd = 2, lty = 2, col = 2)
lines(quantiles, 
      fun2(mspiro$gamlss$bct$full, mspiro$gamlss$bct$weight, quantiles = quantiles), 
      lwd = 2, lty = 3, col = 3)
lines(quantiles, 
      fun2(mspiro$gamlss$bct$full, mspiro$gamlss$bct$interact, quantiles = quantiles), 
      lwd = 2, lty = 4, col = 4)
legend("topright", legend = c("age", "height", "weight", "interact"), 
       lty = 1:4, col = 1:4, lwd = 2, bty = "n")
dev.off()


# simulated --------------------------------------------------------------------
dat <- simulate(n = 1000, sharex5 = 0.02, seed = 42)

## plot variables and effects
plotvs <- function(var, data) {
  plot(data[[var]], xlab = NA, ylab = var)
  hist(data[[var]], xlab = var, main = NA)
  plot(data[[var]], data[[paste0('f', var)]],
       pch = 16, col = scales::alpha('black', 0.1),
       xlab = var, ylab = paste0('f(', var, ')'),
       ylim = c(-1.2, 1.2))
}

png("plot/sim01.png", width = 10, height = 10, units = "in", res = 300) 
par(mfrow = c(5, 3), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
lapply(paste0("x", 1:5), function(var) plotvs(var, data = dat))
dev.off()

m11 <- gamlss2(y ~ s(x1) + s(x2) + s(x3) + s(x4) + s(x5) | ., data = dat)
m12 <- gamlss2(y ~ s(x2) + s(x3) + s(x4) + s(x5) | ., data = dat)
m13 <- gamlss2(y ~ s(x1) + s(x3) + s(x4) + s(x5) | ., data = dat)
m14 <- gamlss2(y ~ s(x1) + s(x2) + s(x4) + s(x5) | ., data = dat)
m15 <- gamlss2(y ~ s(x1) + s(x2) + s(x3) + s(x5) | ., data = dat)
m16 <- gamlss2(y ~ s(x1) + s(x2) + s(x3) + s(x4) | ., data = dat)

png("plot/sim02.png", width = 10, height = 10, units = "in", res = 300) 
plot(m11, pages = 1)
dev.off()

png("plot/sim03.png", width = 10, height = 6, units = "in", res = 300) 
par(mfrow = c(2, 3), mar = c(3.1, 3.1, 3.1, 1.1), mgp = c(2, 0.5, 0))
ylim <- c(-5, 5)
fun1(m11, m12, ylim = ylim)
fun1(m11, m13, ystat = FALSE, legend = FALSE, ylim = ylim)
fun1(m11, m14, ystat = FALSE, legend = FALSE, ylim = ylim)
fun1(m11, m15, ystat = FALSE, legend = FALSE, ylim = ylim)
fun1(m11, m16, ystat = FALSE, legend = FALSE, ylim = ylim)
dev.off()

png("plot/sim04.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(1, 1), mar = c(3.1, 3.1, 0.1, 0.1), mgp = c(2, 0.5, 0))
plot(quantiles, fun2(m11, m12, quantiles = quantiles), 
     type = "l", lwd = 2, 
     ylim = c(0, 0.8),
     xlab = "Quantile", ylab = "Relevance")
lines(quantiles, fun2(m11, m13, quantiles = quantiles), 
      lwd = 2, lty = 2)
lines(quantiles, fun2(m11, m14, quantiles = quantiles), 
      lwd = 2, lty = 3)
lines(quantiles, fun2(m11, m15, quantiles = quantiles), 
      lwd = 2, lty = 4)
lines(quantiles, fun2(m11, m16, quantiles = quantiles), 
      lwd = 2, lty = 5)
legend("topleft", legend = paste0("x", 1:5), 
       lty = 1:5, lwd = 2, bty = "n", horiz = TRUE)
dev.off()


