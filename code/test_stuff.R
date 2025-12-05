# clean up ---------------------------------------------------------------------
rm(list = ls())
try(dev.off(dev.list()["RStudioGD"]), silent = TRUE)
try(graphics.off(), silent = TRUE)
defpar <- par(no.readonly = TRUE)
resetpar <- function() par(defpar)


# working directory and paths --------------------------------------------------
pathwd <- '/home/beng/work/varrel'
# pathwd <- '' # add home path here
setwd(pathwd)


# packages ---------------------------------------------------------------------
library(mgcv)
library(colorspace)


# load functions ---------------------------------------------------------------
source("code/simdata.R")
source("code/varrel.R")
source("code/plot_varrel.R")
source("code/marginal.R")
source("code/plot_marginal.R")

source("code/superplot.R")


# real-world data --------------------------------------------------------------
dat <- haven::read_dta("data/orangejuice.dta")
dat <- as.data.frame(dat)
dat$outlet <- as.factor(dat$outlet)

x11(); par(mfrow = c(4, 2))
hist(dat$logp)
plot(y ~ logp, data = dat)
hist(dat$logpc1)
plot(y ~ logpc1, data = dat)
hist(dat$logpc2)
plot(y ~ logpc2, data = dat)
hist(dat$logpc3)
plot(y ~ logpc3, data = dat)

mod <- gam(y ~ s(logp) + s(logpc1) + s(logpc2) + s(logpc3) + 
             outlet + logp + logpc1 + logpc2 + logpc3, 
           data = dat)

x11(); plot(mod, pages = 1)


## variable relevance
logp_varrel <- varrel(mod, dat, y = "y", term = "s(logp)")
x11(); plot_varrel(logp_varrel)

# logpc1_varrel <- varrel(mod, dat, y = "y", term = "s(logpc1)")
# logpc2_varrel <- varrel(mod, dat, y = "y", term = "s(logpc2)")
# logpc3_varrel <- varrel(mod, dat, y = "y", term = "s(logpc3)")


## marginal effects
logp_marginal <- marginal(model = mod, data = dat, term = "s(logp)",
                          cquantiles = seq(0, 1, 0.01))
x11(); plot_marginal(logp_marginal, effselect = 1)

# logpc1_marginal <- marginal(model = mod, data = dat, term = "s(logpc1)",
#                             cquantiles = seq(0, 1, 0.01))
# x11(); plot_marginal(logpc1_marginal, effselect = 2)
# 
# logpc2_marginal <- marginal(model = mod, data = dat, term = "s(logpc2)",
#                             cquantiles = seq(0, 1, 0.01))
# x11(); plot_marginal(logpc1_marginal, effselect = 3)
# 
logpc3_marginal <- marginal(model = mod, data = dat, term = "s(logpc3)",
                            cquantiles = seq(0, 1, 0.01))
x11(); plot_marginal(logpc3_marginal, effselect = 4)


## combined
x11()
superplot(mod)


## without log
# dat2 <- dat
# dat2$y <- round(exp(dat$y))
# 
# mod2 <- gam(y ~ s(p) + s(pc1) + s(pc2) + s(pc3) + outlet,
#             data = dat2)
# 
# plot(mod2, pages = 1)
# superplot(mod2)


# simulated data ---------------------------------------------------------------
dat3 <- simdat(n = 1000, sharex5 = 0.02, seed = 42)

## plot variables and effects
plotvs <- function(var, data) {
  plot(data[[var]], xlab = NA, ylab = var)
  hist(data[[var]], xlab = var, main = NA)
  plot(data[[var]], data[[paste0('f', var)]],
       pch = 16, col = scales::alpha('black', 0.1),
       xlab = var, ylab = paste0('f(', var, ')'),
       ylim = c(-1.2, 1.2))
}
x11(); par(mfrow = c(5, 3))
lapply(paste0("x", 1:5), function(var) plotvs(var, data = dat3))

## model
mod3 <- gam(y ~ s(x1) + s(x2) + s(x3) + s(x4) + s(x5), data = dat3)
x11(); plot(mod3, pages = 1)
superplot(mod3, cquantiles = seq(0, 1, 0.05))

