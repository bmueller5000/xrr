set.seed(42)

# data -------------------------------------------------------------------------
library(haven)
miete <- haven::read_dta("data/rent99_stata9.dta")
miete <- data.frame(miete)
miete$location <- as.factor(miete$location)


# folds ------------------------------------------------------------------------
K <- 5
n <- nrow(miete)
fold_id <- sample(rep(1:K, length.out = n))

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))

cv_model <- function(fit_fun, pred_fun, data, fold_id) {
  rmses <- numeric(max(fold_id))
  for (k in seq_along(rmses)) {
    train <- data[fold_id != k, ]
    test  <- data[fold_id == k, ]
    fit   <- fit_fun(train)
    yhat  <- pred_fun(fit, test)
    rmses[k] <- rmse(test$rent, yhat)
  }
  mean(rmses)
}


# models -----------------------------------------------------------------------
cv_gamlss_bccg <- cv_model(
  fit_fun = function(d)
    gamlss2(rent ~ s(area) + s(yearc) | . | .,
            data = d, family = BCCG),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata, what = "mu"),
  data = miete,
  fold_id = fold_id
)

cv_gamlss_bccg_location <- cv_model(
  fit_fun = function(d)
    gamlss2(rent ~ s(area) + s(yearc) + location | . | .,
            data = d, family = BCCG),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata, what = "mu"),
  data = miete,
  fold_id = fold_id
)

cv_gamlss_normal_homo <- cv_model(
  fit_fun = function(d)
    gamlss2(rent ~ s(area) + s(yearc),
            data = d),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata, what = "mu"),
  data = miete,
  fold_id = fold_id
)

cv_gamlss_normal_hetero <- cv_model(
  fit_fun = function(d)
    gamlss2(rent ~ s(area) + s(yearc) | .,
            data = d),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata, what = "mu"),
  data = miete,
  fold_id = fold_id
)

cv_gam <- cv_model(
  fit_fun = function(d)
    gam(rent ~ s(area) + s(yearc),
        data = d),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata),
  data = miete,
  fold_id = fold_id
)

cv_nnet <- cv_model(
  fit_fun = function(d)
    nnet(rent ~ area + yearc, data = d,
         linout = TRUE, size = 150, decay = 0.1,
         maxit = 1000, trace = FALSE),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata),
  data = miete,
  fold_id = fold_id
)

cv_cforest <- cv_model(
  fit_fun = function(d)
    cforest(rent ~ area + yearc, ntree = 100, data = d),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata),
  data = miete,
  fold_id = fold_id
)


# results ----------------------------------------------------------------------
cv_results <- data.frame(
  model = c(
    "gamlss2 bccg",
    "gamlss2 bccg + location",
    "gamlss2 normal homo",
    "gamlss2 normal hetero",
    "gam",
    "nnet",
    "cforest"
  ),
  rmse = c(
    cv_gamlss_bccg,
    cv_gamlss_bccg_location,
    cv_gamlss_normal_homo,
    cv_gamlss_normal_hetero,
    cv_gam,
    cv_nnet,
    cv_cforest
  )
)

cv_results[order(cv_results$rmse), ]


