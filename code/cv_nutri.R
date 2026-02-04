set.seed(42)

# data -------------------------------------------------------------------------
data("ZambiaNutrition", package = "R2BayesX")


# folds ------------------------------------------------------------------------
K <- 5
n <- nrow(ZambiaNutrition)
fold_id <- sample(rep(1:K, length.out = n))

rmse <- function(y, yhat) sqrt(mean((y - yhat)^2))

cv_model <- function(fit_fun, pred_fun, data, fold_id) {
  rmses <- numeric(max(fold_id))
  for (k in seq_along(rmses)) {
    train <- data[fold_id != k, ]
    test  <- data[fold_id == k, ]
    fit   <- fit_fun(train)
    yhat  <- pred_fun(fit, test)
    rmses[k] <- rmse(test$stunting, yhat)
  }
  mean(rmses)
}


# models -----------------------------------------------------------------------
cv_gamlss_sst <- cv_model(
  fit_fun = function(d)
    gamlss2(stunting ~ mbmi + s(mbmi) + s(agechild) + te(mbmi, agechild) | . | . | .,
            data = d, family = SST),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata, what = "mu"),
  data = ZambiaNutrition,
  fold_id = fold_id
)

cv_gamlss_normal_homo <- cv_model(
  fit_fun = function(d)
    gamlss2(stunting ~ mbmi + s(mbmi) + s(agechild) + te(mbmi, agechild),
            data = d),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata, what = "mu"),
  data = ZambiaNutrition,
  fold_id = fold_id
)

cv_gamlss_normal_hetero <- cv_model(
  fit_fun = function(d)
    gamlss2(stunting ~ mbmi + s(mbmi) + s(agechild) + te(mbmi, agechild) | .,
            data = d),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata, what = "mu"),
  data = ZambiaNutrition,
  fold_id = fold_id
)

cv_gam <- cv_model(
  fit_fun = function(d)
    gam(stunting ~ mbmi + s(mbmi) + s(agechild) + te(mbmi, agechild),
        data = d),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata),
  data = ZambiaNutrition,
  fold_id = fold_id
)

cv_nnet <- cv_model(
  fit_fun = function(d)
    nnet(stunting ~ mbmi + agechild, data = d,
         linout = TRUE, size = 100, decay = 0.1,
         maxit = 1000, trace = FALSE),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata),
  data = ZambiaNutrition,
  fold_id = fold_id
)

cv_cforest <- cv_model(
  fit_fun = function(d)
    cforest(stunting ~ mbmi + agechild, ntree = 100, data = d),
  pred_fun = function(fit, newdata)
    predict(fit, newdata = newdata),
  data = ZambiaNutrition,
  fold_id = fold_id
)


# results ----------------------------------------------------------------------
cv_results <- data.frame(
  model = c(
    "gamlss2 sst",
    "gamlss2 normal homo",
    "gamlss2 normal hetero",
    "gam",
    "nnet",
    "cforest"
  ),
  rmse = c(
    cv_gamlss_sst,
    cv_gamlss_normal_homo,
    cv_gamlss_normal_hetero,
    cv_gam,
    cv_nnet,
    cv_cforest
  )
)

cv_results[order(cv_results$rmse), ]


