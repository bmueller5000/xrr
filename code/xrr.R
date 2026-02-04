# variables --------------------------------------------------------------------
variables <- function(object, y = FALSE) {
  ## extract terms
  if(inherits(object, "ranger")) {
    ts <- terms(as.formula(object$call[[2]]))
  } else ts <- terms(object)
  
  ## variables
  vs <- as.character(attr(ts, "variables")[-1])
  if(y) vs <- vs[1] else vs <- vs[-1]
  
  return(vs)
}


# predictions ------------------------------------------------------------------
predictions <- function(object, ...) UseMethod("predictions")

## gamlss2
predictions.gamlss2 <- function(object, newdata = NULL, what = "mean", quantile = NULL, ...) {
  ## get data
  if(is.null(newdata)) newdata <- model.frame(object)
  
  ## predict parameters
  par <- predict(object, newdata = newdata, type = "parameter")
  
  ## transform and return
  if(what == "mean") {
    if(is.null(family(object)$mean)) stop("Mean function not defined for this family! Try what = 'quantile' and specify a quantile via the argument quantile.")
    fit <- family(object)$mean(par)
  } else if(what == "quantile") {
    if(is.null(quantile) || length(quantile) != 1L || !is.numeric(quantile) || quantile < 0 || quantile > 1) {
      stop("quantile must be between 0 and 1!")
    }
    fit <- family(object)$quantile(quantile, par)
  } else if (what == "variance") {
    fit <- family(object)$variance(par)
  } else {
    stop("what must be either mean or quantile!")
  }
  return(fit)
}

## gam
predictions.gam <- function(object, newdata = NULL, what = "mean", ...) {
  ## get data
  if(is.null(newdata)) newdata <- model.frame(object)
  
  ## predict and return
  if(what == "mean") {
    fit <- as.vector(predict(object, newdata = newdata, type = "response"))
    return(fit)
  } else {
    stop("what must be mean for gam!")
  }
}

## nnet
predictions.nnet <- function(object, newdata = NULL, what = "mean", ...) {
  ## get data
  if(is.null(newdata)) newdata <- model.frame(object)
  
  ## predict and return
  if(what == "mean") {
    fit <- as.vector(predict(object, newdata = newdata, type = "raw"))
    return(fit)
  } else {
    stop("what must be mean for nnet!")
  }
}

## ranger
predictions.ranger <- function(object, newdata = NULL, what = "mean", ...) {
  ## get data
  if(is.null(newdata)) newdata <- model.frame(object)
  
  ## predict and return
  if(what == "mean") {
    fit <- as.vector(predict(object, data = newdata, type = "response"))
    return(fit)
  } else {
    stop("what must be mean for nnet!")
  }
}

## cforest
predictions.cforest <- function(object, newdata = NULL, what = "mean", ...) {
  ## get data
  if(is.null(newdata)) newdata <- model.frame(object)
  
  ## predict and return
  if(what == "mean") {
    fit <- as.vector(predict(object, newdata = newdata))
    return(fit)
  } else {
    stop("what must be mean for nnet!")
  }
}


# fun1 -------------------------------------------------------------------------
fun1 <- function(object1, object2,
                 what = "mean", quantile = NULL, 
                 plot = TRUE, summary = TRUE, ...) {
  
  ##############################################################################
  # object1 <- mspiro11
  # object2 <- mspiro12
  # what <- "mean"
  # quantile <- NULL
  # what <- "quantile"
  # quantile <- 0.5
  ##############################################################################
  
  ## output list
  lout <- list(what = what)
  if(what == "quantile") lout$quantile <- quantile
  
  ## variables
  vars1 <- variables(object1)
  vars2 <- variables(object2)
  if(length(vars2) > length(vars1)) 
    stop("Model 2 is not nested within Model 1!")
  exclvar <- setdiff(union(vars1, vars2), intersect(vars1, vars2))
  lout$variables <- vars1
  lout$excluded <- exclvar
  lout$y <- variables(object1, y = TRUE)
  
  ## data
  dat <- model.frame(object1)
  lout$data <- dat
  
  ## predictions
  pred1 <- predictions(object1, what = what, quantile = quantile)
  pred2 <- predictions(object2, what = what, quantile = quantile)
  lout$pred1 <- pred1
  lout$pred2 <- pred2
  
  ## relevance as absolute prediction difference
  predabsdiff <- abs(pred1 - pred2)
  if(all(predabsdiff == 0)) 
    stop(paste0("Absolute prediction difference zero for '", exclvar,"'!"))
  
  ## marginal effects
  xseq <- seq(min(dat[[exclvar]]), max(dat[[exclvar]]), length.out = 300)
  lout$xseq <- xseq
  lmarg <- list()
  
  ### mean
  val <- mean(pred2)
  idx <- which.min(abs(pred2 - val))
  nd <- dat[rep(idx, length(xseq)),]
  nd[[exclvar]] <- xseq
  meff <- predictions(object1, what = what, quantile = quantile, newdata = nd)
  lmarg$mean <- list(value = val,
                     idx = idx,
                     effect = meff)
  
  ### quantiles
  probs <- seq(0.01, 0.99, 0.01)
  vals  <- unname(quantile(pred2, probs = probs))
  idxs  <- vapply(vals, function(v)
    which.min(abs(pred2 - v)), integer(1))
  nd <- dat[rep(idxs, each = length(xseq)), ]
  nd[[exclvar]] <- rep(xseq, times = length(idxs))
  
  meff_all <- tryCatch(
    {
      predictions(object1, what = what, quantile = quantile, newdata = nd)
    },
    warning = function(w) {
      # Handle warnings
      warning("Quantiles failed: ", conditionMessage(w))
      NULL
    }
  )
  
  if(!is.null(meff_all)) {
    meff_mat <- matrix(meff_all, nrow = length(xseq), byrow = FALSE)
    
    lmarg$quantiles <- setNames(
      lapply(seq_along(probs), function(k) {
        list(prob  = probs[k],
             value = vals[k],
             idx   = idxs[k],
             effect = meff_mat[, k])
      }),
      paste0("q", probs)
    )
  } else lmarg$quantiles <- NULL
  
  
  ## save in output list
  lout$relevance <- predabsdiff
  lout$marginal <- lmarg
  
  ## class
  class(lout) <- "fun1"
  
  ## plot and relevance
  if(plot) {plot(lout, ...)}
  # if(relevance) {print(lout)}
  
  ## return
  invisible(lout)
}


# optimal round ----------------------------------------------------------------
optround <- function(x, neg_digit = FALSE, sig = 3) {
  if (anyNA(x)) {
    warning("The input number/vector contains NAs! Remove before using the function.")
    return(x)
  }
  
  m <- try(mean(abs(x)), silent = TRUE)
  if (!is.numeric(m) || m == 0) return(x)
  
  ## order of magnitude
  k <- floor(log10(m))
  
  ## number of digits to round to
  digits <- sig - k - 1
  
  ## optionally restrict negative digits
  if (!neg_digit) {
    digits <- max(digits, 0)
  }
  
  round(x, digits)
}


# plot fun1 object -------------------------------------------------------------
plot.fun1 <- function(object, 
                      relevance = TRUE, ystats = TRUE, relevancepos = "topleft", 
                      median = TRUE,
                      fade = TRUE, fadealpha = NULL,
                      allmarginal = TRUE, probsmarginal = NULL, 
                      seqhcl = "Sunset",
                      rug = TRUE,
                      ylim = NULL, 
                      xlab = NULL, ylab = NULL, main = NULL, 
                      legend = TRUE, legendpos = "topright",
                      ...) {
  
  ##############################################################################
  # x11()
  # object <- superfun(m1, m2, plot = FALSE)
  # object <- superfun(m1, m2, what = "quantile", quantile = 0.9, plot = FALSE)
  # fade <- TRUE
  ##############################################################################
  
  ## marginal probs
  if(is.null(probsmarginal)) {
    mprobs <- sort(unname(unique(unlist(lapply(object$marginal$quantiles, function(q) q$prob)))))
  } else mprobs <- probsmarginal
  
  ## colors for plotting
  mprobsscols <- rev(sequential_hcl(100000, seqhcl))
  
  ## y limit
  if(is.null(ylim))
    ylim <- range(c(object$marginal$mean$effect, 
                    unlist(lapply(object$marginal$quantiles, function(q) q$effect))))
  
  ## xlab and ylab
  if(is.null(xlab)) xlab <- object$exclude
  if(is.null(ylab)) {
    if(object$what == "quantile") { ##################################################### is that okay?
      ylab <- paste0("Effect on ", object$quantile*100, "% ",
                     object$what, " of ", object$y)
    } else {
      ylab <- paste0("Effect on ", object$what, " of ", object$y)
    }
  }
  
  ## main
  if(is.null(main)) main <- NA
  
  ## plot
  plot(object$xseq, object$marginal$mean$effect, 
       type = 'l', lty = 1, lwd = 2, ylim = ylim, 
       xlab = xlab, ylab = ylab, main = main, ...)
  if(median) lines(object$xseq, object$marginal$quantiles$q0.5$effect, lty = 2, lwd = 2)
  
  ## plot quantiles of pred2
  if(allmarginal) {
    ## density of excluded
    dpred2 <- density(object$pred2)
    dpred2$y <- dpred2$y/max(dpred2$y)
    
    ## plot lines
    for(mp in mprobs) {
      lines(object$xseq, object$marginal$quantiles[[paste0("q", mp)]]$effect,
            col = mprobsscols[
              round(100000*approx(dpred2$x,
                                  dpred2$y,
                                  xout = object$marginal$quantiles[[paste0("q", mp)]]$value)$y)])
    }
    lines(object$xseq, object$marginal$mean$effect, lty = 1, lwd = 2)
    if(median) lines(object$xseq, object$marginal$quantiles$q0.5$effect, lty = 2, lwd = 2)
  }
  
  ## plot rug
  if(rug) rug(object$data[[object$excluded]])
  
  ## fade
  if(fade) {
    ## smooth fading based on density of data
    h <- hist(object$data[[object$excluded]], plot = FALSE)
    
    ## add a little bit over min and max
    xseq_rect <- object$xseq
    xseq_rect[1] <- xseq_rect[1] - 0.05*abs(xseq_rect[1])
    xseq_rect[length(object$xseq)] <- 
      xseq_rect[length(object$xseq)] + 0.05*abs(xseq_rect[length(object$xseq)])
    
    ## fade with alpha
    dens_interp <- approx(x = h$mids, y = h$density, xout = xseq_rect, rule = 2)$y
    if(!is.null(fadealpha)) {
      alpha_min <- fadealpha[1]; alpha_max <- fadealpha[2]
    } else {alpha_min <- 0.1; alpha_max <- 1}
    
    alpha_vals <- alpha_min + (dens_interp / max(dens_interp)) * (alpha_max - alpha_min)
    
    ## draw rectangles with transparency, no border
    rect(xleft   = xseq_rect[-length(xseq_rect)],
         xright  = xseq_rect[-1],
         ybottom = ylim[1] - abs(ylim[1]),
         ytop    = ylim[2] + abs(ylim[2]),
         col     = rgb(1, 1, 1, alpha = 1 - alpha_vals[-length(alpha_vals)]),
         border  = NA)
    
    ## draw box again
    box()
  }
  
  ## legend
  if(legend) {
    if(median) {
      legend(legendpos, legend = c("Mean", "Median"),
             lty = c(1, 2), lwd = 2, bty = "n")
    } else {
      legend(legendpos, legend = "Mean", lwd = 2, bty = "n")
    }
  }
  
  ## add relevance number to plot
  if(relevance && ystats) {
    legend(relevancepos,
           legend = sprintf(
             "Rel = %s | Y: Mean = %s, SD = %s",
             optround(mean(object$relevance)),
             optround(mean(object$data[[object$y]])),
             optround(sd(object$data[[object$y]]))), 
           bty = "n",
           x.intersp = ifelse(relevancepos == "topleft", -0.5, 1))
  } 
  if(relevance && !ystats) {
    legend(relevancepos,
           legend = sprintf("Rel = %s", optround(mean(object$relevance))), 
           bty = "n",
           x.intersp = ifelse(relevancepos == "topleft", -0.5, 1))
  }
}


# print summary relevance stats ------------------------------------------------
# summary.superfun <- function(object) {
#   ## digits and terms
#   digs <- 3
#   ts <- names(object$relevance)
#   w1 <- max(nchar(ts), 13)
#   
#   ## sort by mean
#   means <- sapply(object$relevance, function(x)
#     mean(x$predabsdiff)
#   )
#   ord <- order(means, decreasing = TRUE)
#   
#   ## create string for printing
#   ss <- paste0(
#     sapply(ord, function(i)
#       sprintf(
#         paste0("%-", w1, "s %10.3f %10.3f\n "),
#         ts[i],
#         round(means[i], digs),
#         round(median(object$relevance[[i]]$predabsdiff), digs)
#       )
#     ),
#     collapse = ""
#   )
#   
#   ## print
#   w2 <- w1 + 22
#   w3 <- w2 - 25
#   cat(
#     sprintf("\n"),
#     c("Relevance (based on APD):\n"),
#     # c("\n"),
#     paste0(strrep("=", w2), "\n"),
#     sprintf(paste0("%-", w1, "s %10s %10s\n"), "Term", "Mean", "Median"),
#     paste0(strrep("-", w2), "\n"),
#     substr(ss, 1, nchar(ss) - 2),
#     c("\n"),
#     paste0(strrep("=", w2), "\n\n"),
#     sprintf(paste0("Number of observations   %", w3, "d\n"), nrow(object$modelframe)),
#     sprintf(paste0("Mean of y                %", w3, ".3f\n"), 
#             round(mean(object$modelframe[[object$yterm]]), digs)),
#     sprintf(paste0("SD of y                  %", w3,".3f\n"), 
#             round(sd(object$modelframe[[object$yterm]]), digs))
#   )
# }


# fun2 --------------------------------------------------------------------
fun2 <- function(object1, object2, 
                 quantiles = NULL, 
                 parallel = TRUE) {
  
  ##############################################################################
  # object1 <- mspiro11
  # object2 <- mspiro12
  # quantiles <- seq(0.01, 0.99, 0.01)
  # parallel <- TRUE
  # light <- TRUE
  ##############################################################################
  
  ## output list
  # lout <- list(what = "quantile")
  
  ## variables
  # vars1 <- variables(object1)
  # vars2 <- variables(object2)
  # if(length(vars2) > length(vars1)) 
  #   stop("Model 2 is not nested within Model 1!")
  # exclvar <- setdiff(union(vars1, vars2), intersect(vars1, vars2))
  # lout$variables <- vars1
  # lout$excluded <- exclvar
  # lout$y <- list(name = variables(object1, y = TRUE),
  #                y = as.vector(model.frame(object1)[variables(object1, y = TRUE)]))
  # 
  ## percentiles if not specified
  if(is.null(quantiles)) quantiles <- seq(0.01, 0.99, 0.01)
  # lout$quantiles <- quantiles
  
  ## calc relevance
  if(parallel) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      stop("Package 'parallel' is required but not installed!")
    }
    
    # relevance as absolute prediction difference
    rel <- unlist(
      parallel::mclapply(
        quantiles,
        function(q) {
          pred1 <- predictions(object1, what = "quantile", quantile = q)
          pred2 <- predictions(object2, what = "quantile", quantile = q)
          mean(abs(pred1 - pred2))
        },
        mc.cores = max(1, parallel::detectCores() - 2)
      )
    )
  } else {
    rel <- numeric(length(quantiles))
    
    pb <- txtProgressBar(min = 0, max = length(quantiles), style = 3)
    
    for(i in seq_along(quantiles)) {
      pred1 <- predictions(object1, what = "quantile", quantile = quantiles[i])
      pred2 <- predictions(object2, what = "quantile", quantile = quantiles[i])
      
      ## relevance as absolute prediction difference
      rel[i] <- mean(abs(pred1 - pred2))
      
      setTxtProgressBar(pb, i)
    }
    close(pb)
  }
  # lout$relevance <- rel
  
  ## return
  # if(light) 
  return(rel) 
  # else return(lout)
}


# fun3 -------------------------------------------------------------------------
fun3 <- function(objectlist) {
  stopifnot(is.list(objectlist), !is.null(names(objectlist)))
  
  predrelmat <- function(object) {
    if (inherits(object, c("cforest", "nnet"))) {
      predictions(object)
      
    } else if (inherits(object, c("gam", "gamlss2"))) {
      fam <- family(object)$family
      if (!is.null(fam) && fam == "BCCG") {
        warning("Family BCCG uses median instead of mean!")
        predictions(object, what = "quantile", quantile = 0.5)
      } else {
        predictions(object)
      }
      
    } else {
      stop("Unsupported model class: ", paste(class(object), collapse = ", "))
    }
  }
  
  n <- length(objectlist)
  relmat <- matrix(0, n, n, dimnames = list(names(objectlist), names(objectlist)))
  
  for (i in seq_len(n)) {
    pred_i <- predrelmat(objectlist[[i]])
    for (j in seq_len(n)) {
      if (i != j) {
        pred_j <- predrelmat(objectlist[[j]])
        relmat[i, j] <- mean(abs(pred_i - pred_j))
      }
    }
  }
  
  # Keep only upper triangular (set lower triangular to NA)
  relmat[lower.tri(relmat, diag = TRUE)] <- NA
  return(relmat)
}


