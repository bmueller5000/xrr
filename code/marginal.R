marginal <- function(model, 
                     data, 
                     term,
                     nvarseq = 1000,
                     cquantiles = seq(0, 1, 0.1)) {
  
  ##############################################################################
  ## testing
  # model <- mod
  # data <- dat
  # term <- 's(logp)'
  # nvarseq <- 1000
  # cmean <- TRUE
  # cquantiles <- c(0.5, 0.05, 0.25, 0.75, 0.95)
  ##############################################################################
  
  ## extract variable (the first word after '(')
  var <- sub(".*\\(\\s*([[:alnum:]_]+).*", "\\1", term)
  
  ## predictions
  yhat <- predict(model, nd = data)
  fhat <- predict(model, nd = data, terms = term)
  
  ## constant c
  c <- yhat - fhat
  
  ## c stats 
  lcstats <- list()
  varseq <- seq(min(data[[var]]), max(data[[var]]), length.out = nvarseq)
  
  ## mean
  cmean <- TRUE
  if(cmean) {
    l <- list()
    l$stat <- mean(c)
    diffs <- abs(c - l$stat)
    l$mindiff <- min(diffs)
    l$idx <- which.min(diffs)
    l$nd <- data[rep(l$idx, length(varseq)),]
    l$nd[[var]] <- varseq
    l$me <- predict(model, newdata = l$nd)
    lcstats[['mean']] <- l
  } 
  
  ## quantiles
  if (!all(is.na(cquantiles)) && !is.null(cquantiles) &&
      all(cquantiles >= 0) && all(cquantiles <= 1)) {
    for(i in seq_along(cquantiles)) {
      l <- list()
      l$quantile <- cquantiles[i]
      l$stat <- unname(quantile(c, cquantiles[i]))
      diffs <- abs(c - l$stat)
      l$mindiff <- min(diffs)
      l$idx <- unname(which.min(diffs))
      l$nd <- data[rep(l$idx, length(varseq)),]
      l$nd[[var]] <- varseq
      l$me <- predict(model, newdata = l$nd)
      lcstats$quantiles[[paste0("q", cquantiles[i])]] <- l
    }
  } else {return(cat("Provide valid quantiles!"))}
  
  ## return
  return(c(list(model = model,
                data = data,
                term = term, 
                var = var,
                yhat = yhat,
                fhat = fhat,
                c = c,
                varseq = varseq,
                cmean = cmean,
                cquantiles = cquantiles),
           lcstats))
}


