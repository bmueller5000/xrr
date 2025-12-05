superplot <- function(model, 
                      cquantiles = seq(0, 1, 0.01),
                      returnstat = FALSE) {
  
  ##############################################################################
  # model <- mod
  # cquantiles <- seq(0, 1, 0.01)
  ##############################################################################
  
  ## model
  m <- model
  
  ## data
  d <- m$model
  
  ## smooth terms
  slabs <- vapply(m$smooth, function(ms) ms$label, character(1))
  svars <- vapply(m$smooth, function(ms) ms$term, character(1))
  
  ## y prediction
  yhat <- as.vector(predict(m))
  
  ## helper function to get cstats and marginal
  getme <- function(cval, c, d, x) {
    diffs <- abs(c - cval)
    mindiff <- min(diffs)
    idx <- which.min(diffs)
    nd <- d[rep(idx, length(x)),]
    nd[[var]] <- x
    me <- as.vector(predict(m, newdata = nd))
    return(list(cval = cval,
                mindiff = mindiff,
                idx = idx,
                me = me))
  }
  
  ## do marginal for every smooth effect
  # lvar <- list(y = m$y)
  lvar <- list()
  l <- list()
  for(i in seq_along(slabs)) {
    
    ############################################################################
    # i <- 1
    ############################################################################
    
    ## variable and term
    var <- svars[i]
    l$var <- var
    l$term <- slabs[i]
    
    ## variable relevance ------------------------------------------------------
    yhat_without <- predict(m, exclude = slabs[i])
    l$yhat_without
    pred_diff <- abs(yhat_without - yhat)
    l$pred_diff <- pred_diff
    
    
    ## marginal effects --------------------------------------------------------
    ## constant c
    c <- yhat - predict(m, terms = slabs[i])
    l$c <- c
    
    ## equidist x
    xme <- seq(min(d[[var]]), max(d[[var]]), length.out = 1000)
    l$xme <- xme
    
    ## mean
    l[["cmean"]] <- getme(mean(c), c, d, xme)
    
    ## quantiles
    for(j in seq_along(cquantiles)) {
      l[["cquantiles"]][[paste0("q", cquantiles[j])]] <- 
        c(list(prob = cquantiles[j]),
          getme(unname(quantile(c, cquantiles[j])), c, d, xme))
    }
    
    ## save in var list
    lvar[[var]] <- l
    l <- list()
  }
  

  
  ## plot everything
  par(mfrow = c(length(slabs), 2))
  getylim <- function(lvar) {
    range(sapply(lvar, function(var) 
      range(unlist(
        lapply(var[["cquantiles"]], `[[`, "me")), na.rm = TRUE)))
  }
  meylim <- getylim(lvar)
  
  for(i in seq_along(slabs)) {
    
    ############################################################################
    # i <- 1
    ############################################################################
    
    ## centered effects
    plot(m, select = i, main = "Estimated effect (centered)")
    
    ## marginal effects
    l <- lvar[[i]]
    plot(l$xme, l$cmean$me, type = 'l',
         lwd = 2, ylim = meylim,
         xlab = l$var, ylab = 'y', main = 'Marginal effects')
    
    cquantiles <- sort(unique(cquantiles))
    cqcols <- rev(sequential_hcl(100000, "Sunset"))
    densc <- density(l$c)
    densc$y <- densc$y / max(densc$y)
    
    for(j in seq_along(l$cquantiles)) {
      lines(l$xme, l$cquantiles[[j]]$me,
            col = cqcols[round(100000*approx(densc$x, 
                                             densc$y, 
                                             xout = l$cquantiles[[j]]$cval)$y)])
    }
    lines(l$xme, l$cmean$me, lty = 1, lwd = 2)
    lines(l$xme, l$cquantiles$q0.5$me, lty = 2, lwd = 2)
    
    legend("topright", legend = c("Mean", "Median"),
           lty = c(1, 2), lwd = 2, bty = "n")
    
  }
  
  
  ## relevance stats
  digs <- 3
  w <- max(nchar(svars), nchar("Variable"))
  ss <- paste0(
    sapply(seq_along(svars), function(i)
      sprintf(
        paste0("%-", 13, "s %10.3f %10.3f\n "),
        svars[i],
        round(mean(lvar[[i]]$pred_diff), digs),
        round(median(lvar[[i]]$pred_diff), digs)
      )
    ),
    collapse = ""
  )
  
  cat(
    sprintf("\n"),
    c("Relevance (based on APD):\n"),
    # c("\n"),
    paste0(strrep("=", 36), "\n"),
    sprintf(paste0("%-", 13, "s %10s %10s\n"), "Variable", "Mean", "Median"),
    paste0(strrep("-", 36), "\n"),
    substr(ss, 1, nchar(ss) - 2),
    c("\n"),
    paste0(strrep("=", 36), "\n\n"),
    sprintf("Number of observations   %10d\n", nrow(d)),
    sprintf("Mean of y                %10.3f\n", round(mean(m$y), digs)),
    sprintf("SD of y                  %10.3f\n", round(sd(m$y), digs))
  )
  
  
  
  ## return stats
  if(returnstat) {return(lvar)}
}


