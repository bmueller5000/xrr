plot_marginal <- function(object, 
                          histsmallfreq = 200,
                          histsmallfreqcol = "darkred",
                          histlegend = TRUE,
                          histlegendpos = "topleft",
                          effselect = 1,
                          clegendpos = "topright",
                          cquantcols = "Sunset",
                          marefflegendpos = "topright",
                          onlymarginal = FALSE) {
  
  ##############################################################################
  ## testing
  # object <- logp_marginal
  # histsmallfreq = 200
  # histsmallfreqcol = "darkred"
  # histlegend = TRUE
  # histlegendpos = "topleft"
  # effselect = 1
  # clegendpos = "topright"
  # cquantcols = "Sunset"
  # onlymarginal = FALSE
  ##############################################################################
  
  model <- object$model
  data <- object$data
  var <- object$var
  c <- object$c
  cquantiles <- object$cquantiles
  
  if(!onlymarginal) {
    
    par(mfrow = c(2, 2))
    
    ## hist
    h <- hist(data[[var]], xlab = var, main = paste("Histogram of", var))
    h$counts[h$counts > histsmallfreq] <- 0
    plot(h, col = histsmallfreqcol, add = TRUE)
    abline(h = histsmallfreq, col = histsmallfreqcol, lty = 2, lwd = 2)
    if(histlegend) {
      legend(histlegendpos, legend = paste0("Freq. < ", histsmallfreq),
             fill = histsmallfreqcol, bty = "n")
    }
    
    
    ## estimated effect from model
    plot(model, select = effselect,
         main = paste("Estimated effect of", var))
    
    ## density of c
    plot(density(object$c), main = "Density of c")
    abline(v = object$mean$stat, lwd = 2)
    if(!is.null(object$quantiles$q0.5)) {
      abline(v = object$quantiles$q0.5$stat, lty = 2, lwd = 2)
      legend(clegendpos, legend = c("Mean", "Median"),
             lty = c(1, 2), lwd = c(2, 2), bty = "n")
    } else {legend(clegendpos, legend = c("Mean"),lwd = 2, bty = "n")}
    
  }
  
  ## marginal effects
  plot(object$varseq, object$mean$me, type = 'l',
       lwd = 2, ylim = range(c(object$mean$me,
                               sapply(object$quantiles, function(x) x$me))),
       xlab = var, ylab = 'y', main = 'Marginal effects')
  
  if(!is.null(object$quantiles)) {
    cquantiles <- sort(unique(cquantiles))
    cqcols <- rev(sequential_hcl(100000, cquantcols))
    densc <- density(c)
    densc$y <- densc$y / max(densc$y)
    
    for(i in seq_along(object$quantiles)) {
      lines(object$varseq, object$quantiles[[i]]$me,
            col = cqcols[round(100000*approx(densc$x, 
                                             densc$y, 
                                             xout = object$quantiles[[i]]$stat)$y)])
    }
    lines(object$varseq, object$mean$me, lty = 1, lwd = 2)
    lines(object$varseq, object$quantiles$q0.5$me, lty = 2, lwd = 2)
    
    legend(marefflegendpos, legend = c("Mean", "Median"),
           lty = c(1, 2), lwd = 2, bty = "n")
  } else {
    legend(marefflegendpos, legend = c("Mean"),
           lwd = 2, bty = "n")
  }
  
}


