plot_varrel <- function(object) {
  par(mfrow = c(1, 1))
  plot(density(object$pred_diff), 
       main = 'Density of absolute prediction difference')
}


