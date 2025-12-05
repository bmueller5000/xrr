varrel <- function(model, data, y, term) {
  
  ##############################################################################
  # ## testing
  # model <- mod
  # data <- dat
  # y = "y"
  # term <- "s(logp)"
  ##############################################################################
  
  pred <- as.data.frame(predict(model, type = "terms"))
  
  if(!any(names(pred) %in% term)) {
    stop("\nTerm not found!")
  }
  
  yhat_without <- unname(rowSums(pred[, !names(pred) %in% term]))
  yhat <- unname(rowSums(pred))
  
  pred_diff <- abs(yhat_without - yhat)
  
  stats <- cat(
    sprintf("\n"),
    sprintf("Number of observations  : %10d\n", nrow(data)),
    sprintf("Mean of y               : %10.5f\n", mean(data[[y]])),
    sprintf("SD of y                 : %10.5f\n", sd(data[[y]])),
    sprintf("Mean of pred diff       : %10.5f\n", mean(pred_diff)),
    sprintf("Median of pred diff     : %10.5f\n", median(pred_diff))
  )
  
  return(list(model = model,
                data = data,
                y = y,
                term = term, 
                pred = pred,
                yhat_without = yhat_without,
                yhat = yhat,
                pred_diff = pred_diff,
                stats = stats))
}


