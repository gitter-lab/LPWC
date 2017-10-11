#' Weight in correlation
#'  
#' This function computes the weight used for correlation using timepoints used and lags used
#' 
#' @param t a vector of timepoints 
#' @param k a integer value of lag used 
#' @return a vector of weights used in correlation 
#'
#' @example weight(t = c(0, 5, 10, 15, 20), k = 1)
#' 
#' 
#' @author Thevaa Chandereng, Anthony Gitter
#' 
#'


weight <- function(t, lag){
  tlag <- rbind(t[(lag + 1):length(t)], t[1:(length(t) - lag)])
  weight <- exp(-1 / C * apply(tlag, 2, function(x){(diff(x))^2}) / sum(apply(tlag, 2, function(x){(diff(x))^2})))
  return(weight)
}
