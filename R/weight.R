#' Weight in correlation
#'
#' This function computes the weight used for correlation using timepoints used and lags used
#'
#' @param t a vector of timepoints
#' @param lag a integer value of the lag used
#' @param C a numeric of the constant used in the penalty and weight inside the Gaussian kernel
#' @return a list containing w0 and vector w used for computing weighted correlation
#'
#' @examples
#' weight(t = c(0, 5, 10, 15, 20), lag = 1, C = 20)
#' weight(t = c(0, 2, 5, 10, 14, 19, 22), lag = 1,C = 100)
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'


#computing weights for inside the weighted correlation and outside penalty function
weight <- function(t, lag, C){
  #checking if all the variables are right
  stopifnot(length(t)/4 >= lag, is.numeric(C), all(is.numeric(t)), is.numeric(lag))
  #creating two rows with lagged timepoints
  tlag <- rbind(t[(lag + 1):length(t)], t[1:(length(t) - lag)])
  # w0 is the outside penalty function
  w0 <- exp(-1 / C * mean(apply(tlag, 2, function(x){(diff(x))^2})))
  # w is the inside weight function in the weighted correlation
  w <- exp(-1 / C * apply(tlag, 2, function(x){(diff(x))^2}))
  #returning a list of inside and outside weights
  return(list(w = w, w0 = w0))
}
