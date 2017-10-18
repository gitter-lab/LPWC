#' Weight in correlation
#'
#' This function computes the weight used for correlation using timepoints used and lags used
#'
#' @param t a vector of timepoints
#' @param k a integer value of lag used
#' @return a list containing w0 and vector w used in computing weighted correlation
#'
#' @example weight(t = c(0, 5, 10, 15, 20), lag = 1, C = 20)
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'


weight <- function(t, lag, C){
  stopifnot(length(t)/4 >= lag, is.numeric(C), all(is.numeric(t)), is.numeric(lag))
  tlag <- rbind(t[(lag + 1):length(t)], t[1:(length(t) - lag)])
  w0 <- exp(-1 / C * mean(apply(tlag, 2, function(x){(diff(x))^2})))
  w <- exp(-1 / C * apply(tlag, 2, function(x){(diff(x))^2}))
  return(list(w = w, w0 = w0))
}
