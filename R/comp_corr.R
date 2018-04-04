#' Computing corr
#'
#' This function computes the correlation upon picking the best lag and
#' adjusting the data for lags
#'
#' @param data a matrix with rows representing genes and columns representing
#' different timepoints (NA's added when lags are needed)
#' @param time a vector which contains all the timepoints represented in the data
#' @param C a numeric value of C used in computing weighted correlation
#' @return a simmilarity matrix with values between -1 and 1
#' (1 highly correlated, 0 no correlation)
#'
#' @examples
#' timepoints <- t(array(c(0, 5, 10, 15, 20, 25, 30, 35), c(8, 5)))
#' comp.corr(array(rnorm(40), c(5, 8)), time = timepoints, C = 10)
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'


comp.corr <- function(data, time, C){
  stopifnot(all(dim(data) == dim(time)), is.numeric(C))
  corr <- array(NA, c(dim(data)[1], dim(data)[1]))
  for(j in 1:(dim(data)[1] - 1)){
    for(i in (j + 1):dim(data)[1]){
      pair <- weight.lag(data[i, !is.na(data[i, ])], data[j, !is.na(data[j, ])])
      times <- weight.lag(time[i, !is.na(time[i, ])], time[j, !is.na(time[j, ])])
      weights <- apply(times, 2, function(x){(diff(x)) ^ 2})
      corr[i, j] <- exp(-1 / C * mean(weights)) * wt.corr(pair[1, ], pair[2, ], exp(-1 / C * weights))
    }
  }
  return(as.dist(corr))
}


