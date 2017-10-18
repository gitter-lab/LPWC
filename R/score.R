#' Score of Lags
#'
#' This function computes the score of best lags by summing the correlation of corresponding lags
#'
#' @param corr a vector of computed correlation
#' @param lags a vector of same length with corr and holds the lags corresponding to the corr vector
#' @return a numerical value of best lag picked based on corr and lags
#'
#' @example score(runif(10, 0, 1), c(2, 0, 0, 0, 3, 2, -1, 2, 0, 1))
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'


score <- function(corr, lags){
  stopifnot(length(corr) == length(lags), all(is.numeric(corr)), all(is.numeric(lags)))
  alags <- sort(unique(lags))
  value <- rep(NA, length(alags))
  for(i in seq(alags)){
    value[i] <- sum(corr[lags == alags[i]])
  }
  return(alags[which.max(value)])
}

