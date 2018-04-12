#' Score of Lags
#'
#' This function computes the score of best lags by summing the correlation of corresponding lags
#'
#' @param corr a vector of computed correlation
#' @param lags a vector of same length with corr that holds the lags corresponding to the corr vector
#' @return a numerical value of best lag picked based on corr and lags
#'
#' @examples score(runif(10, 0, 1), c(2, 0, 0, 0, 3, 2, -1, 2, 0, 1))
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'


score <- function(corr, lags){
  #checking the corr and lags vector
  stopifnot(length(corr) == length(lags), all(is.numeric(corr)), all(is.numeric(lags)))
  #taking unique lags and sorting them
  uni.lags <- sort(unique(lags))
  value <- rep(NA, length(uni.lags))
  #assigning score for each lag vector
  for(i in seq(uni.lags)){
    value[i] <- sum(corr[lags == uni.lags[i]])
  }
  #printing the lag with max score
  return(uni.lags[which.max(value)])
}





