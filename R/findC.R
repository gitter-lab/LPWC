#' Finding best C
#'
#' This function computes the best C using the timepoints and max lag in the dataset
#'
#' @param timepoints a vector of timepoints used in the dataset
#' @param max.lag a numeric value with maximum lags allowed, if null, defaults to the floor of the number of timepoints divided by 4
#' @param pi a numeric value between 0.5 and 1 for the upper bound on the penalty
#' @param iter a numeric value with number of penalty
#' @return a vector of length (10 or indicated by iter) different C to be tested
#'
#' @examples findC(c(0, 5, 10, 15, 20, 25), max.lag = 1, iter = 15)
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'



findC <- function(timepoints, max.lag = NULL, pi = 0.95, iter = 10){
  if(is.null(max.lag)){
    max.lag <- floor(length(timepoints) / 4)
  }
  stopifnot(all(is.numeric(timepoints)), max.lag <= length(timepoints) / 4, max.lag %% 1 == 0)
  penalty <- seq(0.5, 0.95, length.out = iter)
  vals <- NULL
  for(i in 1:max.lag){
    vals <- c(vals, mean((timepoints[(i + 1):length(timepoints)] -
                                timepoints[1:(length(timepoints) - i)])^2))
  }
  app <- - mean(vals) / log(penalty)
  realC <- rep(NA, length(penalty))
  for(b in 1:length(penalty)){
    fun <- function(C) { mean(exp(- 1 / C * vals)) - penalty[b]}
    realC[b] <- uniroot(f = fun, c(0, app[b]))$root
  }
  return(realC)
}



