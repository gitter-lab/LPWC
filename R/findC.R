#' Finding best C
#'
#' This function computes the best C using the timepoints and max lag in the dataset
#'
#' @param timepoints a vector of timepoints used in the dataset
#' @param max.lag a numeric value with maximum lags allowed,
#' if null, defaults to the floor of the number of timepoints divided by 4
#' @param pi a numeric value between 0.5 and 1 for the upper bound on the penalty
#' @param iter a numeric value with number of penalty
#' @return a vector of length (10 or indicated by iter) different C to be tested
#'
#' @examples
#' findC(c(0, 5, 10, 15, 20, 25), max.lag = 1, iter = 15)
#' findC(c(2, 4, 8, 16, 32, 64, 128, 256), iter = 5)
#' findC(c(2, 6, 10, 15, 22, 30, 40, 55, 80), pi = 0.8, iter = 20)
#' findC(c(1, 2, 3.2, 4, 5.3, 7), pi = 0.99)
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'



findC <- function(timepoints, max.lag = NULL, pi = 0.95, iter = 10){
  #if lags are not provided, lags are assigned to floor of length(timepoints) / 4
  if(is.null(max.lag)){
    max.lag <- floor(length(timepoints) / 4)
  }
  #checking through all the conditions
  stopifnot(all(is.numeric(timepoints)), max.lag <= length(timepoints) / 4, max.lag %% 1 == 0)
  #running through the penalty for different iterations and pi's
  penalty <- seq(0.5, pi, length.out = iter)
  vals <- NULL
  #finding the mean difference in timepoints with different lags
  for(i in 1:max.lag){
    vals <- c(vals, mean((timepoints[(i + 1):length(timepoints)] -
                                timepoints[1:(length(timepoints) - i)])^2))
  }
  #taking the mean over the log penalty
  app <- - mean(vals) / log(penalty)
  realC <- rep(NA, length(penalty))
  #solving the root euqations for the summation
  for(b in 1:length(penalty)){
    fun <- function(C) { mean(exp(- 1 / C * vals)) - penalty[b]}
    realC[b] <- uniroot(f = fun, c(0, app[b]))$root
  }
  #printing different C for different penalty values
  return(realC)
}



