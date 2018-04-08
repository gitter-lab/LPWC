#' Weighted correlation
#'
#' This function computes weighted Pearson correlation between two vectors with weights given. The output is between -1 and 1 with 1
#' being highly positively correlated, -1 being highly negatively correlated, and 0 being no correlation
#'
#' @param x a vector
#' @param y a vector with same length of x
#' @param w a vector with same length of x and y
#' @return a numerical value of weighted Pearson correlation
#'
#' @examples
#' wt.corr(c(1, 2, -9, 4, 5), c(2:6), c(0.5, 1, 2, 0.5, 2))
#'
#' @source \url{https://en.wikipedia.org/wiki/Pearson_correlation_coefficient#Weighted_correlation_coefficient}
#'
#'
wt.corr <- function(x, y, w){
  #checking of all the conditions are fulfilled
  stopifnot(all(is.numeric(c(x, y, w))),
            length(x) == length(y), length(x) == length(w))
  #check if the weights are 0, if they are 0, add 1 to all of them
  if(all(w == 0)){
    w <- w + 1
  }
  #checking if the sum of the weights are 1, if not normalize them
  else if(sum(w) != 1){
    w <- w / sum(w)
  }
  #weighted mean of x and y
  mx <- sum(w * x)
  my <- sum(w * y)
  #covariance of weighted x and y
  cov <- sum(w * (x - mx) * (y - my))
  #std deviation of weighted x and y
  sdx <- sqrt(sum(w * (x - mx)^2))
  sdy <- sqrt(sum(w * (y - my)^2))
  #return the weighted correlation value
  return(cov / (sdx * sdy))
}
