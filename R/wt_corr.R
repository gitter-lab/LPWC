#' Weighted correlation
#'
#' This function computes weighted pearson correlation between two vectors with weights given. The ouput is between -1 and 1 with 1
#' being highly positive correlated, -1 with highly negative correlation and 0 being no correlation
#'
#' @param x a vector
#' @param y a vector with same length of x
#' @param w a vector with same length of x and y
#' @return a numerical value of weighted pearson correlation
#'
#' @example
#' wt.corr(c(1, 2, -9, 4, 5), c(2:6) c(0.5, 1, 2, 0.5, 2))
#'
#' @source \url{https://en.wikipedia.org/wiki/Pearson_correlation_coefficient#Weighted_correlation_coefficient}
#'
#'
wt.corr <- function(x, y, w){
  stopifnot(all(is.numeric(c(x, y, w))),
            length(x) == length(y), length(x) == length(w))
  if(all(w == 0)){
    w <- w + 1
  }
  else if(sum(w) != 1){
    w <- w / sum(w)
  }
  mx <- sum(w * x)
  my <- sum(w * y)
  cov <- sum(w * (x - mx) * (y - my))
  sdx <- sqrt(sum(w * (x - mx)^2))
  sdy <- sqrt(sum(w * (y - my)^2))
  return(cov / (sdx * sdy))
}
