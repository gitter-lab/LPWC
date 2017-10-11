#' Mean Square Distance
#'
#' This function computes difference in mean square distance between two vector
#'
#' @param x1 a vector
#' @param x2 a vector with same length of x1
#' @return a numerical value of difference in squared mean of vector x1 and x2
#'
#' @example
#' meansq.dist(x1 = c(2, 6, 8, 9), x2 = c(3, 4, 8, 3))
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'


meansq.dist <- function(x1, x2) {
  return(mean(((x1 - x2)^2) / (sum((x1 - x2)^2))))
}
