#' Mean Square Distance
#'
#' This function computes difference in mean square distance between two vector
#'
#' @param x1 a vector
#' @param x2 a vector with same length of x1
#' @return a numerical value of difference in squared mean of vector x1 and x2
#'
#' @example weight.lag(1:5, 2:6)
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'



weight.lag <- function(x1, x2){
  stopifnot(all(is.numeric(x1)), all(is.numeric(x2)))
  if(length(x1) < length(x2)){
    x2 <- x2[1:length(x1)]
  }
  else{
    x1 <- x1[1:length(x2)]
  }
  return(as.matrix(rbind(x1, x2)))
}
