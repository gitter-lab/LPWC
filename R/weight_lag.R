#' Weight Lag
#'
#' This function matches vector of two different lengths
#'
#' @param x1 a vector
#' @param x2 a vector
#' @return a matrix with two rows with the shortest length of the vector as the number of columns
#'
#' @example weight.lag(1:5, 2:9)
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
