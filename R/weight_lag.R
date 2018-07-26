#' Weight Lag
#'
#' This function matches vectors of two different lengths
#'
#' @param x1 a vector
#' @param x2 a vector
#' @return a matrix with two rows with the shortest length of the vector as the number of columns
#'
#' @examples
#' weight.lag(1:5, 2:9)
#' weight.lag (seq(0, 10, 2), seq(4, 10, 2))
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#' @export weight.lag
#'



weight.lag <- function(x1, x2){
  #checking if all the values are numeric
  stopifnot(all(is.numeric(x1)), all(is.numeric(x2)))
  #picking the shortest vector and making the other vector shorter
  if(length(x1) < length(x2)){
    x2 <- x2[1:length(x1)]
  }
  else{
    x1 <- x1[1:length(x2)]
  }
  #returning the shortest vector of both vector
  return(as.matrix(rbind(x1, x2)))
}
