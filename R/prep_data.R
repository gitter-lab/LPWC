#' Preparing Data
#'
#' This function prepares the data to compute correlation by introducing NA's when lags are needed
#'
#' @param data a matrix with columns representing different timepoints
#' @param lags a vector of same length as the number of rows in the data column indicating the best lags
#' @return a matrix with NA's introduced where lags are needed
#'
#' @example prep.data(array(rnorm(20), c(5, 4)), c(0, 0, 0, -1, 1))
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'



prep.data <- function(data, lags){
  stopifnot(dim(data)[1] == length(lags), is.vector(lags),
            all(is.numeric(lags)), all(lags < dim(data)[2]) )
  new.data <- array(NA, dim(data))
  for(i in 1:dim(data)[1]){
    #given no lag
    if(lags[i] == 0){new.data[i, ] <- data[i, ]}
    #positive lag
    else if(lags[i] < 0){
      new.data[i, ] <- c(data[i, -(1:-lags[i])], rep(NA, lags[i]))
    }
    #negative lag
    else if(lags[i] > 0){
      new.data[i, ] <- c(rep(NA, (lags[i])), data[i, 1:(dim(data)[2] - lags[i])])
    }
    else{
      print("There is an error in best lags!")
    }
  }
  return(new.data)
}
