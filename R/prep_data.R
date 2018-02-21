#' Preparing Data
#'
#' This function prepares the data to compute correlation by introducing NA's when lags are needed
#'
#' @param data a matrix with columns representing different timepoints
#' @param lags a vector of same length as the number of rows in the data column indicating the best lags
#' @param timepoints a vector of time points used in the dataset
#' @return a list of two matrix, matrix with NA's for the lags for the dataset and timepoints used for each row in the dataset
#'
#' @example prep.data(array(rnorm(20), c(5, 4)), c(0, 0, 0, -1, 1), timepoints = c(0, 5, 15, 30))
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'



prep.data <- function(data, lags, timepoints){
  stopifnot(dim(data)[1] == length(lags), is.vector(lags), all(is.numeric(lags)),
            all(abs(lags) <= length(timepoints) / 4), dim(data)[2] == length(timepoints),
            is.vector(timepoints))
  new.data <- array(NA, dim(data))
  new.time <- array(NA, dim(data))
  for(i in 1:dim(data)[1]){
    #given no lag
    if(lags[i] == 0){
      new.data[i, ] <- data[i, ]
      new.time[i, ] <- timepoints
      }
    #positive lag
    else if(lags[i] < 0){
      new.data[i, ] <- c(data[i, (1 - lags[i]):(dim(data)[2])], rep(NA, -lags[i]))
      new.time[i, ] <- c(timepoints[1:(dim(data)[2] + lags[i])], rep(NA, -lags[i]))
    }
    #negative lag
    else if(lags[i] > 0){
      new.data[i, ] <- c(rep(NA, lags[i]), data[i, 1:(dim(data)[2] - lags[i])])
      new.time[i, ] <- c(rep(NA, lags[i]), timepoints[1:(dim(data)[2] - lags[i])])
    }
    else{
      print("There is an error in best lags!")
    }
  }
  return(list(data = new.data, time = new.time))
}
