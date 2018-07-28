#' Preparing Data
#'
#' This function prepares the data to compute correlation by introducing NA's when lags are needed
#'
#' @param data a matrix with rows representing genes and columns representing
#' different timepoints
#' @param lags a vector of same length as the number of rows in the data column
#' indicating the best lags
#' @param timepoints a vector of time points used in the dataset
#' @return a list of two matrices, one matrix with NA's for the lags for the dataset
#' and another matrix with the timepoints used for each row in the dataset
#'
#' @examples
#' prep.data(array(rnorm(20), c(5, 4)), c(0, 0, 0, -1, 1),
#'           timepoints = c(0, 5, 15, 30))
#' prep.data(array(runif(100, 0, 10), c(10, 10)), sample((-2:2), size = 10, replace = TRUE),
#'           timepoints = c(0, 5, 15, 30, 45, 60, 75, 80, 100, 120))
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#' @export prep.data
#'



prep.data <- function(data, lags, timepoints){
  #checking all the conditions
  stopifnot(dim(data)[1] == length(lags), is.vector(lags), all(is.numeric(lags)),
            all(abs(lags) <= length(timepoints) / 4),
            dim(data)[2] == length(timepoints), is.vector(timepoints),
            all(lags %% 1 == 0))
  data <- as.matrix(data)
  #new matrix for data and time to incorporate lags
  new.data <- array(NA, dim(data))
  new.time <- array(NA, dim(data))
  #going through rows and adding the data with lags
  for(i in 1:dim(data)[1]){
    #given no lag
    if(lags[i] == 0){
      new.data[i, ] <- data[i, ]
      new.time[i, ] <- timepoints
      }
    #positive lag
    else if(lags[i] < 0){
      new.data[i, ] <- c(data[i, (1 - lags[i]):(dim(data)[2])], rep(NA, -lags[i]))
      new.time[i, ] <- c(timepoints[(1 - lags[i]):(dim(data)[2])], rep(NA, -lags[i]))
    }
    #negative lag
    else{
      new.data[i, ] <- c(rep(NA, lags[i]), data[i, 1:(dim(data)[2] - lags[i])])
      new.time[i, ] <- c(rep(NA, lags[i]), timepoints[1:(dim(data)[2] - lags[i])])
    }
  }
  #return a list of lag timepoints and data
  return(list(data = new.data, time = new.time))
}
