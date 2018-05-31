#' Computing corr
#'
#' This function computes the weighted correlation with a penalty for lags.
#' It should only be used after the fixed lags have already been applied to the dataset and timepoints using the functions
#' prep.data() and best.lag().
#'
#'
#' @param data a lagged matrix with rows representing genes and columns representing
#' different timepoints (NA's added when lags are needed)
#' @param time a lagged matrix with rows representing each gene's timepoint and column representing the number of timepoints, NA is introduced when it is lagged
#' @param C a numeric value of C used in computing weighted correlation
#' @return a simmilarity matrix with values between -1 and 1
#' (1 highly correlated, 0 no correlation)
#'
#' @examples
#' comp.corr(array(rnorm(40), c(5, 8)), time = t(array(c(0, 5, 10, 15, 20, 25,
#' 30, 35), c(8, 5))), C = 10)
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'


comp.corr <- function(data, time, C){
  #checking for all the conditions with data, time and C
  stopifnot(all(dim(data) == dim(time)), is.numeric(C))
  #creating an empty matrix to store the correlation values
  corr <- array(NA, c(dim(data)[1], dim(data)[1]))
  #iterating through each i and j to print the correlation value
  for(j in 1:(dim(data)[1] - 1)){
    for(i in (j + 1):dim(data)[1]){
      #alligning the peptides and timepoints
      pair <- weight.lag(data[i, !is.na(data[i, ])], data[j, !is.na(data[j, ])])
      times <- weight.lag(time[i, !is.na(time[i, ])], time[j, !is.na(time[j, ])])
      #computing w0 and w from the alliged timepoints
      weights <- apply(times, 2, function(x){(diff(x)) ^ 2})
      #computing correlation from the values
      corr[i, j] <- exp(-1 / C * mean(weights)) * wt.corr(pair[1, ], pair[2, ], exp(-1 / C * weights))
    }
  }
  #picking the correlation matrix
  return(as.dist(corr))
}


