#' Best Lag
#'
#' This function computes the best lags for a dataset using weighted correlation.
#' The lags obtained are in reference to the original timepoints.
#'
#' @param data a matrix with rows representing genes and columns
#' representing different timepoints
#' @param max.lag a numeric value of the maximum lag allowed, if null,
#' defaults to the floor of the number of timepoints divided by 4
#' @param timepoints a vector of time points used in the dataset
#' @param C a numeric value of C used in computing weighted correlation
#' @return a vector of best lags used in the dataset, one per gene
#'
#' @example best.lag(array(rnorm(20), c(4, 5)), timepoints = c(0, 5, 10,
#'          20, 40), C = 300)
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'

best.lag <- function(data, timepoints, max.lag = NULL, C){
  data <- as.matrix(data)
  #assigning the max lag if it is NULL
  if(is.null(max.lag)){
    max.lag <- floor(length(timepoints) / 4)
  }
  #checking through all the conditions
  stopifnot(dim(data)[2] == length(timepoints), max.lag <= length(timepoints) / 4,
            is.numeric(max.lag), is.numeric(C))
  #creating an empty vector for storing the best lag
  shift <- rep(NA, dim(data)[1])
  for(i in 1:dim(data)[1]){
    lags <- rep(NA, (dim(data)[1]))
    bcorr <- rep(NA, (dim(data)[1]))
    for(j in 1:dim(data)[1]){
      if(i != j){
        #storing the correlation for each i
        corr <- rep(NA, max.lag * 2 + 1)
        #storing correlation for the negative lags
        for(k in max.lag:1){
          allw <- weight(t = timepoints, lag = k, C = C)
          corr[max.lag - k + 1] <- allw$w0 *
            wt.corr(data[i, 1:(length(timepoints) - k)],
                    data[j, (k + 1):length(timepoints)],
                    w = allw$w)
        }
        #simple pearson correlation
        corr[max.lag + 1] <- cor(data[i, ], data[j, ])
        #storing correlation for the positive lags
        for(m in 1:max.lag){
          allw <- weight(t = timepoints, lag = m, C = C)
          corr[max.lag + m + 1] <- allw$w0 *
            wt.corr(data[j, 1:(length(timepoints) - m)],
                    data[i, (m + 1):length(timepoints)],
                    w = allw$w)
        }
        #iteration of each lags from most negative to positive
        val <- max.lag:-max.lag
        #finding the max correlation
        lags[j] <- val[which.max(corr)]
        bcorr[j] <- max(corr)
      }
    }
    #removing peptide i's correlation
    lags <- lags[- i]
    bcorr <- bcorr[- i]
    #finding the best lag
    shift[i] <- score(bcorr, lags)
  }
  return(shift)
}
