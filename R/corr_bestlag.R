#' Computes best lag correlation
#'
#' This function computes correlation based on best picked lags. The lags indicate delayed changes.
#'
#' @param data a matrix with rows representing genes and columns representing different timepoints
#' @param max.lag a integer value of the maximum lags allowed in the dataset,
#' if null, defaults to the floor of the number of timepoints divided by 4
#' @param timepoints a vector of time points used in the dataset
#' @param C a numeric value of C used in computing weighted correlation,
#' if null, a default is computed based on the penalty argument
#' @param penalty a factor with two levels high and low penalty on the weighted correlation
#' @param iter an integer indicating the number of C values to test for low penalty
#' @return a list containing weighted correlation and best lags used in each row
#'
#' @examples
#' corr.bestlag(array(rnorm(30), c(5, 6)), max.lag = 1,
#'           timepoints = c(0, 5, 10, 15, 20, 25), C = 10, penalty = "high")
#' corr.bestlag(array(runif(40, 0, 20), c(4, 10)),
#'           timepoints = c(0, 0.5, 1.5, 3, 6, 12, 18, 26, 39, 50), penalty = "high")
#' corr.bestlag(matrix(data = rexp(n = 40, 2), nrow = 8),
#'           timepoints = c(0, 5, 15, 20, 40), penalty = "low", iter = 5)
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'





corr.bestlag <- function(data, timepoints, max.lag = NULL, C = NULL, penalty = "high", iter = 10){
  #fixing the max lag if it is NULL
  if(is.null(max.lag)){
    max.lag <- floor(length(timepoints) / 4)
  }
  data <- as.matrix(data)
  #checking the condition
  stopifnot(dim(data)[2] == length(timepoints), max.lag <= length(timepoints) / 4,  is.numeric(iter),
            penalty == "high" | penalty == "low", max.lag %% 1 == 0, iter %% 1 == 0, iter > 1,
            max.lag >= 1)
  #finding the values of C
  values <- findC(timepoints, max.lag, iter = iter)
  #if C is already given, the matrix is computed based on that value
  if(is.numeric(C)){
      lags <- best.lag(data, max.lag = max.lag, timepoints, C = C)
      new.data <- prep.data(data, lags, timepoints)
      return(list(corr = comp.corr(new.data$data, new.data$time, C = C), lags = lags,
      C = C))
  }
  # if C is not given but penalty is high, the matrix is computed based on first C value from values
  else if(penalty == "high"){
    lags <- best.lag(data, max.lag = max.lag, timepoints, C = values[1])
    new.data <- prep.data(data, lags, timepoints)
    return(list(corr = comp.corr(new.data$data, new.data$time, C = values[1]), lags = lags,
                C = values[1]))
  }
  #if penalty is low, the matrix is computed for all 10 values in values vector
  else if(penalty == "low"){
    clustdiff <- rep(NA, length(values) - 1)
    allcorr <- NULL
    alllags <- NULL
    for(v in 1:length(values)){
      lags <- best.lag(data, max.lag = max.lag, timepoints, C = values[v])
      new.data <-  prep.data(data, lags, timepoints)
      result <- list(corr = comp.corr(new.data$data, new.data$time, C = values[v]), lags = lags)
      allcorr[[v]] <- result$corr
      alllags[[v]] <- result$lags
    }
    # the adjacent similarity matrix
    for(j in 1:(length(values) - 1)){
      clustdiff[j] <- sum((as.vector(allcorr[[j + 1]]) - as.vector(allcorr[[j]]))^2)
    }
    #the best C matrix is picked based on the smallest different with the adjacent similarity matrix
    return(list(corr = allcorr[[which.min(clustdiff) + 1]], lags = alllags[[which.min(clustdiff) + 1]],
                C = values[which.min(clustdiff) + 1] ))
  }
}
