#' Computes best lag correlation
#'
#' This function computes correlation based on best picked lags. The lags indicate delayed changes. 
#'
#' @param data a matrix with columns representing different timepoints
#' @param max.lag a integer value of the maximum lags allowed in the dataset
#' @param timepoints a vector of time points used in the dataset
#' @param C a numeric value of C used in computing weighted correlation
#' @param penalty a character with two levels high and low penalty on the weighted correlation
#' @param k a integer indicating the number of cluster groups to be assigned
#' @param iter an integer indicating numbers of C's to test for low penalty
#' @return a list containing weighted correlation and best lags used in eacch row
#'
#' @example corr.bestlag(array(rnorm(30), c(5, 6)), max.lag = 1, timepoints = c(0, 5, 10, 15, 20, 25), C = 10, penalty = "high")
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'




corr.bestlag <- function(data, max.lag = 2, timepoints, C = NULL, penalty = "high", k = 10, iter = 10){
  stopifnot(dim(data)[2] == length(timepoints), max.lag <= length(timepoints) / 4 , is.numeric(k), is.numeric(iter), 
            penalty == "high" | penalty == "low", max.lag %% 1 == 0, k %% 1 == 0, iter %% 1 == 0)
  values <- findC(timepoints, max.lag, iter = iter)
  if(penalty == "high" || is.numeric(C)){
    lags <- best.lag(data, max.lag = max.lag, timepoints, C = values[1])
    new.data <- prep.data(data, lags)
    return(list(corr = comp.corr(new.data, timepoints, C = values[1]), lags = lags))
  }
  else if(penalty == "low"){
    clustdiff <- rep(NA, length(values) - 1)
    allcorr <- NULL
    alllags <- NULL
    for(i in 1:length(values)){
      lags <- best.lag(data, max.lag = max.lag, timepoints, C = values[i])
      new.data <- prep.data(data, lags)
      result <- list(corr = comp.corr(new.data, timepoints, C = values[i]), lags = lags)
      allcorr[[i]] <- result$corr
      alllags[[i]] <- result$lags
    }
    for(j in 1:(length(values) - 1)){
      clustdiff[j] <- (cutree(hclust(allcorr[[j + 1]]), k = k) - cutree(hclust(allcorr[[j]]), k = k)) ^ 2
    }
    return(list(corr = allcorr[[which.min(clustdiff) + 1]], lags = alllags[[which.min(clustdiff) + 1]]))
  }
}
