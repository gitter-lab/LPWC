#' Computing corr
#'
#' This function computes the correlation upon picking the best lag and adjusting the data for lags
#'
#' @param data a matrix of data with the columns representing different timepoints
#' @param timepoints a vector which contains all the timepoints represented in the data
#' @param C a numeric value of C used in computing weighted correlation
#' @return a simmilarity matrix with values between -1 and 1 (1 highly correlated, 0 no correlation )
#'
#' @example comp.corr(t(array(c(rnorm(16), NA, NA, rnorm(6), rnorm(7), NA, rnorm(8)), c(8, 5))), timepoints = c(0, 5, 10, 15, 20, 25, 30, 35), C = 10)
#'
#'
#' @author Thevaa Chandereng, Anthony Gitter
#'
#'


comp.corr <- function(data, timepoints, C){
  stopifnot(dim(data)[2] == length(timepoints), all(is.numeric(timepoints)),
            is.numeric(C))
  corr <- array(NA, c(dim(data)[1], dim(data)[1]))
  for(j in 1:(dim(data)[1] - 1)){
    for(i in (j + 1):dim(data)[1]){
      pair <- rbind(data[i, ], data[j, ])
      if(all(apply(pair, 2, function(x){all(!is.na(x))}))){
        corr[i, j] <- cor(pair[1, ], pair[2, ])
      }
      else{
        t1 <- timepoints[complete.cases(pair[1, ])]
        t2 <- timepoints[complete.cases(pair[2, ])]
        weight <- weight.lag(t1, t2)
        comp <- complete.cases(t(pair))
        weight <- weight[, 1:sum(comp)]
        pair.full <- pair[, comp]
        if(all(weight[1,] == weight[2,])){
          corr[i, j] <- cor(pair.full[1, ], pair.full[2, ])
        }
        else{
          weights <- exp(-1/C * apply(weight, 2, function(x){(diff(x))^2}))
          corr[i, j] <- exp(- 1 / C * meansq.dist(weight[1, ], weight[2, ])) *
            wt.corr(pair.full[1, ], pair.full[2, ], weights)
        }
      }
    }
  }
  return(as.dist(corr))
}
