# takes a vector of correlation and a vector of lags, returns the best lag
#   based on sum of highest correlation
score <- function(corr, lags){
    alags <- sort(unique(lags))
    value <- rep(NA, length(alags))
    for(i in seq(alags)){
        value[i] <- sum(corr[lags == alags[i]])
    }
    return(alags[which.max(value)])
}

#computes the weighted correlation between x and y based on the weight w
wt.corr <- function(x, y, w){
    stopifnot(is.vector(x), is.vector(y), is.vector(x),
    length(x) == length(y), length(x) == length(w))
    if(sum(w) != 1){
        w <- w / sum(w)
    }
    mx <- sum(w * x)
    my <- sum(w * y)
    cov <- sum(w * (x - mx) * (y - my))
    sdx <- sqrt(sum(w * (x - mx)^2))
    sdy <- sqrt(sum(w * (y - my)^2))
    return(cov / (sdx * sdy))
}

#computes mean difference squared between x1 and x2
meansq.dist <- function(x1, x2) {mean((x1 - x2) ^ 2)}

#returns possible values of C based on timepoints and max lag
findC <- function(timepoints, max.lag = NULL, pi = 0.95, iter = 10){
    stopifnot(is.vector(timepoints), max.lag <= length(timepoints) / 4, is.numeric(max.lag))
    penalty <- seq(0.5, 0.95, length.out = iter)
    vals <- NULL
    for(i in 1:max.lag){
        vals <- c(vals, meansq.dist(timepoints[(i + 1):length(timepoints)],
        timepoints[1:(length(timepoints) - i)]))
    }
    vals <- - mean(vals) / log(penalty)
    return(vals)
}

#returns the best possible lags for each row in the dataset after considering max.lag
best.lag <- function(data, max.lag = NULL, timepoints, C){
  if(is.null(max.lag)){
    max.lag <- floor(length(timepoints) / 4)
  }
  stopifnot(dim(data)[2] == length(timepoints), max.lag <= length(timepoints) / 4)
  shift <- rep(NA, dim(data)[1])
  for(i in 1:dim(data)[1]){
        lags <- rep(NA, (dim(data)[1]))
        bcorr <- rep(NA, (dim(data)[1]))
        for(j in 1:dim(data)[1]){
            if(i != j){
                corr <- rep(NA, max.lag * 2 + 1)
                for(k in max.lag:1){
                    weight <- rbind(timepoints[(k + 1):length(timepoints)],
                    timepoints[1:(length(timepoints) - k)])
                    weight <- exp(-1/C * apply(weight, 2, function(x){(diff(x))^2}))
                    corr[max.lag - k + 1] <- exp(-1/C * meansq.dist(timepoints[(k + 1):length(timepoints)],
                    timepoints[1:(length(timepoints) - k)])) *
                    wt.corr(data[i, 1:(length(timepoints) - k)],
                    data[j, (k + 1):length(timepoints)],
                    w = weight)
                }
                corr[max.lag + 1] <- cor(data[i, ], data[j, ])
                for(m in 1:max.lag){
                    weight <- rbind(timepoints[(m + 1):length(timepoints)], timepoints[1:(length(timepoints) - m)])
                    weight <- exp(-1/C * apply(weight, 2, function(x){(diff(x))^2}))
                    corr[max.lag + m + 1] <- exp(-1/C * meansq.dist(timepoints[(m + 1):length(timepoints)],
                    timepoints[1:(length(timepoints) - m)])) *
                    wt.corr(data[j, 1:(length(timepoints) - m)],
                    data[i, (m + 1):length(timepoints)],
                    w = weight)
                }
                val <- -max.lag:max.lag
                lags[j] <- val[which.max(corr)]
                bcorr[j] <- max(corr)
            }
        }
        lags <- lags[- i]
        bcorr <- bcorr[- i]
        shift[i] <- score(bcorr, lags)
    }
    return(shift)
}

# returns the a new dataset from the real dataset with NA's upon accounting the shifts
prep.data <- function(data, lags){
    stopifnot(dim(data)[1] == length(lags), is.vector(lags),
    all(is.numeric(lags)))
    new.data <- array(NA, dim(data))
    for(i in 1:dim(data)[1]){
        #given no lag
        if(lags[i] == 0){new.data[i, ] <- data[i, ]}
        #positive lag
        else if(lags[i] > 0){
            new.data[i, ] <- c(data[i, -(1:lags[i])], rep(NA, lags[i]))
        }
        #negative lag
        else if(lags[i] < 0){
            new.data[i, ] <- c(rep(NA, (-lags[i])), data[i, 1:(dim(data)[2] + lags[i])])
        }
        else{
            print("There is an error in best lags!")
        }
    }
    return(new.data)
}

#takes two vectors with NA's of different length and align them
weight.lag <- function(vector1, vector2){
    if(length(vector1) < length(vector2)){
        vector2 <- vector2[1:length(vector1)]
    }
    else{
        vector1 <- vector1[1:length(vector2)]
    }
    return(as.matrix(rbind(vector1, vector2)))
}

#computes the lag penalized weighted correlation and returns it
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


#incorporates all the small function above and computes lag penalized weighted correlation for
#   high penalty and low penalty and returns a list of correlation distance and best lags
corr.bestlag <- function(data, max.lag = 2, timepoints, C = NULL, penalty = NULL, k = 10, iter = 10){
    stopifnot(dim(data)[2] == length(timepoints), max.lag <= length(timepoints) / 4 )
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


