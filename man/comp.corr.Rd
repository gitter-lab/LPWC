% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/comp_corr.R
\name{comp.corr}
\alias{comp.corr}
\title{Computing corr}
\usage{
comp.corr(data, time, C)
}
\arguments{
\item{data}{a lagged matrix or data frame with rows representing genes and columns representing
different timepoints (NAs added when lags are needed)}

\item{time}{a lagged matrix with rows representing each gene's timepoint and columns representing the number of timepoints, NA is introduced when it is lagged}

\item{C}{a numeric value of C used in computing weighted correlation}
}
\value{
a simmilarity matrix with values between -1 and 1
(1 highly correlated, 0 no correlation)
}
\description{
This function computes the weighted correlation with a penalty for lags.
It should only be used after the fixed lags have already been applied to the dataset and timepoints using the functions
prep.data() and best.lag().
}
\examples{
## This function computes the correlation after the lags (or shifts) have
## been computed.  In this example, the lags argument is randomly sampled
## for the sake of illustrating how prep.data() applies the lags and
## prepares a transformed dataset for comp.corr().
lagged <- prep.data(array(rnorm(30), c(3, 10)), timepoints = seq(0, 45, 5),
          lags = sample(c(0, 1, -1, 2, -2), size = 3))
comp.corr(data = lagged$data, time = lagged$time, C = 10)

## This example shows how comp.corr is used in practice with real data.
## The best.lag() function is called first to pre-compute the lags, which
## are passed to prep.data().
randdata <- array(rnorm(120), c(10, 12))
bl <- best.lag(data = randdata, timepoints = 1:12, C = 5)
lag.data <- prep.data(randdata, timepoints = 1:12, lags = bl)
comp.corr(lag.data$data, time = lag.data$time, C = 5)


}
\author{
Thevaa Chandereng, Anthony Gitter
}
