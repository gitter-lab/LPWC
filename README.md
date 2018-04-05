# LPWC: Lag Penalized Weighted Correlation for Time Series Clustering

[![Build Status](https://travis-ci.org/gitter-lab/LPWC.svg?branch=master)](https://travis-ci.org/gitter-lab/LPWC)

**Authors**: Thevaa Chandereng and Anthony Gitter


Overview
--------
Lag Penalized Weighted Correlation (LPWC) is a method for clustering short time series data.
It is designed to identify groups of biological entities (for example, genes or phosphosites) that exhibit the same pattern of activity changes over time.
LPWC allows lags to incorporate delayed responses in the biological data.
For example, two genes may have similar expression changes over time, but one initiates those changes 5 minutes after the other.
LPWC also supports irregular time intervals between time points collected in biological data.

Installation
------------
The first major release of LPWC will be added to CRAN.
Until then, the easiest way to install LPWC is as follows:
``` r
devtools::install_github("gitter-lab/LPWC")
```

Usage
------------
See the [vignette](vignette/LPWC.Rmd) for usage instructions.

Reference
------------
If you use LPWC, please cite

Lag Penalized Weighted Correlation for Time Series Clustering.
Thevaa Chandereng, Anthony Gitter.
*bioRxiv* 2018. [doi:10.1101/292615](https://doi.org/10.1101/292615)
