# LPWC: Short Time Series Clustering Using Lag Penalized Weighted Correlation

[![Build Status](https://travis-ci.org/gitter-lab/LPC.svg?branch=master)](https://travis-ci.org/gitter-lab/LPC)

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
The first major release of LPWC will be added to Bioconductor.
Until then, the easiest way to install LPC is as follows:
``` r
devtools::install_github("https://github.com/gitter-lab/LPWC")
```

Usage
------------
See the [vignette](http://htmlpreview.github.io/?https://github.com/gitter-lab/LPC/blob/master/vignette/LPC.html) for usage instructions.
