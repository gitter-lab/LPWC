# LPC: Short Time Series Clustering Using Lag Penalized Correlation

**Authors**: Thevaa Chandereng and Anthony Gitter


Overview
--------
Lag Penalized Correlation (LPC) is a method for clustering short time series data.
It is designed to identify groups of biological entities (for example, genes or phosphosites) that exhibit the same pattern of activity changes over time.
LPC allows lags to incorporate delayed responses in the biological data.
For example, two genes may have similar expression changes over time, but one initiates those changes 5 minutes after the other.
LPC also supports irregular time intervals between time points collected in biological data.

Installation
------------
The first major release of LPC will be added to CRAN.
Until then, the easiest way to install LPC is as follows:
``` r
devtools::install_github("https://github.com/gitter-lab/LPC")
```

Usage
------------
See the vignette for usage instructions.
