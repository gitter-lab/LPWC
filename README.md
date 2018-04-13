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
Prior to analyzing your data, the R package needs to be installed.

The easiest way to install LPWC is through CRAN:

``` r
install.packages("LPWC")
```

There are other additional ways to download LPWC.
The first option is most useful if want to download a specific version of LPWC
(which can be found at https://github.com/gitter-lab/LPWC/releases).
``` r 
devtools::install_github("gitter-lab/LPWC@vx.xx.x")
# OR 
library(devtools)
install_version("LPWC", version = "x.x.x", repos = "http://cran.us.r-project.org")
```

The second option is to download through GitHub. 

``` r
library(devtools)
devtools::install_github("gitter-lab/LPWC")
```

After successful installation, the package must be loaded into the working space:

``` r 
library(LPWC)
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

License
------------
LPWC is available under the open source [MIT license](http://opensource.org/licenses/MIT).
