# LPWC: Lag Penalized Weighted Correlation for Time Series Clustering

[![Build Status](https://travis-ci.org/gitter-lab/LPWC.svg?branch=master)](https://travis-ci.org/gitter-lab/LPWC)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/LPWC)](https://cran.r-project.org/package=LPWC)
[![Download badge](https://cranlogs.r-pkg.org/badges/LPWC)](https://cran.r-project.org/package=LPWC)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![codecov](https://codecov.io/gh/gitter-lab/LPWC/branch/master/graph/badge.svg)](https://codecov.io/gh/gitter-lab/LPWC)
[![Build status](https://ci.appveyor.com/api/projects/status/851q74xh2ue87tid?svg=true)](https://ci.appveyor.com/project/gitter-lab/lpwc)
[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/gitter-lab/LPWC-examples/master?urlpath=rstudio)

**Authors**: Thevaa Chandereng and Anthony Gitter


Overview
--------
Lag Penalized Weighted Correlation (LPWC) is a method for clustering short time series data.
It is designed to identify groups of biological entities (for example, genes or phosphosites) that exhibit the same pattern of activity changes over time.
LPWC allows lags to incorporate delayed responses in the biological data.
For example, two genes may have similar expression changes over time, but one initiates those changes 5 minutes after the other.
LPWC also supports irregular time intervals between time points collected in biological data.
The LPWC website is available [here](https://gitter-lab.github.io/LPWC/). 

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
devtools::install_version("LPWC", version = "x.x.x", repos = "http://cran.us.r-project.org")
```

The second option is to download through GitHub. 

``` r
devtools::install_github("gitter-lab/LPWC")
```

After successful installation, the package must be loaded into the working space:

``` r 
library(LPWC)
```

Usage
------------
See the [vignette](https://gitter-lab.github.io/LPWC/articles/LPWC.html) for usage instructions.


Examples
------------
The LPWC example repository is available [here](https://github.com/gitter-lab/LPWC-examples).
All the example code can be executed in [binder](https://mybinder.org/v2/gh/gitter-lab/LPWC-examples/master?urlpath=rstudio). 

Reference
------------
If you use LPWC, please cite

[doi.org/10.1186/s12859-019-3324-1](Chandereng, T., Gitter, A. Lag penalized weighted correlation for time series clustering. *BMC Bioinformatics* 21, 21 (2020). https://doi.org/10.1186/s12859-019-3324-1)


License
------------
LPWC is available under the open source [MIT license](http://opensource.org/licenses/MIT).
