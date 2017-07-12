# What is transformeR?

`transformeR` is a R package for climate data transformation, seamlessly integrated with the [`climate4R` bundle](https://github.com/SantanderMetGroup/loadeR). It allows to easily perform many typical climate data manipulations in a flexible way, including:

* Computing and visualizing climatological maps
* Detrending and application of different time filters
* Interpolation/regridding
* PCA/EOF analysis
* Convenient options for subsetting and aggregation along any object dimension
* Conversion to other convenient formats and bridging functions to other R packages
* And more ...

It also contains several buit-in climate datasets and provides parallel computing support and different internal utilities for the R package [`downscaleR`](https://github.com/SantanderMetGroup/downscaleR) for bias correction and statistical downscaling.

***
**NOTE**: The utilities in `transformeR` were formerly part of `downscaleR` (up to v1.3-4). Since `downscaleR` v2.0-0, these are in `transformeR` and `downscaleR` is strictly aimed to statistical downscaling and bias correction.
***

## Installation

You can install using the `devtools` utlity `install_github` directly from a R session:

```r
devtools::install_github("SantanderMetGroup/transformeR")
```

Alternatively, you can download the source files of any version until the current stable one from the [releases tab](https://github.com/SantanderMetGroup/transformeR/releases)



