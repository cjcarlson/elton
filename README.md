# The elton R package :globe_with_meridians:


<img align="right" src="two of them.jpg" height="285"/>

Authors
----------

Phillip P.A. Staniczenko (pstaniczenko@sesync.org)

Colin J. Carlson (ccarlson@sesync.org)

Dependencies
----------------------

The package depends on igraph, raster, and some additional packages. Note that some dependencies of the required R package "gRain" are no longer hosted on CRAN. However, these dependencies can be obtained from the "bioconductor" package. Code to install those dependencies:

``` r 
source("http://bioconductor.org/biocLite.R") 
biocLite() 
biocLite(pkgs=c("RBGL"))
``` 

Installing the package
----------------------

Install directly from Github:

``` r
knitr::opts_chunk$set(echo = FALSE)

# If you don't have devtools:
# install.packages("devtools")

devtools::install_github("cjcarlson/elton")
```

``` r
# Load the package

library(elton)
```
