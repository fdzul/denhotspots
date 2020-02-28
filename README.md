# **denhotspots**

[![Build Status](https://travis-ci.org/pages-themes/cayman.svg?branch=master)](https://travis-ci.org/pages-themes/cayman) [![Gem Version](https://badge.fury.io/rb/jekyll-theme-cayman.svg)](https://badge.fury.io/rb/jekyll-theme-cayman)

**denhotspots is a package developed in the department of prevention and control of diseases transmitted by vector of the [Secretary of Health of Veracruz](https://www.ssaver.gob.mx/) and with colaboration of the federal level**


## **overview**

**denhotspots** . 

  - **`gihi()`** calculate gi and hi local spatial statistic.
  - **`bivariate_map()`** generate a bivariate map.
  - **`subset_den()`** is a function for subset de dengue dataset.
  - **`data_geocoden()`** this function creates an address vector and replaces incorrect text.
  - **`geocoden()`** this function geocodes the addresses of the sinave database using Geocoding API.
  - **`save_geocoden()`** This function generates the RData file where it contains the geocoded data and the sinave dataset.
  - **`point_to_polygons()`** this function counts how many events for spatial uni.

## Instalation

``` r
# The easiest way to get dplyr is to install the whole tidyverse:
install.packages("denhotspots")
```

### Development version

To get a bug fix, or use a feature from the development version, you can
install deneggs from GitHub.

``` r
# install.packages("devtools")
devtools::install_github("fdzul/denhotspots")
``` 

## usage example 1.
``` r
library(dendata)
library(magrittr)
library(sf)
y <- denhotspots::gihi(x = den_loc_mex %>% 
                           dplyr::filter(loc == "Acapulco"),
                        gi_hi = "gi_hi",
                        id =  c("CVEGEO", "loc"),
                        dis = "DENV",
                        time = "year",
                        alpha = 0.95)
denhotspots::bivariate_map(w = y,
                           x = hotspots_gi,
                           y = hotspots_hi,
                           pal = "DkBlue",
                           dim = 2,
                           style = "equal",
                           size_axis = 10,
                           x_leg = 0.1, 
                           y_leg = 0.1,
                           scale_leg = 1.2)
``` 

## usage example 2
``` r

``` 

## Authors

* **Felipe Antonio Dzul Manzanilla** -**https://github.com/fdzul** - Packages developed in github:

  1) [denhotspots](https://github.com/fdzul/denhotspots). 
  2) [boldenr](https://github.com/fdzul/boldenr). 
  3) [dendata](https://github.com/fdzul/dendata).

* **Fabian Correa Morales**
* **Gonzalo Vazquez-Prokopec**




See also the list of [contributors](https://github.com/fdzul/denhotspots/contributors) who participated in this project.

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details


## Inspiration

The package was inspired by the need to contribute to making decisions in the dengue prevention and control program, specifically to identify dengue vector hotspots and use the entomological information generated by the program.

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/fdzul/deneggs/issues). For questions
and other discussion, please feel free to contact me (felipe.dzul.m@gmail.com)

-----

Please note that this project is released with a [Contributor Code of
Conduct](https://dplyr.tidyverse.org/CODE_OF_CONDUCT). By participating
in this project you agree to abide by its terms.
