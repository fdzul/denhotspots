
<!-- README.md is generated from README.Rmd. Please edit that file -->

# denhotspots

<!-- badges: start -->
<!-- badges: end -->

***denhotspots*** is package development by department of prevention and
control of vector-borne diseases of the [Secretary of Health of
Veracruz](https://www.ssaver.gob.mx/) with colaboration of the national
dengue prevention and control program of the
[CENAPRECE](https://www.gob.mx/salud/cenaprece) and entomology
laboratory of
[INDRE](https://www.gob.mx/salud/acciones-y-programas/instituto-de-diagnostico-y-referencia-epidemiologicos-indre)\*

The goal of ***denhotspots*** is to

## Installation

You can install the development version of denhotspots from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("fdzul/denhotspots")
```

or

``` r
# install.packages("remotes")
remotes::install_github("fdzul/denhotspots")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(denhotspots)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date. `devtools::build_readme()` is handy for this. You could also
use GitHub Actions to re-render `README.Rmd` every time you push. An
example workflow can be found here:
<https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub and CRAN.

## Authors

-   **Felipe Antonio Dzul Manzanilla** -**<https://github.com/fdzul>** -
    Packages developed in github:

    1)  [boldenr](https://github.com/fdzul/boldenr).
    2)  [dendata](https://github.com/fdzul/dendata).
    3)  [deneggs](https://github.com/fdzul/deneggs).
    4)  [rgeomex](https://github.com/fdzul/rgeomex).
    5)  [covid19mx](https://github.com/fdzul/covid19mx).

-   **Fabian Correa Morales** -

See also the list of
[contributors](https://github.com/fdzul/deneggs/contributors) who
participated in this project.

## License

This project is licensed under the MIT License - see the
[LICENSE.md](LICENSE.md) file for details

## Inspiration

The package was inspired by the need to contribute to generate health
diagnoses at the state, municipality and local level.

## Getting help

If you encounter a clear bug, please file a minimal reproducible example
on [github](https://github.com/fdzul/denhotspots/issues). For questions
and other discussion, please feel free to contact me
(<felipe.dzul.m@gmail.com>)

------------------------------------------------------------------------

Please note that this project is released with a [Contributor Code of
Conduct](https://dplyr.tidyverse.org/CODE_OF_CONDUCT). By participating
in this project you agree to abide by its terms.
