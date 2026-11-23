# Static map of the SaTScan clusters

This function generate the map of the output for
\[SaTScan\](https://www.satscan.org/) or
\[rsatscan\](https://cran.r-project.org/web/packages/rsatscan/vignettes/rsatscan.html).

## Usage

``` r
staticmap_satscan(
  x,
  rsatscan = NULL,
  satscan,
  locality,
  cve_edo,
  path_shapeclust = NULL,
  path_gis = NULL
)
```

## Arguments

- x:

  is the dengue cases of the target locality.

- rsatscan:

  is the output of the space-time analysis with
  \[rsatscan\](https://cran.r-project.org/web/packages/rsatscan/vignettes/rsatscan.html)
  package.

- satscan:

  is a logical value for indicating if TRUE for output of
  \[SaTScan\](https://www.satscan.org/).

- locality:

  is the target locality.

- cve_edo:

  is the id of state.

- path_shapeclust:

  is the directory of the output of the space-time analysis with
  \[SaTScan\](https://www.satscan.org/). is the col.shp file.

- path_gis:

  is the directory of the output of the space-time analysis with
  \[SaTScan\](https://www.satscan.org/).is the gis.shp file.

## Value

a ggplot map.

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>.
