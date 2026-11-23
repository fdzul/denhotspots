# Join the COVID-19 cases with the municipal polygons

sf_joint_cov this function apply the dplyr::left_join between sf and
covid data of México

## Usage

``` r
sf_joint_cov(path_dat, path_sf)
```

## Arguments

- path_dat:

  is the directory of the covid data.

- path_sf:

  is the directory of the municipality shapefile of Mexico.

## Value

a sf object with a dengue cases accumulated by municipality.

## Details

The function use the
[left_join](https://dplyr.tidyverse.org/reference/mutate-joins.html) of
dplyr.

## Note

the dataset of SARS-COV2 of Mexico is downloaded homepage of DGE
<https://www.gob.mx/salud/documentos/datos-abiertos-152127?idiom=es> and
adapted to work with the sinave <http://www.sinave.gob.mx/> database.

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
