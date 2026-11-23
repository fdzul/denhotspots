# Risk of transmission by H3 hexagon

risk h3 identifies epidemiological scenarios based on historical
epidemiological and entomological information.

## Usage

``` r
risk_h3(betas, hotspots, intensity_perc, locality, cve_edo)
```

## Arguments

- betas:

  It is the dataset of the regression coefficients of the geostatistical
  model with INLA-SPDE. The betas are calculated with the deneggs
  package.

- hotspots:

  It is the database of the results of the hotspots analysis with the
  local statistician Getis&Ord. Hotspots are calculated with the
  denhotspots package.

- intensity_perc:

  It is the percentage of intensity of egg hotspots.

- locality:

  is the locality name.

- cve_edo:

  is the id of state.

## Value

a sf object.

## See also

[eggs_hotspots](https://fdzul.github.io/deneggs/reference/eggs_hotspots.html),
[spde_pred_map](https://fdzul.github.io/deneggs/reference/spde_pred_map.html),
[eggs_hotspots_week](https://fdzul.github.io/deneggs/reference/eggs_hotspots_week.html)
& @seealso [inla](https://rdrr.io/pkg/INLA/man/inla.html)

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
