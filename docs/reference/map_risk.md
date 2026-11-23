# Map of the risk of transmission

The map_risk function generates the map of the operational scenarios or
epidemiological scenarios based on the hotspots of dengue transmission
and the hotspots of the abundance of dengue vector eggs.

## Usage

``` r
map_risk(risk, staticmap)
```

## Arguments

- risk:

  is the spatial risk dataset.

- staticmap:

  is a logical value, if true the map is static else the map is
  interactive.

## Value

a ggplot o mapview object

## Note

To generate the map with this function, you must first run the risk_ageb
function.

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>

## Examples

``` r
1+1
```
