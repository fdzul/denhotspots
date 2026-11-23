# Map of the space-time links of the Knox test

The purpose of this function is to visualize the transmission chains
resulting from the knox test.

## Usage

``` r
space_time_link_map(x, locality, cve_edo, maptype, facetmap = NULL)
```

## Arguments

- x:

  is the result of knox test.

- locality:

  is the locality target.

- cve_edo:

  is the id of state.

- maptype:

  is the map type, the values are staticmap & interactive_map.

- facetmap:

  is a logical value for facet, if true the map is facet, else is not
  facet.

## Value

a ggmap

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
