# Static map of the space-time links of the Knox test

This function generate the space-time link map.

## Usage

``` r
staticmap_knoxs(
  locality,
  cve_geo,
  path_data,
  path_knoxs,
  pal,
  option = NULL,
  name = NULL,
  pal_name,
  breaks,
  dir_pal = NULL,
  x_leg,
  y_leg
)
```

## Arguments

- locality:

  is the locality target.

- cve_geo:

  is the id of state.

- path_data:

  is the directory of geocoded dengue cases dataset.

- path_knoxs:

  is the output of the space-time analysis (Knoxs method) with
  ClusterSeer.

- pal:

  is the palette.

- option:

  is the palette option.

- name:

  is the palette name. the option and name are very similar, some
  package use name (ej. rcartocolor::carto_pal) and other option (ex.
  viridis:viridis).

- pal_name:

  is a logical value, if pal_name TRUE is for packages that use name,
  else the packages that use option.

- breaks:

  is a numeric value. Is the increment of the sequence.

- dir_pal:

  is the direction of the palette, 1 forward, -1 reverse.

- x_leg:

  is the x coordinates of legend.

- y_leg:

  is the x coordinates of legend.

## Value

a ggplot map.

## Details

The staticmap_knox implement the space-time link map with Knoxs method
in \[ClusterSeer\](https://www.biomedware.com/software/clusterseer/).

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>.

## Examples

``` r
1+1
```
