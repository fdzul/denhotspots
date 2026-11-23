# Static map of the intensity of cases

This function generate the intensity map of hotspots.

## Usage

``` r
staticmap_intensity(
  x,
  pal,
  option = NULL,
  name = NULL,
  pal_name,
  breaks,
  dir_pal = NULL,
  x_leg,
  y_leg,
  ageb
)
```

## Arguments

- x:

  is the hotspots dataset.

- pal:

  is the palette.

- option:

  is the palette option.

- name:

  is the palette name. the option and name are very similar, some
  package use name (ej. rcartocolor::carto_pal) and other option (ex.
  viridis:viridis)

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

- ageb:

  is a logical value, if ageb is TRUE plot the intensity map of ageb,
  else plot the intensity map by other spatial unit.

## Value

a ggplot map.

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>.

## Examples

``` r
1+1
```
