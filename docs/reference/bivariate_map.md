# Bivariate choropleth map of two variables

generate a bivariate map

## Usage

``` r
bivariate_map(w, x, y, dim, pal, style, size_axis, scale_leg, x_leg, y_leg)
```

## Arguments

- w:

  Is the sf object with the variables to map.

- x:

  Is the variable x in the sf object to be mapped.

- y:

  Is the variable x in the sf object to be mapped.

- dim:

  The dimensions of the palette, either 2 for a two-by-two palette or 3
  for a three-by-three palette

- pal:

  A palette name; one of "Brown", "DkBlue", "DkCyan", "DkViolet", or
  "GrPink". As biscale::bi_legend.

- style:

  A string identifying the style used to calculate breaks. Currently
  supported styles are "quantile", "equal", "fisher", and "jenks"

- size_axis:

  Size of axis labels.

- scale_leg:

  Scales the grob relative to the rectangle defined by x, y, width,
  height. A setting of scale = 1 indicates no scaling.

- x_leg:

  The x location of the legend in the plot.

- y_leg:

  The y location of the legend in the plot.

## Value

a ggplot object.

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
