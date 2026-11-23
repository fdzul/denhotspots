# Knox test of space-time interaction

This function implement space-time analysis with the knox test

## Usage

``` r
knox_test(x, crs, dt, ds, sym, sp_link, planar_coord)

knox(...)
```

## Arguments

- x:

  is the dataset with the coordinates and onset of symptoms

- crs:

  is the coordinate reference system.

- dt:

  is the temporal windows.

- ds:

  is the spatial windows.

- sym:

  is the MonteCarlo replications number.

- sp_link:

  is the logical value. If sp_link is true the space-time link is built
  with sp, else with sf.

- planar_coord:

  is the logical value.if planar_coord is false, the dataset is
  projected to a planar system.

- ...:

  arguments passed to \`knox_test()\`.

## Value

a list with three elements: knox, st_link, space-time link. - \*knox\*
is a list with three elements:

- knox:

  Knox statistic, which is the number of pairs of points found in a
  given space-time distance.

- p_value:

  p-value calculated from MonteCarlo simulation.

- RR:

  Relative Risk - calculated by observed value (Knox statistics) divided
  by mean of simulated values.

\- \*st_link\* is a origen-destination dataset of class dataframe. This
dataset has four variables:

- Xo:

  is the longitude of the point of origin.

- Yo:

  are the latitud of the point of origin.

- Xd:

  are the longitude of the destination point.

- Yd:

  are the latitud of the destination point.

\- \*space-time link\* is a sf object (LINESTRING) with crs 4326.

## Details

This function implements the space-time analysis with the knox test. x
is a dataframe with three variables: x (longitude), y (latitude) and
onset (onset of symptoms) The coordinates should be projected to a
planar system. The function rknox test use the
(knox)\[https://github.com/thanhleviet/knox\] package of
(github)\[https://github.com/\].

\`knox()\` is the former name of \`knox_test()\`; it still works but it
is deprecated, because it has the same name as the function of the knox
package.

## References

**Knox, E. (1964)**. *The detection of space-time interactions.* Journal
of the Royal Statistical Society. Series C (13(1), 25-30. **Tango, T.
(2010)**. *Statistical methods for disease clustering. Springer.*

## See also

[knox_mc](https://rdrr.io/pkg/knox/man/knox_simulation.html),
[st_link](https://rdrr.io/pkg/knox/man/st_links.html),
[knox](https://rdrr.io/pkg/knox/man/knox.html)

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
