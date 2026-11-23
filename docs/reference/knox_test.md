# Knox test of space-time interaction

Tests whether the cases are closer in space and time than expected if
space and time were independent (Knox 1964), and returns the pairs of
cases close in both (the space-time links that form the transmission
chains).

## Usage

``` r
knox_test(x, crs, dt, ds, sym, sp_link, planar_coord)

knox(...)
```

## Arguments

- x:

  a data frame with the coordinates of the cases (\`x\`, \`y\`) and the
  date of onset of symptoms (\`onset\`, a \`Date\` or a number).

- crs:

  the projected coordinate reference system used to measure the
  distances (for example \`"+proj=eqc"\`, in metres).

- dt:

  the time window: two cases are close in time when their onsets are at
  most \`dt\` apart (days if \`onset\` is a \`Date\`).

- ds:

  the space window: two cases are close in space when they are at most
  \`ds\` apart (units of \`crs\`, or of the coordinates if
  \`planar_coord = TRUE\`).

- sym:

  the number of Monte Carlo permutations.

- sp_link:

  logical. If \`TRUE\` the space-time links are built with sp, else with
  sf.

- planar_coord:

  logical. If \`FALSE\`, \`x\` and \`y\` are longitude and latitude
  (EPSG:4326) and are projected to \`crs\`; if \`TRUE\`, they are
  already planar coordinates.

- ...:

  arguments passed to \`knox_test()\`.

## Value

A list with four elements: - \`knox\`, a list with the test:

- knox:

  Knox statistic: number of pairs of cases close in space and time.

- p_value:

  Monte Carlo p-value, \\(m + 1)/(sym + 1)\\, where \\m\\ is the number
  of permutations of the onsets with at least \`knox\` pairs.

- RR:

  observed pairs divided by the mean of the permutations.

- expected:

  expected pairs, \`pairs_space \* pairs_time / pairs\` (Knox 1964;
  Tango 2010).

- RR_expected:

  observed pairs divided by \`expected\`.

- p_poisson:

  Poisson mid-p-value of \`knox\` with mean \`expected\`, as in Tango
  (2010).

- pairs:

  total number of pairs of cases, \\n(n - 1)/2\\.

- pairs_space:

  pairs of cases close in space.

- pairs_time:

  pairs of cases close in time.

\- \`st_link\`, a data frame with one row per space-time link and the
coordinates of its two cases (\`Xo\`, \`Yo\`, \`Xd\`, \`Yd\`). - \`x\`,
the cases with the planar coordinates. - \`space_time_link\`, an sf
object (LINESTRING, EPSG:4326) with the space-time links.

## Details

Two cases are close in space when their distance is at most \`ds\`, and
close in time when their onsets are at most \`dt\` apart, as in the
tables of Tango (2010). The test gives two p-values: the Monte Carlo
p-value (the onsets are permuted among the cases \`sym\` times), and the
Poisson mid-p-value used by Knox (1964) and Tango (2010), which is an
approximation that can be misleading when the expected number of pairs
is not small (Barton and David 1966).

\`knox()\` is the former name of \`knox_test()\`; it still works but it
is deprecated, because it has the same name as the function of the knox
package.

## References

Knox EG (1964). The detection of space-time interactions. \*Journal of
the Royal Statistical Society, Series C\*, 13(1), 25-30.

Tango T (2010). \*Statistical Methods for Disease Clustering\*.
Springer.

## See also

\[kaposi_uganda\] for the example of Tango (2010), and
\[transmission_chains_map()\].

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>

## Examples

``` r
# Kaposi's sarcoma in Uganda (Tango 2010, Table 7.7): 5 km and same year
k <- knox_test(x = kaposi_uganda, crs = NULL, ds = 5, dt = 0, sym = 999,
               sp_link = FALSE, planar_coord = TRUE)
k$knox
```
