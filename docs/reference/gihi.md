# Local spatial statistics Getis-Ord Gi\* and Hi by time period

Calculate gi and hi local spatial statistic

## Usage

``` r
gihi(x, gi_hi, id, dis, time, alpha = NULL)
```

## Arguments

- x:

  it is an sf object with the number of events for each spatial unit

- gi_hi:

  Define the local spatial statistic. It includes three options ("gi",
  "hi", "gi_hi", if "gi" calculates the spatial local statistic getis &
  ord, if it is "hi" calculates hi, and if it is "gi_hi" calculates both
  statistic

- id:

  is or are the identifiers of each spatial unit

- dis:

  it is the prefix of the variable name. Example den_2018 or denv_2018,
  dis is den and denv

- time:

  is the unit of time or the time scale of the analyzed time series.
  example in years, times, months, weeks

- alpha:

  is the value of alpha to determine the threshold of value of gi to
  discriminate hotspots from non-hotspots

## Value

a sf object with the ids and the intensity and hotspots for hi, gi or
each.

## References

Getis A, Ord JK. 1992. The analysis of spatial association by the use of
distance statistics.Geographical Analysis, 24(2):189-206. Ord JK, Getis
A. 1995. Local spatial autocorrelation statistics: distributional issues
and an application. Geographical Analysis, 27, 286–306. Ord JK, Getis A.
2012. Local spatial heteroscedasticity (LOSH), The Annals of Regional
Science, 48 (2), 529–539. Bivand RS, Wong DWS. 2018. Comparing
implementations of global and local indicators of spatial association.
TEST, 27(3), 716–748.

## See also

[LOSH.cs](https://r-spatial.github.io/spdep/reference/LOSH.cs.html),
[localG](https://r-spatial.github.io/spdep/reference/localG.html)

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
