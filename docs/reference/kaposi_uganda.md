# Kaposi's sarcoma in the West Nile district of Uganda

Grid coordinates of the homes and year of onset of the 22 patients with
Kaposi's sarcoma in the West Nile district of Uganda, 1958-1974. It is
the example of Tango (2010, chapter 7) for the tests of space-time
clustering, and it is used to check \[knox_test()\] against his Table
7.7.

## Usage

``` r
kaposi_uganda
```

## Format

A data frame with 22 rows and 4 columns:

- case:

  case number.

- x, y:

  eastings and northings of the home (km).

- onset:

  year of onset.

## Source

Tango T (2010). \*Statistical Methods for Disease Clustering\*.
Springer, Table 7.1 (data from McHardy et al. 1984).

## Examples

``` r
kaposi_uganda
knox_test(x = kaposi_uganda, crs = NULL, ds = 2, dt = 0, sym = 999,
          sp_link = FALSE, planar_coord = TRUE)$knox
```
