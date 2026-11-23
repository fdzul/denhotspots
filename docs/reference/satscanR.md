# Run the SaTScan space-time scan statistic

This function performs space-time analysis with SaTScan.

## Usage

``` r
satscanR(
  cas_file,
  geo_file,
  start_date,
  end_date,
  spatial_window,
  temporal_window,
  ssbatchfilename = "SaTScanBatch",
  path_satscan
)
```

## Arguments

- cas_file:

  is the case file (cas for short).

- geo_file:

  is the geographic or coordinates file (geo for short).

- start_date:

  is the earliest date to be included in the study period. Is calculated
  with min(x_cas\$date).

- end_date:

  is the latest date to be included in the study period. Is calculated
  with max(x_cas\$date).

- spatial_window:

  is the spatial window. The spatial resolution is in km.

- temporal_window:

  is the temporal window. The time resolution is in days.

- ssbatchfilename:

  is the name of the file containing the SaTScan executable.

- path_satscan:

  is the path of directory of SatScan installation.

## Value

a list with eight object (main, col, rr, gis, llr, sci, shapeclust, prm)

## Details

the stascanR implements ths space-time analysis with
\[rsatscan\](https://cran.r-project.org/web/packages/rsatscan/vignettes/rsatscan.html)
package. The rsatscan package only does anything useful if you have
installed \[SaTScan\](https://www.satscan.org/)

## See also

ss.options, satscan

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>.
