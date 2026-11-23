# Geocode the addresses of the cases

this function geocodes the addresses of the sinave database using
Geocoding API.

## Usage

``` r
geocoden(infile, address)
```

## Arguments

- infile:

  is the name of the location that is being geocoded and is used to be
  saved in a file with extension rds.

- address:

  is vector addresses.

## Value

a file with extension rds.

## Details

The function use the
[geocode](https://rdrr.io/pkg/ggmap/man/geocode.html) of ggmap.

## Note

The code for geocoding was taken from the personal page of Shane Lynn
<https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/> and
adapted to work with the sinave <http://www.sinave.gob.mx/> database.

## See also

[geocode](https://rdrr.io/pkg/ggmap/man/geocode.html)

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
