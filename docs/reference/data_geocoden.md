# Prepare the addresses of the cases for geocoding

this function creates an address vector and replaces incorrect text.

## Usage

``` r
data_geocoden(infile, data, sinave_new)
```

## Arguments

- infile:

  is the name of file create with
  [subset_den](https://fdzul.github.io/denhotspots/reference/subset_den.md).

- data:

  is a string for define the addreses or data. if data TRUE, is return
  the dataset.

- sinave_new:

  is logical value. If is TRUE the subset ir for 2016-2019 dengue
  dataset, else the subset ir for 2008-2015 dengue dataset.

## Value

a vector.

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
