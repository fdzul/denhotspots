# Subset the dengue dataset by state, municipality and locality

is a function for subset de dengue dataset

## Usage

``` r
subset_den(x = NULL, path = NULL, edo, mun, loc, sinave_new, age_group, name)
```

## Arguments

- x:

  is the 2008-2015 dengue dataset. Is NULL for 2016-2019 dengue dataset.

- path:

  is the path of the 2016-2019 dengue dataset. Is NULL for 2008-2015
  dengue dataset.

- edo:

  is a string for define the state in uppercase.

- mun:

  is a string for define the municipality in uppercase.

- loc:

  is a string for define the locality in uppercase.

- sinave_new:

  is logical value. If is TRUE the subset ir for 2016-2019 dengue
  dataset, else the subset ir for 2008-2015 dengue dataset.

- age_group:

  is a logical value. If is TRUE the subset for age group (\<= 12 & \>=
  65), else all age group.

- name:

  is the name the csv output

## Value

a object names x and a csv file in working dircetory.

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
