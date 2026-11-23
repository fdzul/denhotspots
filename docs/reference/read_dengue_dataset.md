# Read the dengue dataset of SINAVE

Read the dengue dataset of \[SINAVE\](https://www.sinave.gob.mx/)

## Usage

``` r
read_dengue_dataset(
  path,
  spatial_resolution,
  status_caso,
  des_edo_res = NULL,
  des_mpo_res = NULL
)
```

## Arguments

- path:

  is the path of dengue dataset.

- spatial_resolution:

  is the spatial resolution or the administrative level. The values are
  country, state and municipality.

- status_caso:

  1 probable, 2 confirmado, & 3 descartado.

- des_edo_res:

  is a string for define the state in uppercase.

- des_mpo_res:

  is a string for define the municipality in uppercase.

## Value

a data.table object.

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
