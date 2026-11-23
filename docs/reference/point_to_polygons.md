# Count the geocoded cases by spatial unit and time period

this function counts how many events for spatial unit

## Usage

``` r
point_to_polygons(x, y, ids, time, coords, crs, dis)
```

## Arguments

- x:

  is the point pattern data with the coordinate.

- y:

  is the sf areal data.

- ids:

  is the unique o uniques identifiers of each spatial unit.

- time:

  is the temporal resolution of datasets.

- coords:

  is the coordinates of geocoded dataset.

- crs:

  is the coordinate reference system.

- dis:

  is the name of vector-borne diseases.

## Value

a sf object.

## Author

Felipe Antonio Dzul Manzanilla <felipe.dzul.m@gmail.com>
