# Spatial log-Gaussian Cox process model with INLA

The spatial_lgcp has the objective of performing the spatial analysis
with the log gaussian cox process model in order to predict the
intensity of cases in urban and metropolitan endemic dengue locations in
Mexico.

## Usage

``` r
spatial_lgcp(
  dataset,
  locality,
  cve_edo,
  longitude,
  latitude,
  k,
  plot,
  resolution,
  aproximation,
  integration,
  approach,
  cell_size = NULL,
  name
)
```

## Arguments

- dataset:

  is the dengue geocoded dataset.

- locality:

  is the locality target.

- cve_edo:

  is the text id of the state..

- longitude:

  is the name of the column of the longitude in the geocoded dataset.

- latitude:

  is the name of the column of the latitude in the geocoded dataset.

- k:

  is the parameter for define the triagulization of delauney in the
  inner and the outer area in the argument max.edge in the
  INLA:inla.mesh.2d.

- plot:

  is a logical value for the plot the mesh.

- resolution:

  is a value for set the resolution of the locality raster. resolution
  0.1 = 11.132 km, 0.009 = 1.00 km, 0.005 = 500 m, 0.0027 = 300 m, &
  0.001 = 100 m.

- aproximation:

  aproximation is the aproximation of the joint posterior of the
  marginals and hyperparameter. The options are "adaptative",
  "gaussian", "simplified.laplace" & "laplace".

- integration:

  integration is the integration strategy. The options are
  "auto","grid", "eb" & "ccd".

- approach:

  is algorithm for spatial Log Gaussian Cox Process. The option are
  "lattice", "inlabru" & "simpson" according to Illian 2012, Bachl et al
  2018 & Simpson et al 2016,respectively.

- cell_size:

  is the sample number per location (area of locality/n)

- name:

  is the name of the palette.

## Value

a list with several object.
