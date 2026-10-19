# denhotspots 0.2.0

## Breaking changes

* R >= 4.1.0 is now required (the package uses the native pipe `|>`).
* Only documented functions are exported. Internal helpers are no longer
  visible with `denhotspots::`.

## Bug fixes

* All packages used by the code are now declared in `DESCRIPTION`. Before,
  `Imports` was empty and functions failed when a dependency was not
  installed. Core packages are in `Imports`; packages used by a single
  function are in `Suggests` and the function asks to install them.
* `%>%` is now imported from magrittr, so functions that use it work without
  attaching dplyr.
* `gihi()` is exported again and passes the sf object straight to
  `spdep::poly2nb()` (no conversion to sp needed).
* `spatial_lgcp()` no longer depends on the retired packages rgeos, SDraw and
  raster. The lattice and Simpson approaches now use sf and terra and give the
  same counts and integration weights.

## Other changes

* New `Remotes`: rgeomex, deneggs and knox (GitHub). INLA is installed from
  its own repository (`Additional_repositories`).
* First unit tests (testthat).
