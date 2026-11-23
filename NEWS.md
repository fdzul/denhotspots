# denhotspots (development version)

* `knox_test()` now computes the Knox test itself (with a spatial index, much
  faster) and no longer depends on the knox package. The Monte Carlo
  `p_value` is now `(m + 1) / (sym + 1)` (before it could be 0), and the
  result also gives the expected number of pairs (`expected`, `RR_expected`),
  the Poisson mid-p-value of Tango (2010) (`p_poisson`) and the number of
  pairs (`pairs`, `pairs_space`, `pairs_time`).
* New `kaposi_uganda`: the example of Tango (2010, Table 7.1). `knox_test()`
  reproduces his Table 7.7 (unit tests and a new article in English and
  Spanish).
* `knox()` is now `knox_test()`, so it does not mask `knox::knox()`; `knox()` still works but is deprecated.
* Removed `sf_joint_cov()` (COVID-19 data, outside the scope of the package).
* New `sim_dengue`: simulated geocoded dengue cases of the metropolitan area of Guadalajara.
* README with the dengueverse template (English and Spanish); titles for all the functions.

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
