# Package index

## Data preparation

Read the dengue dataset, geocode the cases and count them by spatial
unit.

- [`read_dengue_dataset()`](https://fdzul.github.io/denhotspots/reference/read_dengue_dataset.md)
  : Read the dengue dataset of SINAVE
- [`read_den_dataset()`](https://fdzul.github.io/denhotspots/reference/read_den_dataset.md)
  : Read the dengue dataset for geocoding
- [`subset_den()`](https://fdzul.github.io/denhotspots/reference/subset_den.md)
  : Subset the dengue dataset by state, municipality and locality
- [`data_geocoden()`](https://fdzul.github.io/denhotspots/reference/data_geocoden.md)
  : Prepare the addresses of the cases for geocoding
- [`geocoden()`](https://fdzul.github.io/denhotspots/reference/geocoden.md)
  : Geocode the addresses of the cases
- [`save_geocoden()`](https://fdzul.github.io/denhotspots/reference/save_geocoden.md)
  : Save the geocoded dataset
- [`point_to_polygons()`](https://fdzul.github.io/denhotspots/reference/point_to_polygons.md)
  : Count the geocoded cases by spatial unit and time period
- [`cases_by_agem()`](https://fdzul.github.io/denhotspots/reference/cases_by_agem.md)
  : Count the cases of a vector-borne disease by municipality

## Hotspots

Local spatial statistics Getis-Ord Gi\* and Hi, and persistence of the
hotspots.

- [`gihi()`](https://fdzul.github.io/denhotspots/reference/gihi.md) :
  Local spatial statistics Getis-Ord Gi\* and Hi by time period
- [`power_law_plot()`](https://fdzul.github.io/denhotspots/reference/power_law_plot.md)
  : Plot of the power law of the hotspots
- [`power_laws`](https://fdzul.github.io/denhotspots/reference/power_laws.md)
  : Theoretical power-law distributions
- [`bivariate_map()`](https://fdzul.github.io/denhotspots/reference/bivariate_map.md)
  : Bivariate choropleth map of two variables

## Space-time analysis

Knox test, space-time links, transmission chains and SaTScan.

- [`knox()`](https://fdzul.github.io/denhotspots/reference/knox.md) :
  Knox test of space-time interaction
- [`space_time_link_map()`](https://fdzul.github.io/denhotspots/reference/space_time_link_map.md)
  : Map of the space-time links of the Knox test
- [`staticmap_knoxs()`](https://fdzul.github.io/denhotspots/reference/staticmap_knoxs.md)
  : Static map of the space-time links of the Knox test
- [`transmission_chains_map()`](https://fdzul.github.io/denhotspots/reference/transmission_chains_map.md)
  : Map of the transmission chains
- [`satscanR()`](https://fdzul.github.io/denhotspots/reference/satscanR.md)
  : Run the SaTScan space-time scan statistic
- [`staticmap_satscan()`](https://fdzul.github.io/denhotspots/reference/staticmap_satscan.md)
  : Static map of the SaTScan clusters

## Intensity of cases

Spatial log-Gaussian Cox process models with INLA.

- [`spatial_lgcp()`](https://fdzul.github.io/denhotspots/reference/spatial_lgcp.md)
  : Spatial log-Gaussian Cox process model with INLA
- [`intensity_plot()`](https://fdzul.github.io/denhotspots/reference/intensity_plot.md)
  : Map of the intensity of cases
- [`staticmap_intensity()`](https://fdzul.github.io/denhotspots/reference/staticmap_intensity.md)
  : Static map of the intensity of cases

## Risk of transmission

Hotspots of cases and of eggs (deneggs) combined by AGEB or H3 hexagon.

- [`risk_ageb()`](https://fdzul.github.io/denhotspots/reference/risk_ageb.md)
  : Risk of transmission by AGEB
- [`risk_h3()`](https://fdzul.github.io/denhotspots/reference/risk_h3.md)
  : Risk of transmission by H3 hexagon
- [`map_risk()`](https://fdzul.github.io/denhotspots/reference/map_risk.md)
  : Map of the risk of transmission

## Other

- [`sf_joint_cov()`](https://fdzul.github.io/denhotspots/reference/sf_joint_cov.md)
  : Join the COVID-19 cases with the municipal polygons
