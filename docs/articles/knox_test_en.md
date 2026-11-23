# Space-time interaction: Knox test and transmission chains

Dengue is transmitted by *Aedes aegypti*, which flies short distances,
and the virus takes days (the extrinsic and intrinsic incubation
periods) to pass from one person to the next. So, when there is local
transmission, cases are **closer in space and time** than expected if
space and time were independent. The **Knox test** (Knox 1964) counts
the pairs of cases that are close in both space (less than `ds` metres)
and time (less than `dt` days) and compares that number with its
distribution when the dates are permuted at random (Monte Carlo). Each
close pair is a **space-time link**, and the linked cases form the
**transmission chains**.

The example uses `sim_dengue`, **simulated** geocoded cases of the
metropolitan area of Guadalajara; with the real geocoded dataset the
steps are the same.

``` r
library(denhotspots)
library(ggplot2)
```

## 1. The cases of a season

[`knox_test()`](https://fdzul.github.io/denhotspots/reference/knox_test.md)
needs a table with the coordinates (`x`, `y`) and the date of onset of
symptoms (`onset`):

``` r
x <- subset(sim_dengue, ANO == 2024)
x <- data.frame(x = x$long, y = x$lat, onset = x$FEC_INI_SIGNOS_SINT)
nrow(x)
#> [1] 1680
```

## 2. Knox test

The windows are 400 m (the flight range of *Ae. aegypti* plus the
movement of people around the house) and 20 days (the time between two
cases of the same chain); the coordinates are projected
(`crs = "+proj=eqc"`, metres) and the test uses 1,000 permutations.

``` r
res <- knox_test(x = x, crs = "+proj=eqc", dt = 20, ds = 400, sym = 1000,
                 sp_link = FALSE, planar_coord = FALSE)
```

``` r
res$knox
#> $knox
#> [1] 3875
#> 
#> $p_value
#> [1] 0.000999001
#> 
#> $RR
#> [1] 1.148744
#> 
#> $expected
#> [1] 3372.004
#> 
#> $RR_expected
#> [1] 1.149168
#> 
#> $p_poisson
#> [1] 1.318799e-17
#> 
#> $pairs
#> [1] 1410360
#> 
#> $pairs_space
#> [1] 14510
#> 
#> $pairs_time
#> [1] 327756
```

- `knox`: number of pairs of cases close in space and time (space-time
  links);
- `p_value`: Monte Carlo p-value, the proportion of the permutations of
  the onsets (plus the observed one) with at least as many pairs;
- `RR`: observed pairs divided by the mean of the permutations;
- `expected`: expected pairs if space and time were independent,
  `pairs_space * pairs_time / pairs` (Knox 1964);
- `RR_expected`: observed pairs divided by `expected`;
- `p_poisson`: Poisson mid-p-value, the one used by Tango (2010);
- `pairs`, `pairs_space`, `pairs_time`: all the pairs of cases, and
  those close in space and in time.

A small p-value (and `RR` \> 1) means space-time interaction: local
transmission. The article [Knox test: validation with the data of
Tango](https://fdzul.github.io/denhotspots/articles/knox_tango_en.md)
checks these numbers against a published example.

## 3. Map of the transmission chains

`res$space_time_link` has one line per link (EPSG:4326) and `res$x` the
cases (in projected coordinates). The static map shows the links and the
linked cases coloured by epidemiological week; the grey points are cases
without a link.

``` r
cases <- sf::st_as_sf(res$x, coords = c("x", "y"), crs = "+proj=eqc")
cases <- sf::st_transform(cases, 4326)
cases$week <- cut(lubridate::epiweek(cases$onset), c(0, 25, 30, 35, 40, 45, 53),
                  labels = c("1-25", "26-30", "31-35", "36-40", "41-45", "46-53"))
linked <- lengths(sf::st_intersects(cases, sf::st_buffer(res$space_time_link, 1))) > 0

loc <- rgeomex::extract_locality(cve_edo = "14",
                                 locality = c("Guadalajara", "Zapopan", "Tlaquepaque", "Tonalá"))
pal <- c("#2166AC", "#1B9E77", "#A6D854", "#F2B134", "#E6550D", "#B2182B")

ggplot() +
    geom_sf(data = loc, fill = "grey93", colour = NA) +
    geom_sf(data = cases[!linked, ], colour = "grey55", size = 0.3) +
    geom_sf(data = res$space_time_link, colour = "grey40", linewidth = 0.25, alpha = 0.6) +
    geom_sf(data = cases[linked, ], aes(colour = week), size = 0.8) +
    scale_colour_manual(values = pal, name = "Week") +
    theme_void()
```

![Space-time links (400 m, 20 days) of the cases of
2024.](knox-chains-1.png)

Space-time links (400 m, 20 days) of the cases of 2024.

The same map, interactive: click a case to see its date of onset, and
use the layer control to show or hide each layer.

``` r
library(leaflet)

cases <- sf::st_as_sf(res$x, coords = c("x", "y"), crs = "+proj=eqc")
cases <- sf::st_transform(cases, 4326)
cases$week <- cut(lubridate::epiweek(cases$onset), c(0, 25, 30, 35, 40, 45, 53),
                  labels = c("1-25", "26-30", "31-35", "36-40", "41-45", "46-53"))
linked <- lengths(sf::st_intersects(cases, sf::st_buffer(res$space_time_link, 1))) > 0
pal <- colorFactor(c("#2166AC", "#1B9E77", "#A6D854", "#F2B134", "#E6550D", "#B2182B"),
                   cases$week)

leaflet(width = "100%", height = 560) |>
    addProviderTiles("CartoDB.Positron") |>
    addCircleMarkers(data = cases[!linked, ], radius = 2, stroke = FALSE,
                     fillColor = "grey", fillOpacity = 0.6, group = "Cases without link") |>
    addPolylines(data = res$space_time_link, color = "#555555", weight = 1,
                 opacity = 0.6, group = "Space-time links") |>
    addCircleMarkers(data = cases[linked, ], radius = 3.5, stroke = FALSE,
                     fillColor = ~pal(week), fillOpacity = 0.9,
                     popup = ~paste0("Onset: ", onset, "<br>", "Week: ", week),
                     group = "Linked cases") |>
    addLegend(pal = pal, values = cases$week, title = "Epi week") |>
    addLayersControl(overlayGroups = c("Cases without link", "Space-time links", "Linked cases"),
                     options = layersControlOptions(collapsed = FALSE))
```

The linked cases form the chains. With the real data,
[`transmission_chains_map()`](https://fdzul.github.io/denhotspots/reference/transmission_chains_map.md)
makes this map directly from the geocoded dataset:

``` r
transmission_chains_map(geocoded_dataset = sim_dengue,
                        cve_edo = "14", locality = "Guadalajara",
                        dengue_cases = "Probable")
```

## 4. The chains by season

``` r
by_year <- lapply(2019:2024, function(y) {
    d <- subset(sim_dengue, ANO == y)
    d <- data.frame(x = d$long, y = d$lat, onset = d$FEC_INI_SIGNOS_SINT)
    k <- knox_test(x = d, crs = "+proj=eqc", dt = 20, ds = 400, sym = 999,
                   sp_link = FALSE, planar_coord = FALSE)$knox
    data.frame(year = y, cases = nrow(d), links = k$knox,
               expected = round(k$expected, 1), RR = round(k$RR, 2),
               p_value = k$p_value, p_poisson = signif(k$p_poisson, 3))
})
do.call(rbind, by_year)
```

    #>   year cases links expected   RR p_value p_poisson
    #> 1 2019  1413  2370   2035.3 1.17   0.001  2.37e-13
    #> 2 2020   414   318    227.3 1.40   0.001  6.69e-09
    #> 3 2021   259   257    150.1 1.72   0.001  1.17e-15
    #> 4 2022   597   624    469.7 1.33   0.001  5.97e-12
    #> 5 2023  1139  1665   1310.4 1.27   0.001  2.74e-21
    #> 6 2024  1680  3875   3372.0 1.15   0.001  1.32e-17

## References

- Knox EG (1964). The detection of space-time interactions. *Journal of
  the Royal Statistical Society, Series C*, 13(1), 25-30.
- Tango T (2010). *Statistical Methods for Disease Clustering*.
  Springer.
