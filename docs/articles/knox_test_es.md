# Interacción espacio-temporal: prueba de Knox y cadenas de transmisión

El dengue lo transmite *Aedes aegypti*, que vuela distancias cortas, y
el virus tarda días (los periodos de incubación extrínseco e intrínseco)
en pasar de una persona a otra. Por eso, cuando hay transmisión local,
los casos están **más cerca en el espacio y en el tiempo** de lo
esperado si espacio y tiempo fueran independientes. La **prueba de
Knox** (Knox 1964) cuenta los pares de casos cercanos a la vez en el
espacio (a menos de `ds` metros) y en el tiempo (a menos de `dt` días) y
compara ese número con su distribución cuando las fechas se permutan al
azar (Monte Carlo). Cada par cercano es un **vínculo espacio-temporal**,
y los casos vinculados forman las **cadenas de transmisión**.

El ejemplo usa `sim_dengue`, casos geocodificados **simulados** de la
zona metropolitana de Guadalajara; con la base geocodificada real los
pasos son los mismos.

``` r
library(denhotspots)
library(ggplot2)
```

## 1. Los casos de una temporada

[`knox_test()`](https://fdzul.github.io/denhotspots/reference/knox_test.md)
necesita una tabla con las coordenadas (`x`, `y`) y la fecha de inicio
de síntomas (`onset`):

``` r
x <- subset(sim_dengue, ANO == 2024)
x <- data.frame(x = x$long, y = x$lat, onset = x$FEC_INI_SIGNOS_SINT)
nrow(x)
#> [1] 1680
```

## 2. Prueba de Knox

Las ventanas son 400 m (el rango de vuelo de *Ae. aegypti* más el
movimiento de las personas alrededor de la casa) y 20 días (el tiempo
entre dos casos de la misma cadena); las coordenadas se proyectan
(`crs = "+proj=eqc"`, metros) y la prueba usa 1,000 permutaciones.

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

- `knox`: número de pares de casos cercanos en espacio y tiempo
  (vínculos espacio-temporales);
- `p_value`: p de Monte Carlo, la proporción de las permutaciones de las
  fechas de inicio (más la observada) con al menos tantos pares;
- `RR`: pares observados entre la media de las permutaciones;
- `expected`: pares esperados si espacio y tiempo fueran independientes,
  `pairs_space * pairs_time / pairs` (Knox 1964);
- `RR_expected`: pares observados entre `expected`;
- `p_poisson`: p de Poisson *mid-p*, el que usa Tango (2010);
- `pairs`, `pairs_space`, `pairs_time`: todos los pares de casos, y los
  cercanos en espacio y en tiempo.

Un p pequeño (y `RR` \> 1) indica interacción espacio-temporal:
transmisión local. El artículo [Prueba de Knox: validación con los datos
de Tango](https://fdzul.github.io/denhotspots/articles/knox_tango_es.md)
compara estos números con un ejemplo publicado.

## 3. Mapa de las cadenas de transmisión

`res$space_time_link` tiene una línea por vínculo (EPSG:4326) y `res$x`
los casos (en coordenadas proyectadas). El mapa estático muestra los
vínculos y los casos vinculados coloreados por semana epidemiológica;
los puntos grises son casos sin vínculo.

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
    scale_colour_manual(values = pal, name = "Semana") +
    theme_void()
```

![Vínculos espacio-temporales (400 m, 20 días) de los casos de
2024.](knox-es-chains-1.png)

Vínculos espacio-temporales (400 m, 20 días) de los casos de 2024.

El mismo mapa, interactivo: da clic en un caso para ver su fecha de
inicio de síntomas, y usa el control de capas para mostrar u ocultar
cada capa.

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
                     fillColor = "grey", fillOpacity = 0.6, group = "Casos sin vínculo") |>
    addPolylines(data = res$space_time_link, color = "#555555", weight = 1,
                 opacity = 0.6, group = "Vínculos espacio-temporales") |>
    addCircleMarkers(data = cases[linked, ], radius = 3.5, stroke = FALSE,
                     fillColor = ~pal(week), fillOpacity = 0.9,
                     popup = ~paste0("Inicio de síntomas: ", onset, "<br>", "Semana: ", week),
                     group = "Casos vinculados") |>
    addLegend(pal = pal, values = cases$week, title = "Semana epi.") |>
    addLayersControl(overlayGroups = c("Casos sin vínculo", "Vínculos espacio-temporales", "Casos vinculados"),
                     options = layersControlOptions(collapsed = FALSE))
```

Los casos vinculados forman las cadenas. Con los datos reales,
[`transmission_chains_map()`](https://fdzul.github.io/denhotspots/reference/transmission_chains_map.md)
hace este mapa directamente desde la base geocodificada:

``` r
transmission_chains_map(geocoded_dataset = sim_dengue,
                        cve_edo = "14", locality = "Guadalajara",
                        dengue_cases = "Probable")
```

## 4. Las cadenas por temporada

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

En las seis temporadas simuladas hay interacción espacio-temporal (p \<
0.001); el RR es mayor en los años con pocos casos, cuando los casos que
hay están más agrupados en cadenas.

## Referencias

- Knox EG (1964). The detection of space-time interactions. *Journal of
  the Royal Statistical Society, Series C*, 13(1), 25-30.
- Tango T (2010). *Statistical Methods for Disease Clustering*.
  Springer.
