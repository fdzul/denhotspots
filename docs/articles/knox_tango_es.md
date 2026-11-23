# Prueba de Knox: validación con los datos de Tango

Este artículo compara
[`knox_test()`](https://fdzul.github.io/denhotspots/reference/knox_test.md)
con un ejemplo publicado: los 22 pacientes con sarcoma de Kaposi del
distrito West Nile de Uganda (1958-1974) del capítulo 7 de Tango (2010),
*Statistical Methods for Disease Clustering*. Tango aplica la prueba de
Knox con cinco límites de espacio (1 a 5 km) y cinco de tiempo (0 a 4
años) y da, para cada par de límites, el número de pares cercanos en
espacio y tiempo, su valor esperado y el p de Poisson *mid-p* (su Tabla
7.7).

``` r
library(denhotspots)
library(ggplot2)
```

## 1. Los datos

`kaposi_uganda` tiene las coordenadas de las viviendas (km) y el año de
inicio (Tango 2010, Tabla 7.1):

``` r
head(kaposi_uganda)
#>   case     x     y onset
#> 1    1 266.8 334.3  1958
#> 2    2 304.4 379.3  1959
#> 3    3 265.5 315.0  1960
#> 4    4 265.0 314.0  1960
#> 5    5 264.2 323.0  1962
#> 6    6 288.7 265.2  1962
```

``` r
ggplot(kaposi_uganda, aes(x, y, colour = onset)) +
    geom_point(size = 3) +
    scale_colour_viridis_c(name = "Inicio") +
    coord_equal() +
    labs(x = "Este (km)", y = "Norte (km)") +
    theme_minimal()
```

![Viviendas de los 22 pacientes, coloreadas por año de
inicio.](tango-es-map-1.png)

Viviendas de los 22 pacientes, coloreadas por año de inicio.

## 2. Una prueba de Knox

Las coordenadas ya son planas (km), así que `planar_coord = TRUE` y `ds`
va en km; el inicio es el año, así que `dt = 0` significa “el mismo
año”. Con `crs = NULL` la función no construye el mapa de los vínculos.

``` r
k <- knox_test(x = kaposi_uganda, crs = NULL, ds = 2, dt = 0, sym = 9999,
               sp_link = FALSE, planar_coord = TRUE)
k$knox
#> $knox
#> [1] 2
#> 
#> $p_value
#> [1] 0.0281
#> 
#> $RR
#> [1] 6.218284
#> 
#> $expected
#> [1] 0.3246753
#> 
#> $RR_expected
#> [1] 6.16
#> 
#> $p_poisson
#> [1] 0.02352772
#> 
#> $pairs
#> [1] 231
#> 
#> $pairs_space
#> [1] 5
#> 
#> $pairs_time
#> [1] 15
```

De los 231 pares de pacientes, 5 viven a 2 km o menos uno del otro y 15
iniciaron el mismo año; 2 pares son cercanos en ambos. Si espacio y
tiempo fueran independientes se esperarían `5 * 15 / 231 = 0.325` pares.
La función da los dos p: el de Poisson *mid-p* que usa Tango y el de
Monte Carlo (9,999 permutaciones de los años entre los pacientes).

## 3. La Tabla 7.7 de Tango

Las 25 pruebas:

``` r
grid <- expand.grid(ds = 1:5, dt = 0:4)
tests <- lapply(seq_len(nrow(grid)), function(i) {
    knox_test(x = kaposi_uganda, crs = NULL, ds = grid$ds[i], dt = grid$dt[i],
              sym = 9999, sp_link = FALSE, planar_coord = TRUE)$knox
})
grid$pairs <- sapply(tests, `[[`, "knox")
grid$expected <- sapply(tests, `[[`, "expected")
grid$p_poisson <- sapply(tests, `[[`, "p_poisson")
grid$p_value <- sapply(tests, `[[`, "p_value")

as_table <- function(v, digits) {
    m <- matrix(round(grid[[v]], digits), nrow = 5,
                dimnames = list(paste0("<= ", 1:5, " km"),
                                c("0", paste0("<= ", 1:4))))
    knitr::kable(m)
}
```

Pares cercanos en espacio y tiempo (filas: límite de espacio; columnas:
límite de tiempo en años):

|          |   0 | \<= 1 | \<= 2 | \<= 3 | \<= 4 |
|:---------|----:|------:|------:|------:|------:|
| \<= 1 km |   1 |     1 |     1 |     1 |     1 |
| \<= 2 km |   2 |     2 |     2 |     2 |     2 |
| \<= 3 km |   2 |     2 |     2 |     2 |     2 |
| \<= 4 km |   2 |     2 |     2 |     4 |     7 |
| \<= 5 km |   2 |     2 |     2 |     4 |     8 |

Pares esperados:

|          |     0 | \<= 1 | \<= 2 | \<= 3 | \<= 4 |
|:---------|------:|------:|------:|------:|------:|
| \<= 1 km | 0.260 | 0.537 | 1.022 | 1.264 | 1.662 |
| \<= 2 km | 0.325 | 0.671 | 1.277 | 1.580 | 2.078 |
| \<= 3 km | 0.325 | 0.671 | 1.277 | 1.580 | 2.078 |
| \<= 4 km | 0.714 | 1.476 | 2.810 | 3.476 | 4.571 |
| \<= 5 km | 0.844 | 1.745 | 3.320 | 4.108 | 5.403 |

p de Poisson *mid-p*:

|          |     0 | \<= 1 | \<= 2 | \<= 3 | \<= 4 |
|:---------|------:|------:|------:|------:|------:|
| \<= 1 km | 0.129 | 0.258 | 0.456 | 0.539 | 0.653 |
| \<= 2 km | 0.024 | 0.088 | 0.251 | 0.340 | 0.480 |
| \<= 3 km | 0.024 | 0.088 | 0.251 | 0.340 | 0.480 |
| \<= 4 km | 0.098 | 0.310 | 0.652 | 0.364 | 0.135 |
| \<= 5 km | 0.131 | 0.388 | 0.744 | 0.490 | 0.138 |

p de Monte Carlo (9,999 permutaciones):

|          |     0 | \<= 1 | \<= 2 | \<= 3 | \<= 4 |
|:---------|------:|------:|------:|------:|------:|
| \<= 1 km | 0.233 | 0.440 | 0.694 | 0.783 | 0.886 |
| \<= 2 km | 0.028 | 0.122 | 0.344 | 0.477 | 0.681 |
| \<= 3 km | 0.028 | 0.122 | 0.344 | 0.477 | 0.681 |
| \<= 4 km | 0.133 | 0.436 | 0.847 | 0.455 | 0.115 |
| \<= 5 km | 0.180 | 0.545 | 0.922 | 0.624 | 0.118 |

## 4. Comparación con Tango

- **Pares y valores esperados**: las 25 celdas son idénticas a la Tabla
  7.7 (2.810 y 2.809 solo difieren en el redondeo). Aunque el texto de
  Tango define “cercano” con `<`, su tabla usa `<=` (los pares cercanos
  en tiempo son 15, 31, 59, 73 y 96 para 0, `<= 1`, …, `<= 4` años),
  igual que
  [`knox_test()`](https://fdzul.github.io/denhotspots/reference/knox_test.md).
- **p de Poisson *mid-p***: 22 de las 25 celdas coinciden al tercer
  decimal. En la fila de 1 km, la Tabla 7.7 repite para 2, 3 y 4 años
  los valores de la fila de 2 km (0.251, 0.259, 0.645); con 1 par y los
  esperados de su propia tabla (1.022, 1.264, 1.662), los *mid-p* son
  0.456, 0.539 y 0.653. En la celda de 5 km y 0 años el libro da 0.138 y
  el esperado 0.844 da 0.131.
- **Monte Carlo vs Poisson**: los p de Monte Carlo son mayores que los
  *mid-p* (para 2 km y el mismo año, 0.028 vs 0.024). La aproximación de
  Poisson trata a los pares como independientes, y no lo son, así que
  puede engañar (Barton y David 1966); la referencia es el p de Monte
  Carlo, sobre todo con muchos casos, como en dengue.

El p más pequeño es el de 2-3 km y el mismo año (2 pares, 0.325
esperados, *mid-p* = 0.024): los pacientes podrían estar agrupados en
espacio y tiempo, pero después de 25 pruebas la evidencia es débil; la
prueba Max de Baker (1996), que ajusta por los múltiples límites, da p =
0.106 (Tango 2010).

Las pruebas unitarias del paquete (`tests/testthat/test-knox.R`) revisan
estos números cada vez que se construye el paquete.

## Referencias

- Knox EG (1964). The detection of space-time interactions. *Journal of
  the Royal Statistical Society, Series C*, 13(1), 25-30.
- Tango T (2010). *Statistical Methods for Disease Clustering*.
  Springer. Capítulo 7; los datos de McHardy et al. (1984) y los
  resultados de Barton y David (1966) y de Baker (1996) se citan de este
  libro.
