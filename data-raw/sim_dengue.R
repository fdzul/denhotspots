# denhotspots, data-raw/sim_dengue.R
# SIMULATED geocoded dengue cases of the metropolitan area of Guadalajara
# (Guadalajara, Zapopan, Tlaquepaque and Tonala), 2019-2024, with the columns
# of the geocoded SINAVE dataset. They are NOT real cases: they are made to
# show the functions of the package (persistent hotspots, space-time
# clustering, transmission chains) without publishing surveillance data.
#
# How they are made:
#   1. background cases: placed in the 400 m H3 hexagons of Kontur 2023
#      (popmex) with probability proportional to their population;
#   2. persistent hotspots: 6 fixed foci, active (with different weights)
#      every year, so Gi* finds them year after year;
#   3. transmission chains: each "primary" case may produce secondary cases
#      within 0-300 m and 3-18 days, which gives the space-time interaction
#      that the Knox test detects;
#   4. onset dates follow a seasonal curve (peak in epidemiological weeks
#      36-42) and the yearly totals vary as in an endemic city.
#
# Run from the root of the package:  source("data-raw/sim_dengue.R")

set.seed(20261122)
library(sf)

loc <- rgeomex::extract_locality(cve_edo = "14",
                                 locality = c("Guadalajara", "Zapopan", "Tlaquepaque", "Tonal\u00e1"))
loc <- sf::st_transform(sf::st_make_valid(loc), 32613)          # UTM 13N, metres
mpo_names <- c("039" = "GUADALAJARA", "120" = "ZAPOPAN", "098" = "SAN PEDRO TLAQUEPAQUE",
               "101" = "TONALA")

hex <- popmex::hdx_pop2023 |> sf::st_transform(32613)
hex <- hex[sf::st_intersects(hex, sf::st_union(loc), sparse = FALSE)[, 1], ]
hex <- hex[hex$population > 0, ]
hex_xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(hex)))

years <- 2019:2024
n_year <- c(`2019` = 1450, `2020` = 420, `2021` = 260, `2022` = 610, `2023` = 1180, `2024` = 1720)

# 6 persistent foci (inside the city), with a weight per year
foci <- hex_xy[sample(nrow(hex_xy), 6, prob = hex$population^1.5), , drop = FALSE]
foci_w <- matrix(stats::runif(6 * length(years), 0.4, 1.6), nrow = 6)

# seasonal curve of onset (epidemiological week 1-52)
week_p <- stats::dnorm(1:52, mean = 39, sd = 6.5) + 0.002
week_p <- week_p / sum(week_p)

inside <- function(xy) {
    p <- sf::st_as_sf(data.frame(x = xy[, 1], y = xy[, 2]), coords = c("x", "y"), crs = 32613)
    lengths(sf::st_intersects(p, loc)) > 0
}

one_year <- function(yr, n) {
    n_hot <- round(n * 0.35)                      # cases of the persistent foci
    n_chain <- round(n * 0.30)                    # secondary cases of the chains
    n_bg <- n - n_hot - n_chain                   # background, by population
    # background
    k <- sample(nrow(hex_xy), n_bg, replace = TRUE, prob = hex$population)
    bg <- hex_xy[k, , drop = FALSE] + matrix(stats::runif(2 * n_bg, -200, 200), ncol = 2)
    # persistent foci: gaussian around each focus (sd 450 m)
    f <- sample(6, n_hot, replace = TRUE, prob = foci_w[, match(yr, years)])
    hot <- foci[f, , drop = FALSE] + matrix(stats::rnorm(2 * n_hot, 0, 450), ncol = 2)
    xy <- rbind(bg, hot)
    wk <- sample(1:52, nrow(xy), replace = TRUE, prob = week_p)
    onset <- as.Date(paste0(yr, "-01-01")) + (wk - 1) * 7 + sample(0:6, nrow(xy), replace = TRUE)
    # transmission chains: secondary cases near a primary case, 3-18 days later
    p <- sample(nrow(xy), n_chain, replace = TRUE)
    ang <- stats::runif(n_chain, 0, 2 * pi); r <- stats::runif(n_chain, 20, 300)
    xy2 <- xy[p, , drop = FALSE] + cbind(r * cos(ang), r * sin(ang))
    onset2 <- onset[p] + sample(3:18, n_chain, replace = TRUE)
    xy <- rbind(xy, xy2); onset <- c(onset, onset2)
    ok <- inside(xy) & format(onset, "%Y") == as.character(yr)
    data.frame(x = xy[ok, 1], y = xy[ok, 2], onset = onset[ok])
}

cases <- do.call(rbind, lapply(years, function(yr) one_year(yr, n_year[[as.character(yr)]])))
cases <- cases[order(cases$onset), ]
pts <- sf::st_as_sf(cases, coords = c("x", "y"), crs = 32613)
mun <- loc$CVE_MUN[sf::st_nearest_feature(pts, loc)]
ll <- sf::st_coordinates(sf::st_transform(pts, 4326))
n <- nrow(cases)

# serotypes: a different mix every year
sero_mix <- list(`2019` = c(.15, .70, .05, .10), `2020` = c(.20, .55, .05, .20),
                 `2021` = c(.25, .40, .10, .25), `2022` = c(.20, .30, .35, .15),
                 `2023` = c(.10, .25, .55, .10), `2024` = c(.10, .20, .60, .10))
yr <- as.integer(format(cases$onset, "%Y"))
sero <- vapply(yr, function(y) sample(c("DENV-1", "DENV-2", "DENV-3", "DENV-4"), 1,
                                      prob = sero_mix[[as.character(y)]]), character(1))
status <- sample(c(1L, 2L), n, replace = TRUE, prob = c(.62, .38))   # 1 probable, 2 confirmed

sim_dengue <- data.frame(
    VEC_ID = sprintf("SIM%06d", seq_len(n)),
    ANO = yr,
    SEM = as.integer(lubridate::epiweek(cases$onset)),
    FEC_INI_SIGNOS_SINT = cases$onset,
    IDE_EDA_ANO = pmin(pmax(round(stats::rgamma(n, shape = 2.6, scale = 11)), 0), 95),
    IDE_SEX = sample(c(1L, 2L), n, replace = TRUE),
    CVE_EDO_RES = 14L,
    DES_EDO_RES = "JALISCO",
    CVE_MPO_RES = as.integer(mun),
    DES_MPO_RES = unname(mpo_names[mun]),
    ESTATUS_CASO = status,
    DES_DIAG_FINAL = sample(c("DENGUE NO GRAVE", "DENGUE CON SIGNOS DE ALARMA", "DENGUE GRAVE"),
                            n, replace = TRUE, prob = c(.74, .23, .03)),
    DENGUE_SER_TRIPLEX = ifelse(status == 2L & stats::runif(n) < .45, sero, NA_character_),
    long = round(ll[, 1], 6),
    lat = round(ll[, 2], 6),
    stringsAsFactors = FALSE
)
rownames(sim_dengue) <- NULL
message(nrow(sim_dengue), " simulated cases; by year: ",
        paste(names(table(sim_dengue$ANO)), table(sim_dengue$ANO), sep = "=", collapse = ", "))
usethis::use_data(sim_dengue, overwrite = TRUE, compress = "xz")
