# Kaposi's sarcoma in the West Nile district of Uganda, 1958-1974: grid
# coordinates (km) of the homes and year of onset of the 22 patients.
# Source: Tango T (2010), Statistical Methods for Disease Clustering,
# Springer, Table 7.1 (data from McHardy et al. 1984).

kaposi_uganda <- data.frame(
    case = 1:22,
    x = c(266.8, 304.4, 265.5, 265.0, 264.2, 288.7, 290.2, 265.6, 263.7, 271.3, 267.4,
          267.4, 276.5, 260.2, 264.0, 263.8, 300.5, 270.8, 258.7, 282.7, 265.3, 285.3),
    y = c(334.3, 379.3, 315.0, 314.0, 323.0, 265.2, 294.3, 318.2, 344.4, 333.5, 344.4,
          344.4, 344.6, 358.2, 296.8, 344.3, 373.0, 326.1, 344.8, 322.3, 314.9, 261.0),
    onset = c(1958, 1959, 1960, 1960, 1962, 1962, 1964, 1964, 1965, 1966, 1968,
              1968, 1968, 1971, 1972, 1972, 1972, 1973, 1974, 1974, 1974, 1974))

usethis::use_data(kaposi_uganda, overwrite = TRUE)
