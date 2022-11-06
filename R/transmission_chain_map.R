#' transmission_chain_map
#'
#' the function generate the space-time links map with mapview package.
#'
#' @param geocoded_dataset is the dengue geocoded dataset.
#' @param cve_edo is the id of state.
#' @param locality is the target locality
#' @param dengue_cases is string for define the positive of suspected dengue cases
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return a mapview
#' @export
#'
#' @examples 1+1
transmission_chains_map <-function(geocoded_dataset,
                                   cve_edo,
                                   locality,
                                   dengue_cases){


    # Step 1. create the column onset and conver to sf object ####
    geocoded_dataset <- geocoded_dataset |>
        dplyr::mutate(onset = FEC_INI_SIGNOS_SINT) |>
        dplyr::mutate(x = long,
                      y = lat) |>
        sf::st_as_sf(coords = c("long", "lat"),
                     crs = 4326) |>
        dplyr::select(DES_MPO_RES,VEC_ID, onset, SEM, ESTATUS_CASO, x, y,
                      DENGUE_SER_TRIPLEX, DES_DIAG_FINAL, IDE_EDA_ANO) |>
        dplyr::mutate(ESTATUS_CASO = ifelse(ESTATUS_CASO == 2,
                                            "Confirmado",
                                            "Probable"))

    # Step 2. define the state or locality, probable of confirmed #####
    if(is.null(locality) == TRUE){
        if(dengue_cases == "Probable"){
            x <- geocoded_dataset |>
                dplyr::select(onset, VEC_ID, x, y, IDE_EDA_ANO, ESTATUS_CASO ) |>
                dplyr::filter(ESTATUS_CASO == "Probable") |>
                sf::st_drop_geometry()

        } else if(dengue_cases == "Confirmado"){
            x <- geocoded_dataset |>
                dplyr::select(onset, VEC_ID, x, y, IDE_EDA_ANO, ESTATUS_CASO ) |>
                dplyr::filter(ESTATUS_CASO == "Confirmado") |>
                sf::st_drop_geometry()
        } else{

        }
    } else{
        extract_locality <- function(cve_edo, locality){
            rgeomex::loc_inegi19_mx |>
                dplyr::filter(CVE_ENT %in% c(cve_edo)) |>
                dplyr::filter(NOMGEO %in% c(rgeomex::find_most_similar_string(locality, unique(NOMGEO)))) |>
                sf::st_make_valid()
        }
        loc <- extract_locality(cve_edo = cve_edo ,
                                locality = locality)

        geocoded_dataset <- geocoded_dataset[loc, ]

        if(dengue_cases == "Probable"){
            x <- geocoded_dataset |>
                dplyr::select(onset, VEC_ID, x, y, IDE_EDA_ANO, ESTATUS_CASO ) |>
                dplyr::filter(ESTATUS_CASO == "Probable") |>
                sf::st_drop_geometry()

        } else if(dengue_cases == "Confirmado"){
            x <- geocoded_dataset |>
                dplyr::select(onset, VEC_ID, x, y, IDE_EDA_ANO, ESTATUS_CASO ) |>
                dplyr::filter(ESTATUS_CASO == "Confirmado") |>
                sf::st_drop_geometry()

        } else{

        }

    }

    # Step 3. apply the knox test
    knox_res <- denhotspots::knox(x = x,
                                  crs = "+proj=eqc",
                                  ds = 400, # distance in meters
                                  dt = 20,  # days 0 to 20 day
                                  sym = 1000,
                                  sp_link = FALSE, # for sf
                                  planar_coord = FALSE)
    # Step 4. calculate the week factor ####
    z <- knox_res$x |>
        dplyr::mutate(week = lubridate::epiweek(onset)) |>
        dplyr::mutate(week_factor = ifelse(week <= 10, "1-10",
                                           ifelse(week > 10 & week <= 20, "11-20",
                                                  ifelse(week > 20 & week <= 25, "21-25",
                                                         ifelse(week > 25 & week <= 30, "26-30",
                                                                ifelse(week > 30 & week <= 35, "31-35",
                                                                       ifelse(week > 35 & week <= 40, "36-40",
                                                                              ifelse(week > 40 & week <= 45, "41-45",
                                                                                     ifelse(week > 45 & week <= 53, "46-53",
                                                                                            NA))))))))) |>
        sf::st_as_sf(coords = c("x", "y"),
                     crs = "+proj=eqc") |>
        sf::st_transform(crs = 4326)

    # Step 5. load the Space-Time link ####
    st_link <- knox_res$space_time_link |>
        sf::st_set_crs(value = 4326)

    # Step 6. extract the dengue cases of space links ####
    w <- z[st_link,] |>
        dplyr::mutate(week = lubridate::epiweek(onset)) |>
        dplyr::mutate(week_factor = ifelse(week <= 10, "1-10",
                                           ifelse(week > 10 & week <= 20, "11-20",
                                                  ifelse(week > 20 & week <= 25, "21-25",
                                                         ifelse(week > 25 & week <= 30, "26-30",
                                                                ifelse(week > 30 & week <= 35, "31-35",
                                                                       ifelse(week > 35 & week <= 40, "36-40",
                                                                              ifelse(week > 40 & week <= 45, "41-45",
                                                                                     ifelse(week > 45 & week <= 53, "46-53",
                                                                                            NA))))))))) %>%
        dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                      y = sf::st_coordinates(geometry)[,2])

    # Step 7. add the week  to space-time link  ####
    st_link_week <- sf::st_join(x = st_link,
                                y = w[, c("week_factor")])

    # Step 8. add the map
    mapview::mapview(geocoded_dataset |>
                         dplyr::filter(ESTATUS_CASO == "Probable"),
                     layer.name = "Probables Acumulados",
                     legend =TRUE,
                     alpha = 0.5,
                     color = "white",
                     col.regions = c("#36C5F0")) +
        mapview::mapview(geocoded_dataset |>
                             dplyr::filter(ESTATUS_CASO == "Confirmado") |>
                             dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                                 "DENGUE GRAVE",
                                                                 "ENGUE NO GRAVE")),
                         layer.name = "Confirmados Acumulados",
                         legend =TRUE,
                         alpha = 0.5,
                         color = "white",
                         col.regions = c("#ECB22E")) +
        mapview::mapview(geocoded_dataset |>
                             dplyr::filter(ESTATUS_CASO == "Probable") |>
                             dplyr::filter(SEM >= lubridate::epiweek(Sys.Date())-3),
                         layer.name = "Transmisión Activa (Probables)",
                         #legend =TRUE,
                         alpha = 0.5,
                         color = "white",
                         col.regions = c("#2EB67D")) +
        mapview::mapview(geocoded_dataset |>
                             dplyr::filter(ESTATUS_CASO == "Confirmado") |>
                             dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE CON SIGNOS DE ALARMA",
                                                                 "DENGUE GRAVE",
                                                                 "ENGUE NO GRAVE")) |>
                             dplyr::filter(SEM >= lubridate::epiweek(Sys.Date())-3),
                         layer.name = "Transmisión Activa (Confirmados)",
                         legend =TRUE,
                         #alpha = 0.5,
                         color = "white",
                         col.regions = c("#E01E5A")) +
        mapview::mapview(w,
                         layer.name = "Cadenas de Transmisión (Casos)",
                         legend =TRUE,
                         #alpha = 0.5,
                         color = "white",
                         col.regions = fishualize::fish(n = length(unique(w$week_factor)),
                                                        option = "Scarus_hoefleri",
                                                        direction = -1),
                         zcol = "week_factor") +
        mapview::mapview(st_link_week,
                         layer.name = "Cadenas de Transmisión (Links)",
                         legend =FALSE,
                         #alpha = 0.5,
                         color = fishualize::fish(n = length(unique(st_link_week$week_factor)),
                                                  option = "Scarus_hoefleri",
                                                  direction = -1),
                         zcol = "week_factor")



}
