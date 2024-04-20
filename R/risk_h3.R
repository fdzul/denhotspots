#' risk_h3
#'
#' risk h3 identifies epidemiological scenarios based on historical epidemiological and entomological information.
#'
#' @param betas It is the dataset of the regression coefficients of the geostatistical model with INLA-SPDE. The betas are calculated with the deneggs package.
#' @param hotspots It is the database of the results of the hotspots analysis with the local statistician Getis&Ord. Hotspots are calculated with the denhotspots package.
#' @param intensity_perc It is the percentage of intensity of egg hotspots.
#' @param locality is the locality name.
#' @param cve_edo is the id of state.
#'
#' @return a sf object.
#' @export
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @seealso \link[deneggs]{eggs_hotspots}, \link[deneggs]{spde_pred_map}, \link[deneggs]{eggs_hotspots_week} & @seealso \link[INLA]{inla}
#' @examples
risk_ageb <- function(betas, hotspots, intensity_perc, locality, cve_edo){

    # extract the locality ####
    locality <- rgeomex::extract_locality(locality = locality,
                                          cve_edo = cve_edo)


    # extract the hotspots of locality ####
    hotspots <- hotspots[locality, ]

    # extract the betas of locality
    x <- betas |>
        dplyr::mutate(long = x,
                      lat = y) |>
        sf::st_as_sf(coords = c("long", "lat"),
                     crs = 4326)

    x <- x[locality, ] |>
        sf::st_drop_geometry()

    intensity_function <- function(x){
        x |>
            dplyr::mutate(hotspots_binary = ifelse(hotspots == "Hotspots", 1, 0))|>
            dplyr::select(x, y, week, hotspots_binary)|>
            tidyr::pivot_wider(id_cols = c(x, y),
                               names_from = "week",
                               names_prefix = "week_",
                               values_from = "hotspots_binary") |>
            dplyr::mutate(intensity = rowMeans(dplyr::across(dplyr::starts_with("week_")))) |>
            dplyr::select(x, y, intensity)
    }

    # step 2.2 apply the function
    x <- x |>
        dplyr::group_by(year) |>
        tidyr::nest() |>
        dplyr::mutate(intensity = purrr::map(data,intensity_function))|>
        dplyr::select(-data)|>
        tidyr::unnest(cols = c(intensity))|>
        dplyr::arrange(dplyr::desc(intensity))|>
        as.data.frame() |>
        sf::st_as_sf(coords = c("x", "y"),
                     crs = 4326)

    # 3. riesgo ####
    # Step 3.1 extract the hotspots  and no hotspots of locality
    loc_hotspots <- hotspots|>
        dplyr::filter(hotspots_gi >= 1)

    loc_hotspots_no <- hotspots|>
        dplyr::filter(hotspots_gi == 0)

    # Step 3.2 extract the very high risk
    risk_a_eggs <- sf::st_intersection(x = x |> dplyr::filter(intensity >= intensity_perc/100),
                                       y = loc_hotspots)
    risk_a_agebs <- loc_hotspots[risk_a_eggs, ]|>
        dplyr::mutate(risk = "Muy Alto Riesgo")

    # Steo 3.3 extract the high risk
    #risk_b_agebs <- sf::st_difference(x = loc_hotspots, y = sf::st_union(risk_a_agebs))|>
    #    dplyr::mutate(risk = "Alto Riesgo")

    risk_b_agebs <- loc_hotspots |>
        dplyr::filter(!h3 %in% c(risk_a_agebs$h3)) |>
        dplyr::mutate(risk = "Alto Riesgo")


    # Step 3.4 extract the medium risk
    risk_c_eggs <- sf::st_intersection(x = x |>
                                           dplyr::filter(intensity >= intensity_perc/100),
                                       y = hotspots |>
                                           dplyr::filter(hotspots_gi == 0))

    risk_c_agebs <- loc_hotspots_no[risk_c_eggs,] |>
        dplyr::mutate(risk = "Mediano Riesgo")

    # Step 3.4 extract the low risk
    a <- rbind(risk_a_agebs, risk_b_agebs, risk_c_agebs)
    risk_d_agebs <- hotspots|>
        dplyr::filter(!h3 %in% c(a$h3)) |>
        dplyr::mutate(risk = "Bajo Riesgo")

    # Step 3.5 row binding dataset
    risk <- rbind(a, risk_d_agebs)

    # Step 3.6 add the label risk
    risk$risk <- factor(risk$risk,
                        labels = c("Riesgo Alto"," Riesgo Bajo",
                                   "Riesgo Mediano", "Riesgo Muy Alto")[c(4,1, 3,2)],
                        levels = c("Alto Riesgo","Bajo Riesgo",
                                   "Mediano Riesgo", "Muy Alto Riesgo")[c(4,1, 3,2)])
    risk
}
