#' staticmap_knoxs
#'
#' This function generate the space-time link map.
#'
#' @param locality
#' @param cve_geo
#' @param path_data
#' @param path_knoxs
#' @param pal is the palette.
#' @param option is the palette option.
#' @param name is the palette name. the option and name are very similar, some package use name (ej. rcartocolor::carto_pal) and other option (ex. viridis:viridis).
#' @param pal_name is a logical value, if pal_name TRUE is for packages that use name, else the packages that use option.
#' @param breaks is a numeric value. Is the increment of the sequence.
#' @param dir_pal is the direction of the palette, 1 forward, -1 reverse.
#' @param x_leg is the x coordinates of legend.
#' @param y_leg is the x coordinates of legend.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}.
#'
#' @return a ggplot map.
#'
#' @export
#'
#' @examples
staticmap_knoxs <- function(locality, cve_geo, path_data, path_knoxs,
                            pal, option = NULL, name = NULL,
                            pal_name, breaks, dir_pal = NULL,
                            x_leg, y_leg){

    # Step 1. load the locality ####
    y <- rgeomex::extract_ageb(locality = locality,
                               cve_geo = cve_geo)

    # Step 2. load the dengue cases ####

    z <- sf::st_read(path_data)


    # Step 3. load the Space-Time link ####
    x <- sf::st_read(path_knoxs) %>%
        sf::st_set_crs(value = 4326)


    # Step 4. extract the dengue cases ####
    w <- z[x,] %>%
        dplyr::mutate(week = lubridate::epiweek(onset))

    # Step 5. map the space-time links ####
    if(pal_name == TRUE){
        ggplot2::ggplot() +
            ggplot2::geom_sf(data = y$ageb,
                             fill = "gray90",
                             col = "white",
                             lwd = 0.01) +
            ggplot2::theme_void() +
            ggplot2::geom_sf(data = y$locality,
                             fill = NA,
                             col = "gray70",
                             lwd = 0.01) +
            ggplot2::geom_sf(data = z,
                             fill = "gray40",
                             col = "gray90",
                             shape = 21,
                             lwd = 1) +
            ggplot2::geom_sf(data = w,
                             ggplot2::aes(fill = week),
                             col = "white",
                             shape = 21,
                             size = 2) +
            ggplot2::geom_sf(data = x, fill = NA, col = "black",lwd = .5) +
            ggplot2::scale_fill_gradientn(name = "week",
                                          colours = c(pal(n = max(w$week),
                                                          name = name)),
                                          breaks = seq(from = 1,
                                                       to = max(w$week),
                                                       by = breaks),
                                          guide = "legend") +
            ggplot2::theme(legend.position = c(x_leg, y_leg))
    } else{
        ggplot2::ggplot() +
            ggplot2::geom_sf(data = y$ageb,
                             fill = "gray90",
                             col = "white",
                             lwd = 0.01) +
            ggplot2::theme_void() +
            ggplot2::geom_sf(data = y$locality,
                             fill = NA,
                             col = "gray70",
                             lwd = 0.01) +
            ggplot2::geom_sf(data = z,
                             fill = "gray40",
                             col = "gray90",
                             shape = 21,
                             lwd = 1) +
            ggplot2::geom_sf(data = w,
                             ggplot2::aes(fill = week),
                             col = "white",
                             shape = 21,
                             size = 2) +
            ggplot2::geom_sf(data = x, fill = NA, col = "black",lwd = .5) +
            ggplot2::scale_fill_gradientn(name = "week",
                                          colours = c(pal(n = max(w$week),
                                                          option = option,
                                                          direction = dir_pal)),
                                          breaks = seq(from = 1,
                                                       to = max(w$week),
                                                       by = breaks),
                                          guide = "legend") +
            ggplot2::theme(legend.position = c(x_leg, y_leg))
    }



}
