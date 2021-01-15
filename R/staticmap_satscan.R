#' staticmap_satscan
#'
#' This function generate the map of the output for [SaTScan](https://www.satscan.org/) or [rsatscan](https://cran.r-project.org/web/packages/rsatscan/vignettes/rsatscan.html).
#'
#' @param x is the dengue cases of the  target locality.
#' @param rsatscan is the output of the space-time analysis with [rsatscan](https://cran.r-project.org/web/packages/rsatscan/vignettes/rsatscan.html) package.
#' @param satscan is a logical value for indicating if TRUE for output of [SaTScan](https://www.satscan.org/).
#' @param locality is the target locality.
#' @param cve_edo is the id of state.
#' @param path_shapeclust is the directory of the output of the space-time analysis with [SaTScan](https://www.satscan.org/). is the col.shp file.
#' @param path_gis is the directory of the output of the space-time analysis with [SaTScan](https://www.satscan.org/).is the gis.shp file.
#'
#' @return a ggplot map.
#' @export
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}.
#' @examples
staticmap_satscan <- function(x,
                              rsatscan = NULL, satscan,
                              locality, cve_edo,
                              path_shapeclust = NULL,
                              path_gis = NULL){


    # Step 1. extract the locality and agebs ####

    loc <- rgeomex::extract_ageb(locality = locality,
                                 cve_geo = cve_edo)

    # Step 2. load the dengue cases ####
    x <- x %>% sf::st_as_sf(coords = c("x", "y"),
                            crs = 4326)
    x <- x[loc$locality, ]


    if(satscan == FALSE){

        # Step 3.1 load the gis file ####
        shapeclust <- sf::st_as_sf(rsatscan$shapeclust)


        # Step 3.2 load the gis file ####
        gis <- rsatscan$gis %>%
            dplyr::mutate(x = LOC_LONG,
                          y = LOC_LAT) %>%
            sf::st_as_sf(coords = c("LOC_LONG", "LOC_LAT"),
                         crs = 4326)

    } else {
        # Step 3.1 load the gis file ####
        shapeclust <- sf::st_read(path_shapeclust,  quiet = TRUE)


        # Step 3.2 load the gis file ####
        gis <- sf::st_read(path_gis, quiet = TRUE) %>%
            dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                          y = sf::st_coordinates(geometry)[,2])
    }

    p <-  ggplot2::ggplot()+
        ggplot2::geom_sf(data = loc$ageb,
                         fill = "gray92",
                         col = "white",
                         lwd = 0.01) +
        ggplot2::geom_sf(data = x,
                         col = "gray40",
                         shape = 19,
                         size = .5) +
        ggplot2::geom_sf(data = gis[shapeclust %>%
                                        dplyr::filter(P_VALUE > 0.05),],
                         col = "white",
                         shape = 21,
                         fill = "#0499EAFF",
                         size = 3) +
        ggplot2::geom_sf(data = shapeclust,
                         fill = NA,
                         col = "#0499EAFF",
                         lwd = 0.8) +
        ggplot2::geom_sf(data = shapeclust %>%
                             dplyr::filter(P_VALUE < 0.05),
                         fill = NA,
                         col = "#D2372CFF",
                         lwd = .8)
    if(satscan == FALSE){
        p +
            ggspatial::geom_spatial_path(data = gis[shapeclust %>%
                                                        dplyr::filter(P_VALUE > 0.05),],
                                         ggplot2::aes(x = x,
                                                      y = y,
                                                      group = CLUSTER),
                                         col = "black",
                                         lwd = 0.5,
                                         linetype = 1,
                                         crs = 4326) +
            ggplot2::geom_sf(data = gis[shapeclust %>%
                                            dplyr::filter(P_VALUE < 0.05),],
                             col = "white",
                             shape = 21,
                             fill = "#D2372CFF",
                             size = 3) +
            ggspatial::annotation_scale() +
            cowplot::theme_map()
    } else{
        p +
            ggplot2::geom_sf(data = gis[shapeclust %>%
                                            dplyr::filter(P_VALUE < 0.05),],
                             col = "white",
                             shape = 21,
                             fill = "#D2372CFF",
                             size = 3) +
            ggspatial::annotation_scale() +
            cowplot::theme_map()
    }
}
