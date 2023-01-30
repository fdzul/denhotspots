#' space-time link map
#'
#' The purpose of this function is to visualize the transmission chains resulting from the knox test.
#'
#' @param x is the result of knox test.
#' @param locality is the locality target.
#' @param cve_edo is the id of state.
#' @param maptype is the map type, the values are staticmap & interactive_map.
#' @param facetmap is a logical value for facet, if true the map is facet, else is not facet.
#'
#' @return a ggmap
#' @export
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @examples
space_time_link_map <- function(x,
                                locality,
                                cve_edo,
                                maptype,
                                facetmap = NULL){
    # Step 1. load the locality ####
    y <- rgeomex::extract_ageb(locality = locality,
                               cve_geo = cve_edo)

    # Step 2. load the dengue cases ####
    z <- x$x %>%
        dplyr::mutate(week = lubridate::epiweek(onset)) %>%
        dplyr::mutate(week_factor = ifelse(week <= 10, "1-10",
                                           ifelse(week > 10 & week <= 20, "11-20",
                                                  ifelse(week > 20 & week <= 25, "21-25",
                                                         ifelse(week > 25 & week <= 30, "26-30",
                                                                ifelse(week > 30 & week <= 35, "31-35",
                                                                       ifelse(week > 35 & week <= 40, "36-40",
                                                                              ifelse(week > 40 & week <= 45, "41-45",
                                                                                     ifelse(week > 45 & week <= 53, "46-53",
                                                                                            NA))))))))) %>%
        sf::st_as_sf(coords = c("x", "y"),
                     crs = "+proj=eqc") %>%
        sf::st_transform(crs = 4326)

    # Step 3.1 load the Space-Time link ####
    st_link <- x$space_time_link %>%
        sf::st_set_crs(value = 4326)


    # Step 4. extract the dengue cases ####
    w <- z[st_link,] %>%
        dplyr::mutate(week = lubridate::epiweek(onset)) %>%
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



    # Step 5. count the cases with space-link by ageb ####
    ageb_st_link <- denhotspots::point_to_polygons(y = y$ageb,
                                                   x = w %>% sf::st_drop_geometry(),
                                                   coords = c("x", "y"),
                                                   time = week,
                                                   ids = c("OBJECTID","CVEGEO","CVE_ENT",
                                                           "CVE_MUN","CVE_LOC","CVE_AGEB",
                                                           "Ambito","Shape_Leng","Shape_Area"),
                                                   dis = "DENV",
                                                   crs = 4326) %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("DENV"),
                            names_to = "week",
                            values_to = "n") %>%
        dplyr::filter(n > 0) %>%
        dplyr::mutate(week = stringr::str_sub(week, start = 6, end = -1)) %>%
        dplyr::mutate(week_factor = ifelse(week <= 10, "1-10",
                                           ifelse(week > 10 & week <= 20, "11-20",
                                                  ifelse(week > 20 & week <= 25, "21-25",
                                                         ifelse(week > 25 & week <= 30, "26-30",
                                                                ifelse(week > 30 & week <= 35, "31-35",
                                                                       ifelse(week > 35 & week <= 40, "36-40",
                                                                              ifelse(week > 40 & week <= 45, "41-45",
                                                                                     ifelse(week > 45 & week <= 53, "46-53",
                                                                                            NA))))))))) %>%
        sf::st_set_geometry(value = "geometry")



    # Step 6. add the week  to space-time link  ####
    st_link_week <- sf::st_join(x = st_link,
                                y = w[, c("week_factor")])


    # Step 7. map the Space-Time links ####
    if(maptype == "staticmap"){
        p <- ggplot2::ggplot() +
            ggplot2::geom_sf(data = y$ageb,
                             fill = "gray92",
                             col = "white",
                             lwd = 0.01) +
            ggplot2::theme_void() +
            ggplot2::geom_sf(data = z,
                             col = "gray40",
                             shape = 19,
                             size = .5) +
            ggplot2::geom_sf(data = w,
                             ggplot2::aes(fill = week_factor),
                             col = "white",
                             shape = 21,
                             stroke = .1,
                             size = 2.5) +
            ggplot2::geom_sf(data = st_link_week,
                             ggplot2::aes(col = week_factor),
                             lwd = .5) +
            fishualize::scale_fill_fish_d("Semana",
                                          option = "Scarus_hoefleri",
                                          direction = -1) +
            fishualize::scale_colour_fish_d("Space-Time Links",
                                            option = "Scarus_hoefleri",
                                            direction = 1) +
            ggplot2::theme(legend.key.size = ggplot2::unit(0.5, "cm"))
        if(facetmap == TRUE){
            p + ggplot2::facet_wrap("week_factor",
                                    ncol = 4,
                                    nrow = 4) +
                ggplot2::theme(legend.position="bottom",
                               legend.box = "vertical",
                               legend.direction = "horizontal") +
                ggplot2::guides(col = ggplot2::guide_legend(ncol = 8),
                                fill = ggplot2::guide_legend(ncol = 8))
        } else{
            p + ggspatial::annotation_scale(location = "bl")
        }

    } else if(maptype == "interactive_map"){
        pal <- leaflet::colorFactor(palette = fishualize::fish(n = length(unique(w$week_factor)),
                                                               option = "Scarus_hoefleri",
                                                               direction = -1),
                                    domain = w$week_factor)
        l <- leaflet::leaflet() %>%
            leaflet::addTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
            leaflet::addPolylines(data = st_link_week,
                                  color = ~pal(week_factor),
                                  weight = 1,
                                  opacity = .8) %>%
            leaflet::addCircleMarkers(data = w,
                                      radius = 5,
                                      fillColor = ~pal(week_factor),
                                      fillOpacity = 1,
                                      stroke = 1,
                                      weight = 2,
                                      color = "white",
                                      opacity = .8) %>%
            leaflet::addLegend(pal = pal,
                               values = w$week_factor,
                               opacity = 1,
                               title = "Semana",
                               position = "topright")
        esri <- grep("^Esri|OpenTopoMap|OpenStreetMap|HERE|CartoDB|NASAGIBS", leaflet::providers, value = TRUE)


        for (provider in esri) {
            l <- l %>% leaflet::addProviderTiles(provider,
                                                 group = provider)
        }

        l %>%
            leaflet::addLayersControl(baseGroups = names(esri),
                                      options = leaflet::layersControlOptions(collapsed = TRUE)) %>%
            leaflet::addMiniMap(tiles = esri[[1]],
                                toggleDisplay = TRUE,
                                position = "bottomleft") %>%
            htmlwidgets::onRender(" function(el, x) {
            var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }")


    }
}
