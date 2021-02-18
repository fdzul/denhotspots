#' point_to_polygon
#'
#' this function counts how many events for spatial unit
#'
#' @param y is the sf areal data.
#' @param x is the point pattern data with the coordinate.
#' @param ids is the unique o uniques identifiers of each spatial unit.
#' @param time is the temporal resolution of datasets.
#' @param coords is the coordinates of geocoded dataset.
#' @param crs  is the coordinate reference system.
#' @param dis is the name of vector-borne diseases.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a sf object.
#' @export
#'
point_to_polygons <- function(x, y, ids, time, coords, crs, dis){
    point_in_polygons <- function(x,y, ids){
        # y is the polygon in sf
        # x is the point pattern data in sf
        y$n <- unlist(purrr::map(sf::st_contains(x = y, y = x), length))
        y <- sf::st_drop_geometry(y)
        y[,c(ids, "n")]

    }
    time <- dplyr::enquo(time)
    z <- x %>%
        sf::st_as_sf(coords = coords,
                     crs = crs) %>%
        dplyr::group_by(!! time) %>%   ## !!
        tidyr::nest() %>%
        dplyr::mutate(count = purrr::map(data, point_in_polygons, y,
                                         ids = c(ids))) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(count) %>%
        tidyr::pivot_wider(names_from = dplyr::as_label(time),  ## ??? how parametrizar
                           values_from = "n")
    w <- dplyr::left_join(x = y[, c(ids)],
                          y = z,
                          by = c(ids))

    names(w)[-c(1:length(ids), ncol(w))] <- paste(dis, names(w)[-c(1:length(ids), ncol(w))], sep = "_")
    w
}


