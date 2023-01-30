#' knox test
#'
#' @description This function implement space-time analysis with the knox test
#'
#' @param x is the dataset with the coordinates and onset of symptoms
#' @param crs is the coordinate reference system.
#' @param dt is the temporal windows.
#' @param ds is the spatial windows.
#' @param sym is the MonteCarlo replications number.
#' @param sp_link is the logical value. If sp_link is true the space-time link is  built with sp, else with sf.
#' @param planar_coord is the logical value.if planar_coord is false, the dataset is projected to a planar system.
#' @return a list with three elements: knox, st_link, space-time link.
#'      - *knox* is a list with three elements:
#'      \item{knox}{Knox statistic, which is the number of pairs of points found in a given space-time distance.}
#'      \item{p_value}{p-value calculated from MonteCarlo simulation.}
#'      \item{RR}{Relative Risk - calculated by observed value (Knox statistics) divided by mean of simulated values.}
#'      - *st_link* is a origen-destination dataset of class dataframe. This dataset has four variables:
#'      \item{Xo}{is the longitude of the point of origin.}
#'      \item{Yo}{are the latitud of the point of origin.}
#'      \item{Xd}{are the longitude of the destination point.}
#'      \item{Yd}{are the latitud of the destination point.}
#'      - *space-time link* is a sf object (LINESTRING) with crs 4326.
#' @export
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @seealso \link[knox]{knox_mc}, \link[knox]{st_link}, \link[knox]{knox}
#' @references \strong{Knox, E. (1964)}. \emph{The detection of space-time interactions.} Journal of the Royal Statistical Society. Series C (13(1), 25-30.
#' \strong{Tango, T. (2010)}. \emph{Statistical methods for disease clustering. Springer.}
#' @details This function implements the space-time analysis with the knox test. x is a dataframe with three variables: x (longitude), y (latitude) and onset (onset of symptoms)
#'      The coordinates should be projected to a planar system.
#'      The function rknox test use the (knox)[https://github.com/thanhleviet/knox] package of (github)[https://github.com/].
#'
#' @examples
knox <- function(x, crs,dt, ds, sym, sp_link, planar_coord){
    # step 1. transform dataframe to sf object ####
    if(planar_coord == FALSE){
        x <- x %>%
            sf::st_as_sf(coords = c("x", "y"),
                         crs = 4326) %>%
            sf::st_transform(crs = crs) %>%
            dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                          y = sf::st_coordinates(geometry)[,2]) %>%
            sf::st_drop_geometry()
    }

    # step 2. apply the knox test with the Montecarlo ####
    set.seed(123456)
    knox <- knox::knox_mc(x = x$x,
                          y = x$y,
                          time = x$onset,
                          sigma = ds,
                          tau = dt,
                          perm = sym)
    # step 3. generate the origen-destination dataset ####
    st_link <- knox::st_link(x = x$x,
                             y = x$y,
                             time = x$onset,
                             ds = ds,
                             dt = dt)

    # step 4. built the space-time link ####
    if(sp_link == TRUE){
        l <- list()
        for (i in 1:nrow(st_link)) {
            l[[i]] <- with(st_link[i,],sp::Lines(sp::Line(cbind(c(Xo,Xd),
                                                                c(Yo,Yd))),
                                                 ID = i))
        }
        id <- data.frame(id = c(1:nrow(st_link)))
        shape <- sp::SpatialLines(l)
        shape.line.df <- sp::SpatialLinesDataFrame(shape,id,match.ID = TRUE)


        space_time_link <- sf::st_as_sf(shape.line.df) %>%
            sf::st_set_crs(crs) %>%
            sf::st_transform(crs = 4326)
    } else{
        linestring_od_point <- function(x) {
            w <- sf::st_sfc(sf::st_point(c(x$Xo, x$Yo)))
            z <- sf::st_sfc(sf::st_point(c(x$Xd, x$Yd)))
            sf::st_combine(c(w,z)) %>%
                sf::st_cast("LINESTRING") %>%
                sf::st_sfc() %>%
                sf::st_as_sf(crs = crs)
        }

        ###
        space_time_link <- st_link %>%
            dplyr::mutate(id = 1:dplyr::n()) %>%
            dplyr::group_by(id ) %>%
            tidyr::nest() %>%
            dplyr::mutate(linestring = purrr::map(data,linestring_od_point)) %>%
            dplyr::select(-data) %>%
            tidyr::unnest(cols = c(linestring)) %>%
            as.data.frame() %>%
            sf::st_set_geometry(value = "x") %>%
            sf::st_set_crs(crs) %>%
            sf::st_transform(4326)
    }


    ## Step 5. return the knos test, o-d dataset and space-time link ####
    multi_return <- function() {
        my_list <- list("knox" = knox,
                        "st_link" = st_link,
                        "x" = x,
                        "space_time_link" = space_time_link)
        return(my_list)
    }
    multi_return()
}
