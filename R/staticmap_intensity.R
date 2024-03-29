#' staticmap_intensity
#'
#' This function generate the intensity map of hotspots.
#'
#' @param x is the hotspots dataset.
#' @param pal is the palette.
#' @param option is the palette option.
#' @param name is the palette name. the option and name are very similar, some package use name (ej. rcartocolor::carto_pal) and other option (ex. viridis:viridis)
#' @param pal_name is a logical value, if pal_name TRUE is for packages that use name, else the packages that use option.
#' @param breaks is a numeric value. Is the increment of the sequence.
#' @param dir_pal is the direction of the palette, 1 forward, -1 reverse.
#' @param x_leg is the x coordinates of legend.
#' @param y_leg is the x coordinates of legend.
#' @param ageb is a logical value, if ageb is TRUE plot the intensity map of ageb, else plot the intensity map by other spatial unit.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}.
#'
#' @return a ggplot map.
#' @export
#'
#' @examples 1+1
staticmap_intensity <- function(x, pal, option = NULL, name = NULL,
                                pal_name, breaks, dir_pal = NULL,
                                x_leg, y_leg, ageb){
    if(ageb == TRUE){

        if(pal_name == TRUE){
            ggplot2::ggplot()+
                ggplot2::geom_sf(data = x,
                                 fill = "grey88",
                                 col = "white",
                                 lwd = 0.3) +
                ggplot2::theme_void() +
                ggplot2::geom_sf(data = x |> dplyr::filter(intensity_gi > 0),
                                 ggplot2::aes(fill = intensity_gi),
                                 col = "white",
                                 lwd = 0.3) +
                ggplot2::scale_fill_gradientn(name = "Intensidad",
                                              colours = c(pal(n = max(x$intensity_gi),
                                                              name = name)),
                                              breaks = seq(from = 1,
                                                           to = max(x$intensity_gi),
                                                           by = breaks),
                                              guide = "legend") +
                ggplot2::theme(legend.position = c(x_leg, y_leg)) +
                ggplot2::guides(fill =  ggplot2::guide_legend(nrow = 1))
        } else{
            ggplot2::ggplot()+
                ggplot2::geom_sf(data = x,
                                 fill = "grey95",
                                 col = "white",
                                 lwd = 0.3) +
                ggplot2::theme_void() +
                ggplot2::geom_sf(data = x |> dplyr::filter(intensity_gi > 0),
                                 ggplot2::aes(fill = intensity_gi),
                                 col = "white",
                                 lwd = 0.3) +
                ggplot2::scale_fill_gradientn(name = "Intensidad",
                                              colours = c(pal(n = max(x$intensity_gi),
                                                              option = option,
                                                              direction = dir_pal)),
                                              breaks = seq(from = 1,
                                                           to = max(x$intensity_gi),
                                                           by = breaks),
                                              guide = "legend") +
                ggplot2::theme(legend.position = c(x_leg, y_leg)) +
                ggplot2::guides(fill =  ggplot2::guide_legend(nrow = 1))
        }
    } else {

    }

}
