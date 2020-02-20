#' bivariate_map
#'
#' generate a bivariate map
#'
#' @param w Is the sf object with the variables to map.
#' @param x Is the variable x in the sf object to be mapped.
#' @param y Is the variable x in the sf object to be mapped.
#' @param dim The dimensions of the palette, either 2 for a two-by-two palette or 3 for a three-by-three palette
#' @param pal A palette name; one of "Brown", "DkBlue", "DkCyan", "DkViolet", or "GrPink". As biscale::bi_legend.
#' @param style A string identifying the style used to calculate breaks. Currently supported styles are "quantile", "equal", "fisher", and "jenks"
#' @param size_axis Size of axis labels.
#' @param scale_leg Scales the grob relative to the rectangle defined by x, y, width, height. A setting of scale = 1 indicates no scaling.
#' @param x_leg The x location of the legend in the plot.
#' @param y_leg The y location of the legend in the plot.
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a ggplot object.
#' @export
#'
#' @examples
bivariate_map <- function(w, x, y, dim, pal, style, size_axis, scale_leg, x_leg, y_leg){
    x <- dplyr::enquo(x)
    y <- dplyr::enquo(y)
    data <- biscale::bi_class(w,
                              x = !! x,
                              y = !! y,
                              style = style,
                              dim = dim)
    map <- ggplot2::ggplot(data = data) +
        ggplot2::geom_sf(mapping = ggplot2::aes(fill = bi_class),
                color = "white",
                size = 0.1,
                show.legend = FALSE) +
        biscale::bi_scale_fill(pal = pal,
                               dim = dim) +
        biscale::bi_theme()

    legend <- biscale::bi_legend(pal = pal,
                                 dim = dim,
                                 xlab = paste0(x)[2],
                                 ylab = paste0(y)[2],
                                 size = size_axis)
    cowplot::ggdraw() +
        cowplot::draw_plot(map, 0, 0, 1, 1) +
        cowplot::draw_plot(legend,
                           x = x_leg, y = y_leg,
                           0.35, 0.35,
                           scale = scale_leg)
}
