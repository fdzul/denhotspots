#' power_law_plot
#'
#' this plot generate the all Power Law for hotspots and spatial units.
#'
#' @param x is the dataset.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a ggplot.
#' @export
#'
#' @examples 1+1
power_law_plot <- function(x){

    if(length(unique(x$loc)) == 1){
        p <- ggplot2::ggplot(data = x,
                             ggplot2::aes(x = id_perc_cum,
                                 y = perc_cumsum_n,
                                 col = as.factor(hotspots_gi))) +
            ggplot2::geom_line( size = 1.5)
    } else {
        p <- ggplot2::ggplot() +
            ggplot2::geom_line(data = x,
                               ggplot2::aes(x = id_perc_cum,
                                            y = perc_cumsum_n,
                                            col = loc),
                               alpha = 0.5, size = 1.5)
    }

    p +
        ggplot2::geom_line(data = power_laws,
                           ggplot2::aes(x = x,
                                        y = y,
                                        linetype = power_law,
                                        col = power_law),
                           #linetype = power_law,
                           size = 1,
                           alpha = 0.5) +
        ggplot2::ylab("Casos (%)") +
        ggplot2::xlab("Unidades Espaciales(%)") +
        fishualize::scale_color_fish_d(option = "Scarus_quoyi", direction = -1) +
        #ggplot2::scale_color_manual(values = c("darkred", "darkblue"),aesthetics = "fill") +
        ggplot2::theme(legend.position = "None") +
        ggplot2::scale_x_continuous(breaks = seq(from = 0, to = 100, by = 10)) +
        ggplot2::scale_y_continuous(breaks = seq(from = 0, to = 100, by = 20)) +
        #facet_wrap("agebs") +
        ###################################
    ggplot2::geom_segment(x = 50,
                          y = 0,
                          xend = 50,
                          yend = 50,
                          colour = "black",
                          alpha = 0.3,
                          lwd = 0.5,
                          linetype = 1) +
        ggplot2::geom_segment(x = 0,
                              y = 50,
                              xend = 50,
                              yend = 50,
                              colour = "black",
                              alpha = 0.3,
                              lwd = 0.5,
                              linetype = 1) +
        ggplot2::annotate(geom = "text",
                          x = 70,
                          y = 50,
                          label= "Power Law 50:50",
                          color = "black",
                          size = 2.5) +
        #############################
    ggplot2::geom_segment(x = 40,
                          y = 0,
                          xend = 40,
                          yend = 60,
                          colour = "black",
                          alpha = 0.3,
                          lwd = 0.5,
                          linetype = 2)+
        ggplot2::geom_segment(x = 40,
                              y = 60,
                              xend = 0,
                              yend = 60,
                              colour = "black",
                              alpha = 0.3,
                              lwd = 0.5,
                              linetype = 2) +
        ggplot2::annotate(geom = "text",
                          x = 60,
                          y = 60,
                          label= "Power Law 60:40",
                          color = "black",
                          size = 2.5) +
        ################################
    ggplot2::geom_segment(x = 30,
                          y = 0,
                          xend = 30,
                          yend = 70,
                          colour = "black",
                          alpha = 0.3,
                          lwd = 0.5,
                          linetype = 3)+
        ggplot2::geom_segment(x = 30,
                              y = 70,
                              xend = 0,
                              yend = 70,
                              colour = "black",
                              alpha = 0.3,
                              lwd = 0.5,
                              linetype = 3) +
        ggplot2::annotate(geom = "text",
                          x = 50,
                          y = 70,
                          label= "Power Law 70:30",
                          color = "black",
                          size = 2.5) +
        #################################
    ggplot2::geom_segment(x = 20,
                          y = 0,
                          xend = 20,
                          yend = 80,
                          colour = "black",
                          alpha = 0.3,
                          lwd = 0.5,
                          linetype = 4) +
        ggplot2::geom_segment(x = 20,
                              y = 80,
                              xend = 0,
                              yend = 80,
                              colour = "black",
                              alpha = 0.3,
                              lwd = 0.5,
                              linetype = 4) +
        ggplot2::annotate(geom = "text",
                          x = 50,
                          y = 80,
                          label= "Power Law 80:20 (Pareto Rule)",
                          color = "black",
                          size = 2.5) +
        ###############################
    ggplot2::geom_segment(x = 10,
                          y = 0,
                          xend = 10,
                          yend = 90,
                          colour = "black",
                          alpha = 0.3,
                          lwd = 0.5,
                          linetype = 5) +
        ggplot2::geom_segment(x = 10,
                              y = 90,
                              xend = 0,
                              yend = 90,
                              colour = "black",
                              alpha = 0.3,
                              lwd = 0.5,
                              linetype = 5) +
        ggplot2::annotate(geom = "text",
                          x = 30,
                          y = 90,
                          label= "Power Law 90:10",
                          color = "black",
                          size = 2.5) +
        ggplot2::theme(strip.text =  ggplot2::element_text(face = "bold",
                                                 size = 10)) +
        ggplot2::theme(axis.text =  ggplot2::element_text(face = "bold",
                                                size = 8)) +
        ggplot2::annotate(geom = "point",
                          y = 50,
                          x = 50,
                          fill = "red",
                          col = "white")

}
