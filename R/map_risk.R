#' map_risk
#'
#' The map_risk function generates the map of the operational scenarios or epidemiological scenarios based on the hotspots of dengue transmission and the hotspots of the abundance of dengue vector eggs.
#'
#' @param risk is the spatial risk dataset.
#' @param staticmap is a logical value, if true the map is static else the map is interactive.
#'
#' @return a ggplot o mapview object
#' @export
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @note To generate the map with this function, you must first run the risk_ageb function.
#' @examples 1+1
map_risk <- function(risk, staticmap){
    if(staticmap == TRUE){
        ggplot2::ggplot() +
            ggplot2::geom_sf(data = risk,
                             mapping = ggplot2::aes(fill = risk),
                             col = "white") +
            ggplot2::theme_void()+
            ggplot2::scale_fill_manual("",values = c("#F44B1FFF",
                                                     "#FF9000FF",
                                                     "gold",
                                                     "#00F293FF"))
    } else{
        mapview::mapview(risk,
                         zcol = "risk",
                         color = "white",
                         alpha.regions = 0.5,
                         lwd = 0.5,
                         col.regions = c("#df382c",
                                         "#FF9000FF",
                                         "gold",
                                         "#20c997"),
                         layer.name = "Escenarios Epidemiológicos")
    }
}
