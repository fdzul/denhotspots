#' sf_joint_cov
#' this function apply the dplyr::left_join between sf and covid data of México
#'
#' @param path_dat is the directory of the covid data.
#' @param path_sf  is the directory of the municipality shapefile of Mexico.
#'
#' @return a sf object with a dengue cases accumulated by municipality.
#' @export
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @note the dataset of SARS-COV2 of Mexico is downloaded homepage of DGE \url{https://www.gob.mx/salud/documentos/datos-abiertos-152127?idiom=es} and adapted to work with the sinave \url{http://www.sinave.gob.mx/} database.
#'
#' @details The function use the \link[dplyr]{left_join} of dplyr.
#'
#' @examples
sf_joint_cov <- function(path_dat, path_sf){
    # Step 1. load the dataset ####
    covid <- data.table::fread(path_dat,
                               header = TRUE)



    # Step 2. count the n cases by municipality and non tidy dataset ####
    x <- covid %>%
        dplyr::filter(RESULTADO_LAB == 1) %>%
        dplyr::mutate(date = lubridate::ymd(FECHA_SINTOMAS),
                      week = lubridate::week(date)) %>%
        dplyr::group_by(week, ENTIDAD_RES, MUNICIPIO_RES) %>%
        dplyr::summarise(n = dplyr::n()) %>%
        tidyr::pivot_wider(names_from = week,
                           values_from = n,
                           values_fill = list(n = 0)) %>%
        as.data.frame()
    names(x)[3:ncol(x)] <- paste("covid", names(x)[3:ncol(x)], sep = "_")

    # Step 3. loa the shape file of municipalities ####
    mun_sf <- sf::st_read(path_sf, quiet = TRUE)

    # Step 4. joint the municipa
    mun_sf$CVE_ENT <- as.integer(mun_sf$CVE_ENT)
    mun_sf$CVE_MUN <- as.integer(mun_sf$CVE_MUN)
    mun_cov <- dplyr::left_join(x = mun_sf,
                                y = x,
                                by = c("CVE_ENT" = "ENTIDAD_RES",
                                       "CVE_MUN" = "MUNICIPIO_RES")) %>%
        sf::st_drop_geometry()
    # Step 4. sustitute the values na by 0 ####
    mun_cov[is.na(mun_cov)] <- 0

    # Step 5. rbind the dataset
    sf:::cbind.sf(mun_sf, mun_cov[,5:ncol(mun_cov)])

}
