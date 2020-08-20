#' subset_den
#'
#' is a function for subset de dengue dataset
#'
#' @param x is the 2008-2015 dengue dataset. Is NULL for 2016-2019 dengue dataset.
#' @param path is the path of the 2016-2019 dengue dataset. Is NULL for 2008-2015 dengue dataset.
#' @param edo is a string for define the state in uppercase.
#' @param mun is a string for define the municipality in uppercase.
#' @param loc is a string for define the locality in uppercase.
#' @param sinave_new is logical value. If is TRUE the subset ir for 2016-2019 dengue dataset, else the subset ir for 2008-2015 dengue dataset.
#' @param age_group is a logical value. If is TRUE the subset for age group (<= 12 & >= 65), else all age group.
#' @param name is the name the csv output
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a object names x and a csv file in working dircetory.
#' @export
#'
#' @examples
subset_den <- function(x = NULL, path = NULL, edo, mun, loc, sinave_new, age_group, name){

    if (sinave_new == TRUE){
        cols_18_19 <- c("IDE_EDA_ANO", "IDE_SEX", "IDE_CAL",
                        "NUM_EXT", "NUM_INT", "IDE_COL", "IDE_CP", "CVE_LOC_RES",
                        "DES_LOC_RES", "CVE_MPO_RES", "DES_MPO_RES",
                        "DES_JUR_RES", "CVE_EDO_RES", "DES_EDO_RES",
                        "ESTATUS_CASO", "CVE_DIAG_PROBABLE", "DES_DIAG_PROBABLE",
                        "DES_DIAG_FINAL", "FEC_INI_SIGNOS_SINT", "ANO",
                        "SEM", "DENGUE_SER_TRIPLEX",
                        "MANEJO", "DES_INS_UNIDAD", "FEC_INGRESO")
        cols_16_17 <- c("IDE_EDA_ANO", "IDE_SEX", "IDE_CAL",
                        "NUM_EXT", "NUM_INT", "IDE_COL", "IDE_CP", "CVE_LOC_RES",
                        "DES_LOC_RES", "CVE_MPO_RES", "DES_MPO_RES",
                        "DES_JUR_RES", "CVE_EDO_RES", "DES_EDO_RES",
                        "ESTATUS_CASO", "CVE_DIAG_PROBABLE", "DES_DIAG_PROBABLE",
                        "DES_DIAG_FINAL", "FEC_INI_SIGNOS_SINT", "ANO",
                        "SEM","RESULTADO_PCR",
                        "MANEJO", "DES_INS_UNIDAD","FEC_INGRESO")
        w <- purrr::map_dfr(stringr::str_subset(list.files(path = path,
                                                           full.names = TRUE,
                                                           pattern = "txt"),
                                                "2018|2019|2020"),
                            data.table::fread,
                            header = TRUE,
                            select = cols_18_19,
                            quote = "",
                            fill = TRUE, encoding = "Latin-1")
        x <- purrr::map_dfr(stringr::str_subset(list.files(path = path,
                                                           full.names = TRUE,
                                                           pattern = "txt"),
                                                "2016|2017"),
                            data.table::fread,
                            header = TRUE,
                            select = cols_16_17,
                            quote = "",
                            fill = TRUE,
                            encoding = "Latin-1")
        names(x) <- names(w)
        x <- rbind(x, w) %>%
        dplyr::filter(DES_EDO_RES %in% c(edo)) %>%
            dplyr::filter(DES_MPO_RES %in% c(mun)) %>%
            dplyr::filter(DES_LOC_RES %in% c(loc)) %>%
            dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE GRAVE",
                                                "DENGUE NO GRAVE",
                                                "DENGUE CON SIGNOS DE ALARMA"))

        if (age_group == TRUE){
            x <- x %>% dplyr::filter(IDE_EDA_ANO <= 12 | IDE_EDA_ANO >= 65)
            write.csv(x, file = paste(name, ".csv", sep = ""))
            x
        } else {
            write.csv(x, file = paste(name, ".csv", sep = ""))
            x
        }

    } else {
        x$DES_EDO.x <- stringr::str_trim(x$DES_EDO.x, side = "both")
        x$DES_DIAG_FINAL <- stringr::str_trim(x$DES_DIAG_FINAL, side = "both")
        x$DES_MPO.x <- stringr::str_trim(x$DES_MPO.x, side = "both")
        x$DES_LOC <- stringr::str_trim(x$DES_LOC, side = "both")
        x <- x %>%
            dplyr::filter(DES_EDO.x == edo) %>%
            dplyr::filter(DES_DIAG_FINAL %in% c("FIEBRE HEMORRAGICA POR DENGUE",
                                                "FIEBRE POR DENGUE")) %>%
            dplyr::filter(DES_MPO.x %in% c(mun) & DES_LOC %in% c(loc))

        if (age_group == TRUE){
            x <- x %>% dplyr::filter(IDE_EDA_ANO <= 12 | IDE_EDA_ANO >= 65)
            write.csv(x, file = paste(name, ".csv", sep = ""))
            x
        } else {
            write.csv(x, file = paste(name, ".csv", sep = ""))
            x
        }
    }
}
