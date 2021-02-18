#' cases_by_agem
#'
#' this function left joint sf AGEM and the cases by municipality.
#'
#' @param path is the directory where the txt of dengue file exist.
#' @param disease  is a string for define the vector borne disease. The option are "Paludismo", "Dengue" & "covid.
#'
#' @return a sf object. The municipality with the dengue cases by year.
#' @export
#'
#' @examples 1+1
cases_by_agem <- function(path, disease){

    # step 1.1 Define the columns ####
    if(disease == "Paludismo"){
        col_names <- c("FOL_ID", "VEC_ID", "IDE_SEX",
                       "CVE_EDO_RES", "DES_EDO_RES",
                       "CVE_JUR_RES", "DES_JUR_RES",
                       "CVE_MPO_RES", "DES_MPO_RES",
                       "CVE_LOC_RES", "DES_LOC_RES",
                       "IDE_CALLE1", "IDE_CALLE2", "IDE_CP",
                       "CVE_DIAG_PROBABLE", "DES_DIAG_PROBABLE",
                       "CVE_DIAG_FINAL", "DES_DIAG_FINAL",
                       "FEC_INI_SIGNOS_SINT", "ANO", "SEM",
                       "FIEBRE", "FEC_INI_FIEBRE", "FEC_INI_SIGNOS_ALARMA",
                       "MANEJO", "FEC_INGRESO", "EVOLUCION",
                       "ESPECIE_PALUDISMO",
                       "TRATAMIENTO_PALUDISMO", "FEC_INI_TRAT_PAL", "FEC_FIN_TRAT_PAL",
                       "MED_TRAT_PAL", "MED_TRAT_PAL_1", "MED_TRAT_PAL_2")
    }
    if(disease == "Dengue"){
        col_names <- c("IDE_EDA_ANO", "IDE_SEX",
                       "IDE_CAL", "NUM_EXT", "NUM_INT",
                       "IDE_COL", "IDE_CP",
                       "CVE_LOC_RES", "DES_LOC_RES", "CVE_MPO_RES", "DES_MPO_RES",
                       "DES_JUR_RES",
                       "CVE_EDO_RES", "DES_EDO_RES",
                       "ESTATUS_CASO", "CVE_DIAG_PROBABLE", "DES_DIAG_PROBABLE", "DES_DIAG_FINAL",
                       "FEC_INI_SIGNOS_SINT", "ANO", "SEM",
                       "RESULTADO_NS1", "RESULTADO_IGM", "RESULTADO_IGG",
                       "RESULTADO_PCR", "RESULTADO_IGMC", "RESULTADO_MAC",
                       "MANEJO")
    }
    # Step 1.2. read the files with datatable ####
    if(disease == "covid"){
        y <- data.table::fread(path, header = TRUE) %>%
            dplyr::filter(CLASIFICACION_FINAL %in% c(1:3)) %>%
            dplyr::mutate(date = lubridate::ymd(FECHA_SINTOMAS),
                          week = lubridate::week(date)) %>%
            dplyr::group_by(week, ENTIDAD_RES, MUNICIPIO_RES) %>%
            dplyr::summarise(n = dplyr::n(),  .groups = "drop") %>%
            tidyr::pivot_wider(names_from = week,
                               values_from = n,
                               values_fill = list(n = 0)) %>%
            dplyr::mutate(CVE_EDO_RES = stringr::str_pad(ENTIDAD_RES,
                                                         pad = "0",
                                                         side = "left",
                                                         width = 2),
                          CVE_MPO_RES = stringr::str_pad(MUNICIPIO_RES,
                                                         pad = "0",
                                                         side = "left",
                                                         width = 3),
                          ENTIDAD_RES = NULL,
                          MUNICIPIO_RES = NULL) %>%
            as.data.frame()
    } else if (disease == "Paludismo") {

        y <- purrr::map_dfr(.x = list.files(path = path,
                                            full.names = TRUE,
                                            pattern = "txt"),
                            .f = data.table::fread,
                            stringsAsFactors = FALSE,
                            select = col_names,
                            quote = "",
                            fill = TRUE) %>%
            dplyr::filter(DES_DIAG_FINAL == "PALUDISMO") %>%
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA",
                                              "OTROS PAISES")) %>%
            dplyr::group_by(CVE_EDO_RES, CVE_MPO_RES, ANO) %>%
            dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
            tidyr::pivot_wider(names_from = ANO,
                               values_from = n,
                               values_fill = list(n = 0)) %>%
            dplyr::mutate(CVE_EDO_RES = stringr::str_pad(CVE_EDO_RES,
                                                         pad = "0",
                                                         side = "left",
                                                         width = 2),
                          CVE_MPO_RES = stringr::str_pad(CVE_MPO_RES,
                                                         pad = "0",
                                                         side = "left",
                                                         width = 3)) %>%
            as.data.frame()
    } else if(disease == "Dengue"){
        y <- purrr::map_dfr(.x = list.files(path = path,
                                            full.names = TRUE,
                                            pattern = "txt"),
                            .f = data.table::fread,
                            stringsAsFactors = FALSE,
                            select = col_names,
                            quote = "",
                            fill = TRUE) %>%
            dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE GRAVE",
                                                "DENGUE NO GRAVE",
                                                "DENGUE CON SIGNOS DE ALARMA")) %>%
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA",
                                              "OTROS PAISES")) %>%
            dplyr::group_by(CVE_EDO_RES, CVE_MPO_RES, ANO) %>%
            dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
            tidyr::pivot_wider(names_from = ANO,
                               values_from = n,
                               values_fill = list(n = 0)) %>%
            dplyr::mutate(CVE_EDO_RES = stringr::str_pad(CVE_EDO_RES,
                                                         pad = "0",
                                                         side = "left",
                                                         width = 2),
                          CVE_MPO_RES = stringr::str_pad(CVE_MPO_RES,
                                                         pad = "0",
                                                         side = "left",
                                                         width = 3)) %>%
            as.data.frame()


    } else{

    }


    # step 3. load the AGEM of inegi 2019 ####
    x <- rgeomex::AGEM_inegi19_mx

    # step 4. left joint between aggregated data and AGEM ######
    xy <- dplyr::left_join(x = x,
                           y = y,
                           by = c("CVE_ENT"  = "CVE_EDO_RES",
                                  "CVE_MUN"  = "CVE_MPO_RES"))

    # step 5. sustitute the NA values by 0 ####
    xy[is.na(xy)] <- 0

    # step 6. set column names ####
    if(disease == "Paludismo"){
        names(xy)[-c(1:4,ncol(xy))] <- paste("PAL",
                                             names(xy)[-c(1:4,ncol(xy))], sep = "_")
    }

    if(disease == "Dengue"){
        names(xy)[-c(1:4,ncol(xy))] <- paste("DENV",
                                             names(xy)[-c(1:4,ncol(xy))], sep = "_")
    }
    if(disease == "covid"){
        names(xy)[-c(1:4,ncol(xy))] <- paste("COVID",
                                             names(xy)[-c(1:4,ncol(xy))], sep = "_")
    }

    # step 7. return the result ####
    xy
}
