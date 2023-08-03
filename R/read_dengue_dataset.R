#' read_dengue_dataset
#'
#' Read the dengue dataset of [SINAVE](https://www.sinave.gob.mx/)
#'
#' @param path is the path of dengue dataset.
#' @param spatial_resolution is the spatial resolution or the administrative level. The values are country, state and municipality.
#' @param des_edo_res is a string for define the state in uppercase.
#' @param des_mpo_res is a string for define the municipality in uppercase.
#' @param estatus_caso 1 probable, 2 confirmado, & 3 descartado.
#'
#' @return a data.table object.
#' @export
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @examples
read_dengue_dataset <- function(path, spatial_resolution,status_caso, des_edo_res = NULL, des_mpo_res = NULL){
    vect_cols <- c("FOL_ID","VEC_ID","IDE_EDA_ANO", "IDE_SEX",
                   "DES_CAL","IDE_CAL", "NUM_EXT", "NUM_INT",
                   "IDE_COL", "IDE_CP",
                   "CVE_LOC_RES", "DES_LOC_RES", "CVE_MPO_RES", "DES_MPO_RES",
                   "DES_JUR_RES", "CVE_EDO_RES", "DES_EDO_RES",
                   "ESTATUS_CASO", "CVE_DIAG_PROBABLE", "DES_DIAG_PROBABLE", "DES_DIAG_FINAL",
                   "FEC_INI_SIGNOS_SINT", "ANO", "SEM",
                   #"RESULTADO_NS1", "RESULTADO_IGM", "RESULTADO_IGG",
                   #"RESULTADO_PCR", "RESULTADO_IGMC", "RESULTADO_MAC",
                   "MANEJO",
                   "DES_INS_UNIDAD", "DENGUE_SER_TRIPLEX","FEC_INGRESO")


    if(spatial_resolution == "country"){

        data.table::fread(path,
                          header = TRUE,
                          quote = "",
                          select = vect_cols,
                          fill = TRUE,
                          encoding = "Latin-1") |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA")) |>
            dplyr::filter(ESTATUS_CASO %in% c(status_caso))

    } else if(spatial_resolution == "state"){

        data.table::fread(path,
                          header = TRUE,
                          quote = "",
                          select = vect_cols,
                          fill = TRUE,
                          encoding = "Latin-1") |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA")) |>
            dplyr::filter(ESTATUS_CASO %in% c(status_caso)) |>
            dplyr::filter(DES_EDO_RES %in% c(des_edo_res))

    } else if(spatial_resolution == "municipality"){

        data.table::fread(path,
                          header = TRUE,
                          quote = "",
                          select = vect_cols,
                          fill = TRUE,
                          encoding = "Latin-1") |>
            dplyr::filter(!DES_EDO_RES %in% c("OTROS PAISES",
                                              "OTROS PAISES DE LATINOAMERICA",
                                              "ESTADOS UNIDOS DE NORTEAMERICA")) |>
            dplyr::filter(ESTATUS_CASO %in% c(status_caso)) |>
            dplyr::filter(DES_EDO_RES %in% c(des_edo_res)) |>
            dplyr::filter(DES_MPO_RES %in% c(des_mpo_res))
    } else{

    }


}
