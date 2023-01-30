#' Read the dengue dataset for geocoding
#'
#' @param x is the path of dengue dataset.
#' @param des_edo_res is a string for define the state in uppercase.
#' @param des_mpo_res is a string for define the municipality in uppercase
#' @return a data.table
#' @export
#'
#' @examples 1+1
read_den_dataset <- function(x, des_edo_res, des_mpo_res){
    vect_cols <- c("VEC_ID","IDE_EDA_ANO", "IDE_SEX",
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
    data.table::fread(x,
                      header = TRUE,
                      quote = "",
                      select = vect_cols,
                      fill = TRUE,
                      encoding = "Latin-1") %>%
        dplyr::filter(DES_DIAG_FINAL %in% c("DENGUE GRAVE",
                                            "DENGUE CON SIGNOS DE ALARMA",
                                            "DENGUE NO GRAVE")) %>%
        dplyr::filter(DES_EDO_RES %in% c(des_edo_res)) %>%
        dplyr::filter(DES_MPO_RES %in% c(des_mpo_res))
}
