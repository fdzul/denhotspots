#' data_geocoden
#'
#' this function creates an address vector and replaces incorrect text.
#'
#' @param infile is the name of file create with \link[denhotspots]{subset_den}.
#' @param data is a string for define the addreses or data. if data TRUE, is return the dataset.
#' @param sinave_new is logical value. If is TRUE the subset ir for 2016-2019 dengue dataset, else the subset ir for 2008-2015 dengue dataset.
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @return a vector.
#' @export
#'
#' @examples
data_geocoden <- function(infile, data, sinave_new){
    x <- data.table::fread(paste0("./", infile, ".csv" ))

    if(sinave_new == TRUE){
        addresses <- paste(paste("CALLE", x$IDE_CAL, x$NUM_EXT, sep = " "),
                           x$IDE_COL,
                           paste(x$IDE_CP,  x$DES_MPO_RES, sep = " "),
                           paste(stringr::str_to_title(x$DES_EDO_RES), "Mexico", sep = " "),
                           sep = ", ")

        addresses <- stringr::str_replace_all(addresses, pattern = " , , NA ", replacement = ",")
        addresses <- stringr::str_replace_all(addresses, pattern = " S/N | SN| S/N | S/N| # | #", replacement = "")
        addresses <- stringr::str_replace_all(addresses, pattern = " Ejido| Fraccionamiento| Congregación| Colonia| Barrio| Unidad habitacional| Pueblo| Ranchería", replacement = "")
        addresses <- stringr::str_replace_all(addresses, pattern = "AV\\.| AV\\.| AVE\\. | AV ", replacement = "AVENIDA")
        addresses <- stringr::str_replace_all(addresses, pattern = "S\\.N\\.,|S\\.N,", replacement = ",")
        addresses <- stringr::str_replace_all(addresses, pattern = " C\\.", replacement = " ")
        addresses <- stringr::str_replace_all(addresses, pattern = " PRIV\\. ", replacement = " ")
        addresses <- stringr::str_replace_all(addresses, pattern = "CALLE AVENIDA", replacement = "AVENIDA ")
        addresses <- stringr::str_replace_all(addresses, pattern = " NUM ", replacement = " ")
        addresses <- stringr::str_replace_all(addresses, pattern = " SIN NUMERO ,| SIN NUMERO ", replacement = ",")
        addresses <- stringr::str_replace_all(addresses, pattern = " S/N,| S/N ,| SN,| SN ,|  S N,|  SN ", replacement = ",")
        addresses <- stringr::str_replace_all(addresses, pattern = " , , NA |, , NA ", replacement = ",")
        addresses <- stringr::str_replace_all(addresses, pattern = "CALLE CALLE |CALLE CALLEJON ", replacement = "CALLE ")
        addresses <- stringr::str_replace_all(addresses, pattern = " NUM | NO | N | #| # | NO\\. | NO\\.",
                                              replacement = " ")
        addresses <- stringr::str_replace_all(addresses, pattern = " S/N | SN| S/N | S/N", replacement = "")
        addresses <- stringr::str_replace_all(addresses, pattern = " NULL", replacement = " ")
    } else {
        x$IDE_CAL <- stringr::str_trim(x$IDE_CAL, side = "both")
        x$IDE_COL <- stringr::str_trim(x$IDE_COL, side = "both")
        x$IDE_CP <- stringr::str_trim(x$IDE_CP, side = "both")
        x$DES_MPO.x <- stringr::str_trim(x$DES_MPO.x, side = "both")

        addresses <- paste(paste("CALLE", x$IDE_CAL, sep = " "),
                           x$IDE_COL,
                           paste(x$IDE_CP, x$DES_MPO.x, sep = " "),
                           paste(stringr::str_to_title(x$DES_EDO.x), "Mexico", sep = " "),
                           sep = ", ")
        addresses <- stringr::str_replace_all(addresses, pattern = "CALLE CALLE |CALLE CALLEJON ", replacement = "CALLE ")
        addresses <- stringr::str_replace_all(addresses, pattern = " AV\\.| AV ", replacement = " AVENIDA ")
        addresses <- stringr::str_replace_all(addresses, pattern = " AND\\.", replacement = " ANDADOR ")
        addresses <- stringr::str_replace_all(addresses, pattern = " FRACC\\.| FRACC ", replacement = " FRACCIONAMIENTO ")
        addresses <- stringr::str_replace_all(addresses, pattern = " INF\\.|INF ", replacement = " INFONAVIT ")
        addresses <- stringr::str_replace_all(addresses, pattern = "NULL | SIN NUMERO", replacement = " ")
        addresses <- stringr::str_replace_all(addresses, pattern = " N° | # | NUM\\. | N0\\. | #| NO\\. | NO | N | NUM |#",
                                              replacement = " NUMERO ")
        addresses <- stringr::str_replace_all(addresses, pattern = "CALLE AVENIDA", replacement = "AVENIDA ")
    }

   if(data == TRUE){
       x
   } else {addresses
       }
}


