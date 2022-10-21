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
        # house number ####
        x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = "[[:punct:]]", replacement = "")
        x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = "SN|\\t|SIN|SN|#", replacement = "")
        x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = " M | MZ | MZN", replacement = "MANZANA ")
        x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = " Z", replacement = " ")
        x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = "L", replacement = "LOTE ")
        x$NUM_EXT <- stringr::str_replace(x$NUM_EXT, pattern = " T", replacement = " ")

        # street name ####
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "\\s{2}", replacement = "")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "[[:punct:]]", replacement = "")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "\\.", replacement = "")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "\\t{1,30}", replacement = "")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = " SIN NUMERO|SIN NOMBRE|SIN NUMERO| SIN NUM| CALLE DOMICILIO CONOCIDO ,",
                                          replacement = "")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "SIN DOCUMENTAR|SIN DATO EXACTO|SIN DATO|CONOCIDO SIN NUMERO",
                                          replacement = "")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = " SEC ",
                                          replacement = "")
        x$IDE_CAL <- paste(x$DES_CAL, x$IDE_CAL, sep = " ")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "CALLE CALLE ", replacement = "CALLE ")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL,
                                          pattern = "CALLE AV |CALLE AVE |CALLE AVENIDA |CALLE AV",
                                          replacement = "AVENIDA ")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = " NUM ",  replacement = " NUMERO ")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = " MZA | MZ | MZNA | MZ\\d{1}| M | MA ",  replacement = " MANZANA ")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = " LT | LTE | LT\\d{1}| L\\d{1}| L ",  replacement = " LOTE ")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "C\\s{1,2}\\d",  replacement = "")
        x$IDE_CAL <- stringr::str_replace(x$IDE_CAL, pattern = "CALLE SINNOMBRE|CALLE CONOCIDO",  replacement = "")
        x$IDE_CAL[x$IDE_CAL  %in% c(stringr::str_subset(x$IDE_CAL, "CARRE"))] <- NA
        x$IDE_CAL[x$IDE_CAL  %in% c(stringr::str_subset(x$IDE_CAL, " FRENTE "))] <- NA
        x$IDE_CAL[x$IDE_CAL  %in% c(stringr::str_subset(x$IDE_CAL, "CAMINO "))] <- NA

        ##  neighborhoods
        x$IDE_COL <- stringr::str_replace_all(x$IDE_COL, pattern = "\\.", replacement = "")
        x$IDE_COL <- paste(stringr::str_extract(x$IDE_COL, "[A-Z]{1}[a-z]{1,40}"),
                           stringr::str_extract(x$IDE_COL, "([[:digit:]., -]|[[:upper:]., ]){1,40}"),
                           sep = " ")

        #  vector addresses
        addresses <- paste(paste(x$IDE_CAL, x$NUM_EXT, sep = " "),
                           x$IDE_COL,
                           paste(x$IDE_CP,  x$DES_MPO_RES, sep = " "),
                           paste(stringr::str_to_title(x$DES_EDO_RES), "Mexico", sep = ","),
                           sep = ", ")
        addresses <- stringr::str_replace(addresses, pattern = " C,| B,| F,| V,| P,| R,| E, | A,", replacement = " ,")
        addresses <- stringr::str_replace(addresses,
                                          pattern = "\\w{1,10} 0, ",
                                          replacement = " ,")
        addresses <- stringr::str_replace(addresses, pattern = "\\w{1,10} SN ,", replacement = " ,")
        addresses <- stringr::str_replace_all(addresses,
                                              pattern = "(?<=Colonia ).+(?= CENTRO)",
                                              replacement = " Colonia CENTRO ")
        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " Colonia  Colonia CENTRO  CENTRO |Colonia  Colonia CENTRO  CENTRO ",
                                              replacement = " Colonia CENTRO ")


        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " Ejido | Rancher |Rancher | Poblado | Pueblo | Ampliaci ",
                                              replacement = " ")


        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " 1 RA ",
                                              replacement = " PRIMERA ")

        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " ANDRE ",
                                              replacement = " ANDRES")

        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " TLAQUEPAQUE,",
                                              replacement = " SAN PEDRO TLAQUEPAQUE,")
        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " 1ER PRIV ",
                                              replacement = " PRIMERA PRIVADA ")
        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " SANTA FE TEXACAL \\(CENTRO URBANO\\)",
                                              replacement = " SANTA FE TEXACAL")



        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " Colonia LOMAS DEL TAPATÍO , 45628 COL. LOMAS DEL TAPATIO,",
                                              replacement = "  Colonia LOMAS DEL TAPATÍO,")
        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " SECC ",
                                              replacement = " SECCION ")
        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " 3RA | 3RA\\.",
                                              replacement = " TERCERA ")

        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " Villa | Fraccionamiento | Colonia |CALLE ,|Pueblo ",
                                              replacement = " ")


        addresses <- stringr::str_replace_all(addresses,
                                              pattern = "LA CRUCITA \\(BARRIO DE GUADALUPE\\)",
                                              replacement = " LA CRUCITA ")

        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " III ",
                                              replacement = " TRES ")
        addresses <- stringr::str_replace_all(addresses,
                                              pattern = "CALLE DOMICILIO CONOCIDO ,|CALLE DOMICILI CONOCIDO ,",
                                              replacement = "")
        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " #",
                                              replacement = " ")
        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " ACAPULCO DE JUÁREZ,",
                                              replacement = " ACAPULCO,")
        addresses <- stringr::str_replace_all(addresses,
                                              pattern = "NA ,",
                                              replacement = " ")

        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " MICH,",
                                              replacement = " ,")

        addresses <- stringr::str_replace_all(addresses,
                                              pattern = " VER,",
                                              replacement = " ,")
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


