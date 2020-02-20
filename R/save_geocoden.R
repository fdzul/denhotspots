#' save_geocoden
#'
#' This function generates the RData file where it contains the geocoded data and the sinave database.
#'
#' @param x is the geocoded data.
#' @param y is the dataset o sinave.
#' @param directory is the folder where the RData file is to be saved
#' @param loc is the name of geocoded name locality
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @return a RData
#' @export
#'
#' @examples
save_geocoden <- function(x, y, directory, loc){
    x <- x[!duplicated(x$index),]
    y <- cbind(y, x)
    save(y,
         file = paste(paste(paste(directory, "/geo", sep = ""), loc, sep = "_"), ".RData", sep = ""))
}
