#' geocoden
#'
#' this function geocodes the addresses of the sinave database using Geocoding API.
#'
#' @param infile is the name of the location that is being geocoded and is used to be saved in a file with extension rds.
#' @param address is vector addresses.
#
#'
#' @export
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @note The code for geocoding was taken from the personal page of Shane Lynn \url{https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/} and adapted to work with the sinave \url{http://www.sinave.gob.mx/} database.
#'
#' @details The function use the \link[ggmap]{geocode} of ggmap.
#'
#' @return  a file with extension rds.
#'
#' @seealso \link[ggmap]{geocode}
#' @examples
geocoden <- function(infile, address){
    getGeoDetails <- function(address){
        #use the gecode function to query google servers
        geo_reply = ggmap::geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
        #now extract the bits that we need from the returned list
        answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
        answer$status <- geo_reply$status

        #return Na's if we didn't get a match:
        if (geo_reply$status != "OK"){
            return(answer)
        }
        #else, extract what we need from the Google server reply into a dataframe:
        answer$lat <- geo_reply$results[[1]]$geometry$location$lat
        answer$long <- geo_reply$results[[1]]$geometry$location$lng
        if (length(geo_reply$results[[1]]$types) > 0) {
            answer$accuracy <- geo_reply$results[[1]]$types[[1]]
        }
        answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
        answer$formatted_address <- geo_reply$results[[1]]$formatted_address

        return(answer)
    }

    ###########
    #initialise a dataframe to hold the results
    geocoded <- data.frame()
    # find out where to start in the address list (if the script was interrupted before):
    startindex <- 1
    #if a temp file exists - load it up and count the rows!
    tempfilename <- paste0(infile, '_temp_geocoded.rds')
    if (file.exists(tempfilename)){
        print("Found temp file - resuming from index:")
        geocoded <- readRDS(tempfilename)
        startindex <- nrow(geocoded)
        print(startindex)
    }

    # Start the geocoding process - address by address. geocode() function takes care of query speed limit.
    for (ii in seq(startindex, length(addresses))){
        print(paste("Working on index", ii, "of", length(addresses)))
        #query the google geocoder - this will pause here if we are over the limit.
        result = getGeoDetails(address = addresses[ii])
        print(result$status)
        result$index <- ii
        #append the answer to the results file.
        geocoded <- rbind(geocoded, result)
        #save temporary results as we are going along
        saveRDS(geocoded, tempfilename)
    }
}


