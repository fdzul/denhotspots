#' gihi
#'
#' Calculate gi and hi local spatial statistic
#'
#' @param x it is an sf object with the number of events for each spatial unit
#' @param gi_hi Define the local spatial statistic. It includes three options ("gi", "hi", "gi_hi", if "gi" calculates the spatial local statistic getis & ord, if it is "hi" calculates hi, and if it is "gi_hi" calculates both statistic
#' @param id is or are the identifiers of each spatial unit
#' @param dis it is the prefix of the variable name. Example den_2018 or denv_2018, dis is den and denv
#' @param time is the unit of time or the time scale of the analyzed time series. example in years, times, months, weeks
#' @param alpha is the value of alpha to determine the threshold of value of gi to discriminate hotspots from non-hotspots
#'
#' @return a sf object with the ids and the intensity and hotspots for hi, gi or each.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#'
#' @seealso \link[spdep]{LOSH.cs}, \link[spdep]{localG}
#'
#' @references
#' Getis A, Ord JK. 1992. The analysis of spatial association by the use of distance statistics.Geographical Analysis, 24(2):189-206.
#' Ord JK, Getis A. 1995. Local spatial autocorrelation statistics: distributional issues and an application. Geographical Analysis, 27, 286–306.
#' Ord JK,  Getis A. 2012. Local spatial heteroscedasticity (LOSH), The Annals of Regional Science, 48 (2), 529–539.
#' Bivand RS, Wong DWS. 2018. Comparing implementations of global and local indicators of spatial association. TEST, 27(3), 716–748.
gihi <- function(x, gi_hi, id, dis, time, alpha = NULL){
    # nested dataset ####
    w <- x %>%
        sf::st_drop_geometry() %>%
        tidyr::pivot_longer(names_to = time,
                            values_to = "n",
                            cols = -id) %>%
        tidyr::separate(col = time, into = c("dis", time)) %>%
        dplyr::filter(stringr::str_detect(dis, dis)) %>%
        dplyr::group_by(!! rlang::sym(time)) %>%
        tidyr::nest()

    # Convert the sf object to sp object and ####
    # create the neighboorhoods

    z_hi <- spdep::poly2nb(as(x, "Spatial"), queen = TRUE)
    z_gi <- spdep::include.self(spdep::poly2nb(as(x, "Spatial"),queen = TRUE))

    # Spatial Weigth Matriz ####
    # Spatial weights for neighbours lists
    swm_hi <- spdep::nb2listw(z_hi, style = "B", zero.policy = TRUE)
    swm_gi <- spdep::nb2listw(z_gi, style = "B", zero.policy = TRUE)

    # Make the functions for calculate the gi ####
    getis_ord <- function(x){
        x[is.na(x)] <- 0
        y <- spdep::localG(x  = (x$n- mean(x$n, na.rm = TRUE))/sd(x$n, na.rm = TRUE),
                           listw = swm_gi,
                           zero.policy = TRUE)
        attributes(y) <- NULL
        y <- as.data.frame(y)
        y
    }

    # The critical values for cutt off of Getis values ####
    ## Bonferroni-corrected z-value (Getis & Ord, 1995)
    getis_ord_umbral <- function(n, alpha){
        round(qnorm(p = -((1-alpha)/n) + 1, mean = 0, sd = 1), digits = 4)
    }
    cutt_off <- getis_ord_umbral(n = nrow(x), alpha = alpha)

    # detect the hotspot based in the cutt_off of benferroni ####
    hotspots_gi <- function(x){
        as.numeric(ifelse(x >= cutt_off, 1, 0))
    }

    ## Step 3.1 Make the functions for calculate the LOSH cs and extract the values ####
    losh_cs <- function(x){
        x[is.na(x)] <- 0
        y <- spdep::LOSH.cs(x  = (x$n- mean(x$n, na.rm = TRUE))/sd(x$n, na.rm = TRUE),
                            listw = swm_hi,
                            zero.policy = TRUE)
        y <- as.data.frame(y)
        y
    }
    extract_losh <- function(x, hi){
        if(hi == TRUE){
            x %>% dplyr::select(1)
        } else {
            x %>% dplyr::select(7)
        }
    }
    hotspot_hi <- function(x){
        y <- x %>% dplyr::mutate(hotspot.hi = ifelse(`Pr()` < 0.05,
                                                     1,
                                                     0))
        y %>% dplyr::select(hotspot.hi)
    }

    if (gi_hi == "gi_hi"){
        # apply the functions for estimate gi ####
        gi <- w %>%
            dplyr::mutate(getis = purrr::map(data, getis_ord)) %>%
            tidyr::unnest(cols = c(data, getis)) %>%
            as.data.frame() %>%
            dplyr::mutate(time = paste("gi", !! rlang::sym(time), sep = "_")) %>%
            tidyr::pivot_wider(id_cols = id,
                               names_from = time,
                               values_from = y)
        hot_gi <- w %>%
            dplyr::mutate(getis = purrr::map(data, getis_ord)) %>%
            dplyr::mutate(hotspots_gi = purrr::map(getis, hotspots_gi)) %>%
            dplyr::select(-getis) %>%
            tidyr::unnest(cols = c(data, hotspots_gi)) %>%
            as.data.frame() %>%
            dplyr::mutate(time = paste("hotspot.gi", !! rlang::sym(time), sep = "_")) %>%
            tidyr::pivot_wider(id_cols = id,
                               names_from = time,
                               values_from = hotspots_gi) %>%
            as.data.frame()

        hot_gi$intensity_gi <- rowSums(hot_gi %>% dplyr::select(-id))
        hot_gi$hotspots_gi <- ifelse(hot_gi$intensity_gi > 0,1,0)


        # apply the functions for estimate hi ####
        losh.cs <- w %>%
            dplyr::mutate(losh_cs = purrr::map(data, losh_cs)) %>%
            dplyr::mutate(hi = purrr::map(losh_cs, extract_losh, hi = TRUE)) %>%
            dplyr::mutate(p_value = purrr::map(losh_cs, extract_losh, hi = FALSE)) %>%
            dplyr::mutate(hotspothi = purrr::map(p_value, hotspot_hi))

        # extract de loss values, column bind ####
        hi <- losh.cs %>%
            dplyr::select(data, hi) %>%
            tidyr::unnest(cols = c(data, hi)) %>%
            as.data.frame() %>%
            dplyr::mutate(time = paste("Hi", !! rlang::sym(time), sep = "_")) %>%
            tidyr::pivot_wider(id_cols = id,
                               names_from = time,
                               values_from = "Hi")

        # extract the pa values of loss ####
        p_values <- losh.cs %>%
            dplyr::select(data,p_value) %>%
            tidyr::unnest(cols = c(data, p_value)) %>%
            as.data.frame() %>%
            dplyr::mutate(time = paste("p_val", !! rlang::sym(time), sep = "_")) %>%
            tidyr::pivot_wider(id_cols = id,
                               names_from = time,
                               values_from = "Pr()")

        # define the hotspots ####
        hot_hi <- losh.cs %>%
            dplyr::select(data, hotspothi) %>%
            tidyr::unnest(cols = c(data, hotspothi)) %>%
            as.data.frame() %>%
            dplyr::mutate(time = paste("hotspot.hi", !! rlang::sym(time), sep = "_")) %>%
            tidyr::pivot_wider(id_cols = id,
                               names_from = time,
                               values_from = "hotspot.hi")

        hot_hi$intensity_hi <- rowSums(hot_hi %>% dplyr::select(-id))
        hot_hi$hotspots_hi <- ifelse(hot_hi$intensity_hi > 0,1,0)

        # we joint the sf object with the dataframe of hotspot of gi and hi ####
        sf:::cbind.sf(x,
                      gi %>% dplyr::select(-id),
                      hot_gi %>% dplyr::select(-id),
                      hi %>% dplyr::select(-id),
                      p_values %>% dplyr::select(-id),
                      hot_hi %>% dplyr::select(-id))[, c(id, "intensity_gi", "hotspots_gi",
                                                         "intensity_hi", "hotspots_hi")]


    } else if(gi_hi == "gi"){

        # apply the functions for estimate gi ####
        gi <- w %>%
            dplyr::mutate(getis = purrr::map(data, getis_ord)) %>%
            tidyr::unnest(cols = c(data, getis)) %>%
            as.data.frame() %>%
            dplyr::mutate(time = paste("gi", !! rlang::sym(time), sep = "_")) %>%
            tidyr::pivot_wider(id_cols = id,
                               names_from = time,
                               values_from = y)
        hot_gi <- w %>%
            dplyr::mutate(getis = purrr::map(data, getis_ord)) %>%
            dplyr::mutate(hotspots_gi = purrr::map(getis, hotspots_gi)) %>%
            dplyr::select(-getis) %>%
            tidyr::unnest(cols = c(data, hotspots_gi)) %>%
            as.data.frame() %>%
            dplyr::mutate(time = paste("hotspot.gi", !! rlang::sym(time), sep = "_")) %>%
            tidyr::pivot_wider(id_cols = id,
                               names_from = time,
                               values_from = hotspots_gi) %>%
            as.data.frame()

        hot_gi$intensity_gi <- rowSums(hot_gi %>% dplyr::select(-id))
        hot_gi$hotspots_gi <- ifelse(hot_gi$intensity_gi > 0,1,0)

        # we joint the sf object with the dataframe of hotspot
        sf:::cbind.sf(x,
                      gi %>% dplyr::select(-id),
                      hot_gi %>% dplyr::select(-id))[, c(id, "intensity_gi", "hotspots_gi")]

    } else if(gi_hi == "hi"){

        # apply the functions ####
        losh.cs <- w %>%
            dplyr::mutate(losh_cs = purrr::map(data, losh_cs)) %>%
            dplyr::mutate(hi = purrr::map(losh_cs, extract_losh, hi = TRUE)) %>%
            dplyr::mutate(p_value = purrr::map(losh_cs, extract_losh, hi = FALSE)) %>%
            dplyr::mutate(hotspothi = purrr::map(p_value, hotspot_hi))

        # extract de loss values, column bind ####
        hi <- losh.cs %>%
            dplyr::select(data, hi) %>%
            tidyr::unnest(cols = c(data, hi)) %>%
            as.data.frame() %>%
            dplyr::mutate(time = paste("Hi", !! rlang::sym(time), sep = "_")) %>%
            tidyr::pivot_wider(id_cols = id,
                               names_from = time,
                               values_from = "Hi")

        # extract the pa values of loss ####
        p_values <- losh.cs %>%
            dplyr::select(data,p_value) %>%
            tidyr::unnest(cols = c(data, p_value)) %>%
            as.data.frame() %>%
            dplyr::mutate(time = paste("p_val", !! rlang::sym(time), sep = "_")) %>%
            tidyr::pivot_wider(id_cols = id,
                               names_from = time,
                               values_from = "Pr()")

        # define the hotspots ####
        hot_hi <- losh.cs %>%
            dplyr::select(data, hotspothi) %>%
            tidyr::unnest(cols = c(data, hotspothi)) %>%
            as.data.frame() %>%
            dplyr::mutate(time = paste("hotspot.hi", !! rlang::sym(time), sep = "_")) %>%
            tidyr::pivot_wider(id_cols = id,
                               names_from = time,
                               values_from = "hotspot.hi")

        hot_hi$intensity_hi <- rowSums(hot_hi %>% dplyr::select(-id))
        hot_hi$hotspots_hi <- ifelse(hot_hi$intensity_hi > 0,1,0)

        # we joint the sf object with the dataframe of hotspot
        sf:::cbind.sf(x,
                      hi %>% dplyr::select(-id),
                      p_values %>% dplyr::select(-id),
                      hot_hi %>% dplyr::select(-id))[, c(id, "intensity_hi", "hotspots_hi")]
    } else {}
}
