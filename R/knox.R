#' Knox test of space-time interaction
#'
#' @description Tests whether the cases are closer in space and time than
#'   expected if space and time were independent (Knox 1964), and returns the
#'   pairs of cases close in both (the space-time links that form the
#'   transmission chains).
#'
#' @param x a data frame with the coordinates of the cases (`x`, `y`) and the
#'   date of onset of symptoms (`onset`, a `Date` or a number).
#' @param crs the projected coordinate reference system used to measure the
#'   distances (for example `"+proj=eqc"`, in metres).
#' @param dt the time window: two cases are close in time when their onsets
#'   are at most `dt` apart (days if `onset` is a `Date`).
#' @param ds the space window: two cases are close in space when they are at
#'   most `ds` apart (units of `crs`, or of the coordinates if
#'   `planar_coord = TRUE`).
#' @param sym the number of Monte Carlo permutations.
#' @param sp_link logical. If `TRUE` the space-time links are built with sp,
#'   else with sf.
#' @param planar_coord logical. If `FALSE`, `x` and `y` are longitude and
#'   latitude (EPSG:4326) and are projected to `crs`; if `TRUE`, they are
#'   already planar coordinates.
#' @return A list with four elements:
#'   - `knox`, a list with the test:
#'     \describe{
#'       \item{knox}{Knox statistic: number of pairs of cases close in space and time.}
#'       \item{p_value}{Monte Carlo p-value, \eqn{(m + 1)/(sym + 1)}, where \eqn{m} is
#'         the number of permutations of the onsets with at least `knox` pairs.}
#'       \item{RR}{observed pairs divided by the mean of the permutations.}
#'       \item{expected}{expected pairs, `pairs_space * pairs_time / pairs` (Knox 1964; Tango 2010).}
#'       \item{RR_expected}{observed pairs divided by `expected`.}
#'       \item{p_poisson}{Poisson mid-p-value of `knox` with mean `expected`, as in Tango (2010).}
#'       \item{pairs}{total number of pairs of cases, \eqn{n(n - 1)/2}.}
#'       \item{pairs_space}{pairs of cases close in space.}
#'       \item{pairs_time}{pairs of cases close in time.}
#'     }
#'   - `st_link`, a data frame with one row per space-time link and the
#'     coordinates of its two cases (`Xo`, `Yo`, `Xd`, `Yd`).
#'   - `x`, the cases with the planar coordinates.
#'   - `space_time_link`, an sf object (LINESTRING, EPSG:4326) with the
#'     space-time links.
#' @export
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}
#' @seealso [kaposi_uganda] for the example of Tango (2010), and
#'   [transmission_chains_map()].
#' @references Knox EG (1964). The detection of space-time interactions.
#'   *Journal of the Royal Statistical Society, Series C*, 13(1), 25-30.
#'
#'   Tango T (2010). *Statistical Methods for Disease Clustering*. Springer.
#' @details Two cases are close in space when their distance is at most `ds`,
#'   and close in time when their onsets are at most `dt` apart, as in the
#'   tables of Tango (2010). The test gives two p-values: the Monte Carlo
#'   p-value (the onsets are permuted among the cases `sym` times), and the
#'   Poisson mid-p-value used by Knox (1964) and Tango (2010), which is an
#'   approximation that can be misleading when the expected number of pairs is
#'   not small (Barton and David 1966).
#' @examples
#' # Kaposi's sarcoma in Uganda (Tango 2010, Table 7.7): 5 km and same year
#' k <- knox_test(x = kaposi_uganda, crs = NULL, ds = 5, dt = 0, sym = 999,
#'                sp_link = FALSE, planar_coord = TRUE)
#' k$knox
knox_test <- function(x, crs, dt, ds, sym, sp_link, planar_coord){
    # step 1. project the coordinates ####
    if(planar_coord == FALSE){
        x <- x |>
            sf::st_as_sf(coords = c("x", "y"),
                         crs = 4326) |>
            sf::st_transform(crs = crs) |>
            dplyr::mutate(x = sf::st_coordinates(geometry)[,1],
                          y = sf::st_coordinates(geometry)[,2]) |>
            sf::st_drop_geometry()
    }

    # step 2. Knox test (Monte Carlo and Poisson) ####
    set.seed(123456)
    res <- .knox_core(x = x$x, y = x$y, time = x$onset,
                      ds = ds, dt = dt, sym = sym)
    knox <- res$stat

    # step 3. the origin-destination dataset of the space-time links ####
    st_link <- data.frame(Xo = x$x[res$i], Yo = x$y[res$i],
                          Xd = x$x[res$j], Yd = x$y[res$j])

    # step 4. built the space-time link ####
    if(is.null(crs)){
        space_time_link <- NULL
    } else if(sp_link == TRUE){
        rlang::check_installed("sp", reason = "to use `sp_link = TRUE`.")
        l <- list()
        for (i in seq_len(nrow(st_link))) {
            l[[i]] <- with(st_link[i,],sp::Lines(sp::Line(cbind(c(Xo,Xd),
                                                                c(Yo,Yd))),
                                                 ID = i))
        }
        id <- data.frame(id = seq_len(nrow(st_link)))
        shape <- sp::SpatialLines(l)
        shape.line.df <- sp::SpatialLinesDataFrame(shape,id,match.ID = TRUE)
        space_time_link <- sf::st_as_sf(shape.line.df) |>
            sf::st_set_crs(crs) |>
            sf::st_transform(crs = 4326)
    } else{
        lines <- lapply(seq_len(nrow(st_link)), function(k) {
            sf::st_linestring(matrix(c(st_link$Xo[k], st_link$Xd[k],
                                       st_link$Yo[k], st_link$Yd[k]), ncol = 2))
        })
        space_time_link <- sf::st_sf(id = seq_len(nrow(st_link)),
                                     x = sf::st_sfc(lines, crs = crs)) |>
            sf::st_transform(4326)
    }

    # step 5. return the knox test, o-d dataset and space-time link ####
    list("knox" = knox,
         "st_link" = st_link,
         "x" = x,
         "space_time_link" = space_time_link)
}

# Knox statistic, Monte Carlo and Poisson p-values, and the pairs of cases
# close in space and time. Only the pairs close in space are kept (a spatial
# index finds them), so the permutations do not need the n x n matrices.
.knox_core <- function(x, y, time, ds, dt, sym) {
    time <- as.numeric(time)
    n <- length(time)
    pts <- sf::st_as_sf(data.frame(x = x, y = y), coords = c("x", "y"))
    nb <- sf::st_is_within_distance(pts, pts, dist = ds)
    i <- rep(seq_len(n), lengths(nb))
    j <- unlist(nb, use.names = FALSE)
    keep <- i < j
    i <- i[keep]
    j <- j[keep]

    pairs <- n * (n - 1) / 2
    pairs_space <- length(i)
    ts <- sort(time)
    pairs_time <- (sum(findInterval(ts + dt, ts) -
                           findInterval(ts - dt, ts, left.open = TRUE)) - n) / 2

    close <- abs(time[i] - time[j]) <= dt
    obs <- sum(close)
    sims <- vapply(seq_len(sym), function(k) {
        tp <- sample(time)
        sum(abs(tp[i] - tp[j]) <= dt)
    }, numeric(1))
    expected <- pairs_space * pairs_time / pairs

    list(stat = list(knox = obs,
                     p_value = (sum(sims >= obs) + 1) / (sym + 1),
                     RR = obs / mean(sims),
                     expected = expected,
                     RR_expected = obs / expected,
                     p_poisson = stats::ppois(obs, expected, lower.tail = FALSE) +
                         0.5 * stats::dpois(obs, expected),
                     pairs = pairs,
                     pairs_space = pairs_space,
                     pairs_time = pairs_time),
         i = i[close], j = j[close])
}


#' @rdname knox_test
#' @param ... arguments passed to `knox_test()`.
#' @details `knox()` is the former name of `knox_test()`; it still works but it
#'   is deprecated, because it has the same name as the function of the knox
#'   package.
#' @export
knox <- function(...) {
    .Deprecated("knox_test", package = "denhotspots")
    knox_test(...)
}
