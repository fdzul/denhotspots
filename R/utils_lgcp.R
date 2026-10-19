# Internal helpers for spatial_lgcp(). They replace the functions of the
# retired packages rgeos, SDraw and raster with sf and terra equivalents.

#' Count cases on a regular lattice clipped to a locality
#'
#' Builds a raster of the given resolution over the locality, counts the
#' points that fall in each cell and returns the cells as polygons in the
#' order expected by the INLA `rw2d` model.
#'
#' @param loc sf polygon of the locality (EPSG:4326).
#' @param pts sf points of the cases.
#' @param resolution cell size in degrees.
#'
#' @return a list with `grid` (sf polygons with columns id, Y, layer and
#'   cellarea, only cells that touch the locality), `nrow`, `ncol` and the
#'   SpatRaster `raster`.
#' @keywords internal
#' @noRd
lgcp_lattice_grid <- function(loc, pts, resolution) {
    r <- terra::rast(terra::vect(loc), resolution = resolution)
    names(r) <- "layer"
    nr <- terra::nrow(r)
    nc <- terra::ncol(r)
    r[] <- 0

    # count the cases by cell
    cells <- terra::cellFromXY(r, sf::st_coordinates(pts)[, 1:2, drop = FALSE])
    tab <- table(cells[!is.na(cells)])
    r[as.numeric(names(tab))] <- as.numeric(tab)

    # one polygon per cell, in cell (row-major) order
    grid <- terra::as.polygons(r, aggregate = FALSE, values = TRUE, na.rm = FALSE) |>
        sf::st_as_sf()
    sf::st_crs(grid) <- sf::st_crs(loc)

    # reorder as in the original implementation (column-major index for rw2d)
    grid <- grid[as.vector(t(matrix(seq_len(nrow(grid)), nrow = nc, ncol = nr))), ]
    grid$id <- seq_len(nrow(grid))
    grid$Y <- grid$layer
    grid$cellarea <- resolution * resolution

    # keep only the cells that touch the locality
    inside <- lengths(sf::st_intersects(grid, sf::st_geometry(loc))) > 0
    grid <- grid[inside, ]

    list(grid = grid, nrow = nr, ncol = nc, raster = r)
}

#' Integration weights of mesh nodes (Simpson et al. 2016)
#'
#' Computes, for every mesh node, the area of its Voronoi tile that lies
#' inside the locality. Areas are planar (in squared degrees), exactly as the
#' former rgeos::gArea() implementation computed them.
#'
#' @param mesh_loc matrix with the coordinates of the mesh nodes (x, y).
#' @param loc sf polygon of the locality.
#'
#' @return numeric vector of weights, one per mesh node, in node order.
#' @keywords internal
#' @noRd
lgcp_voronoi_weights <- function(mesh_loc, loc) {
    xy <- as.matrix(mesh_loc[, 1:2, drop = FALSE])
    nodes <- sf::st_sfc(lapply(seq_len(nrow(xy)), function(i) sf::st_point(xy[i, ])))

    bdy <- sf::st_union(sf::st_set_crs(sf::st_geometry(loc), NA))

    # envelope larger than both the nodes and the locality
    bb <- sf::st_bbox(c(nodes, bdy))
    pad <- max(bb["xmax"] - bb["xmin"], bb["ymax"] - bb["ymin"])
    env <- sf::st_as_sfc(sf::st_bbox(c(xmin = bb[["xmin"]] - pad, ymin = bb[["ymin"]] - pad,
                                       xmax = bb[["xmax"]] + pad, ymax = bb[["ymax"]] + pad)))

    tiles <- sf::st_voronoi(sf::st_union(nodes), envelope = env) |>
        sf::st_collection_extract("POLYGON")

    # st_voronoi does not keep the input order: match each tile to its node
    tiles <- tiles[unlist(lapply(sf::st_intersects(nodes, tiles), `[`, 1))]

    inter <- suppressWarnings(
        sf::st_intersection(sf::st_sf(id = seq_along(tiles), geometry = tiles), bdy)
    )
    w <- numeric(nrow(xy))
    if (nrow(inter) > 0) {
        a <- tapply(as.numeric(sf::st_area(inter)), inter$id, sum)
        w[as.integer(names(a))] <- a
    }
    w
}
