make_locality <- function() {
    sf::st_sf(geometry = sf::st_sfc(sf::st_polygon(list(rbind(
        c(-99.30, 18.85), c(-99.15, 18.84), c(-99.12, 18.98),
        c(-99.25, 19.02), c(-99.30, 18.85)))), crs = 4326))
}

test_that("lgcp_lattice_grid counts every case once", {
    skip_if_not_installed("terra")
    set.seed(1)
    loc <- make_locality()
    pts <- sf::st_as_sf(sf::st_sample(loc, 150))
    lat <- lgcp_lattice_grid(loc, pts, resolution = 0.01)

    expect_s3_class(lat$grid, "sf")
    expect_equal(sum(lat$grid$Y), 150)
    expect_equal(lat$grid$Y, lengths(sf::st_intersects(lat$grid, pts)))
    expect_true(all(lat$grid$cellarea == 0.01^2))
})

test_that("lgcp_lattice_grid uses the column-major index of rw2d", {
    skip_if_not_installed("terra")
    set.seed(2)
    loc <- make_locality()
    pts <- sf::st_as_sf(sf::st_sample(loc, 50))
    lat <- lgcp_lattice_grid(loc, pts, resolution = 0.02)

    xy <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(lat$grid)))
    rc <- terra::rowColFromCell(lat$raster, terra::cellFromXY(lat$raster, xy))
    expect_equal(lat$grid$id, (rc[, 2] - 1) * lat$nrow + rc[, 1])
})

test_that("lgcp_voronoi_weights add up to the area of the locality", {
    loc <- make_locality()
    nodes <- as.matrix(expand.grid(seq(-99.40, -99.02, length.out = 10),
                                   seq(18.75, 19.10, length.out = 10)))
    w <- lgcp_voronoi_weights(nodes, loc)
    area <- as.numeric(sf::st_area(sf::st_set_crs(sf::st_geometry(loc), NA)))

    expect_length(w, nrow(nodes))
    expect_true(all(w >= 0))
    expect_equal(sum(w), area, tolerance = 1e-8)
})
