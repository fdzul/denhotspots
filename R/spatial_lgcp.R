#' Spatial Log Gaussian Cox Process
#'
#' The spatial_lgcp has the objective of performing the spatial analysis with the log gaussian cox process model in order to predict the intensity of cases in urban and metropolitan endemic dengue locations in Mexico.
#'
#' @param dataset is the dengue geocoded dataset.
#' @param locality is the locality target.
#' @param cve_edo is the text id of the state..
#' @param longitude is the name of the column of the longitude in the geocoded dataset.
#' @param latitude is the name of the column of the latitude in the geocoded dataset.
#' @param k  is the parameter for define the triagulization of delauney in the inner and the outer area in the argument max.edge in the INLA:inla.mesh.2d.
#' @param plot is a logical value for the plot the mesh.
#' @param resolution is a value for set the resolution of the locality raster. resolution 0.1  = 11.132 km, 0.009  = 1.00 km, 0.005  = 500 m, 0.0027  = 300 m, & 0.001 = 100 m.
#' @param aproximation aproximation is the aproximation of the joint posterior of the marginals and hyperparameter. The options are "adaptative", "gaussian", "simplified.laplace" & "laplace".
#' @param integration integration is the integration strategy. The options are "auto","grid", "eb" & "ccd".
#' @param approach is algorithm for spatial Log Gaussian Cox Process. The option are "lattice", "inlabru" & "simpson" according to Illian 2012, Bachl et al 2018 & Simpson et al 2016,respectively.
#' @param cell_size is the sample number per location (area of locality/n)
#' @param name is the name of the palette.
#'
#' @return a list with several object.
#' @export
#'
#' @examples
spatial_lgcp <- function(dataset, locality, cve_edo,
                         longitude, latitude, k, plot, resolution,
                         aproximation, integration,approach,
                         cell_size = NULL, name ){

    # Step 1.1 Generate Boundary ####
    locality <- rgeomex::extract_ageb(locality = locality,
                                      cve_geo = cve_edo)


    if(nrow(locality$locality) > 1){
        loc <- locality$locality %>% sf::st_union()

    } else {
        loc <-  locality$locality
    }

    # Step 1.2 convert the locality sf in raster ####
    r_loc <- raster::raster(x = loc,
                            resolution = resolution)

    # Step 1.3 convert the locality sf in sp ####
    loc_sp <- sf::as_Spatial(from = loc, cast = TRUE)


    # Step 4. spatial point pattern dataset ####
    y <- dataset %>%
        sf::st_as_sf(coords = c(longitude, latitude),
                     crs= 4326)
    y <- y[loc,]

    x <- sf::as_Spatial(from = y, cast = TRUE)

    # Step 4. make the mesh ####
    mesh <- deneggs::mesh(x = dataset,
                          k = k,
                          loc_limit = loc_sp,
                          plot = TRUE,
                          long = longitude,
                          lat = latitude)
    gg_mesh <- ggplot2::ggplot() +
        inlabru::gg(data = mesh) +
        inlabru::gg(data = x,
                    shape = 21,
                    fill = "tomato",
                    alpha = 0.7,
                    color= "white",
                    stroke = 1,
                    lwd = 3) +
        inlabru::gg(loc_sp) +
        ggplot2::coord_fixed() +
        ggplot2::theme_void()

    if(approach == "lattice"){
        (nrow <- nrow(r_loc))
        (ncol <- ncol(r_loc))
        (nrow*ncol)

        # Step 3. We initially set to 0 the values of all the raster cells
        r_loc[] <- 0

        # Step 4. count the dengue cases by cell ####
        tab <- table(raster::cellFromXY(object = r_loc, xy = x))
        r_loc[as.numeric(names(tab))] <- tab

        # Step 5. convert the raster r to a SpatialPolygonsDataFrame ###

        grid <- raster::rasterToPolygons(r_loc)



        # step 6.  add to grid the data needed for modeling ####
        grid <- grid[as.vector(t(matrix(1:nrow(grid), nrow = ncol, ncol = nrow))), ]
        grid$id <- 1:nrow(grid)
        grid$Y <- grid$layer
        grid$cellarea <- resolution*resolution

        # Step 7.  add a climates variables  ###
        # grid$cov <- extract(rcov, coordinates(grid))

        # Step 8. we delete the cells of grid that lie outside locality ####

        gridmap <- raster::intersect(x = grid,
                                     y = sf::as_Spatial(loc)) ## check
        grid <- grid[grid$id %in% gridmap$id, ]

        # Step 9. define the formula ####
        grid$id2 <- grid$id
        formula <- Y ~ 1 +
            f(id, model="rw2d", nrow = nrow, ncol = ncol) +
            f(id2, model="iid")

        # Step 10. Run INLA ####
        res <- INLA::inla(formula,
                          family = "poisson",
                          data = grid@data,
                          E = cellarea,
                          control.predictor = list(compute = TRUE))

        # Step 11. Extract the spatially structured effect ####
        grid$respa <- res$summary.random$id[grid$id, "mean"]


        # Step 12. Extract the posterior mean of the unstructured random effect ####
        grid$reiid <- res$summary.random$id2[, "mean"]


        # Step 13. The mean and quantiles of the predicted intensity ####
        cellarea <- resolution*resolution
        grid$intensity <- round(res$summary.fitted.values[, "mean"] * cellarea, digits = 1)
        grid$LL <- round(res$summary.fitted.values[, "0.025quant"] * cellarea, digits = 1)
        grid$UL <- round(res$summary.fitted.values[, "0.975quant"] * cellarea, digits = 1)
        grid <- raster::crop(x = grid,
                             y = sf::as_Spatial(from = loc,
                                                cast = TRUE))
        ###
        y <- sf::st_as_sf(grid)
        map <- plotly::ggplotly(ggplot2::ggplot() +
                                    ggplot2::geom_sf(data = y,
                                                     ggplot2::aes(fill = intensity),
                                                     colour = "white",
                                                     lwd = 0.01) +
                                    ggplot2::geom_tile() +
                                    ggplot2::geom_sf(data = loc,
                                                     fill = NA,
                                                     col = "black",
                                                     lwd = .3) +
                                    ggplot2::scale_fill_distiller("Casos",
                                                                  palette = name,
                                                                  direction = 1) +
                                    cowplot::theme_map())

        ## Step 9. return the map and the prediction values ####
        multi_return <- function() {
            my_list <- list("data" = x,
                            "pred" = grid,
                            "loc" = loc,
                            "map" = map)
            return(my_list)
        }
        multi_return()

    } else if(approach == "inlabru"){

        # Step 4. Define SPDE prior ####
        matern <- INLA::inla.spde2.pcmatern(mesh,
                                            prior.sigma = c(0.1, 0.01),
                                            prior.range = c(0.01, 0.01))
        # Step 5. define the formula ####
        cmp <- coordinates ~ mySmooth(main = coordinates,
                                      model = matern) + Intercept

        # Step 5. fit the model ####
        mod <- inlabru::lgcp(components = cmp,
                             data = x,
                             samplers = loc_sp,
                             domain = list(coordinates = mesh),
                             options = list(control.inla = list(strategy = aproximation,
                                                                int.strategy = integration),
                                            control.compute = list(dic = TRUE,
                                                                   waic = TRUE)))
        # Step 6. Predict the spatial intensity surface ####
        lambda <- predict(mod, inlabru::pixels(mesh,
                                               mask = loc_sp), ~ exp(mySmooth + Intercept))

        # step 7.
        lambda_sf <- lambda@data  %>%
            dplyr::mutate(x = lambda@coords[,1],
                          y = lambda@coords[,2]) %>%
            sf::st_as_sf(coords = c("x", "y"),
                         crs= 4326)
        lambda_sf <- lambda_sf[locality$locality, ]

        # Step 8.
        map <- plotly::ggplotly(ggplot2::ggplot() +
                                    inlabru::gg(lambda) +
                                    ggplot2::scale_fill_distiller("Casos",
                                                                  palette = name,
                                                                  direction = 1) +
                                    cowplot::theme_map())

        # Step 9. return the map and the prediction values ####
        multi_return <- function() {
            my_list <- list("data" = y,
                            "gg_mesh" = gg_mesh,
                            "spp" = x,
                            "lambda_sp" = lambda,
                            "lambda_sf" = lambda_sf,
                            "locality" = locality,
                            "loc_sp" = loc_sp,
                            "mod" = mod,
                            "mesh" = mesh,
                            "map" = map,
                            "dics" = mod$dic$dic)
            return(my_list)
        }
        multi_return()


    } else if(approach == "simpson"){
        # Step 1. define the mesh ####

        # Step 2. dataset of location of points ####
        pts <- as.matrix(sf::st_coordinates(y))

        # Step 3. dataset of the integration points in the mesh ####

        mesh.pts <- as.matrix(mesh$loc[, 1:2])

        # Step 4. expanded dataset

        allpts <- rbind(pts, mesh.pts)

        # Step 5. create the vertices and n points ####
        # Number of vertices in the mesh
        nv <- mesh$n
        # Number of points in the data
        n <- nrow(pts)


        # Step 6. Create SPDE ####
        spde <- INLA::inla.spde2.pcmatern(mesh = mesh,
                                          alpha = 2,
                                          prior.range = c(50, 0.9), # P(range < 50) = 0.9
                                          prior.sigma = c(1, 0.01)) # P(sigma > 10) = 0.01
        spde <- INLA::inla.spde2.matern(mesh = mesh,
                                        alpha = 2,
                                        constr = TRUE)

        # Step 7. calculate the weights associated with the mesh points ####

        # Step 7.1  Voronoi polygons (as SpatialPolygons)
        mytiles <- SDraw::voronoi.polygons(sp::SpatialPoints(mesh$loc[, 1:2]))


        # Step 7.2 bo unday as SpatialPolygons ####

        bdy.sp <- loc_sp

        # Step 7.3 Compute weights ####

        w <- sapply(1:length(mytiles), function(p) {
            aux <- mytiles[p, ]

            if(rgeos::gIntersects(aux, bdy.sp) ) {
                return(rgeos::gArea(rgeos::gIntersection(aux, bdy.sp)))
            } else {
                return(0)
            }
        })

        sum(w)
        rgeos::gArea(bdy.sp)

        #Prepare data
        y.pp = rep(0:1, c(nv, n))
        e.pp = c(w, rep(0, n))

        lmat <- INLA::inla.spde.make.A(mesh, pts)
        imat <- Matrix::Diagonal(nv, rep(1, nv))


        # projector matrix we use for modeling ####
        A_mod <- rbind(imat, lmat)

        ## Step 4. Define the Spatial Field (w) ####
        w.index <- INLA::inla.spde.make.index(name = "w",
                                              n.spde = spde$n.spde)

        ## Step 5. Define the stack ####

        ## Step 5.1 Define the stack for the modelling ####
        stack_mod <- INLA::inla.stack(data = list(y = y.pp,
                                                  e = e.pp),
                                      A = list(A_mod, 1),
                                      effects = list(w = w.index,
                                                     Intercept = rep(1, nv + n)),
                                      tag = "fit")
        # Step 5.2 Define the stack for the prediction ####
        # 3.2.2 extract the coordinates of grid point prediction #####

        #p <- deneggs::loc_grid_points(sf = loc, cell_size = cell_size)
        p <- sf::st_sample(x = loc, size = cell_size, type = "regular") |>
            sf::st_as_sf()

        #p <- raster::coordinates(r_loc)

        # 3.2.3 make the projector matrix for use prediction ####
        A_pred <- INLA::inla.spde.make.A(mesh = mesh,
                                         loc =  sf::st_coordinates(p))

        ## Step 5.2 Define the stack for the prediction ####
        stack_pred <- INLA::inla.stack(tag  = "pred",
                                       data   = list(y = NA),
                                       A      = list(A_pred, 1),
                                       effect = list(w         = w.index,
                                                     Intercept = rep(1, nrow(sf::st_coordinates(p)))))

        ## Step 5.3 bind the each stack ####
        stack_full <- INLA::inla.stack(stack_mod, stack_pred)

        # Step  define the formula
        formula <-  y ~ 0 + Intercept +  f(w,
                                           #hyper = hyper1,
                                           model = spde)
        # Step . run the models ####

        mod <- INLA::inla(formula,
                          family = "poisson",
                          data = INLA::inla.stack.data(stack_full),
                          control.predictor = list(A = INLA::inla.stack.A(stack_full),
                                                   compute = TRUE,
                                                   link = 1),
                          control.compute = list(dic = TRUE,
                                                 waic = TRUE),
                          control.inla = list(strategy = aproximation,
                                              int.strategy = integration),
                          E = INLA::inla.stack.data(stack_full)$e)

        ##############

        ## Step 8.1 extract the index of predictions ####
        index <- INLA::inla.stack.index(stack = stack_full, tag = "pred")$data

        ## Step 8.2. extract the predictions ####
        p <- data.frame(sf::st_coordinates(p))

        names(p) <- c("x", "y")

        p$pred_mean <- mod$summary.fitted.values[index, "mean"]
        p$pred_sd <- mod$summary.fitted.values[index, "sd"]
        p$pred_ll <- mod$summary.fitted.values[index, "0.025quant"]
        p$pred_ul <- mod$summary.fitted.values[index, "0.025quant"]
        p$year <- unique(lubridate::year(y$onset))

        p <- p %>%
            dplyr::mutate(X = x,
                          Y = y) %>%
            sf::st_as_sf(coords = c("X", "Y"),
                         crs = 4326)
        p_sf <- p[locality$locality,]
        p <- p_sf %>%
            sf::st_drop_geometry()

        map <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = p,
                               ggplot2::aes(fill = pred_mean,
                                            x = x,
                                            y = y)) +
            ggplot2::scale_fill_distiller("Casos",
                                          palette = name,
                                          direction = 1) +
            cowplot::theme_map()

        # Step 8. return the map and the prediction values ####
        multi_return <- function() {
            my_list <- list("data" = y,
                            "pred_df" = p,
                            "pred_sf" = p_sf,
                            "gg_mesh" = gg_mesh,
                            "map" = map,
                            "spp" = x,
                            "locality" = locality,
                            "loc_sp" = loc_sp,
                            "loc_raster" = r_loc,
                            "mod" = mod,
                            "mesh" = mesh)
            return(my_list)
        }
        multi_return()

    } else {

    }
}
