#' satscanR
#'
#' This function performs space-time analysis with SaTScan.
#'
#' @param cas_file is the case file (cas for short).
#' @param geo_file is the geographic or coordinates file (geo for short).
#' @param spatial_window is the spatial window. The spatial resolution is in km.
#' @param temporal_window is the temporal window. The time resolution is in days.
#' @param start_date is the earliest date to be included in the study period. Is calculated with min(x_cas$date).
#' @param end_date is the latest date to be included in the study period. Is calculated with max(x_cas$date).
#' @param path_satscan is the path of directory of SatScan installation.
#' @param ssbatchfilename is the name of the file containing the SaTScan executable.
#'
#' @author Felipe Antonio Dzul Manzanilla \email{felipe.dzul.m@gmail.com}.
#'
#' @return a list with eight object (main, col, rr, gis, llr, sci, shapeclust, prm)
#' @seealso \link[rsatscan]{ss.options}, \link[rsatscan]{satscan}
#' @details the stascanR implements ths space-time analysis with [rsatscan](https://cran.r-project.org/web/packages/rsatscan/vignettes/rsatscan.html) package. The rsatscan package only does anything useful if you have installed [SaTScan](https://www.satscan.org/)
#' @export
#'
#' @examples
satscanR <- function(cas_file, geo_file,
                     start_date, end_date,
                     spatial_window,
                     temporal_window,
                     ssbatchfilename = "SaTScanBatch",
                     path_satscan){
    # step 1. reset the option ####
    invisible(rsatscan::ss.options(reset = TRUE))

    #  Step 2. define the input windows ####

    # Step 2.1 Input windows ####
    rsatscan::ss.options(list(CaseFile = "x.cas",
                              CoordinatesFile = "x.geo",
                              PrecisionCaseTimes = 3,
                              StartDate = start_date,
                              EndDate = end_date,
                              CoordinatesType = 1))
    # Step 2.2. Analysis windows ####
    rsatscan::ss.options(list(AnalysisType = 3,
                              ModelType = 2,
                              ScanAreas = 1,
                              TimeAggregationUnits = 3))

    # Step 2.3. Advanced Analysis Features ####

    # Step 2.3.1. Spatial Windows ####
    rsatscan::ss.options(list(UseDistanceFromCenterOption = "y",
                              MaxSpatialSizeInDistanceFromCenter = spatial_window,
                              SpatialWindowShapeType = 0))

    # Step 2.3.2. Temporal Windows ####
    rsatscan::ss.options(list(MaxTemporalSizeInterpretation = 1,
                              MaxTemporalSize = temporal_window))

    # Step 2.3.3. Space and Time Adjustments ####
    #ss.options(list())

    # Step 2.3.4. Inference Windows ####
    rsatscan::ss.options(list(PValueReportType = 1,
                              MonteCarloReps = 999))

    # Step 2.3.5. Power Evaluation Windows ####
    #ss.options(list())



    # Step 2.3. Output Windows ####
    #ss.options(list(ResultsFile = "D:\Users\OneDrive\proyects\satscan\9.output\guadalajara_2020\guadalajara.txt"))
    # Step 3. define the temporal direction #
    td = tempdir()
    rsatscan::write.ss.prm(td, "x")
    rsatscan::write.cas(cas_file, td, "x")
    rsatscan::write.geo(geo_file, td, "x")


    # Step 4. run space-tempo analysis ####
    rsatscan::satscan(prmlocation = td,
                      prmfilename = "x",
                      ssbatchfilename = ssbatchfilename,
                      sslocation = path_satscan)
}
