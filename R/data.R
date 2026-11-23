#' Theoretical power-law distributions
#'
#' A dataset containing the Power Laws dataset.
#'
#' @format A dataframe object with 606 rows and 3 variables:
#' \describe{
#'   \item{y}{are the percentiles of the y variable }
#'   \item{x}{are the percentiles of the x variable}
#'   \item{power_law}{are the power distributions}
#'   ...
#' }
#' @source \url{https://www.tandfonline.com/doi/abs/10.1080/00207390802213609}
"power_laws"

#' Simulated geocoded dengue cases of the metropolitan area of Guadalajara
#'
#' **Simulated** geocoded dengue cases (2019-2024) of Guadalajara, Zapopan,
#' San Pedro Tlaquepaque and Tonalá (Jalisco), with the columns of the
#' geocoded dataset of SINAVE. They are not real cases: they were made to show
#' the functions of the package (persistent hotspots, space-time interaction,
#' transmission chains, intensity of cases) without publishing surveillance
#' data.
#'
#' The cases combine three processes: background cases placed in the 400 m H3
#' hexagons of Kontur 2023 (popmex) in proportion to their population; six
#' persistent foci active every year; and transmission chains (secondary cases
#' within 300 m and 3 to 18 days of a primary case). The onset of symptoms
#' follows a seasonal curve with the peak in the epidemiological weeks 36 to
#' 42. The script that makes them is `data-raw/sim_dengue.R`.
#'
#' @format A data frame with 5,502 rows (cases) and 15 columns:
#' \describe{
#'   \item{VEC_ID}{identifier of the case (simulated).}
#'   \item{ANO}{year of onset of symptoms.}
#'   \item{SEM}{epidemiological week of onset of symptoms.}
#'   \item{FEC_INI_SIGNOS_SINT}{date of onset of symptoms.}
#'   \item{IDE_EDA_ANO}{age in years.}
#'   \item{IDE_SEX}{sex (1 man, 2 woman).}
#'   \item{CVE_EDO_RES, DES_EDO_RES}{state of residence (14, Jalisco).}
#'   \item{CVE_MPO_RES, DES_MPO_RES}{municipality of residence.}
#'   \item{ESTATUS_CASO}{status of the case (1 probable, 2 confirmed).}
#'   \item{DES_DIAG_FINAL}{final diagnosis (dengue no grave, con signos de alarma, grave).}
#'   \item{DENGUE_SER_TRIPLEX}{serotype (DENV-1 to DENV-4) of some confirmed cases.}
#'   \item{long, lat}{coordinates of the residence (WGS84).}
#' }
#' @source Simulated with `data-raw/sim_dengue.R`; population of Kontur
#'   (popmex::hdx_pop2023) and localities of INEGI 2019 (rgeomex).
#' @examples
#' head(sim_dengue)
#' table(sim_dengue$ANO)
"sim_dengue"

#' Kaposi's sarcoma in the West Nile district of Uganda
#'
#' Grid coordinates of the homes and year of onset of the 22 patients with
#' Kaposi's sarcoma in the West Nile district of Uganda, 1958-1974. It is the
#' example of Tango (2010, chapter 7) for the tests of space-time clustering,
#' and it is used to check [knox_test()] against his Table 7.7.
#'
#' @format A data frame with 22 rows and 4 columns:
#' \describe{
#'   \item{case}{case number.}
#'   \item{x, y}{eastings and northings of the home (km).}
#'   \item{onset}{year of onset.}
#' }
#' @source Tango T (2010). *Statistical Methods for Disease Clustering*.
#'   Springer, Table 7.1 (data from McHardy et al. 1984).
#' @examples
#' kaposi_uganda
#' knox_test(x = kaposi_uganda, crs = NULL, ds = 2, dt = 0, sym = 999,
#'           sp_link = FALSE, planar_coord = TRUE)$knox
"kaposi_uganda"
