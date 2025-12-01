#' Get units of the variables available in SAFRAN database
#'
#' @return named [character] [vector] containing units for each climatic variable
#' @export
#'
#' @examples
#' get_safran_var_units()
get_safran_var_units <- function() {
  c(
    huss = "kg/kg",
    prtot = "mm/d",
    rlds = "W/m2",
    rsds = "W/m2",
    sfcWind = "m/s",
    tas = "\u00B0C",
    tasmax = "\u00B0C",
    tasmin = "\u00B0C",
    etpOudin = "mm/d"
  )
}
