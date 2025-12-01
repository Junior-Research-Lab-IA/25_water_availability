#' Read one SAFRAN CSV file (Inrae/HYCAR/Hydro format)
#'
#' @details
#' This function is use in `data-raw/safran_create_dataset.R` for producing the
#' NetCDF file for the SAFRAN database.
#'
#' @param file [character] File path
#' @param dates A [Date] or [POSIXt] vector of the dates to select
#' @param skip [integer] Number of lines of comments in the header
#'
#' @return A [data.frame]
#' @noRd
#' @import dplyr
#' @importFrom utils read.table
#'
read_safran_csv <- function(file,
                            dates,
                            skip = 34L) {
  df <- read.table(file, sep = ";", skip = skip, header = TRUE)
  dates <- format(dates, "%Y%m%d")
  df <- df %>%
    filter(Date %in% dates) %>%
    mutate(prtot = Psol + Pliq) %>%
    rename(huss = Humi,
           rlds = DLI,
           rsds = SSI,
           sfcWind = Vent,
           tas = Temp,
           tasmax = TX,
           tasmin = TN,
           etpOudin = E_OU) %>%
    select(Date, huss, prtot, rlds, rsds, sfcWind, tas, tasmax, tasmin, etpOudin) %>%
    mutate(huss = units::set_units(huss, "kg/kg"),
           prtot = units::set_units(prtot, "mm/d"),
           rlds = units::set_units(rlds, "J/cm2/d"),
           rsds = units::set_units(rsds, "J/cm2/d"),
           sfcWind = units::set_units(sfcWind, "m/s"),
           tas = units::set_units(tas, "\u00B0C"),
           tasmax = units::set_units(tasmax, "\u00B0C"),
           tasmin = units::set_units(tasmin, "\u00B0C"),
           etpOudin = units::set_units(etpOudin, "mm/d")) %>%
    mutate(rlds = units::set_units(rlds, "W/m2"),
           rsds = units::set_units(rsds, "W/m2"))
  return(df)
}
