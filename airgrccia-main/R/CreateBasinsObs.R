#' Create a `BasinsObs` object (Hydroclimatic time series)
#'
#' @param s_sb Climate data (See [get_climate_safran])
#' @param Qobs Observed flows (See [get_hubeau_flows])
#' @inheritParams airGRiwrm::CreateInputsModel.GRiwrm
#' @param Qrelease (optional) [matrix] or [data.frame] of [numeric] containing
#'        release flows by nodes using the model `RunModel_Reservoir` \[m3 per
#'        time step\]
#' @param measures
#'
#' @returns A [list] containing an item`DatesR`, vector of dates corresponding
#' to the time dimension of the data, following by [data.frame]s for each measure
#' including climate data ("Precip", "PotEvap", "TempMean"), and observed
#' flows ("Q").
#' @export
#'
CreateBasinsObs <- function(
  s_sb,
  Qobs = NULL,
  Qinf = NULL,
  Qrelease = NULL,
  measures = c("Precip", "PotEvap", "TempMean")
) {
  BasinsObs <- list(
    DatesR = as.POSIXlt(stars::st_get_dimension_values(s_sb, "time"))
  )
  BasinsObs <- c(
    BasinsObs,
    lapply(setNames(nm = measures), function(x) {
      if (inherits(s_sb[[x]], "units")) {
        s_sb[[x]] <- units::drop_units(s_sb[[x]])
      }
      m <- as.data.frame(t(s_sb[[x]]))
      names(m) <- sf_sb$id
      return(m)
    })
  )
  BasinsObs$DatesR <- as.POSIXlt(stars::st_get_dimension_values(s_sb, "time"))
  if (!is.null(Qobs)) {
    Qobs_DatesR <- Qobs$DatesR
    BasinsObs$Qobs <- Qobs %>% select(-DatesR)
    attr(BasinsObs$Qobs, "DatesR") <- Qobs_DatesR
  }
  if (!is.null(Qinf)) {
    stopifnot(nrow(Qinf) == length(BasinsObs$DatesR))
    BasinsObs$Qinf <- Qinf
  }
  if (!is.null(Qrelease)) {
    stopifnot(nrow(Qrelease) == length(BasinsObs$DatesR))
    BasinsObs$Qrelease <- Qrelease
  }
  class(BasinsObs) <- c("BasinsObs", class(BasinsObs))
  return(BasinsObs)
}
