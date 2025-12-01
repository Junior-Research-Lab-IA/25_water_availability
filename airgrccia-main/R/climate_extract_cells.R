#' Extract cell values from a climate dataset
#'
#' @details
#' SAFRAN cell numbers are compared to numbers from the table available by typing:
#'
#' ```r
#' readr::read_tsv(system.file("extdata", "mailles_safran_drias.tsv", package = "airGRccia"),
#'                 comment = "#",
#'                show_col_types = FALSE)
#' ```
#'
#' @param cells [integer] Number of SAFRAN cells
#' @param s A stars object read by [climate_read]
#' @inheritParams climate_read
#'
#' @return A [list] with one item `date` and one item by variable present in the
#' stars object `s`. The item `date` is a vector and each other item is a data.frame
#' with the SAFRAN cell number as column and one row by time step.
#' @export
#'
climate_extract_cells <- function(
  cells,
  s = climate_read(GCM, RCM, BC, RCP, path),
  GCM,
  RCM,
  BC,
  RCP,
  path = getwd()
) {
  df_cells <- readr::read_tsv(
    system.file("extdata", "mailles_safran_drias.tsv", package = "airGRccia"),
    comment = "#",
    show_col_types = FALSE
  )

  stopifnot(is.numeric(cells), length(cells) > 0, inherits(s, "stars"))

  xy_lambert2 <- df_cells |>
    filter(maille_safran %in% cells) |>
    select(maille_safran, E.lambert2et.m., N.lambert2et.m.)
  cells <- xy_lambert2$maille_safran
  xy_lambert2 <- as.matrix(xy_lambert2[, -1])

  l <- lapply(setNames(nm = names(s)), function(x) {
    df <- as.data.frame(t(stars::st_extract(s[x], xy_lambert2)))
    names(df) <- as.character(cells)
    return(df)
  })
  l$dates <- stars::st_get_dimension_values(s, "time")
  return(l)
}
