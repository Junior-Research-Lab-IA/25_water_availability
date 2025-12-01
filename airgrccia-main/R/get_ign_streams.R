#' Get streams from IGN
#'
#' This function retrieves hydrographic streams from the IGN WFS service and
#' filter them based on specified criteria.
#'
#' @param x A bounding box, a simple feature collection or a simple feature geometry.
#' @param ... Further parameters passed to methods and to [happign::get_wfs]
#'
#' @returns A [`sf`][sf::st_sf] object containing the streams data.
#' @rdname get_ign_streams
#' @export
#'
#' @examples
#' bbox <- c(xmin = 325522.8, ymin = 6250949.2, xmax = 347137.1, ymax = 6270285.1)
#' # Retrieve only river streams that have a name and of specific nature
#' streams <- get_ign_streams(
#'   sf::st_bbox(bbox, crs = 2154),
#'   filters = list(
#'     nature = c("Ecoulement naturel", "Ecoulement canalis\u00E9"),
#'     cpx_toponyme_de_cours_d_eau = not.is.na
#'   )
#' )
#' streams
#' mapview::mapview(streams)
get_ign_streams <- function(x, ...) {
  UseMethod("get_ign_streams", x)
}

#' @rdname get_ign_streams
#' @export
get_ign_streams.sf <- function(x, ...) {
  get_ign_streams(sf::st_as_sfc(x), ...)
}

#' @rdname get_ign_streams
#' @export
get_ign_streams.bbox <- function(x, ...) {
  get_ign_streams(sf::st_as_sfc(x), ...)
}

#' @inheritParams happign::get_wfs
#' @param filters A list of filters to apply to the streams data (See details)
#' @param filename [character] Optional. If provided, the streams geometry will
#' be saved to this file and can be used as cache if `overwrite = FALSE`.
#' @param crs The CRS to use for the output streams (the CRS of `x` is used by default).
#'
#' @details
#' List of filters is structured as follow: name correspond to the column names
#' in the streams data, and values can be:
#' - A character vector to filter by specific values
#' - A function that takes a vector and returns a logical vector (e.g., `not.is.na`)
#' `not.is.na` is equivalent of `!is.na()`.
#'
#' @rdname get_ign_streams
#' @export
get_ign_streams.sfc <- function(
  x,
  layer = "BDCARTO_V5:troncon_hydrographique",
  crs = sf::st_crs(x),
  filters = NULL,
  filename = NULL,
  overwrite = FALSE,
  ...
) {
  if (!overwrite && !is.null(filename) && file.exists(filename)) {
    message("Using cached file: ", filename)
    streams <- sf::read_sf(filename)
  } else {
    streams <- happign::get_wfs(x, layer = layer, ...)
    for (fltr in names(filters)) {
      if (is.function(filters[[fltr]])) {
        streams <- streams %>% filter(filters[[fltr]](get(fltr)))
      } else if (is.character(filters[[fltr]])) {
        streams <- streams %>% filter(get(fltr) %in% filters[[fltr]])
      } else {
        stop("Unsupported filter type for ", fltr)
      }
    }
    # Dictionary: Original column names => Shapefile-compatible names
    shp_names <- c(
      id = "id",
      cleabs = "cleabs",
      etat_de_l_objet = "etat_objet",
      code_hydrographique = "cd_hydro",
      code_du_pays = "cd_pays",
      nature = "nature",
      fictif = "fictif",
      position_par_rapport_au_sol = "pos_sol",
      statut = "statut",
      persistance = "persist",
      navigabilite = "navigab",
      salinite = "salinite",
      origine = "origine",
      sens_de_l_ecoulement = "sens_ecoul",
      reseau_principal_coulant = "rs_princ",
      delimitation = "delim",
      classe_de_largeur = "cls_larg",
      type_de_bras = "type_bras",
      code_du_cours_d_eau_bdcarthage = "cd_carteau",
      cpx_toponyme_de_cours_d_eau = "topo_ce",
      cpx_toponyme_d_entite_de_transition = "topo_trans",
      lien_vers_entite_de_transition = "lien_trans",
      liens_vers_cours_d_eau = "liens_ce",
      geometry = "geometry" # Keep geometry as is
    )
    # Rename columns to match shapefile standards
    names(streams) <- shp_names[names(streams)]
    streams <- sf::st_transform(streams, crs)
    if (!is.null(filename)) {
      if (file.exists(filename)) {
        lapply(
          list.files(
            path = dirname(filename),
            pattern = basename(tools::file_path_sans_ext(filename)),
            full.names = TRUE
          ),
          unlink,
          force = TRUE
        )
      }
      sf::write_sf(streams, filename, append = FALSE)
    }
  }
  return(streams)
}

#' Shortcut for `!is.na`
#'
#' @param x an R object to be tested
#' @returns [logical] See [is.na].
#' @export
not.is.na <- function(x) {
  !is.na(x)
}
