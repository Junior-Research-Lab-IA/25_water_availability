#' Compute number of workers given available memory and CPUs
#'
#' This function computes the number of workers to use for parallel processing
#' taking into account both constraints that are available RAM and CPUs.
#'
#' @param mem_share [numeric] Share of RAM to use given total RAM and currently
#' used RAM by this R session.
#' @param cpu_share [numeric] Share of CPUs to use given total available CPUs.
#'
#' @returns [integer] Number of workers to use.
#' @export
#'
#' @examples
#' get_future_workers()
get_future_workers <- function(mem_share = 0.8, cpu_share = 0.8) {
  stopifnot(mem_share <= 1, cpu_share <= 1)
  nb_cpu <- floor(parallelly::availableCores() * cpu_share)
  nb_mem <- unclass(get_ram() / pryr::mem_used()) * mem_share
  return(as.integer(min(nb_cpu, nb_mem)))
}
