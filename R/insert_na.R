#' Title
#'
#' @param x
#' @param nmax
#' @param n
#'
#' @export
#'
insert_na <- function(x, nmax=25, n = sample(1:nmax, 1)) {
  x[sample(seq_along(x), n)] <- NA
  x
}
