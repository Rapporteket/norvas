#' Automatisk linjebryting av lange tekstetiketter
#'
#' Denne funksjonen tar som input en vektor med streng-elementer og returnerer
#' en vektor med \n satt inn for linjebrudd
#'
#' @param x En tekststreng eller vektor av tekststrenger
#' @param len Lengden strengen skal brytes ved
#'
#' @return En tekststreng med tekstbrudd på angitt lengde
#'
#' @export
#'

# Core wrapping function
wrap.it <- function (x, len) {
  sapply(
    x,
    function(y) paste(strwrap(y, len), collapse = "\n"),
    USE.NAMES = FALSE
  )
}
