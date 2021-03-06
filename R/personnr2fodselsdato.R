#' Trekk ut fødselsdato fra personnummer
#'
#' @param pnr Norsk fødselsnummer enten som skalar eller vektor
#'
#' @return fodselsdato Fødselsdato i datoformat
#'
#' @export
#'
personnr2fodselsdato <- function(pnr) {

  fodselsdato <- as.character(pnr)
  iaar <- as.numeric(substr(Sys.Date(), 3,4))
  if (length(pnr)==1) {
    if (nchar(fodselsdato)==10) {fodselsdato <-  paste0('0', fodselsdato)}
    fodselsAar <- substr(fodselsdato, 5, 6)

    fodselsAarFull <- NA

    if (fodselsAar < iaar) {
      fodselsAarFull <- paste0('20', fodselsAar)
    } else {
      fodselsAarFull <- paste0('19', fodselsAar)
    }
    fodselsdato <- as.Date(paste0(fodselsAarFull, '-', substr(fodselsdato, 3, 4), '-', substr(fodselsdato, 1, 2)))
  } else {
    fodselsdato[nchar(fodselsdato)==10] <- paste0('0', fodselsdato[nchar(fodselsdato)==10])
    fodselsAar <- substr(fodselsdato, 5, 6)
    fodselsAarFull <- NA
    fodselsAarFull[as.numeric(fodselsAar) < iaar] <-
      paste0('20', fodselsAar[as.numeric(fodselsAar) < iaar])
    fodselsAarFull[as.numeric(fodselsAar) >= iaar] <-
      paste0('19', fodselsAar[as.numeric(fodselsAar) >= iaar])
    fodselsdato <- as.Date(paste0(fodselsAarFull, '-', substr(fodselsdato, 3, 4), '-', substr(fodselsdato, 1, 2)))
  }
}
