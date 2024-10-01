#' Funksjon som gjør utvalg av dataene, returnerer det reduserte datasettet og utvalgsteksten.
#'
#' @inheritParams norvasFigAndeler
#' @param fargepalett Hvilken fargepalett skal brukes i figurer (Default: BlaaRapp)
#'
#' @return UtData En liste bestående av det filtrerte datasettet,
#'                utvalgstekst for figur og tekststreng som angir fargepalett
#'
#' @export

norvasUtvalg <- function(RegData,
                         datoFra = "2000-01-01",
                         datoTil = "2100-01-01",
                         datovar='InklusjonDato',
                         minald = 0,
                         maxald = 130,
                         aldervar='PatientAge',
                         erMann = 99,
                         valgtShus='',
                         fargepalett='BlaaRapp',
                         diag_gruppe=99)
{
  # Definerer intersect-operator
  "%i%" <- intersect

  Ninn <- dim(RegData)[1]
  indVarMed <- 1:Ninn
  indAld <- if ((minald>0) | (maxald<130)) {which(RegData[, aldervar] >= minald & RegData[, aldervar] <= maxald)} else {1:Ninn}
  indDato <- which(RegData[, datovar] >= datoFra & RegData[, datovar] <= datoTil)
  indKj <- if (erMann %in% 0:1) {which(RegData$ErMann == erMann)} else {indKj <- 1:Ninn}
  indDiaggr <- if (diag_gruppe %in% 1:3) {which(RegData$Diag_gr_nr == diag_gruppe)} else {indDiaggr <- 1:Ninn}

  indMed <- indVarMed %i% indAld %i% indDato %i% indKj %i% indDiaggr
  RegData <- RegData[indMed,]

  utvalgTxt <- c(paste0(datovar, ': ', min(RegData[, datovar], na.rm=T), ' til ', max(RegData[, datovar], na.rm=T)),
                 if ((minald>0) | (maxald<130)) {
                   paste0('Pasienter fra ', min(RegData$Alder, na.rm=T), ' til ', max(RegData$Alder, na.rm=T), ' år')},
                 if (erMann %in% 0:1) {paste('Kjønn: ', c('Kvinner', 'Menn')[erMann+1], sep='')},
                 if (diag_gruppe %in% 1:3) {paste0('Diagnosegruppe: ', c('Storkarsvaskulitt (LVV)',
                                                                         'ANCA assosiert vaskulitt (AAV)', 'Andre')[diag_gruppe])},
                 if (length(valgtShus)>1) {paste0('Valgte RESH: ', paste(as.character(valgtShus), collapse=', '))}
  )

  UtData <- list(RegData=RegData, utvalgTxt=utvalgTxt, fargepalett=fargepalett) #GronnHNpms624,
  return(invisible(UtData))
}

