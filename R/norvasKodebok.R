#' Lag og dokumenter kodebok
#'
#' Denne funksjonen leser kodebok i xlsx-format og lager kodebok i Rdata-format.
#'
#'
#' @return En formatert kodebok
#'
#' @export
#'

norvasKodebok <- function() {

  norvasskjemanavn <- c('MedisineringInfusjonsLogg', 'MedisineringHistoriskDose', 'Utredning', 'Vdi', 'VaskulittIntervensjon',
                        'KerrsKriterier', 'SelvrapportertAlvorligInfeksjon', 'Oppfølging', 'Medisinering', 'KomorbidTilstand', 'Diagnose',
                        'Blodprøvesvar', 'Bivirkning', 'Bvas', 'Diagnosekriterier', 'Inklusjon', 'SvarFraPasienten')

  kodebok_norvas <- xlsx::read.xlsx(system.file('extdata', 'kodebok_23062020.xlsx', package = 'norvas'), sheetIndex = 1,
                                    encoding = 'UTF-8', stringsAsFactors = F)
  kodebok_norvas$skjema <- norvasskjemanavn[1]

  for (p in 2:length(norvasskjemanavn)) {
    aux <- xlsx::read.xlsx(system.file('extdata', 'kodebok_23062020.xlsx', package = 'norvas'), sheetIndex = p,
                           encoding = 'UTF-8', stringsAsFactors = F)
    aux$skjema <- norvasskjemanavn[p]
    kodebok_norvas <- rbind(kodebok_norvas, aux)
  }
  kodebok_norvas <- tidyr::separate(data = kodebok_norvas, col = Mulige.verdier, into = c("kode", "label"), sep = " = ")

  save(kodebok_norvas, file = 'C:/GIT/norvas/data/kodebok.RData')

}
