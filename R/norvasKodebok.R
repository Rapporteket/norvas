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

  norvasskjemanavn <- c('MedisineringInfusjonsLogg', 'MedisineringHistoriskDose', 'BillegDiagnostikk', 'Vdi', 'VaskulittIntervensjon',
                        'KerrsKriterier', 'SelvrapportertAlvorligInfeksjon', 'Oppfølging', 'Medisinering', 'KomorbidTilstand', 'Diagnose',
                        'Blodprøvesvar', 'Bivirkning', 'Bvas', 'Inklusjon', 'SvarFraPasienten')

  kodebok_norvas <- xlsx::read.xlsx(system.file('extdata', 'kodebok22082018.xlsx', package = 'norvas'), sheetIndex = 1,
                                    encoding = 'UTF-8', stringsAsFactors = F)
  kodebok_norvas$skjema <- norvasskjemanavn[1]

  for (p in 2:length(norvasskjemanavn)) {
    aux <- xlsx::read.xlsx(system.file('extdata', 'kodebok22082018.xlsx', package = 'norvas'), sheetIndex = p,
                           encoding = 'UTF-8', stringsAsFactors = F)
    aux$skjema <- norvasskjemanavn[p]
    kodebok_norvas <- rbind(kodebok_norvas, aux)
  }

  save(kodebok_norvas, file = 'C:/GIT/norvas/data/kodebok.RData')

}
