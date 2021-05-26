###########################################
## Mapping mellom gamle og nye medisinkoder:
mapping_med <- read.table(system.file(file.path('extdata', 'medi_map.csv'), package = 'norvas'), sep=';',
                         stringsAsFactors=FALSE, header=T, fileEncoding = 'latin1')
mapping_med <- tidyr::separate(mapping_med, "Gammel", c("gml_nr", "gml_navn"), sep = " = ") %>%
  tidyr::separate("Ny", c("ny_nr", "ny_navn"), sep = " = ")
mapping_med$ny_navn <- trimws(mapping_med$ny_navn)

usethis::use_data(mapping_med, overwrite = TRUE, internal = FALSE)

###########################################
## Lag ny kodebok:

norvasskjemanavn <- c('Inklusjonskjema', 'OppfølgingSkjema', 'MedisineringSkjema',
                      'BivirkningSkjema', 'KomorbidTilstandSkjema',
                      'VdiSkjema', 'BvasSkjema', 'VaskulittIntervensjonSkjema',
                      'DiagnoseSkjema', 'BlodprøvesvarSkjema', 'Utredning',
                      'SelvrapportertAlvorligInfek', 'KerrsKriterierSkjema', 'MedisineringHistoriskDoseSk',
                      'MedisineringInfusjonsLoggSk', 'Svar fra pasienten', 'Diagnosekriterierskjema')

kodebok_norvas <- readxl::read_excel(system.file(file.path('extdata', 'kodebok_18052021.xlsx'), package = 'norvas'), sheet = 1)
kodebok_norvas$skjema <- norvasskjemanavn[1]

for (p in 2:length(norvasskjemanavn)) {
  aux <- readxl::read_excel(system.file(file.path('extdata', 'kodebok_18052021.xlsx'), package = 'norvas'), sheet = p)
  aux$skjema <- norvasskjemanavn[p]
  kodebok_norvas <- dplyr::bind_rows(kodebok_norvas, aux)
}
kodebok_norvas <- tidyr::separate(data = kodebok_norvas, col = "Mulige verdier", into = c("kode", "label"), sep = " = ")
kodebok_norvas <- kodebok_norvas[, c("Feltnavn", "Variabelnavn", "kode", "label", "Felttype", "Gyldighet", "skjema")]
kodebok_norvas$kode <- as.numeric(kodebok_norvas$kode)

usethis::use_data(kodebok_norvas, overwrite = TRUE, internal = FALSE)






