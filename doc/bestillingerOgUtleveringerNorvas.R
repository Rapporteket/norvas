# Filtreringer som gjøres for alle utvalg:
# - Inklusjon og Diagnoser: Kun nyeste beholdes
#  - Kun diagnoseskjema med tilhørende inklusjonsskjema beholdes



library(norvas)
library(xtable)
library(lubridate)
library(rapFigurer)
rm(list = ls())

Inklusjon <- read.table('I:/norvas/DataDump_Prod_Inklusjonskjema_2019-08-21_identifisert.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM', colClasses = c('Fødselsnummer'='character'))
Inklusjon_pguid <- read.table('I:/norvas/DataDump_Prod_Inklusjonskjema_2019-08-21.csv', header=TRUE, sep=";",
                              stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Inklusjon$PasientGUID <- Inklusjon_pguid$PasientGUID
Inklusjon <- merge(Inklusjon, Inklusjon_pguid[, c("SkjemaGUID", "PasientGUID")], by ='SkjemaGUID')
Oppfolging <- read.table('I:/norvas/DataDump_Prod_OppfølgingSkjema_2019-08-21.csv', header=TRUE, sep=";",
                         stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Diagnoser <- read.table('I:/norvas/DataDump_Prod_DiagnoseSkjema_2019-08-21.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner <- read.table('I:/norvas/DataDump_Prod_MedisineringSkjema_2019-08-21.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
BVAS <- read.table('I:/norvas/DataDump_Prod_BvasSkjema_2019-08-21.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
KERR <- read.table('I:/norvas/DataDump_Prod_KerrsKriterierSkjema_2019-08-21.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
VDI <- read.table('I:/norvas/DataDump_Prod_VdiSkjema_2019-08-21.csv', header=TRUE, sep=";",
                  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Alvorlig_infeksjon <- read.table('I:/norvas/DataDump_Prod_SelvrapportertAlvorligInfeksjonSkjema_2019-08-21.csv', header=TRUE, sep=";",
                                 stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Utredning <- read.table('I:/norvas/DataDump_Prod_BilledDiagnostikkSkjema_2019-08-21.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Labskjema <- read.table('I:/norvas/DataDump_Prod_BlodprøvesvarSkjema_2019-08-21.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

Inklusjon <- norvasPreprosess(Inklusjon)
Oppfolging <- norvasPreprosess(Oppfolging)
Diagnoser <- norvasPreprosess(Diagnoser)
Medisiner <- norvasPreprosess(Medisiner)
BVAS <- norvasPreprosess(BVAS)
KERR <- norvasPreprosess(KERR)
VDI <- norvasPreprosess(VDI)
Alvorlig_infeksjon <- norvasPreprosess(Alvorlig_infeksjon)
Utredning <- norvasPreprosess(Utredning)
Labskjema <- norvasPreprosess(Labskjema)


Inklusjon <- Inklusjon[order(Inklusjon$InklusjonDato), ]
Inklusjon <- Inklusjon[match(unique(Inklusjon$PasientGUID), Inklusjon$PasientGUID), ]

Diagnoser <- Diagnoser[order(Diagnoser$Diagnose_Klinisk_Dato, decreasing = T), ]
Diagnoser <- Diagnoser[match(unique(Diagnoser$PasientGUID), Diagnoser$PasientGUID), ]

Inklusjon <- merge(Inklusjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr",
                                            "Diag_gr", "Navn")],
                   by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
BVAS <- merge(BVAS, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
BVAS <- merge(BVAS, Inklusjon[, c('SkjemaGUID', "InklusjonDato")], by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
Oppfolging <- merge(Oppfolging, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
KERR <- merge(KERR, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
VDI <- merge(VDI, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
Medisiner <- merge(Medisiner, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
Alvorlig_infeksjon <- merge(Alvorlig_infeksjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)


Inklusjon$Diagnose_ny <- NA
Inklusjon$Diagnose_ny[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 90] <- 1
Inklusjon$Diagnose_ny[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) > 90] <- 0

Inklusjon$Fodselsdato <- personnr2fodselsdato(Inklusjon$Fødselsnummer)
Diagnoser <- merge(Diagnoser, Inklusjon[, c("SkjemaGUID", "Fodselsdato", "InklusjonDato")],
                   by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID')

Inklusjon$Inklusjonsaar <- as.numeric(format(Inklusjon$InklusjonDato, format = '%Y'))
Oppfolging$Oppfolgingsaar <- as.numeric(format(Oppfolging$OppfolgingsDato, format = '%Y'))
Inklusjon$Inklusjonsalder <- age(Inklusjon$Fodselsdato, Inklusjon$InklusjonDato)
Diagnoser$DiagnoseAlder <- age(Diagnoser$Fodselsdato, Diagnoser$Diagnose_Klinisk_Dato)


# Diagnoser <- Diagnoser[Diagnoser$InklusjonDato <= '2018-12-31', ]


Diagnoser <- Diagnoser[Diagnoser$Diagnose_Klinisk_Dato  <= '2018-12-31' | Diagnoser$InklusjonDato <= '2018-12-31', ]

Diagnoser <- Diagnoser[ , c("PasientGUID", "Navn", "Icd", "Diagnose_Klinisk_Dato", "InklusjonDato", "UnitId", "Sykehusnavn"), ]
# length(unique(Diagnoser$PasientGUID))

write.csv2(Diagnoser, 'I:/norvas/diagnoser_npr_2018.csv', row.names = F)

Kobling_norvas_pid_fn <- Inklusjon[Inklusjon$PasientGUID %in% Diagnoser$PasientGUID, c("PasientGUID", "Fødselsnummer")]
names(Kobling_norvas_pid_fn)[2] <- 'Fodselsnummer'

write.csv2(Kobling_norvas_pid_fn, 'I:/norvas/kobling_norvas_npr_2018.csv', row.names = F)

## Utvalg: Pasienter med diagnose- og inklusjonsskjema med enten Diagnose_Klinisk_Dato eller InklusjonDato <= 2018-12-31
##        Hvis flere diagnoser er nyeste brukt. Variabelen Icd er mangelfull så diagnosen må leses fra
##        variabelen Navn.



