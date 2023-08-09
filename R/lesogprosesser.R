#' Les og preprosesser data for bruk i Norvas sine rapporter
#'
#' Denne funksjonen gjor nodvendig preprosessering av Norvas sin data for bruk i rapporter
#'
#' @return Ferdig prosessert data for Norvas
#'
#' @export
#'
lesogprosesser <- function(rap_aar = 2022) {
  Inklusjon <- read.table(
    '~/softlinks/mydata/norvas/datadump_aarsrapp2022/DataDump_MRS-PROD_Inklusjonskjema_2023-08-03_1009.csv',
    header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Oppfolging <- read.table(
    '~/softlinks/mydata/norvas/datadump_aarsrapp2022/DataDump_MRS-PROD_OppfølgingSkjema_2023-08-03_1011.csv',
    header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Diagnoser <- read.table(
    '~/softlinks/mydata/norvas/datadump_aarsrapp2022/DataDump_MRS-PROD_DiagnoseSkjema_2023-08-03_1012.csv',
    header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Medisiner <- read.table(
    '~/softlinks/mydata/norvas/datadump_aarsrapp2022/DataDump_MRS-PROD_MedisineringSkjema_2023-08-03_1011.csv',
    header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  BVAS <- read.table(
    '~/softlinks/mydata/norvas/datadump_aarsrapp2022/DataDump_MRS-PROD_BvasSkjema_2023-08-03_1011.csv',
    header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  KERR <- read.table(
    '~/softlinks/mydata/norvas/datadump_aarsrapp2022/DataDump_MRS-PROD_KerrsKriterierSkjema_2023-08-03_1012.csv',
    header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  VDI <- read.table(
    '~/softlinks/mydata/norvas/datadump_aarsrapp2022/DataDump_MRS-PROD_VdiSkjema_2023-08-03_1011.csv',
    header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Alvorlig_infeksjon <- read.table(
    '~/softlinks/mydata/norvas/datadump_aarsrapp2022/DataDump_MRS-PROD_SelvrapportertAlvorligInfeksjonSkjema_2023-08-03_1012.csv',
    header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Utredning <- read.table(
    '~/softlinks/mydata/norvas/datadump_aarsrapp2022/DataDump_MRS-PROD_Utredning_2023-08-03_1012.csv',
    header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Labskjema <- read.table(
    '~/softlinks/mydata/norvas/datadump_aarsrapp2022/DataDump_MRS-PROD_BlodprøvesvarSkjema_2023-08-03_1012.csv',
    header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

  # Inklusjon <- read.table(
  #   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_Inklusjonskjema_2023-04-19_0919.csv',
  #   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  # Oppfolging <- read.table(
  #   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_OppfølgingSkjema_2023-04-19_0919.csv',
  #   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  # Diagnoser <- read.table(
  #   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2023-04-19_0920.csv',
  #   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  # Medisiner <- read.table(
  #   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_MedisineringSkjema_2023-04-19_0919.csv',
  #   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  # BVAS <- read.table(
  #   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_BvasSkjema_2023-04-19_0920.csv',
  #   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  # KERR <- read.table(
  #   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_KerrsKriterierSkjema_2023-04-19_0920.csv',
  #   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  # VDI <- read.table(
  #   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_VdiSkjema_2023-04-19_0919.csv',
  #   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  # Alvorlig_infeksjon <- read.table(
  #   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_SelvrapportertAlvorligInfeksjonSkjema_2023-04-19_0920.csv',
  #   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  # Utredning <- read.table(
  #   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_Utredning_2023-04-19_0920.csv',
  #   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  # Labskjema <- read.table(
  #   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_BlodprøvesvarSkjema_2023-04-19_0920.csv',
  #   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

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

  ### Ny 18.10.2021: Fjerner medikamenter i kategorien "Andre"
  Medisiner <- Medisiner[!(Medisiner$LegemiddelNr %in% c(0, 999)), ]
  ###############################################################################

  sykehusnavn <- sort(unique(Inklusjon$Sykehusnavn))

  Inklusjon$Sykehusnavn <- factor(as.character(Inklusjon$Sykehusnavn), levels = sykehusnavn)
  Oppfolging$Sykehusnavn <- factor(as.character(Oppfolging$Sykehusnavn), levels = sykehusnavn)
  Diagnoser$Sykehusnavn <- factor(as.character(Diagnoser$Sykehusnavn), levels = sykehusnavn)
  Medisiner$Sykehusnavn <- factor(as.character(Medisiner$Sykehusnavn), levels = sykehusnavn)
  BVAS$Sykehusnavn <- factor(as.character(BVAS$Sykehusnavn), levels = sykehusnavn)
  KERR$Sykehusnavn <- factor(as.character(KERR$Sykehusnavn), levels = sykehusnavn)
  VDI$Sykehusnavn <- factor(as.character(VDI$Sykehusnavn), levels = sykehusnavn)
  Alvorlig_infeksjon$Sykehusnavn <- factor(as.character(Alvorlig_infeksjon$Sykehusnavn), levels = sykehusnavn)
  Utredning$Sykehusnavn <- factor(as.character(Utredning$Sykehusnavn), levels = sykehusnavn)
  Labskjema$Sykehusnavn <- factor(as.character(Labskjema$Sykehusnavn), levels = sykehusnavn)


  Inklusjon <- Inklusjon[order(Inklusjon$InklusjonDato), ]
  Inklusjon <- Inklusjon[match(unique(Inklusjon$PasientGUID), Inklusjon$PasientGUID), ]

  Diagnoser <- Diagnoser[order(Diagnoser$Diagnose_Klinisk_Dato, decreasing = T), ]
  Diagnoser <- Diagnoser[match(unique(Diagnoser$PasientGUID), Diagnoser$PasientGUID), ]

  Diagnoser <- merge(Diagnoser, Inklusjon[, c("SkjemaGUID", "InklusjonDato")],
                     by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID')
  Inklusjon <- merge(Inklusjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr",
                                              "Diag_gr", "Diagnose")],
                     by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
  Inklusjon <- Inklusjon %>% dplyr::filter(Diag_gr_nr != 3)

  BVAS <- merge(BVAS, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
                by = 'HovedskjemaGUID', all.x = T)
  BVAS <- merge(BVAS, Inklusjon[, c('SkjemaGUID', "InklusjonDato")], by.x = 'HovedskjemaGUID',
                by.y = 'SkjemaGUID', all.x = T)
  Oppfolging <- merge(Oppfolging, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
                      by = 'HovedskjemaGUID', all.x = T)
  Oppfolging <- merge(Oppfolging, Inklusjon[, c('SkjemaGUID', "InklusjonDato")],
                      by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
  KERR <- merge(KERR, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
                by = 'HovedskjemaGUID', all.x = T)
  KERR <- merge(KERR, Inklusjon[, c('SkjemaGUID', "InklusjonDato")],
                by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
  VDI <- merge(VDI, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
               by = 'HovedskjemaGUID', all.x = T)
  Medisiner <- merge(Medisiner, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
                     by = 'HovedskjemaGUID', all.x = T)
  Alvorlig_infeksjon <- merge(Alvorlig_infeksjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
                              by = 'HovedskjemaGUID', all.x = T)
  Labskjema <- merge(Labskjema, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
                     by = 'HovedskjemaGUID', all.x = T)

  Inklusjon$Diagnose_ny_30 <- NA
  Inklusjon$Diagnose_ny_30[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 30] <- 1
  Inklusjon$Diagnose_ny_30[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) > 30] <- 0
  Inklusjon$Diagnose_ny_30[Inklusjon$InkludertNyEtablertDiagnose==2] <- 0
  Inklusjon$Diagnose_ny_30[Inklusjon$InkludertNyEtablertDiagnose==1] <- 1
  Inklusjon$Diagnose_ny_180 <- NA
  Inklusjon$Diagnose_ny_180[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 180] <- 1
  Inklusjon$Diagnose_ny_180[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) > 180] <- 0
  Inklusjon$Diagnose_ny_180[Inklusjon$InkludertNyEtablertDiagnose==2] <- 0
  Inklusjon$Diagnose_ny_180[Inklusjon$InkludertNyEtablertDiagnose==1] <- 1

  Inklusjon$Inklusjonsaar <- as.numeric(format(Inklusjon$InklusjonDato, format = '%Y'))
  Oppfolging$Oppfolgingsaar <- as.numeric(format(Oppfolging$OppfolgingsDato, format = '%Y'))
  Inklusjon <- Inklusjon[Inklusjon$Inklusjonsaar <= rap_aar, ]
  # Oppfolging <- Oppfolging[Oppfolging$Oppfolgingsaar<= rap_aar, ]
  Inklusjon$Inklusjonsalder <- Inklusjon$PatientAge
  BVAS$BVAS_aar <- as.numeric(format(BVAS$BVAS_Dato, format = '%Y'))
  KERR$KERR_aar <- as.numeric(format(KERR$KerrsKriterier_Dato, format = '%Y'))
  Diagnoser$DiagnoseAlder <- Diagnoser$PatientAge
  Alvorlig_infeksjon$inf_alder <- Alvorlig_infeksjon$PatientAge

  Oppfolging <- Oppfolging[Oppfolging$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
  Diagnoser <- Diagnoser[Diagnoser$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
  Medisiner <- Medisiner[Medisiner$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
  BVAS <- BVAS[BVAS$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
  KERR <- KERR[KERR$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
  VDI <- VDI[VDI$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
  Alvorlig_infeksjon <- Alvorlig_infeksjon[Alvorlig_infeksjon$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
  Utredning <- Utredning[Utredning$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
  Labskjema <- Labskjema[Labskjema$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]

  return(list(Oppfolging=Oppfolging, Diagnoser=Diagnoser, Medisiner=Medisiner,
              BVAS=BVAS, VDI=VDI, Alvorlig_infeksjon=Alvorlig_infeksjon, KERR=KERR,
              Utredning=Utredning, Labskjema=Labskjema, Inklusjon=Inklusjon))

}




