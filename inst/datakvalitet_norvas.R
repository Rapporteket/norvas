library(norvas)
library(xtable)
library(lubridate)
library(tidyverse)
rm(list = ls())
options(dplyr.summarise.inform = FALSE)

rap_aar <- 2022
aarrappdata <- norvas::lesogprosesser(rap_aar = rap_aar)
Inklusjon <- aarrappdata$Inklusjon
Oppfolging <- aarrappdata$Oppfolging
Diagnoser <- aarrappdata$Diagnoser
Medisiner <- aarrappdata$Medisiner
BVAS <- aarrappdata$BVAS
KERR <- aarrappdata$KERR
VDI <- aarrappdata$VDI
Alvorlig_infeksjon <- aarrappdata$Alvorlig_infeksjon
Utredning <- aarrappdata$Utredning
Labskjema <- aarrappdata$Labskjema

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
#
#
# Inklusjon <- norvasPreprosess(Inklusjon)
# Oppfolging <- norvasPreprosess(Oppfolging)
# Diagnoser <- norvasPreprosess(Diagnoser)
# Medisiner <- norvasPreprosess(Medisiner)
# BVAS <- norvasPreprosess(BVAS)
# KERR <- norvasPreprosess(KERR)
# VDI <- norvasPreprosess(VDI)
# Alvorlig_infeksjon <- norvasPreprosess(Alvorlig_infeksjon)
# Utredning <- norvasPreprosess(Utredning)
# Labskjema <- norvasPreprosess(Labskjema)
#
# ### Ny 18.10.2021: Fjerner medikamenter i kategorien "Andre"
# Medisiner <- Medisiner[!(Medisiner$LegemiddelNr %in% c(0, 999)), ]
# ###############################################################################
#
# sykehusnavn <- sort(unique(Inklusjon$Sykehusnavn))
#
# Inklusjon$Sykehusnavn <- factor(as.character(Inklusjon$Sykehusnavn), levels = sykehusnavn)
# Oppfolging$Sykehusnavn <- factor(as.character(Oppfolging$Sykehusnavn), levels = sykehusnavn)
# Diagnoser$Sykehusnavn <- factor(as.character(Diagnoser$Sykehusnavn), levels = sykehusnavn)
# Medisiner$Sykehusnavn <- factor(as.character(Medisiner$Sykehusnavn), levels = sykehusnavn)
# BVAS$Sykehusnavn <- factor(as.character(BVAS$Sykehusnavn), levels = sykehusnavn)
# KERR$Sykehusnavn <- factor(as.character(KERR$Sykehusnavn), levels = sykehusnavn)
# VDI$Sykehusnavn <- factor(as.character(VDI$Sykehusnavn), levels = sykehusnavn)
# Alvorlig_infeksjon$Sykehusnavn <- factor(as.character(Alvorlig_infeksjon$Sykehusnavn), levels = sykehusnavn)
# Utredning$Sykehusnavn <- factor(as.character(Utredning$Sykehusnavn), levels = sykehusnavn)
# Labskjema$Sykehusnavn <- factor(as.character(Labskjema$Sykehusnavn), levels = sykehusnavn)
#
#
# Inklusjon <- Inklusjon[order(Inklusjon$InklusjonDato), ]
# Inklusjon <- Inklusjon[match(unique(Inklusjon$PasientGUID), Inklusjon$PasientGUID), ]
#
# Diagnoser <- Diagnoser[order(Diagnoser$Diagnose_Klinisk_Dato, decreasing = T), ]
# Diagnoser <- Diagnoser[match(unique(Diagnoser$PasientGUID), Diagnoser$PasientGUID), ]
#
# Diagnoser <- merge(Diagnoser, Inklusjon[, c("SkjemaGUID", "InklusjonDato")],
#                    by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID')
# Inklusjon <- merge(Inklusjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr",
#                                             "Diag_gr", "Diagnose")],
#                    by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
# Inklusjon <- Inklusjon %>% dplyr::filter(Diag_gr_nr != 3)
#
# BVAS <- merge(BVAS, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
#               by = 'HovedskjemaGUID', all.x = T)
# BVAS <- merge(BVAS, Inklusjon[, c('SkjemaGUID', "InklusjonDato")], by.x = 'HovedskjemaGUID',
#               by.y = 'SkjemaGUID', all.x = T)
# Oppfolging <- merge(Oppfolging, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
#                     by = 'HovedskjemaGUID', all.x = T)
# Oppfolging <- merge(Oppfolging, Inklusjon[, c('SkjemaGUID', "InklusjonDato")],
#                     by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
# KERR <- merge(KERR, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
#               by = 'HovedskjemaGUID', all.x = T)
# KERR <- merge(KERR, Inklusjon[, c('SkjemaGUID', "InklusjonDato")],
#               by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
# VDI <- merge(VDI, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
#              by = 'HovedskjemaGUID', all.x = T)
# Medisiner <- merge(Medisiner, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
#                    by = 'HovedskjemaGUID', all.x = T)
# Alvorlig_infeksjon <- merge(Alvorlig_infeksjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
#                             by = 'HovedskjemaGUID', all.x = T)
# Labskjema <- merge(Labskjema, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")],
#                    by = 'HovedskjemaGUID', all.x = T)
#
# Inklusjon$Diagnose_ny_30 <- NA
# Inklusjon$Diagnose_ny_30[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 30] <- 1
# Inklusjon$Diagnose_ny_30[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) > 30] <- 0
# Inklusjon$Diagnose_ny_30[Inklusjon$InkludertNyEtablertDiagnose==2] <- 0
# Inklusjon$Diagnose_ny_30[Inklusjon$InkludertNyEtablertDiagnose==1] <- 1
# Inklusjon$Diagnose_ny_180 <- NA
# Inklusjon$Diagnose_ny_180[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 180] <- 1
# Inklusjon$Diagnose_ny_180[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) > 180] <- 0
# Inklusjon$Diagnose_ny_180[Inklusjon$InkludertNyEtablertDiagnose==2] <- 0
# Inklusjon$Diagnose_ny_180[Inklusjon$InkludertNyEtablertDiagnose==1] <- 1
#
# Inklusjon$Inklusjonsaar <- as.numeric(format(Inklusjon$InklusjonDato, format = '%Y'))
# Oppfolging$Oppfolgingsaar <- as.numeric(format(Oppfolging$OppfolgingsDato, format = '%Y'))
# Inklusjon <- Inklusjon[Inklusjon$Inklusjonsaar <= rap_aar, ]
# # Oppfolging <- Oppfolging[Oppfolging$Oppfolgingsaar<= rap_aar, ]
# Inklusjon$Inklusjonsalder <- Inklusjon$PatientAge
# BVAS$BVAS_aar <- as.numeric(format(BVAS$BVAS_Dato, format = '%Y'))
# KERR$KERR_aar <- as.numeric(format(KERR$KerrsKriterier_Dato, format = '%Y'))
# Diagnoser$DiagnoseAlder <- Diagnoser$PatientAge
# Alvorlig_infeksjon$inf_alder <- Alvorlig_infeksjon$PatientAge
#
# Oppfolging <- Oppfolging[Oppfolging$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
# Diagnoser <- Diagnoser[Diagnoser$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
# Medisiner <- Medisiner[Medisiner$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
# BVAS <- BVAS[BVAS$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
# KERR <- KERR[KERR$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
# VDI <- VDI[VDI$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
# Alvorlig_infeksjon <- Alvorlig_infeksjon[Alvorlig_infeksjon$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
# Utredning <- Utredning[Utredning$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]
# Labskjema <- Labskjema[Labskjema$HovedskjemaGUID %in% Inklusjon$SkjemaGUID, ]


## Kompletthet av variabler

Labskjema$BT_utfort <- rowSums(!is.na(Labskjema[, c("BlodtrykkSystoliskVenstre", "BlodtrykkDiastoliskVenstre", "BlodtrykkDiastolisk",
                    "BlodtrykkSystolisk", "BlodtrykkSystoliskHoyre", "BlodtrykkSystoliskVenstre")]))

Labskjema$BT_utfort[Labskjema$BT_utfort > 0] <- 1

Labskjemarap_aar <- Labskjema[which(as.numeric(format(Labskjema$Blodprover_Dato, format="%Y")) == rap_aar), ]

Kompletthet <- data.frame(Variabel = "Blodprøve", Antall_utfylt = sum(Labskjemarap_aar$BT_utfort==1),
                          N = dim(Labskjemarap_aar)[1])

blodprover_utfort <- sum(Labskjemarap_aar$BT_utfort==1)/dim(Labskjemarap_aar)[1]*100


Oppfolgingrap_aar <- bind_rows(Oppfolging[which(Oppfolging$Oppfolgingsaar == rap_aar),
                             c("Tretthet", "PasientGlobalSykdomsaktivitet", "Pasientsmerter", "SkjemaGUID")],
                            Inklusjon[which(Inklusjon$Inklusjonsaar == rap_aar),
                                      c("Tretthet", "PasientGlobalSykdomsaktivitet", "Pasientsmerter", "SkjemaGUID")])


prom_utfylt  <- Oppfolgingrap_aar %>% summarise(Tretthet_utfylt = sum(!is.na(Tretthet)),
                             Sykdomsaktivitet_utfylt = sum(!is.na(PasientGlobalSykdomsaktivitet)),
                             Smerte_utfylt = sum(!is.na(Pasientsmerter)),
                             N = n())
prom_utfylt[1:3]/prom_utfylt[[4]]*100

Kompletthet <- bind_rows(Kompletthet, data.frame(Variabel = c("Tretthet", "PasientGlobalSykdomsaktivitet", "Pasientsmerter"),
           Antall_utfylt = as.numeric(prom_utfylt[1:3]), N = rep(prom_utfylt[[4]], 3)))

# Alvorlig_infeksjonrap_aar <- Alvorlig_infeksjon[which(format(Alvorlig_infeksjon$SelvrapportertAlvorligInfeksjon_Registrert_Dato, format="%Y") == rap_aar), ]

nevner <- Inklusjon$SkjemaGUID[Inklusjon$Inklusjonsaar ==rap_aar & Inklusjon$Diag_gr_nr %in% 2:3]

bp_v_inkl  <- Labskjemarap_aar[which(Labskjemarap_aar$HovedskjemaGUID %in% nevner), ]

bp_v_inkl$hepatitt_samlet <-  rowSums(bp_v_inkl[, c("HepatittBCoreAntistoffpositiv", "HepatittBSurfaceAntistoffpositiv",
                                           "HepatittBSurfaceAntigenpositiv", "HepatittCAntistoffpositiv")] != -1)

bp_v_inkl$hepatitt_samlet[bp_v_inkl$hepatitt_samlet>0] <- 1

bp_v_inkl <- bp_v_inkl %>% group_by(PasientGUID) %>% summarise(hepatitt_test = max(hepatitt_samlet),
                                                  N=n())

table(bp_v_inkl$hepatitt_test, useNA = 'ifany')

andel_hepatestrap_aar <- sum(bp_v_inkl$hepatitt_test)/length(unique(nevner))*100
Kompletthet <- bind_rows(Kompletthet, data.frame(Variabel = "Hepatitt_test",
                                                 Antall_utfylt = sum(bp_v_inkl$hepatitt_test),
                                                 N = length(unique(nevner))))


tbquant <- Labskjemarap_aar[which(Labskjemarap_aar$HovedskjemaGUID %in% nevner), ]
tbquant$tbquant_samlet <- as.numeric(tbquant$Quantiferonpositiv != -1)
tbquant <- tbquant %>% group_by(PasientGUID) %>% summarise(tbquant_test = max(tbquant_samlet),
                                                               N=n())

table(tbquant$tbquant_test, useNA = 'ifany')

tbquant_testrap_aar <- sum(tbquant$tbquant_test)/length(unique(nevner))*100
Kompletthet <- bind_rows(Kompletthet, data.frame(Variabel = "tbquant_test",
                                                 Antall_utfylt = sum(tbquant$tbquant_test),
                                                 N = length(unique(nevner))))

################
Labskjemarap_aargr2 <- Labskjemarap_aar[which(Labskjemarap_aar$Diag_gr_nr == 2), ]


Labskjemarap_aargr2$ancapos_saml <- rowSums(Labskjemarap_aargr2[, c("PR3AncaPositiv", "MPO_AncaPositiv")] != -1)

Labskjemarap_aargr2$ancapos_saml[Labskjemarap_aargr2$ancapos_saml>0] <- 1

sum(Labskjemarap_aargr2$ancapos_saml)/dim(Labskjemarap_aargr2)[1]*100

Kompletthet <- bind_rows(Kompletthet, data.frame(Variabel = "ancapos_test",
                                                 Antall_utfylt = sum(Labskjemarap_aargr2$ancapos_saml),
                                                 N = dim(Labskjemarap_aargr2)[1]))


####

########  BVAS ##############
tmp2 <- merge(Oppfolging, BVAS[, c("SykdomsvurderingLabel", "PasientGUID", "BVAS_Dato")],
              by.x = c('PasientGUID','OppfolgingsDato'),
              by.y = c('PasientGUID','BVAS_Dato'), all.x = T)
tmp2 <- tmp2[tmp2$Diag_gr_nr %in% 2, ]
Tabell_bvas_oppf <- tmp2[tmp2$Oppfolgingsaar==rap_aar, ] %>%
  group_by(Sykehusnavn) %>%
  summarise(andeloppf = sum(!is.na(SykdomsvurderingLabel))/length(SykdomsvurderingLabel)*100,
            N = n())

total <- tibble(Variabel = 'Andel_bvas',
                Antall_utfylt = sum(Tabell_bvas_oppf$andeloppf*Tabell_bvas_oppf$N)/100,
                N = sum(Tabell_bvas_oppf$N))
Kompletthet <- bind_rows(Kompletthet, total)

########  KERR ##############
tmp3 <- merge(Oppfolging, KERR[, c("KerrsKriterier_Dato", "PasientGUID", "SykdomsvurderingLabel")],
              by.x = c('PasientGUID','OppfolgingsDato'),
              by.y = c('PasientGUID','KerrsKriterier_Dato'), all.x = T)
tmp3 <- tmp3[which(tmp3$Diag_gr_nr == 1), ]


Tabell_kerr_oppf <- tmp3[tmp3$Oppfolgingsaar==rap_aar, ] %>%
  group_by(Sykehusnavn) %>%
  summarise(andeloppf = sum(!is.na(SykdomsvurderingLabel))/length(SykdomsvurderingLabel)*100,
            N = n())

total <- tibble(Variabel = 'Andel_kerr',
                Antall_utfylt = sum(Tabell_kerr_oppf$andeloppf*Tabell_kerr_oppf$N)/100,
                N = sum(Tabell_kerr_oppf$N))

Kompletthet <- bind_rows(Kompletthet, total)

########## VDI ########################################################
vdi <- merge(Oppfolging, VDI[, c("Cataract", "PasientGUID", "VDI_Dato")],
              by.x = c('PasientGUID','OppfolgingsDato'),
              by.y = c('PasientGUID','VDI_Dato'), all.x = T) %>%
  dplyr::filter(Diag_gr_nr == 2 &
                  Oppfolgingsaar==rap_aar) %>%
  summarise(Antall_utfylt = sum(!is.na(Cataract)),
            N = n()) %>%
  mutate(Variabel = 'Andel_vdi')

Kompletthet <- bind_rows(Kompletthet, vdi)

##########################################################################

Kompletthet$Kompletthet <- Kompletthet$Antall_utfylt/Kompletthet$N*100



write.csv2(Kompletthet, "~/GIT/norvas/doc/Kompletthet2022.csv", row.names = F, fileEncoding = "Latin1")

