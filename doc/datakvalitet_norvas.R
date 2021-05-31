library(norvas)
library(xtable)
library(lubridate)
rm(list = ls())

rap_aar <- 2020

Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-05-18_1055.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
names(Inklusjon)[names(Inklusjon) == "Fødselsnummer"] <- "PasientGUID"
Oppfolging <- read.table('I:/norvas/DataDump_MRS-PROD_OppfølgingSkjema_2021-05-18_1059.csv', header=TRUE, sep=";",
                         stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2021-05-18_1100.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner <- read.table('I:/norvas/DataDump_MRS-PROD_MedisineringSkjema_2021-05-18_1100.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
BVAS <- read.table('I:/norvas/DataDump_MRS-PROD_BvasSkjema_2021-05-18_1100.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
KERR <- read.table('I:/norvas/DataDump_MRS-PROD_KerrsKriterierSkjema_2021-05-18_1101.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
VDI <- read.table('I:/norvas/DataDump_MRS-PROD_VdiSkjema_2021-05-18_1100.csv', header=TRUE, sep=";",
                  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Alvorlig_infeksjon <- read.table('I:/norvas/DataDump_MRS-PROD_SelvrapportertAlvorligInfeksjonSkjema_2021-05-18_1100.csv',
                                 header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Utredning <- read.table('I:/norvas/DataDump_MRS-PROD_Utredning_2021-05-18_1100.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Labskjema <- read.table('I:/norvas/DataDump_MRS-PROD_BlodprøvesvarSkjema_2021-05-18_1100.csv', header=TRUE, sep=";",
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

### Foreløpig fiks, fjernes når data er ordnet
Medisiner$Medikamentgruppe[Medisiner$Medikamentgruppe == ""] <- "Andre"

varnavn <- kodebok_norvas[which(!is.na(kodebok_norvas$Variabelnavn)), c("Variabelnavn", "skjema")]
indekser_kodebok <- which(kodebok_norvas$Variabelnavn == 'Sykdomsvurdering' & kodebok_norvas$skjema == 'BvasSkjema'):
  (which(kodebok_norvas$Variabelnavn == varnavn$Variabelnavn[which(varnavn$Variabelnavn=='Sykdomsvurdering' & varnavn$skjema == 'BvasSkjema')+1] & kodebok_norvas$skjema == 'BvasSkjema')-1)
BVAS$SykdomsvurderingLabel <- factor(BVAS$Sykdomsvurdering, levels = kodebok_norvas$kode[c(indekser_kodebok[-1])],
                                     labels = kodebok_norvas$label[c(indekser_kodebok[-1])])
tmp <- table(BVAS[, c("PasientGUID", "BVAS_Dato")])
tmp <- as.data.frame(tmp)
tmp <- tmp[tmp$Freq>1, ]
tmp2 <-  merge(BVAS[, c("PasientGUID", "BVAS_Dato", "SkjemaGUID")],
               tmp[, c("PasientGUID", "BVAS_Dato")], by = c('PasientGUID', 'BVAS_Dato'))
BVAS <- BVAS[!(BVAS$SkjemaGUID %in% tmp2$SkjemaGUID), ] ## Fjerner BVAS som har flere registreringer på
## samme pasient på samme dag.

indekser_kodebok <- which(kodebok_norvas$Variabelnavn == 'Sykdomsvurdering' & kodebok_norvas$skjema == 'KerrsKriterierSkjema'):
  (which(kodebok_norvas$Variabelnavn == varnavn$Variabelnavn[which(varnavn$Variabelnavn=='Sykdomsvurdering' & varnavn$skjema == 'KerrsKriterierSkjema')+1] & kodebok_norvas$skjema == 'KerrsKriterierSkjema')-1)
KERR$SykdomsvurderingLabel <- factor(KERR$Sykdomsvurdering, levels = kodebok_norvas$kode[c(indekser_kodebok[-1])],
                                     labels = kodebok_norvas$label[c(indekser_kodebok[-1])])

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

Inklusjon$Fodselsdato <- personnr2fodselsdato(Inklusjon$PasientId)
Diagnoser <- merge(Diagnoser, Inklusjon[, c("SkjemaGUID", "Fodselsdato", "InklusjonDato")],
                   by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID')

Inklusjon <- merge(Inklusjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr",
                                            "Diag_gr", "Navn")],
                   by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
BVAS <- merge(BVAS, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
BVAS <- merge(BVAS, Inklusjon[, c('SkjemaGUID', "InklusjonDato")], by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
Oppfolging <- merge(Oppfolging, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
KERR <- merge(KERR, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
KERR <- merge(KERR, Inklusjon[, c('SkjemaGUID', "InklusjonDato")], by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
VDI <- merge(VDI, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
Medisiner <- merge(Medisiner, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
Alvorlig_infeksjon <- merge(Alvorlig_infeksjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr", "Fodselsdato")], by = 'HovedskjemaGUID', all.x = T)
Labskjema <- merge(Labskjema, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr", "Fodselsdato")], by = 'HovedskjemaGUID', all.x = T)

Inklusjon$Diagnose_ny <- NA
Inklusjon$Diagnose_ny[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 90] <- 1
Inklusjon$Diagnose_ny[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) > 90] <- 0
Inklusjon <- Inklusjon[!is.na(Inklusjon$InklusjonDato), ]

Inklusjon$Inklusjonsaar <- as.numeric(format(Inklusjon$InklusjonDato, format = '%Y'))
Oppfolging$Oppfolgingsaar <- as.numeric(format(Oppfolging$OppfolgingsDato, format = '%Y'))
Inklusjon$Inklusjonsalder <- age(Inklusjon$Fodselsdato, Inklusjon$InklusjonDato)
BVAS$BVAS_aar <- as.numeric(format(BVAS$BVAS_Dato, format = '%Y'))
KERR$KERR_aar <- as.numeric(format(KERR$KerrsKriterier_Dato, format = '%Y'))
Diagnoser$DiagnoseAlder <- age(Diagnoser$Fodselsdato, Diagnoser$Diagnose_Klinisk_Dato)
Alvorlig_infeksjon$inf_alder <- age(Alvorlig_infeksjon$Fodselsdato, Alvorlig_infeksjon$SelvrapportertAlvorligInfeksjon_Registrert_Dato)

Oppfolging <- Oppfolging[Oppfolging$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
Diagnoser <- Diagnoser[Diagnoser$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
Medisiner <- Medisiner[Medisiner$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
BVAS <- BVAS[BVAS$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
KERR <- KERR[KERR$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
VDI <- VDI[VDI$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
Alvorlig_infeksjon <- Alvorlig_infeksjon[Alvorlig_infeksjon$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
Utredning <- Utredning[Utredning$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
Labskjema <- Labskjema[Labskjema$PasientGUID %in% unique(Inklusjon$PasientGUID), ]


#### HVIS SAMME PASIENT HAR OPPSTART MED SAMME LEGEMIDDEL PÅ SAMME DAG, VELG DEN MED TIDLIGST SLUTTDATO

tmp <- Medisiner %>%
  group_by(PasientGUID, Med_StartDato, LegemiddelGenerisk) %>%
  summarise('ant_samme_startdato' = n(),
            Med_SluttDato_min = min(Med_SluttDato, na.rm = T),
            SkjemaGUID_min = if (is.na(which(Med_SluttDato == min(Med_SluttDato, na.rm = T))[1])) {SkjemaGUID[1]}
            else {SkjemaGUID[which(Med_SluttDato == min(Med_SluttDato, na.rm = T))[1]]})

Medisiner <- merge(Medisiner, tmp[, c("SkjemaGUID_min", "ant_samme_startdato")], by.x = "SkjemaGUID", by.y = "SkjemaGUID_min")


#### KOBLING MELLEOM UNITID OG SYKEHUSNAVN
kobl_unitid_shusnavn_norvas <- Inklusjon[match(unique(Inklusjon$UnitId), Inklusjon$UnitId), c("UnitId", "Sykehusnavn")]


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
tmp2 <- merge(Oppfolging, BVAS[, c("Sykdomsvurdering", "PasientGUID", "BVAS_Dato")], by.x = c('PasientGUID','OppfolgingsDato'),
              by.y = c('PasientGUID','BVAS_Dato'), all.x = T)
tmp2 <- tmp2[tmp2$Diag_gr_nr %in% 2, ]
Tabell_bvas_oppf <- tmp2[tmp2$Oppfolgingsaar==rap_aar, ] %>% group_by(Sykehusnavn) %>% summarise(andeloppf = sum(!is.na(Sykdomsvurdering))/length(Sykdomsvurdering)*100, N = n())

total <- tibble(Variabel = 'Andel_bvas',
                Antall_utfylt = sum(Tabell_bvas_oppf$andeloppf*Tabell_bvas_oppf$N)/100,
                N = sum(Tabell_bvas_oppf$N))
Kompletthet <- bind_rows(Kompletthet, total)

########  KERR ##############
tmp3 <- merge(Oppfolging, KERR[, c("KerrsKriterier_Dato", "PasientGUID", "Sykdomsvurdering")], by.x = c('PasientGUID','OppfolgingsDato'),
              by.y = c('PasientGUID','KerrsKriterier_Dato'), all.x = T)
tmp3 <- tmp3[which(tmp3$Diag_gr_nr == 1), ]


Tabell_kerr_oppf <- tmp3[tmp3$Oppfolgingsaar==rap_aar, ] %>% group_by(Sykehusnavn) %>% summarise(andeloppf = sum(!is.na(Sykdomsvurdering))/length(Sykdomsvurdering)*100, N = n())

total <- tibble(Variabel = 'Andel_kerr',
                Antall_utfylt = sum(Tabell_kerr_oppf$andeloppf*Tabell_kerr_oppf$N)/100,
                N = sum(Tabell_kerr_oppf$N))

Kompletthet <- bind_rows(Kompletthet, total)


Kompletthet$Kompletthet <- Kompletthet$Antall_utfylt/Kompletthet$N*100



write.csv2(Kompletthet, "C:/GIT/norvas/doc/Kompletthet.csv", row.names = F, fileEncoding = "Latin1")





# Sykdomsaktivitet målt ved BVAS

# bvas2019 <- BVAS[BVAS$BVAS_aar == 2019, c("PasientGUID", "BVAS_Dato", "BVAS_aar", "bvas_samlet", "BvasPersistentTotal", "BvasnewOrWorseTotal",
#                                           "BvasKommentar")]
#
# oppf2019 <- Oppfolging[Oppfolging$Oppfolgingsaar == 2019, c("PasientGUID", )]
# p_guid_nevner_gr2_3 <- unique(union(Inklusjon$PasientGUID[which(Inklusjon$Inklusjonsaar==2019 & Inklusjon$Diag_gr_nr %in% c(2,3))],
#                            Oppfolging$PasientGUID[which(Oppfolging$Oppfolgingsaar==2019 & Oppfolging$Diag_gr_nr %in% c(2,3))]))
# p_guid_bvas <- unique(BVAS$PasientGUID[which(BVAS$BVAS_aar == 2019 & BVAS$Diag_gr_nr %in% c(2,3))])
# # length(intersect(p_guid_bvas, p_guid_nevner_gr2_3))
#
# bvas_kompletthet <- length(p_guid_bvas)/length(p_guid_nevner_gr2_3)*100
# bvas_kompletthet_v2 <- length(intersect(p_guid_bvas, p_guid_nevner_gr2_3))/length(p_guid_nevner_gr2_3)*100
# ## Sannsynlig at bvas som fylles ut og skal knyttes til historisk debutskjema havner på dagens dato.
#
# table(BVAS$Sykdomsvurdering[BVAS$BVAS_aar == 2019 & BVAS$Diag_gr_nr %in% c(2)], useNA = 'ifany')
# ## Alle utfylte BVas HAR MED SYKDOMSVURDERING
#
# p_guid_nevner_gr1 <- unique(union(Inklusjon$PasientGUID[Inklusjon$Inklusjonsaar==2019 & Inklusjon$Diag_gr_nr %in% c(1)],
#                               Oppfolging$PasientGUID[Oppfolging$Oppfolgingsaar==2019 & Oppfolging$Diag_gr_nr %in% c(1)]))
# p_guid_kerr <- unique(KERR$PasientGUID[format(KERR$KerrsKriterier_Dato, format="%Y") == "2019" & KERR$Diag_gr_nr %in% c(1)])
# length(intersect(p_guid_kerr, p_guid_nevner_gr1))
#
# kerr_kompletthet <- length(p_guid_kerr)/length(p_guid_nevner_gr1)*100
#
#
# p_guid_nevner <- unique(union(Inklusjon$PasientGUID[which(Inklusjon$Inklusjonsaar==2019)],
#                                     Oppfolging$PasientGUID[which(Oppfolging$Oppfolgingsaar==2019)]))
# p_guid_vdi <- unique(VDI$PasientGUID[which(format(VDI$VDI_Dato, format="%Y") == "2019")])
#
# vdi_kompletthet <- length(p_guid_vdi)/length(p_guid_nevner)*100
