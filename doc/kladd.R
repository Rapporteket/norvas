library(norvas)
library(xtable)
library(lubridate)
# library(rapFigurer)
rm(list = ls())
options(dplyr.summarise.inform = FALSE)

rap_aar <- 2021

Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2022-03-17_1013.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2022-03-17_1013.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

Inklusjon <- norvasPreprosess(Inklusjon)
Diagnoser <- norvasPreprosess(Diagnoser)


allevar <- merge(Inklusjon, Diagnoser, by.x = "SkjemaGUID", by.y = "HovedskjemaGUID")












# setwd("C:/GIT/norvas/doc/")
# rm(list = ls())
#
# Inklusjon <- read.table('I:/norvas/DataDump_QA_Inklusjonskjema_2018-08-22.csv', header=TRUE, sep=";",
#                             stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Diagnoser <- read.table('I:/norvas/DataDump_QA_DiagnoseSkjema_2018-08-20.csv', header=TRUE, sep=";",
#                         stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Medisiner <- read.table('I:/norvas/DataDump_QA_MedisineringSkjema_2018-08-20.csv', header=TRUE, sep=";",
#                         stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# table(Inklusjon$UnitId)
# length(unique(Inklusjon$PasientGUID))
# aux <- table(Inklusjon$PasientGUID)
# aux[aux>1]
# sum(is.na(Inklusjon$PasientGUID))
# tmp <- Alvorlig_infeksjon[, c("PasientGUID", "AntallInfeksjoner_num", 'UnitId')] %>% group_by(PasientGUID) %>%
#   summarise(sum=sum(AntallInfeksjoner_num), UnitId=UnitId[1])
# table(tmp$sum)

#############################################################################
library(norvas)
setwd("C:/GIT/norvas/doc/")
rm(list = ls())

Inklusjon_old <- read.table('I:/norvas/DataDump_Prod_Inklusjonskjema_2019-05-31.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Inklusjon_pguid <- read.table('I:/norvas/DataDump_Prod_Inklusjonskjema_2019-05-31 (1).csv', header=TRUE, sep=";",
                              stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Inklusjon_old$PasientGUID <- Inklusjon_pguid$PasientGUID
Oppfolging_old <- read.table('I:/norvas/DataDump_Prod_OppfølgingSkjema_2019-05-31.csv', header=TRUE, sep=";",
                         stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Diagnoser_old <- read.table('I:/norvas/DataDump_Prod_DiagnoseSkjema_2019-05-31.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner_old <- read.table('I:/norvas/DataDump_Prod_MedisineringSkjema_2019-05-31.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
BVAS_old <- read.table('I:/norvas/DataDump_Prod_BvasSkjema_2019-05-31.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
KERR_old <- read.table('I:/norvas/DataDump_Prod_KerrsKriterierSkjema_2019-05-31.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
VDI_old <- read.table('I:/norvas/DataDump_Prod_VdiSkjema_2019-05-31.csv', header=TRUE, sep=";",
                  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Alvorlig_infeksjon_old <- read.table('I:/norvas/DataDump_Prod_SelvrapportertAlvorligInfeksjonSkjema_2019-05-31.csv', header=TRUE, sep=";",
                                 stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Utredning_old <- read.table('I:/norvas/DataDump_Prod_BilledDiagnostikkSkjema_2019-05-31.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Labskjema_old <- read.table('I:/norvas/DataDump_Prod_BlodprøvesvarSkjema_2019-05-31.csv', header=TRUE, sep=";",
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

RegData=Inklusjon
valgtVar='ErMann'
datoFra='2010-01-01'
datoTil='2050-12-31'
minald=0
maxald=130
erMann=99
outfile=''
datovar='InklusjonDato'
reshID=601159
enhetsUtvalg=0
inkl_konf=1
valgtShus=''
tidsenhet='Aar'
aldervar='Inklusjonsalder'
diag_gruppe=99

x11()
norvasFigAndelTid(RegData=Inklusjon, valgtVar='Andel_ANCA', datoFra='2010-01-01', datoTil='2050-12-31',
                              minald=0, maxald=130, erMann=99, outfile='', datovar='InklusjonDato',
                              reshID=601159, enhetsUtvalg=0, inkl_konf=1, aldervar='Inklusjonsalder',
                              valgtShus='', tidsenhet='Aar')

x11()
norvasFigAndelTid(RegData=BVAS, valgtVar='Andel_remisjon', datoFra='2014-01-01', datoTil='2050-12-31',
                  minald=0, maxald=130, erMann=99, outfile='', datovar='BVAS_Dato',
                  reshID=601159, enhetsUtvalg=1, inkl_konf=0, aldervar='PatientAge',
                  valgtShus='', tidsenhet='Aar')



####### LAG gjennomsnittsfigur  - både som tidsvisning og sykehusvisning
### Gjennomsnitt av gj.sn. BVAS pr. pasient pr. tidsenhet

BVAS$Aar <- as.numeric(format(BVAS[, "BVAS_Dato"], '%Y'))
tabell<-BVAS %>% group_by(PasientGUID, Aar) %>% summarise(Gj.bvas.pr.pas = mean(bvas_samlet),
                                                  Sykehusnavn = Sykehusnavn[1],
                                                  N =n())

tabell %>% group_by(Sykehusnavn, Aar) %>% summarise(Gj.bvas.pr.pas.pr.aar = mean(Gj.bvas.pr.pas),
                                                    N = sum(N))



######## Tid fra diagnose til første remisjon ################################

# merge(BVAS, Diagnoser[, c("Diagnose_Klinisk_Dato")])

# tmp <- BVAS[order(BVAS$BVAS_Dato, decreasing = F), ]
# aux <- difftime(BVAS$BVAS_Dato, BVAS$Diagnose_Klinisk_Dato, units = 'days')
# aux[which(aux<0)]

# write.csv2(Inklusjon[Inklusjon$PasientGUID %in% unique(BVAS$PasientGUID[which(aux<0)]), c("PasientGUID", "Fødselsnummer")],
#            "BVAS_foer_diagnose.csv", row.names = F)

BVAS$tid_diag_bvas <- difftime(BVAS$BVAS_Dato, BVAS$Diagnose_Klinisk_Dato, units = 'days')
# BVAS <- BVAS[which(BVAS$tid_diag_bvas >= 0), ]
BVAS <- BVAS[order(BVAS$BVAS_Dato, decreasing = F), ]

# remisjon <- BVAS[which(BVAS$Sykdomsvurdering == "Remisjon"), ]
# remisjon <- remisjon[match(unique(remisjon$PasientGUID), remisjon$PasientGUID), ]
# remisjon$tid_diag_bvas

# table(BVAS$Sykdomsvurdering[which(BVAS$PasientGUID %in% setdiff(unique(BVAS$PasientGUID), remisjon$PasientGUID))],
      # useNA = 'ifany')
# Inkluderer kun de med diagnosedato og inklusjonasdato innenfor 30 dager

### Versjon 1 nysyke: Differanse mellom Diagnose_Klinisk_Dato og InklusjonDato
# nysyke <- Inklusjon[which(abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 30), ]
# BVAS_utvalg <- BVAS[which(BVAS$PasientGUID %in% nysyke$PasientGUID), ]
### Versjon 2 nysyke: Differanse mellom BVAS_Dato og InklusjonDato
nysyke <- BVAS[which(abs(difftime(BVAS$BVAS_Dato, BVAS$InklusjonDato, units = 'days')) <= 30), ]
nysyke <- nysyke[nysyke$Sykdomsvurdering == 'Debut', ]
BVAS_utvalg <- BVAS[which(BVAS$PasientGUID %in% nysyke$PasientGUID), ]

# remisjon <- BVAS_utvalg[which(BVAS_utvalg$Sykdomsvurdering == "Remisjon" | BVAS_utvalg$bvas_samlet == 0), ]
remisjon <- BVAS_utvalg[which(BVAS_utvalg$Sykdomsvurdering == "Remisjon"), ]
remisjon <- remisjon[match(unique(remisjon$PasientGUID), remisjon$PasientGUID), ]
remisjon$tid_diag_bvas

gr <- c(0,30, 90, 180, 360, 100000)
remisjon$tid_diag_bvas_gr <- cut(as.numeric(remisjon$tid_diag_bvas), breaks = gr, include.lowest = T)
## Wenche undersøker:
# Inklusjon$Fødselsnummer[Inklusjon$PasientGUID %in% remisjon$PasientGUID[remisjon$tid_diag_bvas > 360]]
# Inklusjon$Fødselsnummer[Inklusjon$PasientGUID %in% remisjon$PasientGUID[remisjon$tid_diag_bvas == 0]]

prednisolon <- Medisiner[which(Medisiner$LegemiddelType == "Prednisolon"), ]

PaaPrednisolon <- merge(remisjon, prednisolon, by='HovedskjemaGUID', all.x = T)
PaaPrednisolon$medisinert <- 0
PaaPrednisolon$medisinert[which((PaaPrednisolon$BVAS_Dato >= PaaPrednisolon$StartDato & PaaPrednisolon$BVAS_Dato <= PaaPrednisolon$SluttDato) |
  (PaaPrednisolon$BVAS_Dato >= PaaPrednisolon$StartDato & is.na(PaaPrednisolon$SluttDato)))] <- 1
PaaPrednisolon <- PaaPrednisolon[PaaPrednisolon$medisinert==1 | PaaPrednisolon$HovedskjemaGUID %in%
                                   remisjon$HovedskjemaGUID[!(remisjon$HovedskjemaGUID %in% prednisolon$HovedskjemaGUID)], ]
# indekser <- names(sort(table(PaaPrednisolon$PasientGUID.x, useNA = 'ifany'), decreasing = T))[1:5]

# PaaPrednisolon[PaaPrednisolon$PasientGUID.x %in% indekser[1], c("StartDato", "SluttDato", "Mengde", "Status")]
# PaaPrednisolon <- PaaPrednisolon[(PaaPrednisolon$PasientGUID.x %in% indekser & is.na(PaaPrednisolon$SluttDato)), ]
# Når det finnes sluttdato for en av registreringene, velg den. Hvis det ikke finnes sluttdato velg nyeste startdato.

PaaPrednisolon$Mengde[is.na(PaaPrednisolon$Mengde)] <- 0

gj.sn.pred <- PaaPrednisolon %>% group_by(tid_diag_bvas_gr) %>% summarise(gj.sn.pred = mean(Mengde), N = n())
PaaPrednisolon %>% group_by(tid_diag_bvas_gr) %>% summarise(med.sn.pred = median(Mengde), N = n())

# PaaPrednisolon$HovedskjemaGUID[which(PaaPrednisolon$Mengde < )]

# Andel i remisjon etter tid fra diagnose:
cumsum(gj.sn.pred$N)/129*100

# Andel i remisjon og med prednisolondose < x mg etter tid fra diagnose
lavdose <- PaaPrednisolon[which(PaaPrednisolon$Mengde < 7.5), ]
cumsum(table(lavdose$tid_diag_bvas_gr, useNA = 'ifany'))/129*100


## Medikamentdose tid etter debut
BVAS_debut <- BVAS_utvalg[BVAS_utvalg$Sykdomsvurdering == 'Debut',]

# BVAS_debut$x.3mnd <- month(BVAS_debut$BVAS_Dato) + 3

ant_kvartal <- 30
for (i in 1:ant_kvartal ) {
  # BVAS_debut[, paste0('x.', i*3, 'mnd')] <- month(BVAS_debut$BVAS_Dato) + i*3
  BVAS_debut[, paste0('x.', i*3, 'mnd')] <- BVAS_debut$BVAS_Dato %m+% months(i*3)
}

BVAS_debut <- merge(BVAS_debut, prednisolon, by='HovedskjemaGUID')

for (i in 1:ant_kvartal) {
  BVAS_debut[, paste0('medisin.', i*3, 'mnd')] = 0
  BVAS_debut[(BVAS_debut[, paste0('x.', i*3, 'mnd')] >= BVAS_debut$StartDato & BVAS_debut[, paste0('x.', i*3, 'mnd')] <= BVAS_debut$SluttDato & BVAS_debut[, paste0('x.', i*3, 'mnd')] <= today()) |
               (BVAS_debut[, paste0('x.', i*3, 'mnd')] >= BVAS_debut$StartDato & is.na(BVAS_debut$SluttDato) & BVAS_debut[, paste0('x.', i*3, 'mnd')] <= today()), paste0('medisin.', i*3, 'mnd')] <- 1
}

map_dbl(BVAS_debut[ , c( names(BVAS_debut)[(dim(BVAS_debut)[2]-ant_kvartal+1):dim(BVAS_debut)[2]])]*BVAS_debut$Mengde,
        function(x){median(x[x!=0])})

map_dbl(BVAS_debut[ , c( names(BVAS_debut)[(dim(BVAS_debut)[2]-ant_kvartal+1):dim(BVAS_debut)[2]])], sum)


###### Utredninger ######################################################

# Andel utført anca ved debut på anca-assosierte vaskulitter
nysyke <- Inklusjon[which(abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 30), ]
antall_nysyke_anca <- length(which(nysyke$Diag_gr_nr == 2))
table(nysyke$Sykehusnavn[which(nysyke$Diag_gr_nr == 2)], useNA = 'ifany')
aux <- merge(nysyke, Labskjema, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
aux <- aux[which(aux$Diag_gr_nr == 2), ]

aux2 <- aux[which(abs(difftime(aux$Diagnose_Klinisk_Dato, aux$Blodprover_Dato, units = 'days')) <= 30), ]

tmp <- aux2 %>% group_by(SkjemaGUID) %>% summarise(anca = (('Positiv' %in% CAncaPositiv) | ('Negativ' %in% CAncaPositiv)))
tmp2 <- aux2 %>% group_by(Sykehusnavn.x, SkjemaGUID) %>% summarise(anca = (('Positiv' %in% CAncaPositiv) | ('Negativ' %in% CAncaPositiv)))


# shinify
sum(tmp$anca==T)/antall_nysyke_anca *100

# Utredning AAV: Ved debut. andel med utført ctthorax og ct eller mr bihuler
konv_boolsk <- data.frame(gml=c('None', 'Positiv', 'Negativ'), ny=c(NA,TRUE,FALSE))
Utredning$CtBihuler <- as.logical(konv_boolsk$ny[match(Utredning$CtBihuler, konv_boolsk$gml)])
Utredning$CtThorax <- as.logical(konv_boolsk$ny[match(Utredning$CtThorax, konv_boolsk$gml)])
Utredning$MrBihuler <- as.logical(konv_boolsk$ny[match(Utredning$MrBihuler, konv_boolsk$gml)])
Utredning$UlMellomstoreKar <- as.logical(konv_boolsk$ny[match(Utredning$UlMellomstoreKar, konv_boolsk$gml)])
Utredning$CtMellomstoreKar <- as.logical(konv_boolsk$ny[match(Utredning$CtMellomstoreKar, konv_boolsk$gml)])
Utredning$CtAorta <- as.logical(konv_boolsk$ny[match(Utredning$CtAorta, konv_boolsk$gml)])
Utredning$MrMellomstoreKar <- as.logical(konv_boolsk$ny[match(Utredning$MrMellomstoreKar, konv_boolsk$gml)])
Utredning$MrAorta <- as.logical(konv_boolsk$ny[match(Utredning$MrAorta, konv_boolsk$gml)])
Utredning$BlodKar <- as.logical(konv_boolsk$ny[match(Utredning$BlodKar, konv_boolsk$gml)])

aux <- merge(nysyke, Utredning, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
aux <- aux[which(aux$Diag_gr_nr == 2), ]
aux <- aux[which(abs(difftime(aux$Diagnose_Klinisk_Dato, aux$Billeddiagnostikk_Dato, units = 'days')) <= 30), ]

aux$samlet <- !is.na(aux$CtThorax) & (!is.na(aux$CtBihuler) | !is.na(aux$MrBihuler))

# resultat:
sum(aux$samlet)/antall_nysyke_anca*100


# Utredning storkarsvaskulitt
aux <- merge(nysyke, Utredning, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
aux <- aux[which(aux$Diag_gr_nr == 1), ]
aux <- aux[which(abs(difftime(aux$Diagnose_Klinisk_Dato, aux$Billeddiagnostikk_Dato, units = 'days')) <= 30), ]


aux$samlet <- !is.na(aux$UlMellomstoreKar) | !is.na(aux$CtMellomstoreKar) | !is.na(aux$CtAorta) |
  !is.na(aux$MrMellomstoreKar) |!is.na(aux$MrAorta)

## Utredning kjempecellearteritt

aux <- merge(nysyke, Utredning, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
aux <- aux[aux$Navn == "Kjempecelle Arteritt", ]
aux <- aux[which(abs(difftime(aux$Diagnose_Klinisk_Dato, aux$Billeddiagnostikk_Dato, units = 'days')) <= 30), ]

aux$samlet <- !is.na(aux$BlodKar) | !is.na(aux$MrMellomstoreKar) | !is.na(aux$UlMellomstoreKar)



# tmp <- aux
# tmp$CAncaPositiv[tmp$CAncaPositiv=='None'] <- 0
# tmp$CAncaPositiv[tmp$CAncaPositiv!=0] <- 1
# tmp$CAncaPositiv <- as.numeric(tmp$CAncaPositiv)
# tmp$PAncaPositiv[tmp$PAncaPositiv=='None'] <- 0
# tmp$PAncaPositiv[tmp$PAncaPositiv!=0] <- 1
# tmp$PAncaPositiv <- as.numeric(tmp$PAncaPositiv)
# table(tmp$PAncaPositiv+tmp$CAncaPositiv, useNA = 'ifany')

# for (i in 1:10) {
#  print(sum(BVAS_debut[, paste0('medisin.', i*3, 'mnd')]))
# }
#
# month(BVAS_debut$BVAS_Dato[1:10]) <- month(BVAS_debut$BVAS_Dato[1:10]) + 3
# month(BVAS_debut$BVAS_Dato)

# tmp2 <- merge(remisjon, prednisolon, by=c('PasientGUID', 'FormDate'))


# summary(as.numeric(remisjon$tid_diag_bvas))

# gr <- c(0,30, 90, 180, 360, 100000)
# aux <- cut(as.numeric(remisjon$tid_diag_bvas), breaks = gr, include.lowest = T) ## Legg til i fordelingsfigur

# aux <- BVAS[which(BVAS$PasientGUID %in% BVAS$PasientGUID[which(BVAS$Sykdomsvurdering == 'Debut')]), ]
#
# prednisolon <- Medisiner[which(Medisiner$LegemiddelType == "Prednisolon"), ]


#################################################################################
####################  Andel oppfølginger med BVAS (ANCA + andre) eller KERR (Storkarsvaskulitt) #####################

## Lag indikatorfigur

tmp2 <- merge(Oppfolging, BVAS[, c("bvas_samlet", "PasientGUID", "BVAS_Dato")], by.x = c('PasientGUID','OppfolgingsDato'),
      by.y = c('PasientGUID','BVAS_Dato'), all.x = T)
tmp2 <- tmp2[tmp2$Diag_gr_nr %in% 2:3, ]
sum(!is.na(tmp2$bvas_samlet))/dim(tmp2)[1]*100


## Lag tidsvisning
tmp2 %>% group_by(Oppfolgingsaar) %>% summarise(andeloppf = sum(!is.na(bvas_samlet))/length(bvas_samlet)*100,
                                                N = n())


tmp3 <- merge(Oppfolging, KERR[, c("KerrsKriterier_Dato", "PasientGUID", "KerrsKriterierScore")], by.x = c('PasientGUID','OppfolgingsDato'),
              by.y = c('PasientGUID','KerrsKriterier_Dato'), all.x = T)
tmp3 <- tmp3[which(tmp3$Diag_gr_nr == 1), ]
sum(!is.na(tmp3$KerrsKriterierScore))/dim(tmp3)[1]*100

tmp3 %>% group_by(Oppfolgingsaar) %>% summarise(andeloppf = sum(!is.na(KerrsKriterierScore))/length(KerrsKriterierScore)*100,
                                                N = n())


# tmp <- merge
##############################################################################
################################################################################
setdiff(BVAS$PasientGUID, Oppfolging$PasientGUID)
setdiff(Oppfolging$PasientGUID, BVAS$PasientGUID)

aux <- merge(BVAS, Oppfolging, by.x = c('PasientGUID', 'BVAS_Dato'), by.y = c('PasientGUID', 'OppfolgingsDato'))
aux2 <- merge(BVAS, Oppfolging, by.x = c('PasientGUID', 'BVAS_Dato'), by.y = c('PasientGUID', 'FormDate'))
aux2 <- merge(BVAS, Oppfolging, by.x = c('PasientGUID', 'BVAS_Dato'), by.y = c('PasientGUID', 'LastUpdate'))

aux4 <- merge(Oppfolging, BVAS, by = 'PasientGUID', all.x = T)
aux4 <- aux4[!is.na(aux4$BVAS_Dato), ]


aux <- merge(Oppfolging, BVAS, by.x = c('PasientGUID', 'OppfolgingsDato'), by.y = c('PasientGUID', 'BVAS_Dato'),
             all.x = T)
table(aux$bvas_samlet, useNA = 'ifany')

andel_bvas <- aux %>% group_by(Sykehusnavn.x) %>% summarise(antall_utfylt = sum(!is.na(bvas_samlet)),
                                              n = n())
andel_bvas$andel_utfylt <- andel_bvas$antall_utfylt/andel_bvas$n*100


# tmp <- table(BVAS[, c("PasientGUID", "BVAS_Dato")])
# tmp <- as.data.frame(tmp)
# tmp <- tmp[tmp$Freq>1, ]
# tmp <- merge(tmp, Inklusjon[, c("PasientGUID", "Fødselsnummer")], by = "PasientGUID")
#
# tmp2 <-  merge(BVAS[, c("PasientGUID", "BVAS_Dato", "SkjemaGUID")],
#                tmp[, c("PasientGUID", "BVAS_Dato")], by = c('PasientGUID', 'BVAS_Dato'))
# tmp2 %>% group_by(PasientGUID, BVAS_Dato) %>% summarise(SkjemaGUID = SkjemaGUID[1])
#
# BVAS <- BVAS[!(BVAS$SkjemaGUID %in% tmp2$SkjemaGUID), ]

#
tmp <- table(KERR[, c("PasientGUID", "KerrsKriterier_Dato")])
tmp <- as.data.frame(tmp)
tmp <- tmp[tmp$Freq>1, ]
tmp2 <-  merge(KERR[, c("PasientGUID", "KerrsKriterier_Dato", "SkjemaGUID")],
               tmp[, c("PasientGUID", "KerrsKriterier_Dato")], by = c('PasientGUID', 'KerrsKriterier_Dato'))
tmp2 %>% group_by(PasientGUID, KerrsKriterier_Dato) %>% summarise(SkjemaGUID = SkjemaGUID[1])






#############################  Kladd, hjelp Synøve 04.12.2019 ############################################

pnr<-17055231273

Oppfolging[Oppfolging$PasientGUID == Inklusjon[Inklusjon$Fødselsnummer == 17055231273, "PasientGUID"], "EksklusjonsDato"]
Inklusjon[Inklusjon$Fødselsnummer == 17055231273, "PasientGUID"]

Medisiner[Medisiner$PasientGUID == Inklusjon[Inklusjon$Fødselsnummer == 17055231273, "PasientGUID"], ]









