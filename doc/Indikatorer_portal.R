library(norvas)
library(xtable)
library(lubridate)
rm(list = ls())

rap_aar <- 2021
datoTil <- paste0(rap_aar, "-12-31")

Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2022-04-05_0931.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# names(Inklusjon)[names(Inklusjon) == "Fødselsnummer"] <- "PasientGUID"
Oppfolging <- read.table('I:/norvas/DataDump_MRS-PROD_OppfølgingSkjema_2022-04-05_0931.csv', header=TRUE, sep=";",
                         stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2022-04-05_0932.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner <- read.table('I:/norvas/DataDump_MRS-PROD_MedisineringSkjema_2022-04-05_0932.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
BVAS <- read.table('I:/norvas/DataDump_MRS-PROD_BvasSkjema_2022-04-05_0932.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
KERR <- read.table('I:/norvas/DataDump_MRS-PROD_KerrsKriterierSkjema_2022-04-05_0932.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
VDI <- read.table('I:/norvas/DataDump_MRS-PROD_VdiSkjema_2022-04-05_0932.csv', header=TRUE, sep=";",
                  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Alvorlig_infeksjon <- read.table('I:/norvas/DataDump_MRS-PROD_SelvrapportertAlvorligInfeksjonSkjema_2022-04-05_0932.csv',
                                 header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Utredning <- read.table('I:/norvas/DataDump_MRS-PROD_Utredning_2022-04-05_0932.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Labskjema <- read.table('I:/norvas/DataDump_MRS-PROD_BlodprøvesvarSkjema_2022-04-05_0932.csv', header=TRUE, sep=";",
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

# Diagnoser$Navn[Diagnoser$Navn %in% c('Systemisk Vaskulitt sykdom')] <- 'Uspesifisert nekrotiserende vaskulitt'
# Diagnoser <- Diagnoser[-which(Diagnoser$Navn == 'Polymyalgia Rheumatica'), ]

### Foreløpig fiks, fjernes når data er ordnet
Medisiner$Medikamentgruppe[Medisiner$Medikamentgruppe == ""] <- "Andre"

### Ny 18.10.2021: Fjerner medikamenter i kategorien "Andre"
Medisiner <- Medisiner[Medisiner$Medikamentgruppe != "Andre", ]

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

# Inklusjon$Fodselsdato <- personnr2fodselsdato(Inklusjon$PasientId)
Diagnoser <- merge(Diagnoser, Inklusjon[, c("SkjemaGUID", "InklusjonDato")],
                   by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID')

Inklusjon <- merge(Inklusjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr",
                                            "Diag_gr", "Navn")],
                   by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
Inklusjon <- Inklusjon[Inklusjon$Diag_gr_nr != 3, ]

BVAS <- merge(BVAS, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
BVAS <- merge(BVAS, Inklusjon[, c('SkjemaGUID', "InklusjonDato")], by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
Oppfolging <- merge(Oppfolging, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
KERR <- merge(KERR, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
KERR <- merge(KERR, Inklusjon[, c('SkjemaGUID', "InklusjonDato")], by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
VDI <- merge(VDI, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
Medisiner <- merge(Medisiner, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
Alvorlig_infeksjon <- merge(Alvorlig_infeksjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)

Inklusjon$Diagnose_ny <- NA
Inklusjon$Diagnose_ny[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 90] <- 1
Inklusjon$Diagnose_ny[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) > 90] <- 0
Inklusjon <- Inklusjon[!is.na(Inklusjon$InklusjonDato), ]

Inklusjon$Inklusjonsaar <- as.numeric(format(Inklusjon$InklusjonDato, format = '%Y'))
Oppfolging$Oppfolgingsaar <- as.numeric(format(Oppfolging$OppfolgingsDato, format = '%Y'))
# Inklusjon$Inklusjonsalder <- age(Inklusjon$Fodselsdato, Inklusjon$InklusjonDato)
Inklusjon$Inklusjonsalder <- Inklusjon$PatientAge
BVAS$BVAS_aar <- as.numeric(format(BVAS$BVAS_Dato, format = '%Y'))
KERR$KERR_aar <- as.numeric(format(KERR$KerrsKriterier_Dato, format = '%Y'))
# Diagnoser$DiagnoseAlder <- age(Diagnoser$Fodselsdato, Diagnoser$Diagnose_Klinisk_Dato)
Diagnoser$DiagnoseAlder <- Diagnoser$PatientAge
# Alvorlig_infeksjon$inf_alder <- age(Alvorlig_infeksjon$Fodselsdato, Alvorlig_infeksjon$SelvrapportertAlvorligInfeksjon_Registrert_Dato)
Alvorlig_infeksjon$inf_alder <- Alvorlig_infeksjon$PatientAge

Oppfolging <- Oppfolging[Oppfolging$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
Diagnoser <- Diagnoser[Diagnoser$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
Medisiner <- Medisiner[Medisiner$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
BVAS <- BVAS[BVAS$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
KERR <- KERR[KERR$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
VDI <- VDI[VDI$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
Alvorlig_infeksjon <- Alvorlig_infeksjon[Alvorlig_infeksjon$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
Utredning <- Utredning[Utredning$PasientGUID %in% unique(Inklusjon$PasientGUID), ]
Labskjema <- Labskjema[Labskjema$PasientGUID %in% unique(Inklusjon$PasientGUID), ]

#### FJERN MEDISINER UNDER ANNET, DVS. FOLSYRE OG ANNETIMPORTERT
# Medisiner <- Medisiner[-which(Medisiner$Medikamentgruppe=='Annet'), ]

#### HVIS SAMME PASIENT HAR OPPSTART MED SAMME LEGEMIDDEL PÅ SAMME DAG, VELG DEN MED TIDLIGST SLUTTDATO
# Medisiner$LegemiddelType <- Medisiner$LegemiddelType2020
# Medisiner$LegemiddelType[Medisiner$LegemiddelType==""] <- Medisiner$LegemiddelType2019[Medisiner$LegemiddelType==""]
tmp <- Medisiner %>%
  group_by(PasientGUID, Med_StartDato, LegemiddelGenerisk) %>%
  summarise('ant_samme_startdato' = n(),
            Med_SluttDato_min = min(Med_SluttDato, na.rm = T),
            SkjemaGUID_min = if (is.na(which(Med_SluttDato == min(Med_SluttDato, na.rm = T))[1])) {SkjemaGUID[1]}
            else {SkjemaGUID[which(Med_SluttDato == min(Med_SluttDato, na.rm = T))[1]]})

Medisiner <- merge(Medisiner, tmp[, c("SkjemaGUID_min", "ant_samme_startdato")], by.x = "SkjemaGUID", by.y = "SkjemaGUID_min")


#### KOBLING MELLEOM UNITID OG SYKEHUSNAVN
kobl_unitid_shusnavn_norvas <- Inklusjon[match(unique(Inklusjon$UnitId), Inklusjon$UnitId), c("UnitId", "Sykehusnavn")]


########### Andel med 2 eller flere oppfølginger per år ##################################
tmp <- as_tibble(Inklusjon[, c("UnitId", "PasientGUID", "Inklusjonsaar")])
aux <- tibble(UnitId=integer(), PasientGUID=character(), Inklusjonsaar=double(), Oppfolgingsaar=double())

for (id in tmp$PasientGUID) {                                       # For alle pasienter, lag en rad for hvert år pasienten har vært inkludert
  for (aar in tmp$Inklusjonsaar[tmp$PasientGUID==id]:rap_aar) {        # som inneholder UnitId, pasientguid, inklusjonsår og år for potensiell oppfølging
    aux <- bind_rows(aux, bind_cols(tmp[tmp$PasientGUID==id, ], Oppfolgingsaar=aar))

  }
}

Oppfolging$FolgtOpp <- 1 # Legg til en indikatorvariabel som sier om pasinten har vært følgt opp
Oppfolging_v4 <- merge(aux, Oppfolging[, c("UnitId", "PasientGUID", "SkjemaGUID", "Oppfolgingsaar", "FolgtOpp")], by = c("PasientGUID","Oppfolgingsaar") , all.x = T) ## Koble inn Oppfølginginfo for årene det finnes.
Oppfolging_v4$UnitId <- Oppfolging_v4$UnitId.y  ## Bruk UnitId fra oppfølgingsenhet der den finnes,
Oppfolging_v4$UnitId[is.na(Oppfolging_v4$UnitId.y)] <- Oppfolging_v4$UnitId.x[is.na(Oppfolging_v4$UnitId.y)]  # ellers bruk UnitId fra inklusjon
Oppfolging_v4$FolgtOpp[is.na(Oppfolging_v4$FolgtOpp)] <- 0 # Tomme verdier av indikator settes til 0
Oppfolging_v4 <- Oppfolging_v4[Oppfolging_v4$Oppfolgingsaar > Oppfolging_v4$Inklusjonsaar, ] # Beholder kun oppføringer der Oppfolgingsaar > Inklusjonsaar

ant.oppf.pr.pas.pr.hf.pr.aar <- Oppfolging_v4 %>% group_by(PasientGUID, UnitId, Oppfolgingsaar) %>% summarise(Antall_oppf = sum(FolgtOpp)) %>% ungroup()

ind2_Andelminimum_2oppf_NorVas <- ant.oppf.pr.pas.pr.hf.pr.aar
ind2_Andelminimum_2oppf_NorVas$Teller <- as.numeric(ind2_Andelminimum_2oppf_NorVas$Antall_oppf>=2)
ind2_Andelminimum_2oppf_NorVas$Nevner <- 1
ind2_Andelminimum_2oppf_NorVas <- ind2_Andelminimum_2oppf_NorVas[, c(2,3,5,6)]

ind2_Andelminimum_2oppf_NorVas$ind_id <- "norvas_minimum_2oppf"
Indikatorer <- ind2_Andelminimum_2oppf_NorVas

########## BVAS/Kerr ved oppfølging ################################################

######## BVAS ##############
KERR$Sykdomsvurdering[KERR$Sykdomsvurdering==-1] <- NA

tmp3 <- merge(Oppfolging, KERR[, c("KerrsKriterier_Dato", "PasientGUID", "Sykdomsvurdering")], by.x = c('PasientGUID','OppfolgingsDato'),
              by.y = c('PasientGUID','KerrsKriterier_Dato'), all.x = T)
tmp3 <- tmp3[which(tmp3$Diag_gr_nr == 1), ]
tmp2 <- merge(Oppfolging, BVAS[, c("SykdomsvurderingLabel", "PasientGUID", "BVAS_Dato")], by.x = c('PasientGUID','OppfolgingsDato'),
              by.y = c('PasientGUID','BVAS_Dato'), all.x = T)
tmp2 <- tmp2[tmp2$Diag_gr_nr %in% c(2,3), ]

Ind3_AndelBVAS_NorVas <- tmp2[, c("UnitId", "Oppfolgingsaar", "SykdomsvurderingLabel")]
Ind3_AndelBVAS_NorVas$Teller <- as.numeric(!is.na(Ind3_AndelBVAS_NorVas$SykdomsvurderingLabel))
Ind3_AndelBVAS_NorVas$Nevner <- 1
Ind3_AndelBVAS_NorVas <- Ind3_AndelBVAS_NorVas[ , -3]
Ind3_AndelBVAS_NorVas$ind_id <- "norvas_bvas"
Indikatorer <- bind_rows(Indikatorer, Ind3_AndelBVAS_NorVas)

########  KERR ##############
KERR$Sykdomsvurdering[KERR$Sykdomsvurdering==-1] <- NA

tmp3 <- merge(Oppfolging, KERR[, c("KerrsKriterier_Dato", "PasientGUID", "Sykdomsvurdering")], by.x = c('PasientGUID','OppfolgingsDato'),
              by.y = c('PasientGUID','KerrsKriterier_Dato'), all.x = T)
tmp3 <- tmp3[which(tmp3$Diag_gr_nr == 1), ]

ind4_AndelKerr_NorVas <- tmp3[, c("UnitId", "Oppfolgingsaar", "Sykdomsvurdering")]
ind4_AndelKerr_NorVas$Teller <- as.numeric(!is.na(ind4_AndelKerr_NorVas$Sykdomsvurdering))
ind4_AndelKerr_NorVas$Nevner <- 1
ind4_AndelKerr_NorVas <- ind4_AndelKerr_NorVas[ , -3]
ind4_AndelKerr_NorVas$ind_id <- "norvas_kerr"
Indikatorer <- bind_rows(Indikatorer, ind4_AndelKerr_NorVas)


########### ANCA test ved debut  ################################

  # Nysyke definert som de med diagnosedato innen 30 dager av inklusjonsdato
nysyke <- Inklusjon[which(abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 30), ]
aux <- merge(nysyke, Labskjema, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)   # Henter info fra labskjema
aux <- aux[which(aux$Diag_gr_nr == 2), ]                                                  # Ser kun på ANCA

aux2 <- aux[which(abs(difftime(aux$Diagnose_Klinisk_Dato, aux$Blodprover_Dato, units = 'days')) <= 30), ] # Avgrenser til tilfeller med
# blodprøvedato innen 30 dager av diagnosedato
tmp2 <- aux2 %>% group_by(Sykehusnavn.x, SkjemaGUID) %>%
  summarise(anca = ((1 %in% PR3AncaPositiv) | (0 %in% PR3AncaPositiv))) # Ett resultat per sykehus og skjemaguid,

tmp3 <- tmp2 %>% ungroup()
names(tmp3) <- c("Sykehusnavn", "SkjemaGUID", "Teller")
tmp3$Teller <- as.numeric(tmp3$Teller)

tmp4 <- nysyke[which(nysyke$Diag_gr_nr == 2), c("Sykehusnavn", "SkjemaGUID")]
tmp4 <- tmp4[!(tmp4$SkjemaGUID %in% tmp3$SkjemaGUID), ]
tmp4$Teller <- 0
tmp4 <- as_tibble(tmp4)

Ind1_ANCAvdebut_NorVas <- bind_rows(tmp3, tmp4)
Ind1_ANCAvdebut_NorVas$UnitId <- nysyke$UnitId[match(Ind1_ANCAvdebut_NorVas$Sykehusnavn, nysyke$Sykehusnavn)]
Ind1_ANCAvdebut_NorVas$Nevner <- 1
Ind1_ANCAvdebut_NorVas$Aar <- nysyke$Inklusjonsaar[match(Ind1_ANCAvdebut_NorVas$SkjemaGUID, nysyke$SkjemaGUID)]
Ind1_ANCAvdebut_NorVas <- Ind1_ANCAvdebut_NorVas[, c(4,6,3,5)]
Ind1_ANCAvdebut_NorVas$ind_id <- "norvas_anca_ved_debut"
Indikatorer <- bind_rows(Indikatorer, Ind1_ANCAvdebut_NorVas)


############## Andel remisjon 6mnd, ANCA  ######################
BVAS$tid_diag_bvas <- difftime(BVAS$BVAS_Dato, BVAS$Diagnose_Klinisk_Dato, units = 'days')
BVAS <- BVAS[order(BVAS$BVAS_Dato, decreasing = F), ]

nysyke <- BVAS[which(abs(difftime(BVAS$BVAS_Dato, BVAS$InklusjonDato, units = 'days')) <= 30), ]
nysyke <- nysyke[which(nysyke$Sykdomsvurdering == 1 & nysyke$Diag_gr_nr==2), ]
nysyke <- nysyke[which(nysyke$BVAS_Dato <= datoTil), ]
BVAS_utvalg <- BVAS[which(BVAS$PasientGUID %in% nysyke$PasientGUID), ]

remisjon <- BVAS_utvalg[which(BVAS_utvalg$Sykdomsvurdering == 5 & BVAS_utvalg$BVAS_Dato <= datoTil), ]
remisjon <- remisjon[match(unique(remisjon$PasientGUID), remisjon$PasientGUID), ]

remisjon$remisjon_indikator <- 0
remisjon$remisjon_indikator[remisjon$tid_diag_bvas <= 210] <- 1
ind5_AndelAnCA_remisjon_NorVas <- remisjon[, c("UnitId", "BVAS_aar", "remisjon_indikator")]
ind5_AndelAnCA_remisjon_NorVas$Nevner <- 1
names(ind5_AndelAnCA_remisjon_NorVas)[2:3] <- c("Aar", "Teller")
ind5_AndelAnCA_remisjon_NorVas$ind_id <- "norvas_anca_remisjon"
Indikatorer <- bind_rows(Indikatorer, ind5_AndelAnCA_remisjon_NorVas)

########### Andel remisjon 6mnd, GVV ###################################
KERR$tid_diag_kerr <- difftime(KERR$KerrsKriterier_Dato, KERR$Diagnose_Klinisk_Dato, units = 'days')
KERR <- KERR[order(KERR$KerrsKriterier_Dato, decreasing = F), ]

nysyke <- KERR[which(abs(difftime(KERR$KerrsKriterier_Dato, KERR$InklusjonDato, units = 'days')) <= 30), ]
nysyke <- nysyke[which(nysyke$Sykdomsvurdering == 1 & nysyke$Diag_gr_nr==1), ]
nysyke <- nysyke[which(nysyke$KerrsKriterier_Dato <= datoTil), ]
KERR_utvalg <- KERR[which(KERR$PasientGUID %in% nysyke$PasientGUID), ]

remisjon <- KERR_utvalg[which(KERR_utvalg$Sykdomsvurdering == 2 & KERR_utvalg$KerrsKriterier_Dato <= datoTil), ]
remisjon <- remisjon %>% group_by(PasientGUID, Sykehusnavn) %>%
  summarise(tid_diag_kerr = min(tid_diag_kerr, na.rm = T),
            UnitId = UnitId[tid_diag_kerr==min(tid_diag_kerr, na.rm = T)][1],
            KERR_aar = KERR_aar[tid_diag_kerr==min(tid_diag_kerr, na.rm = T)][1])

remisjon$remisjon_indikator <- 0
remisjon$remisjon_indikator[remisjon$tid_diag_kerr <= 210] <- 1

ind_AndelStorkar_remisjon_NorVas <- remisjon[, c("Sykehusnavn", "UnitId", "KERR_aar", "remisjon_indikator")]
ind_AndelStorkar_remisjon_NorVas$Nevner <- 1
names(ind_AndelStorkar_remisjon_NorVas)[3:4] <- c("Aar", "Teller")
ind_AndelStorkar_remisjon_NorVas$ind_id <- "norvas_storkar_remisjon"
Indikatorer <- bind_rows(Indikatorer, ind_AndelStorkar_remisjon_NorVas[, -1])


############### Andel på prednisolon etter 6 mnd ########################################
prednisolon <- Medisiner[which(Medisiner$LegemiddelGenerisk == "Prednisolon"), ]

nysyke <- BVAS[which(abs(difftime(BVAS$BVAS_Dato, BVAS$InklusjonDato, units = 'days')) <= 30), ]
nysyke <- nysyke[which(nysyke$Sykdomsvurdering == 1 & nysyke$Diag_gr_nr==2), ]

PaaPrednisolon <- merge(nysyke, prednisolon, by='HovedskjemaGUID', all.x = T)
PaaPrednisolon$MndFraDebut_6 <- 0
PaaPrednisolon$MndFraDebut_6[which((PaaPrednisolon$BVAS_Dato %m+% months(7) >= PaaPrednisolon$Med_StartDato & PaaPrednisolon$BVAS_Dato %m+% months(7) <= PaaPrednisolon$Med_SluttDato) |
                                  (PaaPrednisolon$BVAS_Dato %m+% months(7) >= PaaPrednisolon$Med_StartDato & is.na(PaaPrednisolon$Med_SluttDato)))] <- 1
indikator_med6mnd <- as.data.frame.matrix(table(PaaPrednisolon$PasientGUID.x, PaaPrednisolon$MndFraDebut_6, useNA = 'ifany'))

PaaPrednisolon$Dose[PaaPrednisolon$PasientGUID.x %in% row.names(indikator_med6mnd[indikator_med6mnd$`1`==0, ])] <- 0
PaaPrednisolon$MndFraDebut_6[PaaPrednisolon$PasientGUID.x %in% row.names(indikator_med6mnd[indikator_med6mnd$`1`==0, ])] <- 1
PaaPrednisolon <- PaaPrednisolon[PaaPrednisolon$MndFraDebut_6==1, ]

ind6_AndelANCA_Prednisolon_NorVas <- PaaPrednisolon[, c("UnitId.x", "BVAS_aar", "Dose")]
ind6_AndelANCA_Prednisolon_NorVas$Teller <- as.numeric(PaaPrednisolon$Dose <= 5)
ind6_AndelANCA_Prednisolon_NorVas$Nevner <- 1
ind6_AndelANCA_Prednisolon_NorVas <- ind6_AndelANCA_Prednisolon_NorVas[, -3]
names(ind6_AndelANCA_Prednisolon_NorVas)[1:2] <- c("UnitId", "Aar")

ind6_AndelANCA_Prednisolon_NorVas$ind_id <- "norvas_anca_prednisolon"
Indikatorer <- bind_rows(Indikatorer, ind6_AndelANCA_Prednisolon_NorVas)

##############################################################################

nysyke <- KERR[which(abs(difftime(KERR$KerrsKriterier_Dato, KERR$InklusjonDato, units = 'days')) <= 30), ]
nysyke <- nysyke[which(nysyke$Sykdomsvurdering == 1 & nysyke$Diag_gr_nr==1), ]

PaaPrednisolon <- merge(nysyke, prednisolon, by='HovedskjemaGUID', all.x = T)
PaaPrednisolon$MndFraDebut_6 <- 0
PaaPrednisolon$MndFraDebut_6[which((PaaPrednisolon$KerrsKriterier_Dato %m+% months(7) >= PaaPrednisolon$Med_StartDato & PaaPrednisolon$KerrsKriterier_Dato %m+% months(7) <= PaaPrednisolon$Med_SluttDato) | (PaaPrednisolon$KerrsKriterier_Dato %m+% months(7) >= PaaPrednisolon$Med_StartDato & is.na(PaaPrednisolon$Med_SluttDato)))] <- 1

indikator_med6mnd <- as.data.frame.matrix(table(PaaPrednisolon$PasientGUID.x, PaaPrednisolon$MndFraDebut_6, useNA = 'ifany'))

PaaPrednisolon$Dose[PaaPrednisolon$PasientGUID.x %in% row.names(indikator_med6mnd[indikator_med6mnd$`1`==0, ])] <- 0
PaaPrednisolon$MndFraDebut_6[PaaPrednisolon$PasientGUID.x %in% row.names(indikator_med6mnd[indikator_med6mnd$`1`==0, ])] <- 1
PaaPrednisolon <- PaaPrednisolon[PaaPrednisolon$MndFraDebut_6==1, ]

ind_Andelstorvas_Prednisolon_NorVas_v2_kerr <- PaaPrednisolon[, c("UnitId.x", "KERR_aar", "Dose")]
ind_Andelstorvas_Prednisolon_NorVas_v2_kerr$Teller <- as.numeric(PaaPrednisolon$Dose <= 7.5)
ind_Andelstorvas_Prednisolon_NorVas_v2_kerr$Nevner <- 1
ind_Andelstorvas_Prednisolon_NorVas_v2_kerr<- ind_Andelstorvas_Prednisolon_NorVas_v2_kerr[, -3]
names(ind_Andelstorvas_Prednisolon_NorVas_v2_kerr)[1:2] <- c("UnitId", "Aar")

ind_Andelstorvas_Prednisolon_NorVas_v2_kerr$ind_id <- "norvas_storvas_prednisolon"
Indikatorer <- bind_rows(Indikatorer, ind_Andelstorvas_Prednisolon_NorVas_v2_kerr)

############## Andel utfort ymse #################################

######### Utredning ANCA ##########################################
nysyke <- Inklusjon[which(abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 30), ]
antall_nysyke_anca <- length(which(nysyke$Diag_gr_nr == 2))

aux <- merge(nysyke, Utredning, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
aux <- aux[which(aux$Diag_gr_nr == 2), ]

jalla <- aux
jalla$ved_debut <- FALSE
jalla$ved_debut[which(abs(difftime(jalla$Diagnose_Klinisk_Dato, jalla$Billeddiagnostikk_Dato, units = 'days')) <= 30)] <- TRUE
jalla$samlet <- jalla$CtThorax != -1
jalla$samlet[is.na(jalla$samlet)] <- FALSE
jalla$samlet2 <- (jalla$CtBihuler != -1 | jalla$MrBihuler != -1)
jalla$samlet2[is.na(jalla$samlet2)] <- FALSE
jalla <- jalla %>% group_by(PasientGUID.x) %>% summarise(ct_thorax = max(samlet & ved_debut),
                                                         ctmr_bihuler = max(samlet2 & ved_debut),
                                                         UnitId = UnitId.x[1],
                                                         Aar = Inklusjonsaar[1])
jalla$Nevner <- 1

ind_andel_ct_thorax <- jalla[, c("UnitId", "Aar", "ct_thorax", "Nevner")]
names(ind_andel_ct_thorax)[names(ind_andel_ct_thorax)=="ct_thorax"] <- "Teller"
ind_andel_ct_thorax$ind_id <- "norvas_andel_ct_thorax_anca"

ind_andel_ctmr_bihuler <- jalla[, c("UnitId", "Aar", "ctmr_bihuler", "Nevner")]
names(ind_andel_ctmr_bihuler)[names(ind_andel_ctmr_bihuler)=="ctmr_bihuler"] <- "Teller"
ind_andel_ctmr_bihuler$ind_id <- "norvas_andel_ctmr_bihuler_anca"

Indikatorer <- bind_rows(Indikatorer, ind_andel_ct_thorax)
Indikatorer <- bind_rows(Indikatorer, ind_andel_ctmr_bihuler)

####### Utredning storkarsvaskulitt ##############################
aux <- merge(nysyke, Utredning, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
aux <- aux[which(aux$Diag_gr_nr == 1), ]

jalla <- aux
jalla$ved_debut <- FALSE
jalla$ved_debut[which(abs(difftime(jalla$Diagnose_Klinisk_Dato, jalla$Billeddiagnostikk_Dato, units = 'days')) <= 30)] <- TRUE
jalla$samlet <- jalla$UlMellomstoreKar != -1 | jalla$CtMellomstoreKar != -1 | jalla$CtAorta != -1 |
  jalla$MrMellomstoreKar!=-1 | jalla$MrAorta != -1
jalla$samlet[is.na(jalla$samlet)] <- FALSE
jalla <- jalla %>% group_by(PasientGUID.x) %>% summarise(ulctmr_storkar = max(samlet & ved_debut),
                                                         UnitId = UnitId.x[1],
                                                         Aar = Inklusjonsaar[1])
jalla$Nevner <- 1
jalla$Teller <- jalla$ulctmr_storkar

ind_andel_ulctmr_kar <- jalla[, c("UnitId", "Aar", "Teller", "Nevner")]
ind_andel_ulctmr_kar$ind_id <- "norvas_andel_ulctmr_storkar"

Indikatorer <- bind_rows(Indikatorer, ind_andel_ulctmr_kar)


aux <- merge(nysyke, Utredning, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
aux <- aux[aux$Navn == "Kjempecelle Arteritt", ]

jalla <- aux
jalla$ved_debut <- FALSE
jalla$ved_debut[which(abs(difftime(jalla$Diagnose_Klinisk_Dato, jalla$Billeddiagnostikk_Dato, units = 'days')) <= 30)] <- TRUE
jalla$samlet <- jalla$BlodKar!=-1 | jalla$MrMellomstoreKar!=-1 | jalla$UlMellomstoreKar!=-1
jalla$samlet[is.na(jalla$samlet)] <- FALSE
jalla <- jalla %>% group_by(PasientGUID.x) %>% summarise(utredet_GCA = max(samlet & ved_debut),
                                                         UnitId = UnitId.x[1],
                                                         Aar = Inklusjonsaar[1])
jalla$Nevner <- 1
jalla$Teller <- jalla$utredet_GCA
ind_andel_utredet_GCA <- jalla[, c("UnitId", "Aar", "Teller", "Nevner")]
ind_andel_utredet_GCA$ind_id <- "norvas_andel_utredet_GCA"

Indikatorer <- bind_rows(Indikatorer, ind_andel_utredet_GCA)

################ Tilpasninger til ønsket format ###################################

nokkeltall <- Inklusjon %>% group_by(Inklusjonsaar) %>% summarise("Antall avdelinger" = length(unique(UnitId)),
                                                                  "Antall inkluderte" = n(),
                                                                  "Inklusjonsalder, gj.sn." = mean(Inklusjonsalder),
                                                                  "Inklusjonsalder, median" = median(Inklusjonsalder),
                                                                  "Andel kvinner" = sum(ErMann==0)/n()*100)

Diagnoser$Inklusjonsaar <- as.numeric(format(Diagnoser$InklusjonDato, "%Y"))

Indikatorer$Oppfolgingsaar[is.na(Indikatorer$Oppfolgingsaar)] <- Indikatorer$Aar[is.na(Indikatorer$Oppfolgingsaar)]
kobl_unitid_shusnavn_norvas$Sykehusnavn <- as.character(kobl_unitid_shusnavn_norvas$Sykehusnavn)

kobl_unitid_shusnavn_norvas <- data.frame(kobl_unitid_shusnavn_norvas, qmongrdata::SykehusNavnStruktur[stringdist::amatch(kobl_unitid_shusnavn_norvas$Sykehusnavn, qmongrdata::SykehusNavnStruktur$SykehusNavn ), c("OrgNrHF", "HF", "OrgNrShus", "SykehusNavn")])

kobl_unitid_shusnavn_norvas$OrgNrShus[kobl_unitid_shusnavn_norvas$UnitId == 104579] <- 974749025
kobl_unitid_shusnavn_norvas$OrgNrShus[kobl_unitid_shusnavn_norvas$UnitId == 601159] <- 974795787
kobl_unitid_shusnavn_norvas$OrgNrShus[kobl_unitid_shusnavn_norvas$UnitId == 700701] <- 974795361
kobl_unitid_shusnavn_norvas$OrgNrShus[kobl_unitid_shusnavn_norvas$UnitId == 4210614] <- 974795515
kobl_unitid_shusnavn_norvas$OrgNrShus[kobl_unitid_shusnavn_norvas$UnitId == 104209] <- 873255102
kobl_unitid_shusnavn_norvas$OrgNrHF[kobl_unitid_shusnavn_norvas$UnitId == 104579] <- 883974832
kobl_unitid_shusnavn_norvas$OrgNrHF[kobl_unitid_shusnavn_norvas$UnitId == 601159] <- 983974899
kobl_unitid_shusnavn_norvas$OrgNrHF[kobl_unitid_shusnavn_norvas$UnitId == 700701] <- 983974910
kobl_unitid_shusnavn_norvas$OrgNrHF[kobl_unitid_shusnavn_norvas$UnitId == 4210614] <- 983974929
kobl_unitid_shusnavn_norvas$OrgNrHF[kobl_unitid_shusnavn_norvas$UnitId == 104209] <- 981275721

kobl_unitid_shusnavn_norvas$HF <- qmongrdata::SykehusNavnStruktur$HF[match(kobl_unitid_shusnavn_norvas$OrgNrHF, qmongrdata::SykehusNavnStruktur$OrgNrHF)]
kobl_unitid_shusnavn_norvas$SykehusNavn <- qmongrdata::SykehusNavnStruktur$SykehusNavn[match(kobl_unitid_shusnavn_norvas$OrgNrShus, qmongrdata::SykehusNavnStruktur$OrgNrShus)]

Indikatorer$orgnr <- kobl_unitid_shusnavn_norvas$OrgNrShus[match(Indikatorer$UnitId, kobl_unitid_shusnavn_norvas$UnitId)]
Indikatorer <- Indikatorer[, c(7,2,3,4,5)]
names(Indikatorer)[2:4] <- c("year", "var", "denominator")
Indikatorer$context <- "caregiver"

write.csv2(Indikatorer, 'I:/norvas/norvas_ind_20210621.csv', na = '', fileEncoding = 'UTF-8', row.names = F)

dg_D690_andre <- read.table("I:/norvas/DGA_resultat_hf_D690.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_D891_andre <- read.table("I:/norvas/DGA_resultat_hf_D891.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_I776_storkar <- read.table("I:/norvas/DGA_resultat_hf_I776.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_M300_andre <- read.table("I:/norvas/DGA_resultat_hf_M300.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_M301_anca <- read.table("I:/norvas/DGA_resultat_hf_M301.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_M313_anca <- read.table("I:/norvas/DGA_resultat_hf_M313.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_M314_storkar <- read.table("I:/norvas/DGA_resultat_hf_M314.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_M315_storkar <- read.table("I:/norvas/DGA_resultat_hf_M315_M316.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_M317_anca <- read.table("I:/norvas/DGA_resultat_hf_M317.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_M319_andre <- read.table("I:/norvas/DGA_resultat_hf_M319.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_M352_andre <- read.table("I:/norvas/DGA_resultat_hf_M352.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_landet <- read.table("I:/norvas/DGA_resultat_landet_hf.csv", sep = ";", fileEncoding = "Latin1", header = T)
dg_landet_innrap <- read.table("I:/norvas/DGA_resultat_landet_hf_innrapp.csv", sep = ";", fileEncoding = "Latin1", header = T)

dg_andre <- merge(dg_D690_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                  dg_D891_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                  by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
  merge(dg_M300_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3")) %>%
  merge(dg_M319_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".4")) %>%
  merge(dg_M352_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".5"))
dg_andre[is.na(dg_andre)] <- 0
dg_andre$norvas <- rowSums(dg_andre[, c("Begge.1", "Kun_norvas.1", "Begge.2", "Kun_norvas.2", "Begge", "Kun_norvas", "Begge.4", "Kun_norvas.4",
                                        "Begge.5", "Kun_norvas.5")])
dg_andre$totalt <- rowSums(dg_andre[, c("Total.1", "Total.2", "Total", "Total.4", "Total.5")])
dg_andre$DG <- dg_andre$norvas/dg_andre$totalt*100

dg_storkar <- merge(dg_I776_storkar[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                    dg_M314_storkar[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                    by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
  merge(dg_M315_storkar[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3"))
dg_storkar[is.na(dg_storkar)] <- 0
dg_storkar$norvas <- rowSums(dg_storkar[, c("Begge.1", "Kun_norvas.1", "Begge.2", "Kun_norvas.2", "Begge", "Kun_norvas")])
dg_storkar$totalt <- rowSums(dg_storkar[, c("Total.1", "Total.2", "Total")])
dg_storkar$DG <- dg_storkar$norvas/dg_storkar$totalt*100

dg_anca <- merge(dg_M301_anca[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                 dg_M313_anca[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                 by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
  merge(dg_M317_anca[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3"))
dg_anca[is.na(dg_anca)] <- 0
dg_anca$norvas <- rowSums(dg_anca[, c("Begge.1", "Kun_norvas.1", "Begge.2", "Kun_norvas.2", "Begge", "Kun_norvas")])
dg_anca$totalt <- rowSums(dg_anca[, c("Total.1", "Total.2", "Total")])
dg_anca$DG <- dg_anca$norvas/dg_anca$totalt*100

dg_landet_innrap$norvas <- rowSums(dg_landet_innrap[, c("Begge", "Kun_norvas")])
dg_landet_innrap$totalt <- dg_landet_innrap$Total

dg_anca$ind_id <- "norvas_dg_anca"
dg_storkar$ind_id <- "norvas_dg_storkar"
dg_andre$ind_id <- "norvas_dg_andre"
dg_landet_innrap$ind_id <- "norvas_dg_alle"

dg_samlet <- bind_rows(dg_anca[, c("hf_standard", "norvas", "totalt", "ind_id")],
                       dg_storkar[, c("hf_standard", "norvas", "totalt", "ind_id")],
                       dg_andre[, c("hf_standard", "norvas", "totalt", "ind_id")],
                       dg_landet_innrap[, c("hf_standard", "norvas", "totalt", "ind_id")])

dg_samlet$orgnr <- qmongrdata::SykehusNavnStruktur[stringdist::amatch(dg_samlet$hf_standard, qmongrdata::SykehusNavnStruktur$HF), c("OrgNrShus")]
dg_samlet$orgnr[dg_samlet$hf_standard == "Betanien, Skien"] <- 873255102
dg_samlet$orgnr[dg_samlet$hf_standard == "Haugesund sanitetsforenings revmatismesykehus"] <- 974724774
dg_samlet$orgnr[dg_samlet$hf_standard == "Martina Hansens hospital"] <- 974116588
dg_samlet$orgnr[dg_samlet$hf_standard == "Revmatismesykehuset, Lillehammer"] <- 874632562
dg_samlet$orgnr[dg_samlet$hf_standard == "Universitetssykehuset Nord-Norge HF"] <- 974795787

dg_samlet <- dg_samlet[!is.na(dg_samlet$orgnr), ]

names(dg_samlet)[match(c("norvas", "totalt"), names(dg_samlet))] <- c("var", "denominator")
dg_samlet$year <- 2020
dg_samlet$context <- "caregiver"
dg_samlet <- dg_samlet[, c("orgnr", "year", "var", "denominator", "ind_id", "context")]

# write.csv2(dg_samlet, 'I:/norvas/norvas_dg_samlet.csv', na = '', fileEncoding = 'UTF-8', row.names = F)














