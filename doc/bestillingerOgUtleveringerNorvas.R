# Filtreringer som gjøres for alle utvalg:
# - Inklusjon og Diagnoser: Kun nyeste beholdes
#  - Kun diagnoseskjema med tilhørende inklusjonsskjema beholdes

library(norvas)
library(xtable)
library(lubridate)
library(rapFigurer)
rm(list = ls())

## Utlevering PhD-prosjekt Hans Kristian Skaug 31.03.2022  ##################


Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2022-03-17_1013.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
names(Inklusjon)[names(Inklusjon) == "Fødselsnummer"] <- "PasientGUID"
Oppfolging <- read.table('I:/norvas/DataDump_MRS-PROD_OppfølgingSkjema_2022-03-17_1013.csv', header=TRUE, sep=";",
                         stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2022-03-17_1013.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner <- read.table('I:/norvas/DataDump_MRS-PROD_MedisineringSkjema_2022-03-17_1013.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
BVAS <- read.table('I:/norvas/DataDump_MRS-PROD_BvasSkjema_2022-03-17_1013.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
KERR <- read.table('I:/norvas/DataDump_MRS-PROD_KerrsKriterierSkjema_2022-03-17_1014.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
VDI <- read.table('I:/norvas/DataDump_MRS-PROD_VdiSkjema_2022-03-17_1013.csv', header=TRUE, sep=";",
                  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Alvorlig_infeksjon <- read.table('I:/norvas/DataDump_MRS-PROD_SelvrapportertAlvorligInfeksjonSkjema_2022-03-17_1014.csv',
                                 header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Utredning <- read.table('I:/norvas/DataDump_MRS-PROD_Utredning_2022-03-17_1014.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Labskjema <- read.table('I:/norvas/DataDump_MRS-PROD_BlodprøvesvarSkjema_2022-03-17_1013.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')







## Legemidler under "Annet" 14.10.2021  ##################
Medisiner <- read.table('I:/norvas/DataDump_MRS-PROD_MedisineringSkjema_2021-05-18_1100.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
medliste1 <- Medisiner[which(Medisiner$Medikamentgruppe==""), c("LegemiddelType2020", "LegemiddelType2019",
                                                                "Legemiddel", "Medikamentgruppe", "LegemiddelNr")]
Medisiner <- norvasPreprosess(Medisiner)
Medisiner$Medikamentgruppe[Medisiner$Medikamentgruppe == ""] <- "Andre"
medliste2 <- Medisiner[which(Medisiner$Medikamentgruppe=="Andre"), c("LegemiddelType2020", "LegemiddelType2019",
                                                                    "Legemiddel", "Medikamentgruppe", "LegemiddelNr",
                                                                    "LegemiddelGenerisk", "LegemiddelTypeLabel")]
# table(medliste2[, "Legemiddel"])
write.csv2(medliste2, "I:/norvas/Medliste_norvas.csv", fileEncoding = "Latin1", row.names = F)


## Pasienter på cyclofosfamid og Rituximab samtidig 27.04.2021  #######################
Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-03-08_0852.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Inklusjon_pguid <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-03-08_0854.csv', header=TRUE, sep=";",
                              stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Inklusjon$PasientGUID <- Inklusjon_pguid$PasientGUID
Inklusjon <- merge(Inklusjon, Inklusjon_pguid[, c("SkjemaGUID", "PasientGUID")], by ='SkjemaGUID')
Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2021-03-08_0858.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner <- read.table('I:/norvas/DataDump_MRS-PROD_MedisineringSkjema_2021-03-08_1410.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

Inklusjon <- norvasPreprosess(Inklusjon)
Diagnoser <- norvasPreprosess(Diagnoser)
Medisiner <- norvasPreprosess(Medisiner)
Diagnoser$Navn[Diagnoser$Navn %in% c('Systemisk Vaskulitt sykdom')] <- 'Uspesifisert nekrotiserende vaskulitt'
Diagnoser <- Diagnoser[-which(Diagnoser$Navn == 'Polymyalgia Rheumatica'), ]

Medisiner <- merge(Medisiner, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
Medisiner <- Medisiner[Medisiner$Diag_gr_nr %in% 2, ]
Medisiner$LegemiddelType <- Medisiner$LegemiddelType2020
Medisiner$LegemiddelType[Medisiner$LegemiddelType==""] <- Medisiner$LegemiddelType2019[Medisiner$LegemiddelType==""]
tmp <- Medisiner %>%
  group_by(PasientGUID, Med_StartDato, LegemiddelType) %>%
  summarise('ant_samme_startdato' = n(),
            Med_SluttDato_min = min(Med_SluttDato, na.rm = T),
            SkjemaGUID_min = if (is.na(which(Med_SluttDato == min(Med_SluttDato, na.rm = T))[1])) {SkjemaGUID[1]}
            else {SkjemaGUID[which(Med_SluttDato == min(Med_SluttDato, na.rm = T))[1]]})

Medisiner <- merge(Medisiner, tmp[, c("SkjemaGUID_min", "ant_samme_startdato")], by.x = "SkjemaGUID", by.y = "SkjemaGUID_min")

Medisiner <- Medisiner[Medisiner$LegemiddelType2020 %in% c("Cyclofosfamid", "Rituximab"), ]
Medisiner$Med_SluttDato[is.na(Medisiner$Med_SluttDato)] <- "2021-03-08" # Erstatt tomme sluttdatoer med dato for nedlasting av datadump
Medisiner$periode <- lubridate::interval(Medisiner$Med_StartDato, Medisiner$Med_SluttDato)

ritux <- Medisiner[Medisiner$LegemiddelType2020=="Rituximab", c("Med_StartDato", "Med_SluttDato", "PasientGUID", "periode")]
cyclofos <- Medisiner[Medisiner$LegemiddelType2020=="Cyclofosfamid", c("Med_StartDato", "Med_SluttDato", "PasientGUID", "periode")]

felles <- intersect(ritux$PasientGUID, cyclofos$PasientGUID)
ritux <- ritux[ritux$PasientGUID %in% felles, ]
cyclofos <- cyclofos[cyclofos$PasientGUID %in% felles, ]

ritux$ant_overlapp <- NA
for (p in 1:dim(ritux)[1]) {
  pas <- which(cyclofos$PasientGUID %in% ritux$PasientGUID[p])
  # print(p)
  ritux$ant_overlapp[p] <- 0
  for (q in pas) {
    ritux$ant_overlapp[p] <- ritux$ant_overlapp[p] + as.numeric(int_overlaps(ritux$periode[p], cyclofos$periode[q]))
  }
}
antall_overlapp <- ritux %>% group_by(PasientGUID) %>%
  summarise(overlapp = sum(ant_overlapp) > 0,
            N = n())
sum(antall_overlapp$overlapp)

###################################################################
#### Antall skjema ved avdelingene 08.03.2021 ####################
Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-03-08_0852.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Inklusjon_pguid <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-03-08_0854.csv', header=TRUE, sep=";",
                              stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Inklusjon$PasientGUID <- Inklusjon_pguid$PasientGUID
Inklusjon <- merge(Inklusjon, Inklusjon_pguid[, c("SkjemaGUID", "PasientGUID")], by ='SkjemaGUID')
Oppfolging <- read.table('I:/norvas/DataDump_MRS-PROD_OppfølgingSkjema_2021-03-08_1409.csv', header=TRUE, sep=";",
                         stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2021-03-08_0858.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner <- read.table('I:/norvas/DataDump_MRS-PROD_MedisineringSkjema_2021-03-08_1410.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
BVAS <- read.table('I:/norvas/DataDump_MRS-PROD_BvasSkjema_2021-03-08_1421.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
KERR <- read.table('I:/norvas/DataDump_MRS-PROD_KerrsKriterierSkjema_2021-03-08_1426.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
VDI <- read.table('I:/norvas/DataDump_MRS-PROD_VdiSkjema_2021-03-08_1420.csv', header=TRUE, sep=";",
                  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Alvorlig_infeksjon <- read.table('I:/norvas/DataDump_MRS-PROD_SelvrapportertAlvorligInfeksjonSkjema_2021-03-08_1426.csv', header=TRUE, sep=";",
                                 stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Utredning <- read.table('I:/norvas/DataDump_MRS-PROD_Utredning_2021-03-08_1425.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Labskjema <- read.table('I:/norvas/DataDump_MRS-PROD_BlodprøvesvarSkjema_2021-03-08_1425.csv', header=TRUE, sep=";",
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

Diagnoser$Navn[Diagnoser$Navn %in% c('Systemisk Vaskulitt sykdom')] <- 'Uspesifisert nekrotiserende vaskulitt'
Diagnoser <- Diagnoser[-which(Diagnoser$Navn == 'Polymyalgia Rheumatica'), ]

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

ant_skjema <- table(Inklusjon$Sykehusnavn, useNA = "always") %>%
  bind_rows(table(Oppfolging$Sykehusnavn, useNA = "always")) %>%
  bind_rows(table(Diagnoser$Sykehusnavn, useNA = "always")) %>%
  bind_rows(table(Medisiner$Sykehusnavn, useNA = "always")) %>%
  bind_rows(table(BVAS$Sykehusnavn, useNA = "always")) %>%
  bind_rows(table(KERR$Sykehusnavn, useNA = "always")) %>%
  bind_rows(table(VDI$Sykehusnavn, useNA = "always")) %>%
  bind_rows(table(Alvorlig_infeksjon$Sykehusnavn, useNA = "always")) %>%
  bind_rows(table(Utredning$Sykehusnavn, useNA = "always")) %>%
  bind_rows(table(Labskjema$Sykehusnavn, useNA = "always"))
ant_skjema$Skjema <- c("Inklusjon", "Oppfolging", "Diagnoser", "Medisiner", "BVAS", "KERR", "VDI",
                       "Alvorlig_infeksjon", "Utredning", "Labskjema")
ant_skjema <- ant_skjema[, c(dim(ant_skjema)[2], 1:(dim(ant_skjema)[2]-1))]

write.csv2(ant_skjema, "I:/norvas/AntallSkjemaNorvas_08012021.csv", row.names = F, fileEncoding = "")


##########################################################
## Tall til dekningsgradsanalyse 2020 ####################

Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-03-08_0852.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM', colClasses = c('Fødselsnummer'='character'))
Inklusjon_pguid <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-03-08_0854.csv', header=TRUE, sep=";",
                              stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Inklusjon <- merge(Inklusjon, Inklusjon_pguid[, c("SkjemaGUID", "PasientGUID")], by ='SkjemaGUID')
Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2021-03-08_0858.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

Inklusjon <- norvasPreprosess(Inklusjon)
Diagnoser <- norvasPreprosess(Diagnoser)

Inklusjon <- Inklusjon[order(Inklusjon$InklusjonDato), ]
Inklusjon <- Inklusjon[match(unique(Inklusjon$PasientGUID), Inklusjon$PasientGUID), ]

Diagnoser <- Diagnoser[order(Diagnoser$Diagnose_Klinisk_Dato, decreasing = T), ]
Diagnoser <- Diagnoser[match(unique(Diagnoser$PasientGUID), Diagnoser$PasientGUID), ]

Inklusjon <- merge(Inklusjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr",
                                            "Diag_gr", "Navn")],
                   by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)

Diagnoser <- merge(Diagnoser, Inklusjon[, c("SkjemaGUID", "InklusjonDato")],
                   by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID')
Diagnoser <- Diagnoser[Diagnoser$Diagnose_Klinisk_Dato  <= '2020-12-31' | Diagnoser$InklusjonDato <= '2020-12-31', ]
Diagnoser <- Diagnoser[ , c("PasientGUID", "Navn", "Icd", "Diagnose_Klinisk_Dato", "InklusjonDato", "UnitId", "Sykehusnavn"), ]
# length(unique(Diagnoser$PasientGUID))

write.csv2(Diagnoser, 'I:/norvas/diagnoser_npr_2020.csv', row.names = F)

Kobling_norvas_pid_fn <- Inklusjon[Inklusjon$PasientGUID %in% Diagnoser$PasientGUID, c("PasientGUID", "Fødselsnummer")]
names(Kobling_norvas_pid_fn)[2] <- 'Fodselsnummer'

write.csv2(Kobling_norvas_pid_fn, 'I:/norvas/kobling_norvas_npr_2020.csv', row.names = F)

## Utvalg: Pasienter med diagnose- og inklusjonsskjema med enten Diagnose_Klinisk_Dato eller InklusjonDato <= 2018-12-31
##        Hvis flere diagnoser er nyeste brukt. Variabelen Icd er mangelfull så diagnosen må leses fra
##        variabelen Navn



#### Anonymt datasett for ehdir - eksempel på komplisert register #####################

Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-01-15_1216.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Oppfolging <- read.table('I:/norvas/DataDump_MRS-PROD_OppfølgingSkjema_2021-01-15_1217.csv', header=TRUE, sep=";",
                         stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2021-01-15_1221.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner <- read.table('I:/norvas/DataDump_MRS-PROD_MedisineringSkjema_2021-01-15_1218.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
BVAS <- read.table('I:/norvas/DataDump_MRS-PROD_BvasSkjema_2021-01-15_1219.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
KERR <- read.table('I:/norvas/DataDump_MRS-PROD_KerrsKriterierSkjema_2021-01-15_1222.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
VDI <- read.table('I:/norvas/DataDump_MRS-PROD_VdiSkjema_2021-01-15_1219.csv', header=TRUE, sep=";",
                  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Alvorlig_infeksjon <- read.table('I:/norvas/DataDump_MRS-PROD_SelvrapportertAlvorligInfeksjonSkjema_2021-01-15_1222.csv', header=TRUE, sep=";",
                                 stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Utredning <- read.table('I:/norvas/DataDump_MRS-PROD_Utredning_2021-01-15_1221.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Labskjema <- read.table('I:/norvas/DataDump_MRS-PROD_BlodprøvesvarSkjema_2021-01-15_1221.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

Inklusjon <- apply(Inklusjon, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
Oppfolging <- apply(Oppfolging, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
Diagnoser <- apply(Diagnoser, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
Medisiner <- apply(Medisiner, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
BVAS <- apply(BVAS, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
KERR <- apply(KERR, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
VDI <- apply(VDI, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
Alvorlig_infeksjon <- apply(Alvorlig_infeksjon, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
Utredning <- apply(Utredning, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()
Labskjema <- apply(Labskjema, 2, function(x){y <- x[sample(length(x), 20)]}) %>% as.data.frame()

write.csv2(Inklusjon, "I:/norvas/Inklusjon.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(Oppfolging, "I:/norvas/Oppfolging.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(Diagnoser, "I:/norvas/Diagnoser.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(Medisiner, "I:/norvas/Medisiner.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(BVAS, "I:/norvas/BVAS.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(KERR, "I:/norvas/KERR.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(VDI, "I:/norvas/VDI.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(Alvorlig_infeksjon, "I:/norvas/Alvorlig_infeksjon.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(Utredning, "I:/norvas/Utredning.csv", row.names = F, fileEncoding = "Latin1")
write.csv2(Labskjema, "I:/norvas/Labskjema.csv", row.names = F, fileEncoding = "Latin1")

#### Antall skjema ved avdelingene 25.01.2021 ####################
Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-01-15_1224.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Inklusjon_pguid <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-01-15_1216.csv', header=TRUE, sep=";",
                              stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Inklusjon$PasientGUID <- Inklusjon_pguid$PasientGUID
Inklusjon <- merge(Inklusjon, Inklusjon_pguid[, c("SkjemaGUID", "PasientGUID")], by ='SkjemaGUID')
Oppfolging <- read.table('I:/norvas/DataDump_MRS-PROD_OppfølgingSkjema_2021-01-15_1217.csv', header=TRUE, sep=";",
                         stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2021-01-15_1221.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner <- read.table('I:/norvas/DataDump_MRS-PROD_MedisineringSkjema_2021-01-15_1218.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
BVAS <- read.table('I:/norvas/DataDump_MRS-PROD_BvasSkjema_2021-01-15_1219.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
KERR <- read.table('I:/norvas/DataDump_MRS-PROD_KerrsKriterierSkjema_2021-01-15_1222.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
VDI <- read.table('I:/norvas/DataDump_MRS-PROD_VdiSkjema_2021-01-15_1219.csv', header=TRUE, sep=";",
                  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Alvorlig_infeksjon <- read.table('I:/norvas/DataDump_MRS-PROD_SelvrapportertAlvorligInfeksjonSkjema_2021-01-15_1222.csv', header=TRUE, sep=";",
                                 stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Utredning <- read.table('I:/norvas/DataDump_MRS-PROD_Utredning_2021-01-15_1221.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Labskjema <- read.table('I:/norvas/DataDump_MRS-PROD_BlodprøvesvarSkjema_2021-01-15_1221.csv', header=TRUE, sep=";",
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

Diagnoser$Navn[Diagnoser$Navn %in% c('Systemisk Vaskulitt sykdom')] <- 'Uspesifisert nekrotiserende vaskulitt'
Diagnoser <- Diagnoser[-which(Diagnoser$Navn == 'Polymyalgia Rheumatica'), ]

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

ant_skjema <- table(Inklusjon$Sykehusnavn, useNA = "ifany") %>%
  bind_rows(table(Oppfolging$Sykehusnavn, useNA = "ifany")) %>%
  bind_rows(table(Diagnoser$Sykehusnavn, useNA = "ifany")) %>%
  bind_rows(table(Medisiner$Sykehusnavn, useNA = "ifany")) %>%
  bind_rows(table(BVAS$Sykehusnavn, useNA = "ifany")) %>%
  bind_rows(table(KERR$Sykehusnavn, useNA = "ifany")) %>%
  bind_rows(table(VDI$Sykehusnavn, useNA = "ifany")) %>%
  bind_rows(table(Alvorlig_infeksjon$Sykehusnavn, useNA = "ifany")) %>%
  bind_rows(table(Utredning$Sykehusnavn, useNA = "ifany")) %>%
  bind_rows(table(Labskjema$Sykehusnavn, useNA = "ifany"))
ant_skjema$Skjema <- c("Inklusjon", "Oppfolging", "Diagnoser", "Medisiner", "BVAS", "KERR", "VDI",
                       "Alvorlig_infeksjon", "Utredning", "Labskjema")
ant_skjema <- ant_skjema[, c(dim(ant_skjema)[2], 1:(dim(ant_skjema)[2]-1))]

write.csv2(ant_skjema, "I:/norvas/AntallSkjemaNorvas.csv", row.names = F, fileEncoding = "")

#### Inklusjoner og Prednisolon for storkarsvaskulitter 21.01.2021 ####################
Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-01-15_1224.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Inklusjon_pguid <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-01-15_1216.csv', header=TRUE, sep=";",
                              stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Inklusjon$PasientGUID <- Inklusjon_pguid$PasientGUID
Inklusjon <- merge(Inklusjon, Inklusjon_pguid[, c("SkjemaGUID", "PasientGUID")], by ='SkjemaGUID')
Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2021-01-15_1221.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner <- read.table('I:/norvas/DataDump_MRS-PROD_MedisineringSkjema_2021-01-15_1218.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
BVAS <- read.table('I:/norvas/DataDump_MRS-PROD_BvasSkjema_2021-01-15_1219.csv', header=TRUE, sep=";",
                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

Inklusjon <- norvasPreprosess(Inklusjon)
Diagnoser <- norvasPreprosess(Diagnoser)
Medisiner <- norvasPreprosess(Medisiner)
BVAS <- norvasPreprosess(BVAS)

sykehusnavn <- sort(unique(Inklusjon$Sykehusnavn))

Inklusjon$Sykehusnavn <- factor(as.character(Inklusjon$Sykehusnavn), levels = sykehusnavn)
Diagnoser$Sykehusnavn <- factor(as.character(Diagnoser$Sykehusnavn), levels = sykehusnavn)
Medisiner$Sykehusnavn <- factor(as.character(Medisiner$Sykehusnavn), levels = sykehusnavn)

Inklusjon <- Inklusjon[order(Inklusjon$InklusjonDato), ]
Inklusjon <- Inklusjon[match(unique(Inklusjon$PasientGUID), Inklusjon$PasientGUID), ]

Diagnoser <- Diagnoser[order(Diagnoser$Diagnose_Klinisk_Dato, decreasing = T), ]
Diagnoser <- Diagnoser[match(unique(Diagnoser$PasientGUID), Diagnoser$PasientGUID), ]
BVAS <- merge(BVAS, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
BVAS <- merge(BVAS, Inklusjon[, c('SkjemaGUID', "InklusjonDato")], by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
BVAS$BVAS_aar <- as.numeric(format(BVAS$BVAS_Dato, format = '%Y'))

Inklusjon$Fodselsdato <- personnr2fodselsdato(Inklusjon$Fødselsnummer)
Diagnoser <- merge(Diagnoser, Inklusjon[, c("SkjemaGUID", "Fodselsdato", "InklusjonDato")],
                   by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID')

Inklusjon <- merge(Inklusjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr",
                                            "Diag_gr", "Navn")],
                   by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)


Inklusjon$Inklusjonsaar <- as.numeric(format(Inklusjon$InklusjonDato, format = '%Y'))
Medisiner <- Medisiner[Medisiner$PasientGUID %in% unique(Inklusjon$PasientGUID), ]

tmp <- as.data.frame.matrix(table(Diagnoser$Navn, Diagnoser$Diag_gr, useNA = 'ifany'))
tmp <- tmp[order(tmp$`Storkarsvaskulitt (LVV)`, tmp$`ANCA assosiert vaskulitt (AAV)`, tmp$Andre, decreasing = T), ]
write.csv2(tmp, "I:/norvas/diag_gr.csv", row.names = T, fileEncoding = "Latin1")
registrerte <- addmargins(table(Inklusjon$Sykehusnavn, Inklusjon$Inklusjonsaar, useNA = 'ifany'))
gvv <- addmargins(table(Inklusjon$Sykehusnavn[which(Inklusjon$Diag_gr_nr == 1)],
                        Inklusjon$Inklusjonsaar[which(Inklusjon$Diag_gr_nr == 1)], useNA = 'ifany'))
write.csv2(registrerte, "I:/norvas/alle_inkluderte.csv", row.names = T, fileEncoding = "Latin1")
write.csv2(gvv, "I:/norvas/storkar.csv", row.names = T, fileEncoding = "Latin1")

Medisiner <-  Medisiner[Medisiner$PasientGUID %in% Inklusjon$PasientGUID[which(Inklusjon$Diag_gr_nr == 1)], ]
BVAS <- BVAS[BVAS$PasientGUID %in% Inklusjon$PasientGUID[which(Inklusjon$Diag_gr_nr == 1)], ]



prednisolon <- Medisiner[which(Medisiner$LegemiddelType2020 == "Prednisolon" | Medisiner$LegemiddelType2019 == "Prednisolon"), ]


###### MERK: ENDRING I DEFINISJON AV NYSYKE, INKLUDERER NY VARIABEL InkludertNyEtablertDiagnose ###########
# nysyke <- BVAS[which(abs(difftime(BVAS$BVAS_Dato, BVAS$InklusjonDato, units = 'days')) <= 30), ]
# nysyke <- nysyke[which(nysyke$Sykdomsvurdering == "Debut" & nysyke$Diag_gr_nr==1), ]
nysyke <- BVAS[which(abs(difftime(BVAS$BVAS_Dato, BVAS$InklusjonDato, units = 'days')) <= 30 |
                       BVAS$PasientGUID %in% Inklusjon$PasientGUID[Inklusjon$InkludertNyEtablertDiagnose=="NyDiagnose"]), ]
nysyke <- nysyke[which(nysyke$Sykdomsvurdering == "Debut" & nysyke$Diag_gr_nr==1), ]
nysyke <- nysyke[order(nysyke$BVAS_Dato, decreasing = F), ]
nysyke <- nysyke[match(unique(nysyke$PasientGUID), nysyke$PasientGUID), ]

PaaPrednisolon <- merge(nysyke, prednisolon, by='HovedskjemaGUID', all.x = T)
PaaPrednisolon$VedDebut <- 0
PaaPrednisolon$VedDebut[which((PaaPrednisolon$BVAS_Dato>= PaaPrednisolon$Med_StartDato & PaaPrednisolon$BVAS_Dato<= PaaPrednisolon$Med_SluttDato) |
                                (PaaPrednisolon$BVAS_Dato >= PaaPrednisolon$Med_StartDato & is.na(PaaPrednisolon$Med_SluttDato)))] <- 1
PaaPrednisolon$MndFraDebut_6 <- 0
PaaPrednisolon$MndFraDebut_6[which((PaaPrednisolon$BVAS_Dato %m+% months(7) >= PaaPrednisolon$Med_StartDato & PaaPrednisolon$BVAS_Dato %m+% months(7) <= PaaPrednisolon$Med_SluttDato) |
                                     (PaaPrednisolon$BVAS_Dato %m+% months(7) >= PaaPrednisolon$Med_StartDato & is.na(PaaPrednisolon$Med_SluttDato)))] <- 1

indikator_med6mnd_v2 <- PaaPrednisolon %>% group_by(PasientGUID.x) %>% summarise(innen_6md = sum(MndFraDebut_6),
                                                                                 ved_debut = sum(VedDebut),
                                                                                 Dose = max(Dose),
                                                                                 N=n())
PaaPrednisolon <- PaaPrednisolon[PaaPrednisolon$MndFraDebut_6==1, ]

oppsum <- PaaPrednisolon %>% group_by(Sykehusnavn.x) %>% summarise(gj.sn.prednisolon.6mnd = mean(Dose),
                                                                   andel.under.7.5 = sum(Dose<=7.5)/n()*100,
                                                                   N = n())
write.csv2(oppsum, "I:/norvas/prednisolon6mnd.csv", row.names = F, fileEncoding = "Latin1")



##########################################################
## Tall til dekningsgradsanalyse 2019 ####################

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



