library(norvas)
library(xtable)
library(lubridate)
library(tidyverse)
rm(list = ls())
options(dplyr.summarise.inform = FALSE)

rap_aar <- 2024
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
Pasientsvar <- aarrappdata$Pasientsvar

######### AD-HOC : Flytt ous hf til rh ############
Inklusjon$UnitId[Inklusjon$UnitId==4001031] <- 4210431
Oppfolging$UnitId[Oppfolging$UnitId==4001031] <- 4210431

#### KOBLING MELLEOM UNITID OG SYKEHUSNAVN
kobl_unitid_shusnavn_norvas <-
  Inklusjon[match(unique(Inklusjon$UnitId),
                  Inklusjon$UnitId), c("UnitId", "Sykehusnavn")]
orgnr_table <-
  tibble::tribble(
    ~UnitId,   ~orgnr,
    104579, 974749025,
    601159, 974795787,
    700701, 974795361,
    4210614, 974795515,
    104209, 873255102,
    110353, 874632562,
    4210431, 874716782,
    110629, 974116588,
    103300, 974631326,
    103725, 974631326,
    108054, 974633698,
    701344, 974703300,
    106841, 974724774,
    104092, 974733013,
    105274, 974744570,
    102708, 974747138,
    105776, 974754118,
    102977, 974557746
  )
kobl_unitid_shusnavn_norvas <- merge(kobl_unitid_shusnavn_norvas, orgnr_table,
                                     by = "UnitId", all.x = TRUE)

figfolder <- "C:/Users/kth200/OneDrive - Helse Nord RHF/Dokumenter/regdata/norvas/aarsrapp2024/figfolder/"
if (!dir.exists(figfolder)) {
  dir.create(figfolder)
}
utformat <- 'pdf'



##### Indikater 1: ANCA-test ved debut av AAV

anca_debut <- Inklusjon %>%
  select(SkjemaGUID, PasientGUID, InklusjonDato, Inklusjonsaar,
         Diagnose_ny_30, Diag_gr_nr, Sykehusnavn, UnitId) %>%
  # filter(Diagnose_ny_30 == 1) %>%
  filter(Inklusjonsaar <= rap_aar) %>%
  filter(Diag_gr_nr == 2) %>%
  merge(Labskjema %>% select(HovedskjemaGUID, Blodprover_Dato,
                             PR3AncaPositiv, MPO_AncaPositiv),
        by.x = c('SkjemaGUID', "InklusjonDato"),
        by.y = c('HovedskjemaGUID', "Blodprover_Dato"),
        all.x = TRUE) %>%
  mutate(anca = ((PR3AncaPositiv %in% c(0,1))  |
                   (MPO_AncaPositiv %in% c(0,1))))

tabell_anca_debut_pr_aar <- anca_debut %>%
  filter(Inklusjonsaar >= rap_aar-2) %>%
  summarise(antall = sum(anca),
            N=n(),
            .by = c(Sykehusnavn, Inklusjonsaar)) %>%
  dplyr::bind_rows(data.frame(
    Sykehusnavn=rep("Total",3),
    summarise(., antall = sum(antall),
              N = sum(N), .by = Inklusjonsaar))) %>%
  mutate(andel = antall/N*100)

Tabell <- tabell_anca_debut_pr_aar %>%
  mutate(verdi = paste0(sprintf("%.1f", andel), " (N=", N, ")")) %>%
  arrange(-Inklusjonsaar, -andel) %>%
  select(Sykehusnavn, Inklusjonsaar, verdi) %>%
  tidyr::pivot_wider(names_from = Inklusjonsaar,
                     values_from = verdi, names_sort = TRUE)


print(xtable::xtable(
  Tabell, align= c('l','l','r','r','r'),
  caption='Andel ANCA test ved debut av ANCA-assosiert vaskulitt etter
  inklusjonsår.\\ Nevner: Pasienter med ANCA-assosiert vaskulitt inkludert
  gjeldende år. Teller: Pasienter som har et labskjema tilknyttet
  inklusjonsskjema som har utfylt enten PR3AncaPositiv eller MPO\\_AncaPositiv',
  include.rownames = F))

Ind1_ANCAvdebut_NorVas <- anca_debut[, c("Sykehusnavn", "SkjemaGUID",
                                         "anca", "UnitId", "Inklusjonsaar")]
Ind1_ANCAvdebut_NorVas$Teller <- as.numeric(Ind1_ANCAvdebut_NorVas$anca)
figurdata <- Ind1_ANCAvdebut_NorVas[Ind1_ANCAvdebut_NorVas$Inklusjonsaar <=rap_aar, ]
outfile <- paste0(figfolder, "anca_v_debut_ancaass_vas.", utformat)
norvasIndikator(figurdata, tittel = c("Andel med utført ANCA-test ved debut", "for ANCA-assosierte vaskulitter"),
                maal = 95, minstekrav = 50, outfile = outfile, xmax = 100)

Ind1_ANCAvdebut_NorVas$Nevner <- 1
Ind1_ANCAvdebut_NorVas <- Ind1_ANCAvdebut_NorVas[, c(4,5,6,7)]

Ind1_ANCAvdebut_NorVas$ind_id <- "norvas_anca_ved_debut"
names(Ind1_ANCAvdebut_NorVas)[2] <- "Oppfolgingsaar"

Indikatorer <- Ind1_ANCAvdebut_NorVas %>%
  rename(year = Oppfolgingsaar,
         var = Teller,
         denominator = Nevner) %>%
  select(UnitId, year, var, denominator, ind_id) %>%
  bind_rows(Indikatorer)



















# library(norvas)
# library(xtable)
# library(lubridate)
# library(dplyr)
# rm(list = ls())
# options(dplyr.summarise.inform = FALSE)
#
# rap_aar <- 2024
# datoTil <- paste0(rap_aar, "-12-31")
#
# aarrappdata <- norvas::lesogprosesser(rap_aar = rap_aar)
# Inklusjon <- aarrappdata$Inklusjon
# Oppfolging <- aarrappdata$Oppfolging
# Diagnoser <- aarrappdata$Diagnoser
# Medisiner <- aarrappdata$Medisiner
# BVAS <- aarrappdata$BVAS
# KERR <- aarrappdata$KERR
# VDI <- aarrappdata$VDI
# Alvorlig_infeksjon <- aarrappdata$Alvorlig_infeksjon
# Utredning <- aarrappdata$Utredning
# Labskjema <- aarrappdata$Labskjema
#
# #### KOBLING MELLEOM UNITID OG SYKEHUSNAVN
# kobl_unitid_shusnavn_norvas <- Inklusjon[match(unique(Inklusjon$UnitId),
#                                                Inklusjon$UnitId), c("UnitId", "Sykehusnavn")]
# orgnr_table <-
#   tibble::tribble(
#     ~UnitId,   ~orgnr,
#     104579, 974749025,
#     601159, 974795787,
#     700701, 974795361,
#     4210614, 974795515,
#     104209, 873255102,
#     110353, 874632562,
#     4210431, 874716782,
#     110629, 974116588,
#     103300, 974631326,
#     103725, 974631326,
#     108054, 974633698,
#     701344, 974703300,
#     106841, 974724774,
#     104092, 974733013,
#     105274, 974744570,
#     102708, 974747138,
#     105776, 974754118,
#     102977, 974557746
#   )
# kobl_unitid_shusnavn_norvas <- merge(kobl_unitid_shusnavn_norvas, orgnr_table,
#                                      by = "UnitId", all.x = TRUE)
#
# figstr <- 0.61
# tmp <- Sys.setlocale(category = "LC_ALL", locale = "nb_NO.UTF-8")
#
# # figfolder <- "~/GIT/fig_og_tab/norvas/"
# # if (!dir.exists(figfolder)) {
# #   dir.create(figfolder)
# # }
# figfolder <- "C:/Users/kth200/OneDrive - Helse Nord RHF/Dokumenter/regdata/norvas/aarsrapp2024/figfolder/"
# if (!dir.exists(figfolder)) {
#   dir.create(figfolder)
# }
#
# ########### Andel med 2 eller flere oppfølginger per år v2 ##################################
#
#
#
# ########### Andel med 2 eller flere oppfølginger per år ##################################
# tmp <- as_tibble(Inklusjon[, c("UnitId", "PasientGUID", "Inklusjonsaar")])
# aux <- tibble(UnitId=integer(), PasientGUID=character(), Inklusjonsaar=double(), Oppfolgingsaar=double())
#
# # For alle pasienter, lag en rad for hvert år pasienten har vært inkludert
# # som inneholder UnitId, pasientguid, inklusjonsår og år for potensiell oppfølging
# for (id in tmp$PasientGUID) {
#   for (aar in tmp$Inklusjonsaar[tmp$PasientGUID==id]:rap_aar) {
#     aux <- bind_rows(aux, bind_cols(tmp[tmp$PasientGUID==id, ],
#                                     Oppfolgingsaar=aar))
#   }
# }
#
# # Legg til en indikatorvariabel som sier om pasinten har vært følgt opp
# Oppfolging$FolgtOpp <- 1
#
# Oppfolging_v5 <- merge(
#   aux,
#   Oppfolging[, c("UnitId", "PasientGUID", "SkjemaGUID",
#                  "Oppfolgingsaar", "FolgtOpp")],
#   by = c("PasientGUID","Oppfolgingsaar"), all.x = T) |>
#   merge(Oppfolging |> dplyr::select(PasientGUID, EksklusjonsDato) |>
#           dplyr::filter(!is.na(EksklusjonsDato)),
#         by = "PasientGUID", all.x = TRUE) |>
#   dplyr::mutate(UnitId = ifelse(is.na(UnitId.y), UnitId.x, UnitId.y),
#                 FolgtOpp = ifelse(is.na(FolgtOpp), 0, FolgtOpp),
#                 Eksklusjonsaar = lubridate::year(EksklusjonsDato),
#                 Eksklusjonsaar = ifelse(is.na(Eksklusjonsaar), 2050,
#                                         Eksklusjonsaar)) |>
#   dplyr::filter(Oppfolgingsaar > Inklusjonsaar,
#                 Oppfolgingsaar <= Eksklusjonsaar)
#
# ant.oppf.pr.pas.pr.hf.pr.aar <- Oppfolging_v5 |>
#   dplyr::summarise(Antall_oppf = sum(FolgtOpp),
#                    .by = c(PasientGUID, UnitId, Oppfolgingsaar))
#
# ind2_Andelminimum_2oppf_NorVas <- ant.oppf.pr.pas.pr.hf.pr.aar
# ind2_Andelminimum_2oppf_NorVas$Teller <- as.numeric(ind2_Andelminimum_2oppf_NorVas$Antall_oppf>=2)
# ind2_Andelminimum_2oppf_NorVas$Nevner <- 1
# ind2_Andelminimum_2oppf_NorVas <- ind2_Andelminimum_2oppf_NorVas[, c(2,3,5,6)]
# ind2_Andelminimum_2oppf_NorVas$ind_id <- "norvas_minimum_2oppf"
#
# ind2_Andelminimum_2oppf_NorVas |>
#   filter(Oppfolgingsaar %in% (rap_aar-2):rap_aar) |>
#   merge(kobl_unitid_shusnavn_norvas[, c("Sykehusnavn", "UnitId")], by = "UnitId") |>
#   summarise(Antall = sum(Teller),
#             N=n(), .by = c(Sykehusnavn, Oppfolgingsaar)) %>%
#   # janitor::adorn_totals(... = c(Antall, N)) %>%
#   mutate(Andel = Antall/N*100) %>%
#   arrange(-Andel) %>%
#   select(Sykehusnavn, Oppfolgingsaar, Andel, N) |>
#   dplyr::mutate(val = paste0(round(Andel, 1), "% (", N, ")")) |>
#   dplyr::select(Sykehusnavn, Oppfolgingsaar, val) |>
#   tidyr::pivot_wider(names_from = Oppfolgingsaar, values_from = val)
#
#
#
# Indikatorer <- ind2_Andelminimum_2oppf_NorVas
#
# ########## BVAS/Kerr ved oppfølging ################################################
#
# ######## BVAS ##############
# KERR$Sykdomsvurdering[KERR$Sykdomsvurdering==-1] <- NA
#
# tmp3 <- merge(Oppfolging, KERR[, c("KerrsKriterier_Dato", "PasientGUID", "Sykdomsvurdering")], by.x = c('PasientGUID','OppfolgingsDato'),
#               by.y = c('PasientGUID','KerrsKriterier_Dato'), all.x = T)
# tmp3 <- tmp3[which(tmp3$Diag_gr_nr == 1), ]
# tmp2 <- merge(Oppfolging, BVAS[, c("SykdomsvurderingLabel", "PasientGUID", "BVAS_Dato")], by.x = c('PasientGUID','OppfolgingsDato'),
#               by.y = c('PasientGUID','BVAS_Dato'), all.x = T)
# tmp2 <- tmp2[tmp2$Diag_gr_nr %in% c(2,3), ]
#
# Ind3_AndelBVAS_NorVas <- tmp2[, c("UnitId", "Oppfolgingsaar", "SykdomsvurderingLabel")]
# Ind3_AndelBVAS_NorVas$Teller <- as.numeric(!is.na(Ind3_AndelBVAS_NorVas$SykdomsvurderingLabel))
# Ind3_AndelBVAS_NorVas$Nevner <- 1
# Ind3_AndelBVAS_NorVas <- Ind3_AndelBVAS_NorVas[ , -3]
# Ind3_AndelBVAS_NorVas$ind_id <- "norvas_bvas"
# Indikatorer <- bind_rows(Indikatorer, Ind3_AndelBVAS_NorVas)
#
# ########  KERR ##############
# KERR$Sykdomsvurdering[KERR$Sykdomsvurdering==-1] <- NA
#
# tmp3 <- merge(Oppfolging, KERR[, c("KerrsKriterier_Dato", "PasientGUID", "Sykdomsvurdering")], by.x = c('PasientGUID','OppfolgingsDato'),
#               by.y = c('PasientGUID','KerrsKriterier_Dato'), all.x = T)
# tmp3 <- tmp3[which(tmp3$Diag_gr_nr == 1), ]
#
# ind4_AndelKerr_NorVas <- tmp3[, c("UnitId", "Oppfolgingsaar", "Sykdomsvurdering")]
# ind4_AndelKerr_NorVas$Teller <- as.numeric(!is.na(ind4_AndelKerr_NorVas$Sykdomsvurdering))
# ind4_AndelKerr_NorVas$Nevner <- 1
# ind4_AndelKerr_NorVas <- ind4_AndelKerr_NorVas[ , -3]
# ind4_AndelKerr_NorVas$ind_id <- "norvas_kerr"
# Indikatorer <- bind_rows(Indikatorer, ind4_AndelKerr_NorVas)
#
#
# ########### ANCA test ved debut  ################################
#
# # Nysyke definert som de med diagnosedato innen 30 dager av inklusjonsdato
# nysyke <- Inklusjon[which(abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 30), ]
# aux <- merge(nysyke, Labskjema, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)   # Henter info fra labskjema
# aux <- aux[which(aux$Diag_gr_nr == 2), ]                                                  # Ser kun på ANCA
#
# aux2 <- aux[which(abs(difftime(aux$Diagnose_Klinisk_Dato, aux$Blodprover_Dato, units = 'days')) <= 30), ] # Avgrenser til tilfeller med
# # blodprøvedato innen 30 dager av diagnosedato
# tmp2 <- aux2 %>% group_by(Sykehusnavn.x, SkjemaGUID) %>%
#   summarise(anca = ((1 %in% PR3AncaPositiv) | (0 %in% PR3AncaPositiv))) # Ett resultat per sykehus og skjemaguid,
#
# tmp3 <- tmp2 %>% ungroup()
# names(tmp3) <- c("Sykehusnavn", "SkjemaGUID", "Teller")
# tmp3$Teller <- as.numeric(tmp3$Teller)
#
# tmp4 <- nysyke[which(nysyke$Diag_gr_nr == 2), c("Sykehusnavn", "SkjemaGUID")]
# tmp4 <- tmp4[!(tmp4$SkjemaGUID %in% tmp3$SkjemaGUID), ]
# tmp4$Teller <- 0
# tmp4 <- as_tibble(tmp4)
#
# Ind1_ANCAvdebut_NorVas <- bind_rows(tmp3, tmp4)
# Ind1_ANCAvdebut_NorVas$UnitId <- nysyke$UnitId[match(Ind1_ANCAvdebut_NorVas$Sykehusnavn, nysyke$Sykehusnavn)]
# Ind1_ANCAvdebut_NorVas$Nevner <- 1
# Ind1_ANCAvdebut_NorVas$Aar <- nysyke$Inklusjonsaar[match(Ind1_ANCAvdebut_NorVas$SkjemaGUID, nysyke$SkjemaGUID)]
# Ind1_ANCAvdebut_NorVas <- Ind1_ANCAvdebut_NorVas[, c(4,6,3,5)]
# Ind1_ANCAvdebut_NorVas$ind_id <- "norvas_anca_ved_debut"
# Indikatorer <- bind_rows(Indikatorer, Ind1_ANCAvdebut_NorVas)
#
#
# ############## Andel remisjon 6mnd, ANCA  ######################
# BVAS$tid_diag_bvas <- difftime(BVAS$BVAS_Dato, BVAS$Diagnose_Klinisk_Dato, units = 'days')
# BVAS <- BVAS[order(BVAS$BVAS_Dato, decreasing = F), ]
#
# nysyke <- BVAS[which(abs(difftime(BVAS$BVAS_Dato, BVAS$InklusjonDato, units = 'days')) <= 30), ]
# nysyke <- nysyke[which(nysyke$Sykdomsvurdering == 1 & nysyke$Diag_gr_nr==2), ]
# nysyke <- nysyke[which(nysyke$BVAS_Dato <= datoTil), ]
# BVAS_utvalg <- BVAS[which(BVAS$PasientGUID %in% nysyke$PasientGUID), ]
#
# remisjon <- BVAS_utvalg[which(BVAS_utvalg$Sykdomsvurdering == 5 & BVAS_utvalg$BVAS_Dato <= datoTil), ]
# remisjon <- remisjon[match(unique(remisjon$PasientGUID), remisjon$PasientGUID), ]
#
# remisjon$remisjon_indikator <- 0
# remisjon$remisjon_indikator[remisjon$tid_diag_bvas <= 210] <- 1
# ind5_AndelAnCA_remisjon_NorVas <- remisjon[, c("UnitId", "BVAS_aar", "remisjon_indikator")]
# ind5_AndelAnCA_remisjon_NorVas$Nevner <- 1
# names(ind5_AndelAnCA_remisjon_NorVas)[2:3] <- c("Aar", "Teller")
# ind5_AndelAnCA_remisjon_NorVas$ind_id <- "norvas_anca_remisjon"
# Indikatorer <- bind_rows(Indikatorer, ind5_AndelAnCA_remisjon_NorVas)
#
# ########### Andel remisjon 6mnd, GVV ###################################
# KERR$tid_diag_kerr <- difftime(KERR$KerrsKriterier_Dato, KERR$Diagnose_Klinisk_Dato, units = 'days')
# KERR <- KERR[order(KERR$KerrsKriterier_Dato, decreasing = F), ]
#
# nysyke <- KERR[which(abs(difftime(KERR$KerrsKriterier_Dato, KERR$InklusjonDato, units = 'days')) <= 30), ]
# nysyke <- nysyke[which(nysyke$Sykdomsvurdering == 1 & nysyke$Diag_gr_nr==1), ]
# nysyke <- nysyke[which(nysyke$KerrsKriterier_Dato <= datoTil), ]
# KERR_utvalg <- KERR[which(KERR$PasientGUID %in% nysyke$PasientGUID), ]
#
# remisjon <- KERR_utvalg[which(KERR_utvalg$Sykdomsvurdering == 2 & KERR_utvalg$KerrsKriterier_Dato <= datoTil), ]
# remisjon <- remisjon %>% group_by(PasientGUID, Sykehusnavn) %>%
#   summarise(tid_diag_kerr = min(tid_diag_kerr, na.rm = T),
#             UnitId = UnitId[tid_diag_kerr==min(tid_diag_kerr, na.rm = T)][1],
#             KERR_aar = KERR_aar[tid_diag_kerr==min(tid_diag_kerr, na.rm = T)][1])
#
# remisjon$remisjon_indikator <- 0
# remisjon$remisjon_indikator[remisjon$tid_diag_kerr <= 210] <- 1
#
# ind_AndelStorkar_remisjon_NorVas <- remisjon[, c("Sykehusnavn", "UnitId", "KERR_aar", "remisjon_indikator")]
# ind_AndelStorkar_remisjon_NorVas$Nevner <- 1
# names(ind_AndelStorkar_remisjon_NorVas)[3:4] <- c("Aar", "Teller")
# ind_AndelStorkar_remisjon_NorVas$ind_id <- "norvas_storkar_remisjon"
# Indikatorer <- bind_rows(Indikatorer, ind_AndelStorkar_remisjon_NorVas[, -1])
#
#
# ############### Andel på prednisolon etter 6 mnd ########################################
# prednisolon <- Medisiner[which(Medisiner$LegemiddelGenerisk == "Prednisolon"), ]
#
# nysyke <- BVAS[which(abs(difftime(BVAS$BVAS_Dato, BVAS$InklusjonDato, units = 'days')) <= 30), ]
# nysyke <- nysyke[which(nysyke$Sykdomsvurdering == 1 & nysyke$Diag_gr_nr==2), ]
#
# PaaPrednisolon <- merge(nysyke, prednisolon, by='HovedskjemaGUID', all.x = T)
# PaaPrednisolon$MndFraDebut_6 <- 0
# PaaPrednisolon$MndFraDebut_6[which((PaaPrednisolon$BVAS_Dato %m+% months(7) >= PaaPrednisolon$Med_StartDato & PaaPrednisolon$BVAS_Dato %m+% months(7) <= PaaPrednisolon$Med_SluttDato) |
#                                      (PaaPrednisolon$BVAS_Dato %m+% months(7) >= PaaPrednisolon$Med_StartDato & is.na(PaaPrednisolon$Med_SluttDato)))] <- 1
# indikator_med6mnd <- as.data.frame.matrix(table(PaaPrednisolon$PasientGUID.x, PaaPrednisolon$MndFraDebut_6, useNA = 'ifany'))
#
# PaaPrednisolon$Dose[PaaPrednisolon$PasientGUID.x %in% row.names(indikator_med6mnd[indikator_med6mnd$`1`==0, ])] <- 0
# PaaPrednisolon$MndFraDebut_6[PaaPrednisolon$PasientGUID.x %in% row.names(indikator_med6mnd[indikator_med6mnd$`1`==0, ])] <- 1
# PaaPrednisolon <- PaaPrednisolon[PaaPrednisolon$MndFraDebut_6==1, ]
#
# ind6_AndelANCA_Prednisolon_NorVas <- PaaPrednisolon[, c("UnitId.x", "BVAS_aar", "Dose")]
# ind6_AndelANCA_Prednisolon_NorVas$Teller <- as.numeric(PaaPrednisolon$Dose <= 5)
# ind6_AndelANCA_Prednisolon_NorVas$Nevner <- 1
# ind6_AndelANCA_Prednisolon_NorVas <- ind6_AndelANCA_Prednisolon_NorVas[, -3]
# names(ind6_AndelANCA_Prednisolon_NorVas)[1:2] <- c("UnitId", "Aar")
#
# ind6_AndelANCA_Prednisolon_NorVas$ind_id <- "norvas_anca_prednisolon"
# Indikatorer <- bind_rows(Indikatorer, ind6_AndelANCA_Prednisolon_NorVas)
#
# ##############################################################################
#
# nysyke <- KERR[which(abs(difftime(KERR$KerrsKriterier_Dato, KERR$InklusjonDato, units = 'days')) <= 30), ]
# nysyke <- nysyke[which(nysyke$Sykdomsvurdering == 1 & nysyke$Diag_gr_nr==1), ]
#
# PaaPrednisolon <- merge(nysyke, prednisolon, by='HovedskjemaGUID', all.x = T)
# PaaPrednisolon$MndFraDebut_6 <- 0
# PaaPrednisolon$MndFraDebut_6[which((PaaPrednisolon$KerrsKriterier_Dato %m+% months(7) >= PaaPrednisolon$Med_StartDato & PaaPrednisolon$KerrsKriterier_Dato %m+% months(7) <= PaaPrednisolon$Med_SluttDato) | (PaaPrednisolon$KerrsKriterier_Dato %m+% months(7) >= PaaPrednisolon$Med_StartDato & is.na(PaaPrednisolon$Med_SluttDato)))] <- 1
#
# indikator_med6mnd <- as.data.frame.matrix(table(PaaPrednisolon$PasientGUID.x, PaaPrednisolon$MndFraDebut_6, useNA = 'ifany'))
#
# PaaPrednisolon$Dose[PaaPrednisolon$PasientGUID.x %in% row.names(indikator_med6mnd[indikator_med6mnd$`1`==0, ])] <- 0
# PaaPrednisolon$MndFraDebut_6[PaaPrednisolon$PasientGUID.x %in% row.names(indikator_med6mnd[indikator_med6mnd$`1`==0, ])] <- 1
# PaaPrednisolon <- PaaPrednisolon[PaaPrednisolon$MndFraDebut_6==1, ]
#
# ind_Andelstorvas_Prednisolon_NorVas_v2_kerr <- PaaPrednisolon[, c("UnitId.x", "KERR_aar", "Dose")]
# ind_Andelstorvas_Prednisolon_NorVas_v2_kerr$Teller <- as.numeric(PaaPrednisolon$Dose <= 7.5)
# ind_Andelstorvas_Prednisolon_NorVas_v2_kerr$Nevner <- 1
# ind_Andelstorvas_Prednisolon_NorVas_v2_kerr<- ind_Andelstorvas_Prednisolon_NorVas_v2_kerr[, -3]
# names(ind_Andelstorvas_Prednisolon_NorVas_v2_kerr)[1:2] <- c("UnitId", "Aar")
#
# ind_Andelstorvas_Prednisolon_NorVas_v2_kerr$ind_id <- "norvas_storvas_prednisolon"
# Indikatorer <- bind_rows(Indikatorer, ind_Andelstorvas_Prednisolon_NorVas_v2_kerr)
#
# ############## Andel utfort ymse #################################
#
# ######### Utredning ANCA ##########################################
# nysyke <- Inklusjon[which(abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 30), ]
# antall_nysyke_anca <- length(which(nysyke$Diag_gr_nr == 2))
#
# aux <- merge(nysyke, Utredning, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
# aux <- aux[which(aux$Diag_gr_nr == 2), ]
#
# jalla <- aux
# jalla$ved_debut <- FALSE
# jalla$ved_debut[which(abs(difftime(jalla$Diagnose_Klinisk_Dato, jalla$Billeddiagnostikk_Dato, units = 'days')) <= 30)] <- TRUE
# jalla$samlet <- jalla$CtThorax != -1
# jalla$samlet[is.na(jalla$samlet)] <- FALSE
# jalla$samlet2 <- (jalla$CtBihuler != -1 | jalla$MrBihuler != -1)
# jalla$samlet2[is.na(jalla$samlet2)] <- FALSE
# jalla <- jalla %>% group_by(PasientGUID.x) %>% summarise(ct_thorax = max(samlet & ved_debut),
#                                                          ctmr_bihuler = max(samlet2 & ved_debut),
#                                                          UnitId = UnitId.x[1],
#                                                          Aar = Inklusjonsaar[1])
# jalla$Nevner <- 1
#
# ind_andel_ct_thorax <- jalla[, c("UnitId", "Aar", "ct_thorax", "Nevner")]
# names(ind_andel_ct_thorax)[names(ind_andel_ct_thorax)=="ct_thorax"] <- "Teller"
# ind_andel_ct_thorax$ind_id <- "norvas_andel_ct_thorax_anca"
#
# ind_andel_ctmr_bihuler <- jalla[, c("UnitId", "Aar", "ctmr_bihuler", "Nevner")]
# names(ind_andel_ctmr_bihuler)[names(ind_andel_ctmr_bihuler)=="ctmr_bihuler"] <- "Teller"
# ind_andel_ctmr_bihuler$ind_id <- "norvas_andel_ctmr_bihuler_anca"
#
# Indikatorer <- bind_rows(Indikatorer, ind_andel_ct_thorax)
# Indikatorer <- bind_rows(Indikatorer, ind_andel_ctmr_bihuler)
#
# ####### Utredning storkarsvaskulitt ##############################
# aux <- merge(nysyke, Utredning, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
# aux <- aux[which(aux$Diag_gr_nr == 1), ]
#
# jalla <- aux
# jalla$ved_debut <- FALSE
# jalla$ved_debut[which(abs(difftime(jalla$Diagnose_Klinisk_Dato, jalla$Billeddiagnostikk_Dato, units = 'days')) <= 30)] <- TRUE
# jalla$samlet <- jalla$UlMellomstoreKar != -1 | jalla$CtMellomstoreKar != -1 | jalla$CtAorta != -1 |
#   jalla$MrMellomstoreKar!=-1 | jalla$MrAorta != -1
# jalla$samlet[is.na(jalla$samlet)] <- FALSE
# jalla <- jalla %>% group_by(PasientGUID.x) %>% summarise(ulctmr_storkar = max(samlet & ved_debut),
#                                                          UnitId = UnitId.x[1],
#                                                          Aar = Inklusjonsaar[1])
# jalla$Nevner <- 1
# jalla$Teller <- jalla$ulctmr_storkar
#
# ind_andel_ulctmr_kar <- jalla[, c("UnitId", "Aar", "Teller", "Nevner")]
# ind_andel_ulctmr_kar$ind_id <- "norvas_andel_ulctmr_storkar"
#
# Indikatorer <- bind_rows(Indikatorer, ind_andel_ulctmr_kar)
#
#
# aux <- merge(nysyke, Utredning, by = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
# aux <- aux[aux$ICD10 == "M31.5/M31.6", ]
#
# jalla <- aux
# jalla$ved_debut <- FALSE
# jalla$ved_debut[which(abs(difftime(jalla$Diagnose_Klinisk_Dato, jalla$Billeddiagnostikk_Dato, units = 'days')) <= 30)] <- TRUE
# jalla$samlet <- jalla$BlodKar!=-1 | jalla$MrMellomstoreKar!=-1 | jalla$UlMellomstoreKar!=-1
# jalla$samlet[is.na(jalla$samlet)] <- FALSE
# jalla <- jalla %>% group_by(PasientGUID.x) %>% summarise(utredet_GCA = max(samlet & ved_debut),
#                                                          UnitId = UnitId.x[1],
#                                                          Aar = Inklusjonsaar[1])
# jalla$Nevner <- 1
# jalla$Teller <- jalla$utredet_GCA
# ind_andel_utredet_GCA <- jalla[, c("UnitId", "Aar", "Teller", "Nevner")]
# ind_andel_utredet_GCA$ind_id <- "norvas_andel_utredet_GCA"
#
# Indikatorer <- bind_rows(Indikatorer, ind_andel_utredet_GCA)
#
# ################ Tilpasninger til ønsket format ###################################
#
# nokkeltall <- Inklusjon %>%
#   group_by(Inklusjonsaar) %>%
#   summarise("Antall avdelinger" = length(unique(UnitId)),
#             "Antall inkluderte" = n(),
#             "Inklusjonsalder, gj.sn." = mean(Inklusjonsalder),
#             "Inklusjonsalder, median" = median(Inklusjonsalder),
#             "Andel kvinner" = sum(ErMann==0)/n()*100)
#
# Diagnoser$Inklusjonsaar <- as.numeric(format(Diagnoser$InklusjonDato, "%Y"))
#
# Indikatorer$Oppfolgingsaar[is.na(Indikatorer$Oppfolgingsaar)] <-
#   Indikatorer$Aar[is.na(Indikatorer$Oppfolgingsaar)]
# kobl_unitid_shusnavn_norvas$Sykehusnavn <-
#   as.character(kobl_unitid_shusnavn_norvas$Sykehusnavn)
#
# shus_struktur <- read.csv("C:/Users/kth200/OneDrive - Helse Nord RHF/Dokumenter/regdata/shus.csv")
#
# kobl_unitid_shusnavn_norvas <-
#   data.frame(
#     kobl_unitid_shusnavn_norvas,
#     qmongrdata::SykehusNavnStruktur[
#       stringdist::amatch(kobl_unitid_shusnavn_norvas$Sykehusnavn,
#                          qmongrdata::SykehusNavnStruktur$SykehusNavn ),
#       c("OrgNrHF", "HF", "OrgNrShus", "SykehusNavn")])
#
#
# kobl_unitid_shusnavn_norvas <-
#   data.frame(
#     kobl_unitid_shusnavn_norvas,
#     shus_struktur[
#       stringdist::amatch(kobl_unitid_shusnavn_norvas$Sykehusnavn,
#                          shus_struktur$short_name ),
#       c("hf_orgnr", "orgnr", "short_name")])
#
#
#
# kobl_unitid_shusnavn_norvas$OrgNrShus[
#   kobl_unitid_shusnavn_norvas$UnitId == 104579] <- 974749025
# kobl_unitid_shusnavn_norvas$OrgNrShus[
#   kobl_unitid_shusnavn_norvas$UnitId == 601159] <- 974795787
# kobl_unitid_shusnavn_norvas$OrgNrShus[
#   kobl_unitid_shusnavn_norvas$UnitId == 700701] <- 974795361
# kobl_unitid_shusnavn_norvas$OrgNrShus[
#   kobl_unitid_shusnavn_norvas$UnitId == 4210614] <- 974795515
# kobl_unitid_shusnavn_norvas$OrgNrShus[
#   kobl_unitid_shusnavn_norvas$UnitId == 104209] <- 873255102
# kobl_unitid_shusnavn_norvas$OrgNrHF[
#   kobl_unitid_shusnavn_norvas$UnitId == 104579] <- 883974832
# kobl_unitid_shusnavn_norvas$OrgNrHF[
#   kobl_unitid_shusnavn_norvas$UnitId == 601159] <- 983974899
# kobl_unitid_shusnavn_norvas$OrgNrHF[
#   kobl_unitid_shusnavn_norvas$UnitId == 700701] <- 983974910
# kobl_unitid_shusnavn_norvas$OrgNrHF[
#   kobl_unitid_shusnavn_norvas$UnitId == 4210614] <- 983974929
# kobl_unitid_shusnavn_norvas$OrgNrHF[
#   kobl_unitid_shusnavn_norvas$UnitId == 104209] <- 981275721
#
# kobl_unitid_shusnavn_norvas$HF <-
#   qmongrdata::SykehusNavnStruktur$HF[
#     match(kobl_unitid_shusnavn_norvas$OrgNrHF,
#           qmongrdata::SykehusNavnStruktur$OrgNrHF)]
# kobl_unitid_shusnavn_norvas$SykehusNavn <-
#   qmongrdata::SykehusNavnStruktur$SykehusNavn[
#     match(kobl_unitid_shusnavn_norvas$OrgNrShus,
#           qmongrdata::SykehusNavnStruktur$OrgNrShus)]
#
# Indikatorer$orgnr <-
#   kobl_unitid_shusnavn_norvas$OrgNrShus[
#     match(Indikatorer$UnitId, kobl_unitid_shusnavn_norvas$UnitId)]
# Indikatorer <- Indikatorer[, c(7,2,3,4,5)]
# names(Indikatorer)[2:4] <- c("year", "var", "denominator")
# Indikatorer$context <- "caregiver"
#
# write.csv2(Indikatorer, 'I:/norvas/norvas_ind_20210621.csv', na = '', fileEncoding = 'UTF-8', row.names = F)
#
# dg_D690_andre <- read.table("I:/norvas/DGA_resultat_hf_D690.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_D891_andre <- read.table("I:/norvas/DGA_resultat_hf_D891.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_I776_storkar <- read.table("I:/norvas/DGA_resultat_hf_I776.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_M300_andre <- read.table("I:/norvas/DGA_resultat_hf_M300.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_M301_anca <- read.table("I:/norvas/DGA_resultat_hf_M301.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_M313_anca <- read.table("I:/norvas/DGA_resultat_hf_M313.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_M314_storkar <- read.table("I:/norvas/DGA_resultat_hf_M314.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_M315_storkar <- read.table("I:/norvas/DGA_resultat_hf_M315_M316.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_M317_anca <- read.table("I:/norvas/DGA_resultat_hf_M317.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_M319_andre <- read.table("I:/norvas/DGA_resultat_hf_M319.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_M352_andre <- read.table("I:/norvas/DGA_resultat_hf_M352.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_landet <- read.table("I:/norvas/DGA_resultat_landet_hf.csv", sep = ";", fileEncoding = "Latin1", header = T)
# dg_landet_innrap <- read.table("I:/norvas/DGA_resultat_landet_hf_innrapp.csv", sep = ";", fileEncoding = "Latin1", header = T)
#
# dg_andre <- merge(dg_D690_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
#                   dg_D891_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
#                   by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
#   merge(dg_M300_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3")) %>%
#   merge(dg_M319_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".4")) %>%
#   merge(dg_M352_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".5"))
# dg_andre[is.na(dg_andre)] <- 0
# dg_andre$norvas <- rowSums(dg_andre[, c("Begge.1", "Kun_norvas.1", "Begge.2", "Kun_norvas.2", "Begge", "Kun_norvas", "Begge.4", "Kun_norvas.4",
#                                         "Begge.5", "Kun_norvas.5")])
# dg_andre$totalt <- rowSums(dg_andre[, c("Total.1", "Total.2", "Total", "Total.4", "Total.5")])
# dg_andre$DG <- dg_andre$norvas/dg_andre$totalt*100
#
# dg_storkar <- merge(dg_I776_storkar[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
#                     dg_M314_storkar[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
#                     by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
#   merge(dg_M315_storkar[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3"))
# dg_storkar[is.na(dg_storkar)] <- 0
# dg_storkar$norvas <- rowSums(dg_storkar[, c("Begge.1", "Kun_norvas.1", "Begge.2", "Kun_norvas.2", "Begge", "Kun_norvas")])
# dg_storkar$totalt <- rowSums(dg_storkar[, c("Total.1", "Total.2", "Total")])
# dg_storkar$DG <- dg_storkar$norvas/dg_storkar$totalt*100
#
# dg_anca <- merge(dg_M301_anca[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
#                  dg_M313_anca[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
#                  by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
#   merge(dg_M317_anca[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3"))
# dg_anca[is.na(dg_anca)] <- 0
# dg_anca$norvas <- rowSums(dg_anca[, c("Begge.1", "Kun_norvas.1", "Begge.2", "Kun_norvas.2", "Begge", "Kun_norvas")])
# dg_anca$totalt <- rowSums(dg_anca[, c("Total.1", "Total.2", "Total")])
# dg_anca$DG <- dg_anca$norvas/dg_anca$totalt*100
#
# dg_landet_innrap$norvas <- rowSums(dg_landet_innrap[, c("Begge", "Kun_norvas")])
# dg_landet_innrap$totalt <- dg_landet_innrap$Total
#
# dg_anca$ind_id <- "norvas_dg_anca"
# dg_storkar$ind_id <- "norvas_dg_storkar"
# dg_andre$ind_id <- "norvas_dg_andre"
# dg_landet_innrap$ind_id <- "norvas_dg_alle"
#
# dg_samlet <- bind_rows(dg_anca[, c("hf_standard", "norvas", "totalt", "ind_id")],
#                        dg_storkar[, c("hf_standard", "norvas", "totalt", "ind_id")],
#                        dg_andre[, c("hf_standard", "norvas", "totalt", "ind_id")],
#                        dg_landet_innrap[, c("hf_standard", "norvas", "totalt", "ind_id")])
#
# dg_samlet$orgnr <- qmongrdata::SykehusNavnStruktur[stringdist::amatch(dg_samlet$hf_standard, qmongrdata::SykehusNavnStruktur$HF), c("OrgNrShus")]
# dg_samlet$orgnr[dg_samlet$hf_standard == "Betanien, Skien"] <- 873255102
# dg_samlet$orgnr[dg_samlet$hf_standard == "Haugesund sanitetsforenings revmatismesykehus"] <- 974724774
# dg_samlet$orgnr[dg_samlet$hf_standard == "Martina Hansens hospital"] <- 974116588
# dg_samlet$orgnr[dg_samlet$hf_standard == "Revmatismesykehuset, Lillehammer"] <- 874632562
# dg_samlet$orgnr[dg_samlet$hf_standard == "Universitetssykehuset Nord-Norge HF"] <- 974795787
#
# dg_samlet <- dg_samlet[!is.na(dg_samlet$orgnr), ]
#
# names(dg_samlet)[match(c("norvas", "totalt"), names(dg_samlet))] <- c("var", "denominator")
# dg_samlet$year <- 2020
# dg_samlet$context <- "caregiver"
# dg_samlet <- dg_samlet[, c("orgnr", "year", "var", "denominator", "ind_id", "context")]
#
# # write.csv2(dg_samlet, 'I:/norvas/norvas_dg_samlet.csv', na = '', fileEncoding = 'UTF-8', row.names = F)














