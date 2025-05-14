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

# Pasientsvar$FormDate <- as.Date(Pasientsvar$FormDate)
# Pasientsvar$SvarDato <- as.Date(Pasientsvar$SvarDato)
# Pasientsvar$CreationDate <- as.Date(Pasientsvar$CreationDate)
#
# tmp <- merge(Oppfolging, Pasientsvar,
#              by.x = c("PasientGUID", "OppfolgingsDato"),
#              by.y = c("PasientGUID", "SvarDato"))

# tmpi <- bind_rows(Oppfolging %>% filter(PasientGUID == tmp$PasientGUID,
#                        OppfolgingsDato == tmp$OppfolgingsDato),
#           Pasientsvar %>% filter(PasientGUID == tmp$PasientGUID,
#                                  SvarDato == tmp$OppfolgingsDato))

overlapp_oppf_pas <- intersect(toupper(names(Oppfolging)), toupper(stringr::str_remove(names(Pasientsvar), pattern = "Prom_")))
overlapp_oppf_pas <- names(Oppfolging)[match(overlapp_oppf_pas, toupper(names(Oppfolging)))]
overlapp_oppf_pas <- overlapp_oppf_pas[!is.na(overlapp_oppf_pas)]
names(Pasientsvar)[
  match(toupper(overlapp_oppf_pas),
        toupper(stringr::str_remove(names(Pasientsvar), pattern = "Prom_")))] <-
  overlapp_oppf_pas

Oppfolging2 <- bind_rows(Oppfolging, Pasientsvar)

## Kompletthet av variabler

Labskjema$BT_utfort <- rowSums(
  !is.na(Labskjema[, c("BlodtrykkSystoliskVenstre", "BlodtrykkDiastoliskVenstre",
                       "BlodtrykkDiastolisk", "BlodtrykkSystolisk",
                       "BlodtrykkSystoliskHoyre", "BlodtrykkSystoliskVenstre")]))

Labskjema$BT_utfort[Labskjema$BT_utfort > 0] <- 1

Labskjemarap_aar <- Labskjema[
  which(as.numeric(format(Labskjema$Blodprover_Dato, format="%Y")) == rap_aar), ]

Kompletthet <- data.frame(Variabel = "Blodprøve",
                          Antall_utfylt = sum(Labskjemarap_aar$BT_utfort==1),
                          N = dim(Labskjemarap_aar)[1])

blodprover_utfort <- sum(Labskjemarap_aar$BT_utfort==1)/dim(Labskjemarap_aar)[1]*100


Oppfolgingrap_aar <- bind_rows(
  Oppfolging[which(Oppfolging$Oppfolgingsaar == rap_aar),
             c("Tretthet", "PasientGlobalSykdomsaktivitet",
               "Pasientsmerter", "SkjemaGUID", "Sykehusnavn")],
  Inklusjon[which(Inklusjon$Inklusjonsaar == rap_aar),
            c("Tretthet", "PasientGlobalSykdomsaktivitet",
              "Pasientsmerter", "SkjemaGUID", "Sykehusnavn")])


prom_utfylt  <- Oppfolgingrap_aar %>%
  summarise(Tretthet_utfylt = sum(!is.na(Tretthet)),
            Sykdomsaktivitet_utfylt = sum(!is.na(PasientGlobalSykdomsaktivitet)),
            Smerte_utfylt = sum(!is.na(Pasientsmerter)),
            N = n())
prom_utfylt[1:3]/prom_utfylt[[4]]*100

Oppfolging3siste <- Oppfolging %>%
  mutate(Aar = Oppfolgingsaar) %>%
  select(Sykehusnavn, Aar, Tretthet, PasientGlobalSykdomsaktivitet, Pasientsmerter) %>%
  filter(Aar %in% (rap_aar-2):rap_aar) %>%
  bind_rows(
    Inklusjon %>%
      mutate(Aar = Inklusjonsaar) %>%
      select(Sykehusnavn, Aar, Tretthet, PasientGlobalSykdomsaktivitet, Pasientsmerter) %>%
      filter(Aar %in% (rap_aar-2):rap_aar)) %>%
  summarise(Tretthet_utfylt = sum(!is.na(Tretthet)),
            Sykdomsaktivitet_utfylt = sum(!is.na(PasientGlobalSykdomsaktivitet)),
            Smerte_utfylt = sum(!is.na(Pasientsmerter)),
            N = n(),
            Andel_Tretthet_utfylt = Tretthet_utfylt/N*100,
            Andel_Sykdomsaktivitet_utfylt = Sykdomsaktivitet_utfylt/N*100,
            Andel_Smerte_utfylt = Smerte_utfylt/N*100,
            .by = c(Sykehusnavn, Aar)) %>%
  pivot_wider(
    names_from = Aar,
    values_from = c(Tretthet_utfylt, Sykdomsaktivitet_utfylt, Smerte_utfylt,
                    Andel_Tretthet_utfylt, Andel_Sykdomsaktivitet_utfylt,
                    Andel_Smerte_utfylt, N)) %>%
  arrange(-(Andel_Tretthet_utfylt_2023 +
              Andel_Sykdomsaktivitet_utfylt_2023 +
              Andel_Smerte_utfylt_2023)) %>%
  janitor::adorn_totals() %>%
  mutate(
    Andel_Tretthet_utfylt_2023 =
      ifelse(Sykehusnavn=="Total", Tretthet_utfylt_2023/N_2023*100, Andel_Tretthet_utfylt_2023),
    Andel_Tretthet_utfylt_2022 =
      ifelse(Sykehusnavn=="Total", Tretthet_utfylt_2022/N_2022*100, Andel_Tretthet_utfylt_2022),
    Andel_Tretthet_utfylt_2024 =
      ifelse(Sykehusnavn=="Total", Tretthet_utfylt_2024/N_2024*100, Andel_Tretthet_utfylt_2024),
    Andel_Sykdomsaktivitet_utfylt_2023 =
      ifelse(Sykehusnavn=="Total", Sykdomsaktivitet_utfylt_2023/N_2023*100, Andel_Sykdomsaktivitet_utfylt_2023),
    Andel_Sykdomsaktivitet_utfylt_2022 =
      ifelse(Sykehusnavn=="Total", Sykdomsaktivitet_utfylt_2022/N_2022*100, Andel_Sykdomsaktivitet_utfylt_2022),
    Andel_Sykdomsaktivitet_utfylt_2024 =
      ifelse(Sykehusnavn=="Total", Sykdomsaktivitet_utfylt_2024/N_2024*100, Andel_Sykdomsaktivitet_utfylt_2024),
    Andel_Smerte_utfylt_2023 =
      ifelse(Sykehusnavn=="Total", Smerte_utfylt_2023/N_2023*100, Andel_Smerte_utfylt_2023),
    Andel_Smerte_utfylt_2022 =
      ifelse(Sykehusnavn=="Total", Smerte_utfylt_2022/N_2022*100, Andel_Smerte_utfylt_2022),
    Andel_Smerte_utfylt_2024 =
      ifelse(Sykehusnavn=="Total", Smerte_utfylt_2024/N_2024*100, Andel_Smerte_utfylt_2024))

write.csv2(Oppfolging3siste,
           "~/regdata/norvas/aarsrapp2024/KompletthetProm3sisteaar.csv",
           row.names = F, fileEncoding = "Latin1")

Kompletthet <- bind_rows(
  Kompletthet,
  data.frame(Variabel = c("Tretthet", "PasientGlobalSykdomsaktivitet",
                          "Pasientsmerter"),
             Antall_utfylt = as.numeric(prom_utfylt[1:3]),
             N = rep(prom_utfylt[[4]], 3)))

# Alvorlig_infeksjonrap_aar <- Alvorlig_infeksjon[which(format(Alvorlig_infeksjon$SelvrapportertAlvorligInfeksjon_Registrert_Dato, format="%Y") == rap_aar), ]

nevner <- Inklusjon$SkjemaGUID[Inklusjon$Inklusjonsaar ==
                                 rap_aar & Inklusjon$Diag_gr_nr %in% 2:3]

bp_v_inkl  <- Labskjemarap_aar[which(Labskjemarap_aar$HovedskjemaGUID %in% nevner), ]

bp_v_inkl$hepatitt_samlet <-
  rowSums(bp_v_inkl[, c("HepatittBCoreAntistoffpositiv", "HepatittBSurfaceAntistoffpositiv",
                        "HepatittBSurfaceAntigenpositiv", "HepatittCAntistoffpositiv")] != -1)

bp_v_inkl$hepatitt_samlet[bp_v_inkl$hepatitt_samlet>0] <- 1

bp_v_inkl <- bp_v_inkl %>% group_by(PasientGUID) %>%
  summarise(hepatitt_test = max(hepatitt_samlet),
            N=n())

table(bp_v_inkl$hepatitt_test, useNA = 'ifany')

andel_hepatestrap_aar <- sum(bp_v_inkl$hepatitt_test)/length(unique(nevner))*100
Kompletthet <- bind_rows(
  Kompletthet,
  data.frame(Variabel = "Hepatitt_test",
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


Labskjemarap_aargr2$ancapos_saml <- rowSums(
  Labskjemarap_aargr2[, c("PR3AncaPositiv", "MPO_AncaPositiv")] != -1)

Labskjemarap_aargr2$ancapos_saml[Labskjemarap_aargr2$ancapos_saml>0] <- 1

sum(Labskjemarap_aargr2$ancapos_saml)/dim(Labskjemarap_aargr2)[1]*100

Kompletthet <- bind_rows(
  Kompletthet,
  data.frame(Variabel = "ancapos_test",
             Antall_utfylt = sum(Labskjemarap_aargr2$ancapos_saml),
             N = dim(Labskjemarap_aargr2)[1]))


####

########  BVAS ##############
tmp2 <- merge(
  Oppfolging, BVAS[, c("SykdomsvurderingLabel", "PasientGUID", "BVAS_Dato")],
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
tmp3 <- merge(
  Oppfolging,
  KERR[, c("KerrsKriterier_Dato", "PasientGUID", "SykdomsvurderingLabel")],
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



vdi_v2 <-

  Kompletthet <- bind_rows(Kompletthet, vdi)


########## Selvrapportert infeksjon ############################################
selvrapp <- merge(
  Oppfolging,
  Alvorlig_infeksjon[, c("OvreLuftveierInfeksjon", "PasientGUID",
                         "SelvrapportertAlvorligInfeksjon_Registrert_Dato")],
  by.x = c('PasientGUID','OppfolgingsDato'),
  by.y = c('PasientGUID','SelvrapportertAlvorligInfeksjon_Registrert_Dato'),
  all.x = T) %>%
  dplyr::filter(Oppfolgingsaar==rap_aar) %>%
  summarise(Antall_utfylt = sum(!is.na(OvreLuftveierInfeksjon)),
            N = n()) %>%
  mutate(Variabel = 'Andel_SelvrapportertAlvorligInfeksjon')

Kompletthet <- bind_rows(Kompletthet, selvrapp)

############# Kompletthet av PROM ##############################################
# prom <- merge(Oppfolging,
#                   Pasientsvar,
#                   by.x = c('PasientGUID','OppfolgingsDato'),
#                   by.y = c('PasientGUID','FormDate'), all.x = T)


##########################################################################

Kompletthet$Kompletthet <- Kompletthet$Antall_utfylt/Kompletthet$N*100



write.csv2(Kompletthet, "~/regdata/norvas/aarsrapp2024/Kompletthet2024.csv", row.names = F,
           fileEncoding = "Latin1")

