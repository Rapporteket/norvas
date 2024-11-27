
library(norvas)
library(lubridate)
library(dplyr)
library(tidyr)
rm(list = ls())
options(dplyr.summarise.inform = FALSE)

rap_aar <- 2024
aarrappdata <- norvas::lesogprosesser(rap_aar = rap_aar)
Inklusjon <- aarrappdata$Inklusjon
Oppfolging <- aarrappdata$Oppfolging
# Diagnoser <- aarrappdata$Diagnoser
# Medisiner <- aarrappdata$Medisiner
BVAS <- aarrappdata$BVAS
KERR <- aarrappdata$KERR
VDI <- aarrappdata$VDI
VaskulittIntervensjon <- aarrappdata$VaskulittIntervensjon
# Alvorlig_infeksjon <- aarrappdata$Alvorlig_infeksjon
# Utredning <- aarrappdata$Utredning
# Labskjema <- aarrappdata$Labskjema
# Pasientsvar <- aarrappdata$Pasientsvar

Inkl_oppf <- dplyr::bind_rows(
  Inklusjon[,c("PasientGUID", "InklusjonDato", "Inklusjonsaar",
               "FormStatus", "Diag_gr_nr", "Diagnose_ny_30", "Sykehusnavn")] %>%
    dplyr::rename(Dato = InklusjonDato,
                  Aar = Inklusjonsaar),
  Oppfolging[c("PasientGUID", "OppfolgingsDato", "Oppfolgingsaar",
               "FormStatus", "Diag_gr_nr", "Sykehusnavn")] %>%
    dplyr::rename(Dato = OppfolgingsDato,
                  Aar = Oppfolgingsaar)
) %>%
  dplyr::mutate(Diag_gr = case_when(
    Diag_gr_nr == 1 ~ "Storkarsvaskulitt (LVV)",
    Diag_gr_nr == 2 ~ "ANCA assosiert vaskulitt (AAV)"
  )) %>%
  dplyr::filter(FormStatus == 2)

# Andel inklusjon/oppfølging med tilhørende VDI-skjema
vasint <- merge(Inkl_oppf,
                VaskulittIntervensjon[, c("TrimetoprimSulfa", "PasientGUID",
                                          "VaskulittIntervensjon_Dato")],
                by.x = c('PasientGUID','Dato'),
                by.y = c('PasientGUID','VaskulittIntervensjon_Dato'), all.x = T) %>%
  summarise(Antall_utfylt = sum(!is.na(TrimetoprimSulfa)),
            N = n(),
            .by = c(Aar, Diag_gr)) %>%
  mutate(andel = Antall_utfylt/N*100,
         andel_txt = paste0(round(andel,1), "% (", Antall_utfylt, " av ", N, ")")) %>%
  tidyr::pivot_wider(id_cols = Aar, names_from = Diag_gr, values_from = andel_txt) %>%
  arrange(Aar)


Sdomdebut <- Inkl_oppf %>%
  dplyr::filter(Diagnose_ny_30 == 1,
                Aar >= rap_aar-2) %>%
  dplyr::inner_join(
    VaskulittIntervensjon %>%
      dplyr::select(PasientGUID, VaskulittIntervensjon_Dato,
                    CaVitaminD, BisfosfonatEllerTilsvarende),
    by = join_by(PasientGUID, Dato == VaskulittIntervensjon_Dato)
  ) %>%
  dplyr::summarise(
    CaVitaminD = sum(CaVitaminD),
    BisfosfonatEllerTilsvarende = sum(BisfosfonatEllerTilsvarende),
    N = dplyr::n(),
    .by = c(Sykehusnavn, Aar, Diag_gr)
  )


oppf_6mnd <- dplyr::inner_join(
  Inkl_oppf %>%
    dplyr::select(-Aar) %>%
    dplyr::filter(Diagnose_ny_30==1),
  Inkl_oppf %>%
    dplyr::filter(is.na(Diagnose_ny_30)) %>%
    dplyr::select(PasientGUID, Dato, Aar),
  by = join_by(PasientGUID)
) %>%
  dplyr::mutate(
    tid_fra_debut = difftime(Dato.y, Dato.x, units = "days"),
    tid_fra_6mnd = abs(tid_fra_debut-182)
  ) %>%
  dplyr::filter(tid_fra_6mnd == min(tid_fra_6mnd), .by = PasientGUID) %>%
  dplyr::distinct(Dato.y, PasientGUID, .keep_all = TRUE) %>%
  dplyr::filter(tid_fra_6mnd <= 90) %>%
  dplyr::distinct(tid_fra_6mnd, PasientGUID, .keep_all = TRUE) %>%
  dplyr::inner_join(
    VaskulittIntervensjon %>%
      dplyr::select(PasientGUID, VaskulittIntervensjon_Dato,
                    CaVitaminD, BisfosfonatEllerTilsvarende),
    by = join_by(PasientGUID, Dato.y == VaskulittIntervensjon_Dato)
  ) %>%
  dplyr::summarise(
    CaVitaminD = sum(CaVitaminD),
    BisfosfonatEllerTilsvarende = sum(BisfosfonatEllerTilsvarende),
    N = dplyr::n(),
    .by = c(Sykehusnavn, Aar, Diag_gr)
  )



CaVitaminD_GCA_2024 <- Sdomdebut %>%
  dplyr::filter(
    Diag_gr == "Storkarsvaskulitt (LVV)",
    Aar == 2024) %>%
  dplyr::select(-Aar, -Diag_gr) %>%
  janitor::adorn_totals() %>%
  dplyr::mutate(Andel = paste0(round(CaVitaminD/N*100,1), "% (", CaVitaminD, " av ", N, ")"))
CaVitaminD_GCA_2023 <- Sdomdebut %>%
  dplyr::filter(
    Diag_gr == "Storkarsvaskulitt (LVV)",
    Aar == 2023) %>%
  dplyr::select(-Aar, -Diag_gr) %>%
  janitor::adorn_totals() %>%
  dplyr::mutate(Andel = paste0(round(CaVitaminD/N*100,1), "% (", CaVitaminD, " av ", N, ")"))
CaVitaminD_GCA_2022 <- Sdomdebut %>%
  dplyr::filter(
    Diag_gr == "Storkarsvaskulitt (LVV)",
    Aar == 2022) %>%
  dplyr::select(-Aar, -Diag_gr) %>%
  janitor::adorn_totals() %>%
  dplyr::mutate(Andel = paste0(round(CaVitaminD/N*100,1), "% (", CaVitaminD, " av ", N, ")"))








