
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
VaskulittIntervensjon <- aarrappdata$VaskulittIntervensjon
# Diagnoser <- aarrappdata$Diagnoser
# Medisiner <- aarrappdata$Medisiner
# BVAS <- aarrappdata$BVAS
# KERR <- aarrappdata$KERR
# VDI <- aarrappdata$VDI
# Alvorlig_infeksjon <- aarrappdata$Alvorlig_infeksjon
# Utredning <- aarrappdata$Utredning
# Labskjema <- aarrappdata$Labskjema
# Pasientsvar <- aarrappdata$Pasientsvar


## Inklusjon og oppfølging samlet
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

# Andel inklusjon/oppfølging med tilhørende VaskulittIntervensjonsskjema
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
                    CaVitaminD, BisfosfonatEllerTilsvarende,
                    TrimetoprimSulfa, Annenantibiotikaprofylakse),
    by = join_by(PasientGUID, Dato == VaskulittIntervensjon_Dato)
  ) %>%
  dplyr::summarise(
    CaVitaminD = sum(CaVitaminD),
    BisfosfonatEllerTilsvarende = sum(BisfosfonatEllerTilsvarende),
    TrimetoprimSulfa = sum(TrimetoprimSulfa),
    Annenantibiotikaprofylakse = sum(Annenantibiotikaprofylakse),
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
                    CaVitaminD, BisfosfonatEllerTilsvarende,
                    TrimetoprimSulfa, Annenantibiotikaprofylakse),
    by = join_by(PasientGUID, Dato.y == VaskulittIntervensjon_Dato)
  ) %>%
  dplyr::summarise(
    CaVitaminD = sum(CaVitaminD),
    BisfosfonatEllerTilsvarende = sum(BisfosfonatEllerTilsvarende),
    TrimetoprimSulfa = sum(TrimetoprimSulfa),
    Annenantibiotikaprofylakse = sum(Annenantibiotikaprofylakse),
    N = dplyr::n(),
    .by = c(Sykehusnavn, Aar, Diag_gr)
  ) %>%
  dplyr::filter(Aar >= rap_aar-2)

oppf_12mnd <- dplyr::inner_join(
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
    tid_fra_6mnd = abs(tid_fra_debut-365)
  ) %>%
  dplyr::filter(tid_fra_6mnd == min(tid_fra_6mnd), .by = PasientGUID) %>%
  dplyr::distinct(Dato.y, PasientGUID, .keep_all = TRUE) %>%
  dplyr::filter(tid_fra_6mnd <= 90) %>%
  dplyr::distinct(tid_fra_6mnd, PasientGUID, .keep_all = TRUE) %>%
  dplyr::inner_join(
    VaskulittIntervensjon %>%
      dplyr::select(PasientGUID, VaskulittIntervensjon_Dato,
                    CaVitaminD, BisfosfonatEllerTilsvarende,
                    TrimetoprimSulfa, Annenantibiotikaprofylakse),
    by = join_by(PasientGUID, Dato.y == VaskulittIntervensjon_Dato)
  ) %>%
  dplyr::summarise(
    CaVitaminD = sum(CaVitaminD),
    BisfosfonatEllerTilsvarende = sum(BisfosfonatEllerTilsvarende),
    TrimetoprimSulfa = sum(TrimetoprimSulfa),
    Annenantibiotikaprofylakse = sum(Annenantibiotikaprofylakse),
    N = dplyr::n(),
    .by = c(Sykehusnavn, Aar, Diag_gr)
  ) %>%
  dplyr::filter(Aar >= rap_aar-2)



andel_pr_shus_aar_diaggr <- function(dataramme, diaggr, aar, variabel) {
  dataramme %>%
    dplyr::filter(
      Diag_gr == diaggr,
      Aar == aar) %>%
    dplyr::select(Sykehusnavn, !! sym(variabel), N) %>%
    janitor::adorn_totals() %>%
    dplyr::mutate(Andel = paste0(round(!! sym(variabel)/N*100,1), "% (",
                                 !! sym(variabel), " av ", N, ")")) %>%
    dplyr::select(Sykehusnavn, Andel)
}

samlet_1aar_og_diaggr <- function(data1, data2, data3, aar, diaggr, variabel) {
  dplyr::full_join(
    andel_pr_shus_aar_diaggr(data1,
                             diaggr = diaggr,
                             aar = aar,
                             variabel = variabel),
    andel_pr_shus_aar_diaggr(data2,
                             diaggr = diaggr,
                             aar = aar,
                             variabel = variabel),
    by = "Sykehusnavn",
    suffix = c("_debut", "_6mnd")
  ) %>%
    dplyr::full_join(
      andel_pr_shus_aar_diaggr(data3,
                               diaggr = diaggr,
                               aar = aar,
                               variabel = variabel),
      by = "Sykehusnavn"
    ) %>%
    dplyr::rename(Andel_12mnd = Andel)
}

samle3aar <- function(Sdomdebut, oppf_6mnd, oppf_12mnd,
                      diaggr = "Storkarsvaskulitt (LVV)",
                      sisteaar = 2024,
                      variabel = "CaVitaminD") {
  dplyr::bind_rows(
    samlet_1aar_og_diaggr(Sdomdebut, oppf_6mnd, oppf_12mnd,
                          diaggr = diaggr,
                          aar = sisteaar,
                          variabel = variabel) %>%
      dplyr::mutate(Aar = sisteaar) %>%
      dplyr::relocate(Aar),
    samlet_1aar_og_diaggr(Sdomdebut, oppf_6mnd, oppf_12mnd,
                          diaggr = diaggr,
                          aar = sisteaar-1,
                          variabel = variabel) %>%
      dplyr::mutate(Aar = sisteaar-1) %>%
      dplyr::relocate(Aar),
    samlet_1aar_og_diaggr(Sdomdebut, oppf_6mnd, oppf_12mnd,
                          diaggr = diaggr,
                          aar = sisteaar-2,
                          variabel = variabel) %>%
      dplyr::mutate(Aar = sisteaar-2) %>%
      dplyr::relocate(Aar)
  )
}

Samlet_CaVitaminD_GCA <-
  samle3aar(Sdomdebut, oppf_6mnd, oppf_12mnd,
            diaggr = "Storkarsvaskulitt (LVV)",
            sisteaar = 2024,
            variabel = "CaVitaminD")
Samlet_Bisfosfonat_GCA <-
  samle3aar(Sdomdebut, oppf_6mnd, oppf_12mnd,
            diaggr = "Storkarsvaskulitt (LVV)",
            sisteaar = 2024,
            variabel = "BisfosfonatEllerTilsvarende")
Samlet_CaVitaminD_ANCA <-
  samle3aar(Sdomdebut, oppf_6mnd, oppf_12mnd,
            diaggr = "ANCA assosiert vaskulitt (AAV)",
            sisteaar = 2024,
            variabel = "CaVitaminD")
Samlet_Bisfosfonat_ANCA <-
  samle3aar(Sdomdebut, oppf_6mnd, oppf_12mnd,
            diaggr = "ANCA assosiert vaskulitt (AAV)",
            sisteaar = 2024,
            variabel = "BisfosfonatEllerTilsvarende")
Samlet_TrimetoprimSulfa_ANCA <-
  samle3aar(Sdomdebut, oppf_6mnd, oppf_12mnd,
            diaggr = "ANCA assosiert vaskulitt (AAV)",
            sisteaar = 2024,
            variabel = "TrimetoprimSulfa")
Samlet_Annenantibiotikaprofylakse_ANCA <-
  samle3aar(Sdomdebut, oppf_6mnd, oppf_12mnd,
            diaggr = "ANCA assosiert vaskulitt (AAV)",
            sisteaar = 2024,
            variabel = "Annenantibiotikaprofylakse")


Diag_aar <- Inklusjon %>%
  dplyr::filter(FormStatus == 2) %>%
  dplyr::mutate(DiagnoseAar = format(Diagnose_Klinisk_Dato, format = "%Y") %>%
                  as.numeric()) %>%
  dplyr::count(DiagnoseAar, Diag_gr) %>%
  tidyr::pivot_wider(names_from = Diag_gr, values_from = n) %>%
  dplyr::arrange(-DiagnoseAar) %>%
  dplyr::filter(DiagnoseAar >= rap_aar-2) %>%
  janitor::adorn_totals()






