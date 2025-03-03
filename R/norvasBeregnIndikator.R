#' Beregn indikatorer
#'
#' Beregn Norvas sine indikatorer
#'
#' @param norvasdata liste med alle relevante tabeller fra Norvas
#' @param ind_id id til indikator som skal beregnes
#' @param rap_aar til og med hvilket år
#'
#' @export
#'
norvasBeregnIndikator <- function(norvasdata,
                                  ind_id = "norvas_anca_ved_debut",
                                  rap_aar = lubridate::year(Sys.Date())) {
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
      102977, 974557746,
      4001031, 993467049
    )

  Inklusjon <- norvasdata$Inklusjon
  Oppfolging <- norvasdata$Oppfolging
  Labskjema <- norvasdata$Labskjema
  BVAS <- norvasdata$BVAS
  KERR <- norvasdata$KERR

  if (ind_id == "norvas_anca_ved_debut") {
    indikator <- Inklusjon %>%
      dplyr::select(SkjemaGUID, PasientGUID, InklusjonDato, Inklusjonsaar,
                    Diagnose_ny_30, Diag_gr_nr, Sykehusnavn, UnitId) %>%
      # dplyr::filter(Diagnose_ny_30 == 1) %>%
      dplyr::filter(Inklusjonsaar <= rap_aar) %>%
      dplyr::filter(Diag_gr_nr == 2) %>%
      merge(Labskjema %>% dplyr::select(HovedskjemaGUID, Blodprover_Dato,
                                        PR3AncaPositiv, MPO_AncaPositiv),
            by.x = c('SkjemaGUID', "InklusjonDato"),
            by.y = c('HovedskjemaGUID', "Blodprover_Dato"),
            all.x = TRUE) %>%
      dplyr::mutate(var = ((PR3AncaPositiv %in% c(0,1))  |
                             (MPO_AncaPositiv %in% c(0,1))) %>% as.numeric(),
                    denominator = 1,
                    context = "caregiver",
                    ind_id = ind_id,
                    orgnr = orgnr_table$orgnr[match(UnitId, orgnr_table$UnitId)]) %>%
      dplyr::rename(year = Inklusjonsaar) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)

    maal_hoy = 95
    maal_moderat = 50
  }

  if (ind_id == "norvas_bvas_utfylt") {
    indikator <-
      dplyr::bind_rows(
        Inklusjon %>% dplyr::select(SkjemaGUID, PasientGUID, Inklusjonsaar,
                                    InklusjonDato, UnitId, Diag_gr_nr) %>%
          dplyr::rename(year = Inklusjonsaar,
                        Dato = InklusjonDato),
        Oppfolging %>% dplyr::select(SkjemaGUID, PasientGUID, Oppfolgingsaar,
                                     OppfolgingsDato, UnitId, Diag_gr_nr) %>%
          dplyr::rename(year = Oppfolgingsaar,
                        Dato = OppfolgingsDato)
      ) %>%
      dplyr::filter(Diag_gr_nr == 2) %>%
      merge(BVAS[, c("Sykdomsvurdering", "PasientGUID", "BVAS_Dato")],
            by.x = c('PasientGUID','Dato'),
            by.y = c('PasientGUID','BVAS_Dato'),
            all.x = T) %>%
      dplyr::mutate(var = as.numeric(!is.na(Sykdomsvurdering)),
                    denominator = 1,
                    context = "caregiver",
                    ind_id = ind_id,
                    orgnr = orgnr_table$orgnr[match(UnitId, orgnr_table$UnitId)]) %>%
      dplyr::select(context, orgnr, year, var, denominator, ind_id)

    maal_hoy = 95
    maal_moderat = 50
  }



  return(list(indikator=indikator,
              maal_hoy = maal_hoy,
              maal_moderat = maal_moderat))


}
