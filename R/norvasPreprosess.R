#' Preprosesser data for bruk i Norvas sine rapporter
#'
#' Denne funksjonen gjor nodvendig preprosessering av Norvas sin data for bruk i rapporter
#'
#' @param Regdata En dataramme med registerdata
#'
#' @return Et preprosessert datasett
#'
#' @export
#'

norvasPreprosess <- function(RegData) {

  RegData <- dplyr::as_tibble(RegData)
  datovars <- norvas::kodebok_norvas$Variabelnavn[
    which(norvas::kodebok_norvas$Felttype == 'Dato/tid')]
  datovars <- intersect(datovars, names(RegData))
  flyttall <- norvas::kodebok_norvas$Variabelnavn[
    which(norvas::kodebok_norvas$Felttype == 'Tall')]
  flyttall <- intersect(flyttall, names(RegData))
  boolsk <- norvas::kodebok_norvas$Variabelnavn[
    which(norvas::kodebok_norvas$Felttype == 'Avkrysning')]
  boolsk <- intersect(boolsk, names(RegData))
  RegData <- RegData |>
    dplyr::mutate_at(datovars, function(x){
      as.Date(x, format="%d.%m.%Y")})
  RegData <- RegData |>
    dplyr::mutate_at(flyttall, function(x){
      as.numeric(gsub(',', '\\.', x))}) # les desimaltall som tall
  RegData <- RegData |>
    dplyr::mutate_at(boolsk, function(x){
      as.logical(x)}) # Gjør booske variabler til logicals

  ######### AD-HOC : Flytt ous hf til rh og konsolider drammen ############
  RegData$UnitId[RegData$UnitId==4001031] <- 4210431
  RegData$UnitId[RegData$UnitId==103300] <- 103725
  # mapEnhet <- data.frame(
  #   UnitId = c(102977, 104579, 105274, 106841,
  #              601159, 700701, 105776, 4210431,
  #              103725, 104092, 104209, 110353,
  #              110629, 102708, 4210614, 108054,
  #              701344, 103300, 4001031, 101865),
  #   Sykehusnavn = c('Haukeland', 'St. Olavs', 'Førde',
  #                   'Haugesund', 'UNN',
  #                   'Nordlandsykehuset', 'Levanger',
  #                   'Rikshospitalet',
  #                   'Drammen', 'Kristiansand',
  #                   'Betanien', 'Lillehammer',
  #                   'Martina Hansen', 'Ålesund',
  #                   'Helgelandssykehuset',
  #                   'Moss', 'Stavanger', 'Drammen',
  #                   'Rikshospitalet',
  #                   'Hammerfest'))
  mapEnhet <- tribble(
    ~kortnavn, ~Sykehusnavn, ~UnitId,
    "Ålesund", "Helse Møre og Romsdal", 102708,
    "UNN", "Universitetssykehuset Nord-Norge", 601159,
    "Stavanger", "Helse Stavanger", 701344,
    "St. Olavs ", "St. Olavs hospital", 104579,
    "Rikshospitalet", "Oslo universitetssykehus", 4210431,
    "Nordlandssykehuset", "Nordlandssykehuset", 700701,
    "Moss", "Sykehuset Østfold", 108054,
    "Levanger", "Helse Nord-Trøndelag", 105776,
    "Kristiansand", "Sørlandet Sykehus", 104092,
    "Helgelandssykehuset", "Helgelandssykehuset", 4210614,
    "Haukeland", "Helse Bergen", 102977,
    "Hammerfest", "Finnmarkssykehuset", 101865,
    "Førde", "Helse Førde", 105274,
    "Drammen", "Vestre Viken", 103725,
    "Betanien", "Betanien hospital Skien", 104209,
    "Haugesund", "HSR", 106841,
    "Martina Hansen", "Martina Hansen", 110629,
    "Lillehammer", "RS Lillehammer", 110353
  )


  RegData$Sykehusnavn <- mapEnhet$Sykehusnavn[
    match(RegData$UnitId, mapEnhet$UnitId)]
  RegData$ErMann <- RegData$PatientGender
  RegData$ErMann[RegData$PatientGender==2] <- 0

  fiksDiagnoserStOlav <- data.frame(
    diagnose = c(
      "Behcets sykdom",
      "Eosinofilisk Granulomatøs Polyangitt (Churg-Strauss)",
      "Granulomatøs Polyangitt (Wegener’s)",
      "Kjempecelle Arteritt",
      "Kjempecellearteritt med polymyalgia rheumatica",
      "Kjempecellearteritt med polymyalgia rheumatica (GCA2)",
      "Polymyalgia Rheumatica",
      "Mikroskopisk Polyangiitis",
      "Primær nekrotiserende systemisk vaskulitt",
      "Systemisk Vaskulitt sykdom",
      "Takayasu Arteritt",
      "Uspesifisert nekrotiserende vaskulitt",
      "Juvenil temporalisarteritt"),
    DiagnoseNr = c(13, 8, 7, 4, 4, 4, 98, 9, 14, 14, 3, 14, 97)
  )
  mapDiagKode <- data.frame(
    navn=c("Takayasu Arteritt"
           ,"Granulomatøs Polyangitt"
           ,"Eosinofil Granulomatøs Polyangitt"
           ,"Kjempecellearteritt"
           ,"Polymyalgia Rheumatica"
           ,"Behcets sykdom"
           ,"Mikroskopisk Polyangitt"
           ,"Aortitt INA"
           ,"Kawasakis syndrom"
           ,"Kryoglobulin Vaskulitt"
           ,"Uspesifisert nekrotiserende vaskulitt"
           ,"Polyarteritis Nodosa"
           ,"IgA Vaskulitt (Henoch-Schoenlein)"
           ,"Systemisk Vaskulitt sykdom"
           ,"Annen Immunkompleks Vaskulitt (Goodpasture)"
           ,"Annen"),
    navn_ny=c("Takayasus sykdom (Aortabuesyndrom)"
              ,"Granulomatose med polyangiitt (GPA)"
              ,"Polyarteritt med lungeaffeksjon (EGPA)"
              ,"Kjempecellearteritt med polymyalgia rheumatica /Annen kjempecellearteritt"
              ,"Polymyalgia Rheumatica"
              ,"Behcets sykdom"
              ,"Mikroskopisk polyangiitt (MPA)"
              ,"Uspesifisert arteritt"
              ,"Kawasakis syndrom"
              ,"Kryoglobulin Vaskulitt"
              ,"Uspesifisert nekrotiserende vaskulitt"
              ,"Polyarteritis Nodosa"
              ,"IgA Vaskulitt (Henoch-Schoenlein)"
              ,"Systemisk Vaskulitt sykdom"
              ,"Annen Immunkompleks Vaskulitt (Goodpasture)"
              ,"Annen"),
    kortnavn = c("TAK"
                 ,"GPA"
                 ,"EGPA"
                 ,"KCA"
                 ,"Polymyalgia Rheumatica"
                 ,"Behcets sykdom"
                 ,"MPA"
                 ,"Aortitt"
                 ,"Kawasakis syndrom"
                 ,"Kryoglobulin Vaskulitt"
                 ,"Uspesifisert nekrotiserende vaskulitt"
                 ,"Polyarteritis Nodosa"
                 ,"IgA Vaskulitt (Henoch-Schoenlein)"
                 ,"Systemisk Vaskulitt sykdom"
                 ,"Annen Immunkompleks Vaskulitt (Goodpasture)"
                 ,"Annen"),
    gtiKode = c(3, 7, 8, 4, 98, 13, 9, 15, 6, 11, 14, 5, 10, 99, 12, 97),
    gruppering=c('Storkarsvaskulitt (LVV)', 'ANCA assosiert vaskulitt (AAV)',
                 'ANCA assosiert vaskulitt (AAV)',
                 'Storkarsvaskulitt (LVV)', 'Andre', 'Andre',
                 'ANCA assosiert vaskulitt (AAV)',
                 'Storkarsvaskulitt (LVV)', 'Andre', 'Andre', 'Andre',
                 'Andre', 'Andre', 'Andre', 'Andre', 'Andre'),
    gr_nr= c(1,2,2,1,3,3,2,1,3,3,3,3,3,3,3,3))

  if ("InklusjonDato" %in% names(RegData)) {
    RegData <- RegData[!is.na(RegData$InklusjonDato), ]
  }
  if ('Icd_IcdDataDump' %in% names(RegData)) {
    names(RegData)[names(RegData)=='Icd_IcdDataDump'] <- 'Icd'}
  if ('Diagnose' %in% names(RegData) & 'Icd' %in% names(RegData)) {
    # Kun store bokstaver
    RegData$Icd <- toupper(RegData$Icd)
    # Fjern alle mellomrom
    RegData$Icd <- gsub(' ', '', RegData$Icd)
    # Fjern alle komma og punktum
    RegData$Icd <- gsub(',', '', RegData$Icd)
    RegData$Icd <- gsub('\\.', '', RegData$Icd)
    RegData$DiagnoseNr[RegData$Diagnose == "Polyarteritis Nodosa"] <- 5
    tmp <- RegData[is.na(RegData$DiagnoseNr), ]
    RegData <- RegData[!is.na(RegData$DiagnoseNr), ]
    tmp$DiagnoseNr <-
      fiksDiagnoserStOlav$DiagnoseNr[
        match(tmp$Diagnose, fiksDiagnoserStOlav$diagnose)]
    RegData <- dplyr::bind_rows(RegData, tmp)
    RegData$Diagnose <- mapDiagKode$navn_ny[
      match(RegData$DiagnoseNr, mapDiagKode$gtiKode)]
    RegData$Diagnose_kortnavn <- mapDiagKode$navn_ny[
      match(RegData$DiagnoseNr, mapDiagKode$gtiKode)]
    RegData$tid_symp_diagnose <- difftime(
      RegData$Diagnose_Klinisk_Dato, RegData$SymptomStartDato,
      units = 'days')
    RegData <- RegData[!is.na(RegData$DiagnoseNr), ]
    RegData$Diag_gr_nr <- mapDiagKode$gr_nr[
      match(RegData$DiagnoseNr, mapDiagKode$gtiKode)]
    RegData$Diag_gr <- factor(
      RegData$Diag_gr_nr, levels = 1:2,
      labels = c('Storkarsvaskulitt (LVV)',
                 'ANCA assosiert vaskulitt (AAV)'))
    RegData$Navn <- RegData$Diagnose
    icd10_map <- data.frame(
      diagnr = c(3,4,7,8,9,15),
      icd10 = c("M31.4", "M31.5/M31.6", "M31.3",
                "M30.1", "M31.7", "I77.6"))
    RegData$ICD10 <- icd10_map$icd10[
      match(RegData$DiagnoseNr, icd10_map$diagnr)]

  }

  varnavn <- norvas::kodebok_norvas[
    which(!is.na(norvas::kodebok_norvas$Variabelnavn)),
    c("Variabelnavn", "skjema")]

  if ('LegemiddelType2019' %in% names(RegData)){
    RegData <- RegData[RegData$LegemiddelType2019 != 17, ] ## Folsyre fjernes
    RegData <- RegData[!is.na(RegData$Med_StartDato), ] # Fjerner reg uten dato
    ### OBS: Må oppdateres ved innføring av nye medisiner
    med2023 <- norvas::kodebok_norvas[which(
      norvas::kodebok_norvas$Variabelnavn == 'LegemiddelType2023' &
        norvas::kodebok_norvas$skjema == 'MedisineringSkjema'):
        (which(norvas::kodebok_norvas$Variabelnavn ==
                 varnavn$Variabelnavn[
                   which(varnavn$Variabelnavn=='LegemiddelType2023' &
                           varnavn$skjema == 'MedisineringSkjema')+1])-1),
      c("kode", "label")] |>
      merge(norvas::mapping_medgr |>
              dplyr::select(kode, NyGruppe),
            by = "kode", all = T)
    RegData <- RegData |>
      dplyr::mutate(
        LegemiddelType2019 = dplyr::if_else(
          LegemiddelType2019 == 0, 0,
          as.numeric(norvas::mapping_med$ny_nr[
            match(LegemiddelType2019,
                  norvas::mapping_med$gml_nr)])),
        LegemiddelType2019 = dplyr::if_else(
          LegemiddelType2019==999, -1, LegemiddelType2019),
        LegemiddelType2020 = dplyr::if_else(
          LegemiddelType2020==999, -1, LegemiddelType2020),
        LegemiddelType2022 = dplyr::if_else(
          LegemiddelType2022==999, -1, LegemiddelType2022),
        LegemiddelType2023 = dplyr::if_else(
          LegemiddelType2023==999, -1, LegemiddelType2023),
        LegemiddelType = pmax(LegemiddelType2019,
                              LegemiddelType2020,
                              LegemiddelType2022,
                              LegemiddelType2023),
        LegemiddelType = dplyr::case_when(
          LegemiddelType == 32 ~ 16,
          LegemiddelType == 59 ~ 40,
          .default = LegemiddelType
        )
      ) |>
      dplyr::filter(
        !(LegemiddelType %in% c(35, 43)),
        !(Legemiddel == "Folsyre" & LegemiddelType == 0)) |>
      dplyr::mutate(
        Legemiddel = dplyr::case_when(
          Legemiddel == "Methotrexate" ~ "Metotreksat",
          Legemiddel == "Azathioprin" ~ "Azatioprin",
          Legemiddel == "Infliximab" ~ "Infliksimab",
          Legemiddel == "Colchicine" ~ "Kolkisin",
          Legemiddel == "Tacrolimus" ~ "Takrolimus",
          Legemiddel == "Mycofenolat mofetil" ~ "Mykofenolsyre",
          substr(Legemiddel, 1, 10) == "LEFLUNOMID" ~ "Leflunomid",
          .default = Legemiddel
        ),
        LegemiddelType = ifelse(
          LegemiddelType %in% c(0, 999) & Legemiddel %in% med2023$label,
          med2023$kode[match(Legemiddel, med2023$label)],
          LegemiddelType
        ),
        Legemiddelgruppe = med2023$NyGruppe[
          match(LegemiddelType, med2023$kode)],
        Legemiddelgruppe = ifelse(Legemiddelgruppe == "",
                                  "Ingen", Legemiddelgruppe),
        LegemiddelGenerisk = med2023$label[
          match(LegemiddelType, med2023$kode)],
        LegemiddelNr = LegemiddelType
      )

    tmp <- RegData %>%
      dplyr::summarise(
        'ant_samme_startdato' = dplyr::n(),
        Med_SluttDato_min = if (sum(!is.na(Med_SluttDato))>0) {
          min(Med_SluttDato, na.rm = T)} else {NA},
        SkjemaGUID_min = if (is.na(Med_SluttDato_min)) {
          SkjemaGUID[1]}
        else {
          SkjemaGUID[which(Med_SluttDato == Med_SluttDato_min)[1]]},
        .by = c(PasientGUID, Med_StartDato,
                LegemiddelType))
    RegData <- merge(
      RegData,
      tmp[, c("SkjemaGUID_min", "ant_samme_startdato")],
      by.x = "SkjemaGUID", by.y = "SkjemaGUID_min")
  }

  if ('BvasPersistentTotal' %in% names(RegData)){
    indekser_kodebok <- which(kodebok_norvas$Variabelnavn == 'Sykdomsvurdering' & kodebok_norvas$skjema == 'BvasSkjema'):
      (which(kodebok_norvas$Variabelnavn == varnavn$Variabelnavn[which(varnavn$Variabelnavn=='Sykdomsvurdering' & varnavn$skjema == 'BvasSkjema')+1])-1)
    RegData$SykdomsvurderingLabel <- factor(RegData$Sykdomsvurdering, levels = kodebok_norvas$kode[c(indekser_kodebok[-1])],
                                            labels = kodebok_norvas$label[c(indekser_kodebok[-1])])
    RegData$bvas_samlet <- RegData$BvasPersistentTotal
    RegData$bvas_samlet[is.na(RegData$bvas_samlet)] <- RegData$BvasRenalNewOrWorseScore[is.na(RegData$bvas_samlet)]

    tmp <- table(RegData[, c("PasientGUID", "BVAS_Dato")])
    tmp <- as.data.frame(tmp)
    tmp <- tmp[tmp$Freq>1, ]
    tmp2 <-  merge(RegData[, c("PasientGUID", "BVAS_Dato", "SkjemaGUID")],
                   tmp[, c("PasientGUID", "BVAS_Dato")], by = c('PasientGUID', 'BVAS_Dato'))

    RegData <- RegData[!(RegData$SkjemaGUID %in% tmp2$SkjemaGUID), ] ## Fjerner BVAS som har flere registreringer på
    ## samme pasient på samme dag.
  }

  if ('KerrsKriterier_Dato' %in% names(RegData)){
    indekser_kodebok <- which(kodebok_norvas$Variabelnavn == 'Sykdomsvurdering' & kodebok_norvas$skjema == 'KerrsKriterierSkjema'):
      (which(kodebok_norvas$Variabelnavn == varnavn$Variabelnavn[which(varnavn$Variabelnavn=='Sykdomsvurdering' & varnavn$skjema == 'KerrsKriterierSkjema')+1] & kodebok_norvas$skjema == 'KerrsKriterierSkjema')-1)
    RegData$SykdomsvurderingLabel <- factor(RegData$Sykdomsvurdering, levels = kodebok_norvas$kode[c(indekser_kodebok[-1])],
                                            labels = kodebok_norvas$label[c(indekser_kodebok[-1])])
  }

  if ('AntallInfeksjoner' %in% names(RegData)){
    # kobl_num_kat <- data.frame(kode=0:4, kode=c('Ingen', 'En', 'To', 'Tre', 'FireEllerFler'))
    # RegData$AntallInfeksjoner_num <- kobl_num_kat$tall[match(RegData$AntallInfeksjoner, kobl_num_kat$kode)]
    indekser_kodebok <- which(kodebok_norvas$Variabelnavn == 'AntallInfeksjoner' & kodebok_norvas$skjema == 'SelvrapportertAlvorligInfek'):
      (which(kodebok_norvas$Variabelnavn == varnavn$Variabelnavn[which(varnavn$Variabelnavn=='AntallInfeksjoner' & varnavn$skjema == 'SelvrapportertAlvorligInfek')+1])-1)
    RegData$AntallInfeksjonerLabel <- factor(RegData$AntallInfeksjoner, levels = kodebok_norvas$kode[c(indekser_kodebok)],
                                             labels = kodebok_norvas$label[c(indekser_kodebok)])
  }

  return(invisible(RegData))

}
