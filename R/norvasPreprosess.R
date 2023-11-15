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
  datovars <- kodebok_norvas$Variabelnavn[which(kodebok_norvas$Felttype == 'Dato/tid')]
  datovars <- intersect(datovars, names(RegData))
  flyttall <- kodebok_norvas$Variabelnavn[which(kodebok_norvas$Felttype == 'Tall')]
  flyttall <- intersect(flyttall, names(RegData))
  boolsk <- kodebok_norvas$Variabelnavn[which(kodebok_norvas$Felttype == 'Avkrysning')]
  boolsk <- intersect(boolsk, names(RegData))
  RegData <- RegData %>% dplyr::mutate_at(datovars, function(x){as.Date(x, format="%d.%m.%Y")})
  RegData <- RegData %>% dplyr::mutate_at(flyttall, function(x){as.numeric(gsub(',', '\\.', x))}) # les desimaltall som tall
  RegData <- RegData %>% dplyr::mutate_at(boolsk, function(x){as.logical(x)}) # Gjør booske variabler til logicals

  mapEnhet <- data.frame(UnitId = c(102977, 104579, 105274, 106841, 601159, 700701, 105776, 4210431,
                                    103725, 104092, 104209, 110353, 110629, 102708, 4210614, 108054,
                                    701344, 103300),
                         Sykehusnavn = c('Haukeland', 'St. Olavs', 'Førde', 'Haugesund', 'UNN',
                                         'Nordlandsykehuset', 'Levanger', 'Rikshospitalet', 'Drammen', 'Kristiansand',
                                         'Betanien', 'Lillehammer', 'Martina Hansen', 'Ålesund', 'Helgelandssykehuset',
                                         'Moss', 'Stavanger', 'Drammen'))
  RegData$Sykehusnavn <- mapEnhet$Sykehusnavn[match(RegData$UnitId, mapEnhet$UnitId)]
  RegData$ErMann <- RegData$PatientGender
  RegData$ErMann[RegData$PatientGender==2] <- 0

  mapDiagKode <- data.frame(navn=c("Takayasu Arteritt"
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
                                   ,"Annen Immunkompleks Vaskulitt (Goodpasture)"),
                            gtiKode = c(3, 7, 8, 4, 98, 13, 9, 15, 6, 11, 14, 5, 10, 99, 12),
                            gruppering=c('Storkarsvaskulitt (LVV)', 'ANCA assosiert vaskulitt (AAV)', 'ANCA assosiert vaskulitt (AAV)',
                                         'Storkarsvaskulitt (LVV)', 'Andre', 'Andre', 'ANCA assosiert vaskulitt (AAV)',
                                         'Storkarsvaskulitt (LVV)', 'Andre', 'Andre', 'Andre', 'Andre', 'Andre', 'Andre', 'Andre'),
                            gr_nr= c(1,2,2,1,3,3,2,1,3,3,3,3,3,3,3))

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
    RegData$Diagnose <- mapDiagKode$navn[match(RegData$DiagnoseNr, mapDiagKode$gtiKode)]
    RegData$tid_symp_diagnose <- difftime(RegData$Diagnose_Klinisk_Dato, RegData$SymptomStartDato, units = 'days')
    RegData <- RegData[!is.na(RegData$DiagnoseNr), ]
    RegData$Diag_gr_nr <- mapDiagKode$gr_nr[match(RegData$DiagnoseNr, mapDiagKode$gtiKode)]
    RegData$Diag_gr <- factor(RegData$Diag_gr_nr, levels = 1:2, labels = c('Storkarsvaskulitt (LVV)',
                                                                           'ANCA assosiert vaskulitt (AAV)'))
    RegData$Navn <- RegData$Diagnose
  }

  kode <- c(1, 2, 3, 4, 5, 6, 8, 12, 14, 15, 16, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30,
            31, 32, 33, 34, 35, 36, 38, 39, 40, 42, 43, 999)
  Legemiddelgruppe <- c("Biologiske legemidler", "Biologiske legemidler",
                        "Biologiske legemidler", "Biologiske legemidler",
                        "Biologiske legemidler", "Biologiske legemidler",
                        "Biologiske legemidler", "Biologiske legemidler",
                        "Biologiske legemidler",
                        "DMARD", "DMARD", "DMARD", "Kortikosteroider", "DMARD",
                        "DMARD", "Kortikosteroider", "DMARD", "DMARD", "Cyclofosfamid",
                        "Biologiske legemidler", "Biologiske legemidler",
                        "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "Immunoglobulin",
                        "Kortikosteroider", "Biologiske legemidler",
                        "Rituximab", "Biologiske legemidler", "DMARD", "Annet")
  kobl_gruppe_kode <- data.frame(kode, Legemiddelgruppe)

  varnavn <- kodebok_norvas[which(!is.na(kodebok_norvas$Variabelnavn)), c("Variabelnavn", "skjema")]

  if ('LegemiddelType2019' %in% names(RegData)){
    RegData <- RegData[RegData$LegemiddelNr != 17, ] ## Folsyre fjernes
    RegData <- RegData[!is.na(RegData$Med_StartDato), ]
    #
    indekser_kodebok <- which(kodebok_norvas$Variabelnavn == 'LegemiddelType2022' & kodebok_norvas$skjema == 'MedisineringSkjema'):
      (which(kodebok_norvas$Variabelnavn == varnavn$Variabelnavn[which(varnavn$Variabelnavn=='LegemiddelType2022' & varnavn$skjema == 'MedisineringSkjema')+1])-1)
    kobl_generisknavn_kode <- data.frame(kode=as.numeric(kodebok_norvas$kode[c(indekser_kodebok[-1], indekser_kodebok[1])]),
                                         label=kodebok_norvas$label[c(indekser_kodebok[-1], indekser_kodebok[1])])
    gml_medisinnr <- which(RegData$LegemiddelType2019 != "")
    RegData$LegemiddelNr[gml_medisinnr] <- as.numeric(norvas::mapping_med$ny_nr[match(RegData$LegemiddelNr[gml_medisinnr],
                                                                                      norvas::mapping_med$gml_nr)])
    ny_medisinnr <- which(RegData$LegemiddelType2022 != "")
    RegData$LegemiddelType2022[RegData$LegemiddelType2022 %in% "MycofenolatMofetil"] <- "Mycofenolat mofetil"
    RegData$LegemiddelNr[ny_medisinnr] <- kobl_generisknavn_kode$kode[match(RegData$LegemiddelType2022[ny_medisinnr],
                                                                            kobl_generisknavn_kode$label)]

    RegData$LegemiddelGenerisk <- NA
    RegData$LegemiddelGenerisk<- kobl_generisknavn_kode$label[match(RegData$LegemiddelNr, kobl_generisknavn_kode$kode)]
    RegData$LegemiddelTypeLabel <- factor(RegData$LegemiddelNr, levels = kodebok_norvas$kode[c(indekser_kodebok[-1], indekser_kodebok[1])],
                                          labels = kodebok_norvas$label[c(indekser_kodebok[-1], indekser_kodebok[1])])
    RegData$Medikamentgruppe[RegData$Medikamentgruppe == ""] <- "Andre"
    RegData$Legemiddelgruppe <- kobl_gruppe_kode$Legemiddelgruppe[match(RegData$LegemiddelNr, kobl_gruppe_kode$kode)]

    tmp <- RegData %>%
      group_by(PasientGUID, Med_StartDato, LegemiddelGenerisk) %>%
      summarise('ant_samme_startdato' = n(),
                Med_SluttDato_min = if (sum(!is.na(Med_SluttDato))>0) {
                  min(Med_SluttDato, na.rm = T)} else {NA},
                SkjemaGUID_min = if (is.na(Med_SluttDato_min)) {SkjemaGUID[1]}
                else {SkjemaGUID[which(Med_SluttDato == Med_SluttDato_min)[1]]})

    RegData <- merge(RegData, tmp[, c("SkjemaGUID_min", "ant_samme_startdato")], by.x = "SkjemaGUID", by.y = "SkjemaGUID_min")
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
