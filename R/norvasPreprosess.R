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

  RegData <- as_tibble(RegData)
  datovars <- kodebok_norvas$Variabelnavn[which(kodebok_norvas$Felttype == 'Dato/tid')]
  datovars <- intersect(datovars, names(RegData))
  flyttall <- kodebok_norvas$Variabelnavn[which(kodebok_norvas$Felttype == 'Numerisk (flyttall)')]
  flyttall <- intersect(flyttall, names(RegData))
  boolsk <- kodebok_norvas$Variabelnavn[which(kodebok_norvas$Felttype == 'Avkrysning')]
  boolsk <- intersect(boolsk, names(RegData))
  # RegData[, datovars] <- mutate_all(RegData[, datovars], funs(as.Date(., format="%d.%m.%Y"))) # Fiks datoformat
  RegData <- RegData %>% mutate_at(datovars, funs(as.Date(., format="%d.%m.%Y"))) # Fiks datoformat
  # RegData[, flyttall] <- mutate_all(RegData[, flyttall], funs(as.numeric(gsub(',', '\\.', .)))) # les desimaltall som tall
  RegData <- RegData %>% mutate_at(flyttall, funs(as.numeric(gsub(',', '\\.', .)))) # les desimaltall som tall
  RegData <- RegData %>% mutate_at(boolsk, funs(as.logical(.))) # Gjør booske variabler til logicals

  mapEnhet <- data.frame(UnitId = c(102977, 104579, 105274, 106841, 601159, 700701, 105776, 4210431,
                                    103725, 104092, 104209, 110353, 110629, 102708, 4210614),
                         Sykehusnavn = c('Haukeland', 'St. Olavs', 'Førde', 'Haugesund', 'UNN',
                                         'Nordlandsykehuset', 'Levanger', 'Rikshospitalet', 'Drammen', 'Kristiansand',
                                         'Betanien', 'Lillehammer', 'Martina Hansen', 'Ålesund', 'Helgelandssykehuset'))
  RegData$Sykehusnavn <- mapEnhet$Sykehusnavn[match(RegData$UnitId, mapEnhet$UnitId)]
  # names(RegData)[names(RegData)=='UnitId'] <- 'AvdRESH'  ## Denne må sjekkes for ev. konsekvenser !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  RegData$ErMann <- NA
  RegData$ErMann[RegData$PatientGender=='Male'] <- 1
  RegData$ErMann[RegData$PatientGender=='Female'] <- 0

  mapDiagKode <- data.frame(navn=c("Takayasu Arteritt"
                                   ,"Granulomatøs Polyangitt (Wegener’s)"
                                   ,"Eosinofilisk Granulomatøs Polyangitt (Churg-Strauss)"
                                   ,"Kjempecelle Arteritt"
                                   ,"Polymyalgia Rheumatica"
                                   ,"Behcets sykdom"
                                   ,"Mikroskopisk Polyangiitis"
                                   ,"Aortitt INA"
                                   ,"Kawasakis syndrom"
                                   ,"Kryoglobulin Vaskulitt"
                                   ,"Uspesifisert nekrotiserende vaskulitt"
                                   ,"Polyarteritis Nodosa"
                                   ,"IgA Vaskulitt (Henoch-Schoenlein)"
                                   ,"Systemisk Vaskulitt sykdom"
                                   ,"Annen Immunkompleks Vaskulitt (Goodpasture)"), gtiKode = c(3, 7, 8, 4, 98, 13, 9, 15, 6, 11, 14, 5, 10, 99, 12),
                            gruppering=c('Storkarsvaskulitt (LVV)', 'ANCA assosiert vaskulitt (AAV)', 'ANCA assosiert vaskulitt (AAV)',
                                         'Storkarsvaskulitt (LVV)', 'Andre', 'Andre', 'ANCA assosiert vaskulitt (AAV)',
                                         'Storkarsvaskulitt (LVV)', 'Andre', 'Andre', 'Andre', 'Andre', 'Andre', 'Andre', 'Andre'),
                            gr_nr= c(1,2,2,1,3,3,2,1,3,3,3,3,3,3,3))

  if ('Diagnose' %in% names(RegData)) {names(RegData)[names(RegData)=='Diagnose'] <- 'Navn'}
  if ('Navn' %in% names(RegData) & 'Icd' %in% names(RegData)) {
    # Kun store bokstaver
    RegData$Icd <- toupper(RegData$Icd)
    # Fjern alle mellomrom
    RegData$Icd <- gsub(' ', '', RegData$Icd)
    # Fjern alle komma og punktum
    RegData$Icd <- gsub(',', '', RegData$Icd)
    RegData$Icd <- gsub('\\.', '', RegData$Icd)
    RegData$GTI_diagkode <- NA
    RegData$GTI_diagkode <- mapDiagKode$gtiKode[match(substr(RegData$Navn, 1, 10), substr(mapDiagKode$navn, 1, 10))]
    # RegData$Diag_gr <- NA
    # RegData$Diag_gr <- mapDiagKode$gruppering[match(substr(RegData$Navn, 1, 10), substr(mapDiagKode$navn, 1, 10))]
    RegData$Diag_gr_nr <- NA
    RegData$Diag_gr_nr <- mapDiagKode$gr_nr[match(substr(RegData$Navn, 1, 10), substr(mapDiagKode$navn, 1, 10))]
    RegData$Diag_gr <- factor(RegData$Diag_gr_nr, levels = 1:3, labels = c('Storkarsvaskulitt (LVV)',
                                                                           'ANCA assosiert vaskulitt (AAV)', 'Andre'))
    RegData$Navn[which(RegData$GTI_diagkode==7)] <- as.character(mapDiagKode$navn[match(7, mapDiagKode$gtiKode)])
    RegData$tid_symp_diagnose <- difftime(RegData$Diagnose_Klinisk_Dato, RegData$SymptomStartDato, units = 'days')
  }

  # handelsnavn <- c("Arava", "CellCept", "Cimzia", "Enbrel", "Everolimus", "Folsyre",
  #                  "HumantImmunoglobulinGiv", "HumantImmunoglobulinGsc", "Imurel", "Inflektra",
  #                  "Lodotra", "MabThera", "MethotrexateImSc", "Prednisolon", "Remicade", "Remsima",
  #                  "RoActemraInfusjon", "Sandimmun", "SendoxanIv", "Tacrolimus", "AnnetImportert", "Benepali",
  #                  "Humira", "Kineret", "MetylprednisolonPo", "Plaquenil", "Salazopyrin", "Simponi", "Stelara", "Talidomid",
  #                  "Rixathon", "Mycofenolsyre", "Sekukinumab")
  # kode <- c(15, 16, 1, 2, 31, 17, 36, 37, 18, 5, 19, 7, 20, 23, 10, 11, 12, 25, 26, 34, 999, 29, 3, 6, 38, 22, 24, 14, 28, 35, 1001, 1002, 1003)
  # kobl_hnavn_kode <- data.frame(kode, handelsnavn)
  # generisknavn <- c("Cetolizumab", "Etanercept", "Etanercept", "Adalimumab", "Infliximab", "Infliximab",
  #                   "Infliximab", "Golimumab", "Anakinra", "Canakinumab", "Tocilizumab", "Tocilizumab",
  #                   "Ustekinumab", "Sekunikumab", "Rituximab", "Abatacept", "Abatacept", "Leflunomid",
  #                   "Mycofenolatmofetil", "Mycofenolsyre", "Azatioprin", "Methotrexat", "Methotrexat",
  #                   "Hydroxychlochin", "Sulfasalazin", "Everolimus", "Sirolimus", "Tallidomid", "Ciclosporin A",
  #                   "Tacrolimus", "Prednisolon/Prednison", "Prednisolon/Prednison", "Methylprednisolon", "Methylprednisolon",
  #                   "Humant immunglobulin", "Humant immunglobulin", "Cyclofosfamid", "Cyclofosfamid", "Folsyre", "Annet",
  #                   "Rituximab", "Mycofenolsyre", "Sekukinumab") ## SPØR WENCHE!!!!
  # kode2 <- c(1, 2, 29, 3, 5, 10, 11, 14, 6, 4, 12, 13, 28, 30, 7, 8, 9, 15, 16, 32,
  #            18, 20, 21, 22, 24, 31, 33, 35, 25, 34, 19, 23, 38, 39, 36, 37, 26, 27, 17, 999, 1001, 1002, 1003)
  # kobl_gennavn_kode <- data.frame(kode2, generisknavn)
  #
  #
  # Legemiddelgruppe <- c("Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
  #                       "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
  #                       "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
  #                       "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
  #                       "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
  #                       "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
  #                       "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
  #                       "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
  #                       "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD",
  #                       "DMARD", "DMARD", "DMARD", "Kortikosteroider", "Kortikosteroider", "Kortikosteroider",
  #                       "Kortikosteroider", "Cyclofosfamid", "Cyclofosfamid", "Rituximab", "Immunglob.G", "Immunglob.G",
  #                       "Annet", "Annet", "Rituximab", "DMARD", "Biologiske legemidler \n (Rituximab ekskludert)")
  #
  # kode3 <- c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 28, 29, 30, 15, 16, 18, 20, 21, 22, 24, 25, 31, 32, 33, 34,
  #            35, 38, 39, 19, 23, 26, 27, 7, 36, 37, 17, 999, 1001, 1002, 1003)

  kode <- c(1, 2, 3, 5, 6, 8, 12, 14, 15, 16, 18, 19, 20, 22, 23, 24, 25, 26, 28, 30,
            31, 32, 34, 35, 36, 38, 39, 40, 42, 43, 999)
  Legemiddelgruppe <- c("Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "DMARD", "DMARD", "DMARD", "Kortikosteroider", "DMARD",
                        "DMARD", "Kortikosteroider", "DMARD", "DMARD", "cyclofosfamid",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "DMARD", "DMARD", "DMARD", "DMARD", "Immunglob.G", "Kortikosteroider",
                        "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Rituximab", "Biologiske legemidler \n (Rituximab ekskludert)", "DMARD", "Annet")
  kobl_gruppe_kode <- data.frame(kode, Legemiddelgruppe)

  if ('LegemiddelType' %in% names(RegData)){
    RegData <- RegData[RegData$LegemiddelType != 17, ]
    varnavn <- kodebok_norvas[which(!is.na(kodebok_norvas$Variabelnavn)), c("Variabelnavn", "skjema")]
    indekser_kodebok <- which(kodebok_norvas$Variabelnavn == 'LegemiddelType' & kodebok_norvas$skjema == 'Medisinering'):
      (which(kodebok_norvas$Variabelnavn == varnavn$Variabelnavn[which(varnavn$Variabelnavn=='LegemiddelType' & varnavn$skjema == 'Medisinering')+1])-1)
    kobl_generisknavn_kode <- data.frame(kode=as.numeric(kodebok_norvas$kode[c(indekser_kodebok[-1], indekser_kodebok[1])]),
                                         label=kodebok_norvas$label[c(indekser_kodebok[-1], indekser_kodebok[1])])

    RegData$LegemiddelKode <- RegData$LegemiddelType
    RegData$LegemiddelKode[RegData$LegemiddelKode==7] <- 40
    RegData$LegemiddelKode[RegData$LegemiddelKode %in% c(10,11)] <- 5
    RegData$LegemiddelKode[RegData$LegemiddelKode==29] <- 2
    RegData$LegemiddelKode[RegData$LegemiddelKode==37] <- 36

    RegData$Legemiddelgruppe <- kobl_gruppe_kode$Legemiddelgruppe[match(RegData$LegemiddelKode, kobl_gruppe_kode$kode)]
    RegData$LegemiddelGenerisk <-  kobl_generisknavn_kode$label[match(RegData$LegemiddelKode, kobl_generisknavn_kode$kode)]
    RegData$LegemiddelTypeLabel <- factor(RegData$LegemiddelKode, levels = kodebok_norvas$kode[c(indekser_kodebok[-1], indekser_kodebok[1])],
                                          labels = kodebok_norvas$label[c(indekser_kodebok[-1], indekser_kodebok[1])])
  }

  if ('BvasPersistentTotal' %in% names(RegData)){
    RegData$bvas_samlet <- RegData$BvasPersistentTotal
    RegData$bvas_samlet[RegData$bvas_samlet==0] <- RegData$BvasnewOrWorseTotal[RegData$bvas_samlet==0]

    tmp <- table(RegData[, c("PasientGUID", "BVAS_Dato")])
    tmp <- as.data.frame(tmp)
    tmp <- tmp[tmp$Freq>1, ]
    tmp2 <-  merge(RegData[, c("PasientGUID", "BVAS_Dato", "SkjemaGUID")],
                   tmp[, c("PasientGUID", "BVAS_Dato")], by = c('PasientGUID', 'BVAS_Dato'))
    tmp2 %>% group_by(PasientGUID, BVAS_Dato) %>% summarise(SkjemaGUID = SkjemaGUID[1])

    RegData <- RegData[!(RegData$SkjemaGUID %in% tmp2$SkjemaGUID), ]

  }

  if ('AntallInfeksjoner' %in% names(RegData)){
    kobl_num_kat <- data.frame(tall=0:4, kode=c('Ingen', 'En', 'To', 'Tre', 'FireEllerFler'))
    RegData$AntallInfeksjoner_num <- kobl_num_kat$tall[match(RegData$AntallInfeksjoner, kobl_num_kat$kode)]
  }

  return(invisible(RegData))

}
