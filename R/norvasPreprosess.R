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

  datovars <- kodebok_norvas$Variabelnavn[which(kodebok_norvas$Felttype == 'Dato/tid')]
  datovars <- intersect(datovars, names(RegData))
  RegData[, datovars] <- mutate_all(RegData[, datovars], funs(as.Date(., format="%d.%m.%Y")))

  mapEnhet <- data.frame(UnitId = c(102977, 104579, 105274, 106841, 601159, 700701),
                         Sykehusnavn = c('Helse Bergen', 'St. Olavs', 'Helse Førde', 'Haugesund', 'UNN', 'Nordlandsykehuset'))
  RegData$Sykehusnavn <- mapEnhet$Sykehusnavn[match(RegData$UnitId, mapEnhet$UnitId)]
  names(RegData)[names(RegData)=='UnitId'] <- 'AvdRESH'
  RegData$ErMann <- NA
  RegData$ErMann[RegData$PatientGender=='Male'] <- 1
  RegData$ErMann[RegData$PatientGender=='Female'] <- 0

  mapDiagKode <- data.frame(navn=c("Takayasu Arteritt"
                                    ,"Granulomatøs Polyangitt (Wegeners)"
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
                                    ,"Systemisk Vaskulitt sykdom"), gtiKode = c(3, 7, 8, 4, 98, 13, 9, 15, 6, 11, 14, 5, 10, 99),
                            gruppering=c('Storkarsvaskulitt (LVV)', 'ANCA assosiert vaskulitt (AAV)', 'ANCA assosiert vaskulitt (AAV)',
                                         'Storkarsvaskulitt (LVV)', 'Andre', 'Andre', 'ANCA assosiert vaskulitt (AAV)',
                                         'Storkarsvaskulitt (LVV)', 'Andre', 'Andre', 'Andre', 'Andre', 'Andre', 'Andre'),
                            gr_nr= c(1,2,2,1,3,3,2,1,3,3,3,3,3,3))


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

  handelsnavn <- c("Arava", "CellCept", "Cimzia", "Enbrel", "Everolimus", "Folsyre",
                   "HumantImmunoglobulinGiv", "HumantImmunoglobulinGsc", "Imurel", "Inflektra",
                   "Lodotra", "MabThera", "MethotrexateImSc", "Prednisolon", "Remicade", "Remsima",
                   "RoActemraInfusjon", "Sandimmun", "SendoxanIv", "Tacrolimus", "AnnetImportert", "Benepali",
                   "Humira", "Kineret", "MetylprednisolonPo", "Plaquenil", "Salazopyrin", "Simponi", "Stelara", "Talidomid")
  kode <- c(15, 16, 1, 2, 31, 17, 36, 37, 18, 5, 19, 7, 20, 23, 10, 11, 12, 25, 26, 34, 999, 29, 3, 6, 38, 22, 24, 14, 28, 35)
  kobl_hnavn_kode <- data.frame(kode, handelsnavn)
  generisknavn <- c("Cetolizumab", "Etanercept", "Etanercept", "Adalimumab", "Infliximab", "Infliximab",
                    "Infliximab", "Golimumab", "Anakinra", "Canakinumab", "Tocilizumab", "Tocilizumab",
                    "Ustekinumab", "Sekunikumab", "Rituximab", "Abatacept", "Abatacept", "Leflunomid",
                    "Mycofenolatmofetil", "Mycofenolsyre", "Azatioprin", "Methotrexat", "Methotrexat",
                    "Hydroxychlochin", "Sulfasalazin", "Everolimus", "Sirolimus", "Tallidomid", "Ciclosporin A",
                    "Tacrolimus", "Prednisolon/Prednison", "Prednisolon/Prednison", "Methylprednisolon", "Methylprednisolon",
                    "Humant immunglobulin", "Humant immunglobulin", "Cyclofosfamid", "Cyclofosfamid", "Folsyre", "Annet") ## SPØR WENCHE!!!!
  kode2 <- c(1, 2, 29, 3, 5, 10, 11, 14, 6, 4, 12, 13, 28, 30, 7, 8, 9, 15, 16, 32,
             18, 20, 21, 22, 24, 31, 33, 35, 25, 34, 19, 23, 38, 39, 36, 37, 26, 27, 17, 999)
  kobl_gennavn_kode <- data.frame(kode2, generisknavn)


  Legemiddelgruppe <- c("Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                        "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD",
                        "DMARD", "DMARD", "DMARD", "Kortikosteroider", "Kortikosteroider", "Kortikosteroider",
                        "Kortikosteroider", "Cytostatika", "Cytostatika", "Rituximab", "Immunglob.G", "Immunglob.G",
                        "Annet", "Annet")

  kode3 <- c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 28, 29, 30, 15, 16, 18, 20, 21, 22, 24, 25, 31, 32, 33, 34,
             35, 38, 39, 19, 23, 26, 27, 7, 36, 37, 17, 999)

  kobl_gruppe_kode <- data.frame(kode3, Legemiddelgruppe)

  if ('LegemiddelType' %in% names(RegData)){
    RegData$LegemiddelKode <- kobl_hnavn_kode$kode[match(RegData$LegemiddelType, kobl_hnavn_kode$handelsnavn)]
    RegData$LegemiddelGenerisk <- kobl_gennavn_kode$generisknavn[match(RegData$LegemiddelKode, kobl_gennavn_kode$kode2)]
    RegData$Legemiddelgruppe <- kobl_gruppe_kode$Legemiddelgruppe[match(RegData$LegemiddelKode, kobl_gruppe_kode$kode3)]
    # Medisiner$Legemiddel[which(is.na(Medisiner$Legemiddelgruppe))]
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
