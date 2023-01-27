#' Klargjør variabler for fremstilling i figur eller tabell
#'
#' @param valgtVar Variabelen som skal fremstilles
#'
#' @return PrepData En liste med plotrelevante størrelser
#'
#' @export
#'
norvasPrepvar <- function(RegData, valgtVar) {

  retn= 'V'; tittel <- ''; cexgr <- 1.0; grtxt <- ''; flerevar <- 0; stabel <- T;

  if (valgtVar=='ErMann') {
    tittel <- 'Kjønn'
    RegData$Variabel <- RegData$ErMann
    grtxt <- c('Kvinne', 'Mann')
    RegData <- RegData[which(RegData$Variabel %in% c(0,1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0,1), labels = grtxt)
    RegData$Gr <- RegData$Diag_gr
    # if (enhetsUtvalg==1) {stabel=T}
  }

  if (valgtVar=='Andel_LVV') {
    tittel <- 'Andel storkarsvaskulitt'
    grtxt <- c('Nei', 'Ja')
    RegData$Variabel <- 0
    RegData$Variabel[RegData$Diag_gr_nr==1] <- 1
    RegData <- RegData[which(RegData$Variabel %in% c(0,1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0,1), labels = grtxt)
  }

  if (valgtVar=='Andel_ANCA') {
    tittel <- 'Andel ANCA-assosiert vaskulitt'
    grtxt <- c('Nei', 'Ja')
    RegData$Variabel <- 0
    RegData$Variabel[RegData$Diag_gr_nr==2] <- 1
    RegData <- RegData[which(RegData$Variabel %in% c(0,1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0,1), labels = grtxt)
  }

  if (valgtVar=='Andel_remisjon') {
    tittel <- 'Andel i remisjon (av andel registreringer)'
    grtxt <- c('Nei', 'Ja')
    RegData$Variabel <- 0
    RegData$Variabel[RegData$Sykdomsvurdering=='Remisjon'] <- 1
    RegData <- RegData[which(RegData$Variabel %in% c(0,1)), ]
    RegData$VariabelGr <- factor(RegData$Variabel, levels=c(0,1), labels = grtxt)
  }






  if (valgtVar == 'Inklusjonsalder') {
    tittel <- 'Aldersfordeling ved inklusjon'
    RegData$Variabel <- RegData[, valgtVar]
    gr <- c(seq(15,75,by=10), 140)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks = gr, include.lowest = T)
    grtxt <- c('15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '>75')
    retn <- 'V'
  }

  if (valgtVar == 'DiagnoseAlder') {
    tittel <- 'Aldersfordeling ved diagnose'
    RegData$Variabel <- RegData[, valgtVar]
    gr <- c(seq(15,75,by=10), 140)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks = gr, include.lowest = T)
    grtxt <- c('15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '>75')
    retn <- 'V'
  }

  if (valgtVar == 'tid_symp_diagnose') {
    tittel <- 'Tid fra symptom til diagnose'
    RegData$Variabel <- as.numeric(RegData[, valgtVar])
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[which(RegData$Variabel>=0), ]
    gr <- c(0,30, 90, 180, 360, 100000)
    RegData$VariabelGr <- cut(RegData$Variabel, breaks = gr, include.lowest = T)
    grtxt <- levels(RegData$VariabelGr)
      # c('15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '>75')
    retn <- 'V'
  }

  if (valgtVar == 'tid_til_remisjon') { ## Vurder å fjerne de med tid fra diagnose til remisjon lik 0
    tittel <- c('Tid fra diagnose til remisjon', '(pasienter inkludert ved diagnosetidspunkt)')
    RegData$tid_diag_bvas <- difftime(RegData$BVAS_Dato, RegData$Diagnose_Klinisk_Dato, units = 'days')
    RegData <- RegData[which(RegData$tid_diag_bvas >= 0), ]  # fjerner registreringer
    RegData <- RegData[order(RegData$BVAS_Dato, decreasing = F), ]
    tmp <- RegData[which(abs(difftime(RegData$Diagnose_Klinisk_Dato, RegData$InklusjonDato, units = 'days')) <= 10), ]
    BVAS_utvalg <- RegData[which(RegData$PasientGUID %in% unique(tmp$PasientGUID)), ]
    remisjon <- BVAS_utvalg[which(BVAS_utvalg$Sykdomsvurdering == "Remisjon" | BVAS_utvalg$bvas_samlet == 0), ]
    remisjon <- remisjon[match(unique(remisjon$PasientGUID), remisjon$PasientGUID), ]
    RegData <- remisjon
    gr <- c(0,30, 90, 180, 360, 100000)
    RegData$VariabelGr <- cut(as.numeric(RegData$tid_diag_bvas), breaks = gr, include.lowest = T)
    grtxt <- levels(RegData$VariabelGr)
    # c('15-24', '25-34', '35-44', '45-54', '55-64', '65-74', '>75')
    retn <- 'V'
  }

  if (valgtVar == 'Navn') {
    tittel <- 'Diagnosefordeling'
    RegData$Variabel <- RegData[, valgtVar]
    grtxt <- names(sort(table(RegData$Variabel), decreasing = T))

    RegData$VariabelGr <- factor(RegData$Variabel, levels=grtxt)  # Sorter fallende
    RegData <- RegData[!is.na(RegData$VariabelGr), ]
    # grtxt <- levels(RegData$VariabelGr)
    retn <- 'H'
  }

  if (valgtVar == 'Diag_gr') {
    tittel <- 'Fordeling av diagnosegrupper'
    RegData$VariabelGr <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$VariabelGr), ]
    RegData$Gr <- factor(RegData$ErMann, levels = 0:1, labels = c('Kvinne', 'Mann'))
    grtxt <- levels(RegData$VariabelGr)
    retn <- 'V'
    stabel <- F
  }


  if (valgtVar == 'ErMann') {
    tittel <- 'Kjønnsfordeling'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    grtxt <- c('Kvinne', 'Mann')
    RegData$VariabelGr <- factor(RegData$Variabel, levels = 0:1, labels = grtxt)
    # retn <- 'H'
  }

  if (valgtVar == 'bvas_samlet') {
    tittel <- 'BVAS'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    gr <- c(0, seq(1,31, 5), 100)

    RegData$VariabelGr <- cut(RegData$Variabel, breaks = gr, include.lowest = T, right = F)
    grtxt <- levels(RegData$VariabelGr)
    grtxt[length(grtxt)] <- '31+'
    # retn <- 'H'
  }

  if (valgtVar == 'bvas_samlet_nyeste') {
    tittel <- 'BVAS'
    RegData$Variabel <- RegData[, 'bvas_samlet']
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$BVAS_Dato, decreasing = T), ]
    RegData <- RegData[match(unique(RegData$PasientGUID), RegData$PasientGUID), ]
    gr <- c(0, seq(1,31, 5), 100)

    RegData$VariabelGr <- cut(RegData$Variabel, breaks = gr, include.lowest = T, right = F)
    grtxt <- levels(RegData$VariabelGr)
    grtxt[length(grtxt)] <- '31+'
    # retn <- 'H'
  }


  if (valgtVar == 'KerrsKriterierScore') {
    tittel <- 'Kerrs kriterier score'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$VariabelGr <- as.factor(RegData$Variabel)
    grtxt <- levels(RegData$VariabelGr)
  }

  if (valgtVar == 'KerrsKriterierScore_nyeste') {
    tittel <- 'Kerrs kriterier score'
    RegData$Variabel <- RegData[, 'KerrsKriterierScore']
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$KerrsKriterier_Dato, decreasing = T), ]
    RegData <- RegData[match(unique(RegData$PasientGUID), RegData$PasientGUID), ]

    RegData$VariabelGr <- as.factor(RegData$Variabel)
    grtxt <- levels(RegData$VariabelGr)
  }

  if (valgtVar == 'Legemiddelgruppe') {
    tittel <- 'Legemiddelgruppe'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$VariabelGr <- as.factor(RegData$Variabel)   # Sortering: Kortiko, dmard, rituximab, Biologiske, cytostatika, immun, annet
    grtxt <- levels(RegData$VariabelGr)
    retn <- 'H'
  }

  if (valgtVar == 'LegemiddelGenerisk') {
    tittel <- 'Legemiddel'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$VariabelGr <- as.factor(RegData$Variabel)   # Sortering: Synkende
    grtxt <- levels(RegData$VariabelGr)
    retn <- 'H'
  }


  if (valgtVar == 'VdiTotal') {
    tittel <- 'Vdi-skår'
    RegData$Variabel <- RegData[, valgtVar]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[order(RegData$VDI_Dato, decreasing = T), ]
    RegData <- RegData[match(unique(RegData$PasientGUID), RegData$PasientGUID), ]

    RegData$Variabel[RegData$Variabel>=10] <- 10
    RegData$VariabelGr <- as.factor(RegData$Variabel)
    grtxt <- levels(RegData$VariabelGr)
    grtxt[grtxt=='10'] <- '10+'
    retn <- 'H'
  }


  if (valgtVar == 'Sykdomsvurdering') {
    tittel <- 'Sykdomsvurdering'
    RegData$Variabel <- RegData[, "SykdomsvurderingLabel"]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$VariabelGr <- RegData$Variabel
    # RegData$VariabelGr <- as.factor(RegData$Variabel) # debut, alv. residiv, lett res., persisterende, remisjon
    # RegData$VariabelGr <- factor(RegData$Sykdomsvurdering, levels = c('Debut', 'Remisjon', 'PersisterendeSykdom', 'LettResidiv', 'AlvorligResidiv'),
    #        labels = c('Debut', 'Remisjon', 'Persisterende sykdom', 'Lett residiv', 'Alvorlig residiv'))
    grtxt <- levels(RegData$VariabelGr)
    retn <- 'H'
  }

  if (valgtVar == 'Sykdomsvurdering_nyeste') {
    tittel <- 'Sykdomsvurdering'
    RegData$Variabel <- RegData[, 'SykdomsvurderingLabel']
    RegData <- RegData[order(RegData$BVAS_Dato, decreasing = T), ]
    RegData <- RegData[match(unique(RegData$PasientGUID), RegData$PasientGUID), ]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$VariabelGr <- RegData$Variabel # debut, alv. residiv, lett res., persisterende, remisjon
    grtxt <- levels(RegData$VariabelGr)
    retn <- 'H'
  }

  if (valgtVar == 'Sykdomsvurdering_kerr') {
    tittel <- 'Sykdomsvurdering'
    RegData$Variabel <- RegData[, "SykdomsvurderingLabel"]
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$VariabelGr <- RegData$Variabel
    # RegData$VariabelGr <- as.factor(RegData$Variabel) # debut, alv. residiv, lett res., persisterende, remisjon
    # RegData$VariabelGr <- factor(RegData$Sykdomsvurdering, levels = c('Debut', 'Remisjon', 'PersisterendeSykdom', 'LettResidiv', 'AlvorligResidiv'),
    #        labels = c('Debut', 'Remisjon', 'Persisterende sykdom', 'Lett residiv', 'Alvorlig residiv'))
    grtxt <- levels(RegData$VariabelGr)
    retn <- 'H'
  }


  if (valgtVar == 'AntallInfeksjoner') {
    tittel <- 'Antall infeksjoner pr. registrering'
    RegData$Variabel <- RegData[, 'AntallInfeksjoner']
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData$VariabelGr <- as.factor(RegData$Variabel) # fiks rekkefølge
    grtxt <- levels(RegData$VariabelGr)
    retn <- 'H'
  }

  if (valgtVar == 'AntallInfeksjoner_kummulativ') {

    RegData$Variabel <- RegData[, 'AntallInfeksjoner']
    RegData <- RegData[!is.na(RegData$Variabel), ]
    RegData <- RegData[, c("PasientGUID", "AntallInfeksjoner", 'UnitId')] %>% group_by(PasientGUID) %>%
      summarise(sum_infeksjon=sum(AntallInfeksjoner), UnitId=UnitId[1])

    RegData$VariabelGr <- as.factor(RegData$sum_infeksjon) # fiks rekkefølge
    grtxt <- levels(RegData$VariabelGr)
    retn <- 'H'
    tittel <- c('Kumulativ antall infeksjoner pr. pasient', paste0('Totalt ', sum(as.numeric(names(table(RegData$sum_infeksjon))) *table(RegData$sum_infeksjon)), ' infeksjoner'))
  }


  PlotParams <- list(RegData=RegData, tittel=tittel, grtxt=grtxt, retn=retn, cexgr=cexgr, flerevar=flerevar, stabel=stabel)

  return(invisible(PlotParams))


}
