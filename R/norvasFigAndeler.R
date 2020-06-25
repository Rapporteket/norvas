#' Lag søylediagram eller stabelplott som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram eller stabelplot som viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @param RegData En dataramme med alle nødvendige variabler fra registeret
#' @param valgtVar Hvilken variabel skal plottes
#' @param datoFra Tidligste dato i utvalget (vises alltid i figuren).
#' @param datoTil Seneste dato i utvalget (vises alltid i figuren).
#' @param minald Alder, fra og med (Default: 0)
#' @param maxald Alder, til og med (Default: 130)
#' @param erMann kjønn
#'                 1: menn
#'                 0: kvinner
#'                 99: begge (alt annet enn 0 og 1) (Default)
#' @param outfile Navn på fil figuren skrives til. Default: '' (Figur skrives
#'    til systemets default output device (som regel skjerm))
#' @param reshID Parameter følger fra innlogging helseregister.no og angir
#'    hvilken enhet i spesialisthelsetjenesten brukeren tilhører
#' @param enhetsUtvalg Lag figur for
#'                 0: Hele landet
#'                 1: Egen enhet mot resten av landet (Default)
#'                 2: Egen enhet
#' @param valgtShus Vektor med UnitId over hvilke sykehus man genererer rapporten for.
#'                  Denne overstyrer reshID og er bare tilgjengelig for SC-bruker.
#'
#' @return En figur med søylediagram eller et stabelplot av ønsket variabel
#'
#' @export
#'

norvasFigAndeler  <- function(RegData=0, valgtVar='Inklusjonsalder', datoFra='2014-01-01', datoTil='2050-12-31',
                              minald=0, maxald=130, erMann=99, outfile='', reshID, enhetsUtvalg=0, preprosess=F,
                              valgtShus=c(''),hentData=F, datovar='InklusjonDato', aldervar='PatientAge',
                              diag_gruppe=99, inkl_N=F)
{

  ## Hvis spørring skjer fra R på server. ######################
  if(hentData){
    RegData <- norvasHentRegData()
  }

  ## Hvis RegData ikke har blitt preprosessert
  if (preprosess){
    RegData <- norvasPreprosess(RegData=RegData)
  }


  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  norvasUtvalg <- norvasUtvalg(RegData=RegData, datoFra=datoFra, datoTil=datoTil, datovar=datovar, minald=minald,
                               maxald=maxald, erMann=erMann, valgtShus=valgtShus, aldervar=aldervar,
                               diag_gruppe=diag_gruppe)
  RegData <- norvasUtvalg$RegData
  utvalgTxt <- norvasUtvalg$utvalgTxt


  if (valgtShus[1]!='') {
    valgtShus <- as.numeric(valgtShus)
    if (length(valgtShus)==1) {reshID<-valgtShus[1]}
  }

  if (enhetsUtvalg==0) {
    shtxt <- 'Hele landet'
  } else {
    shtxt <- as.character(RegData$Sykehusnavn[match(reshID, RegData$UnitId)])
  }

  if (enhetsUtvalg!=0 & length(valgtShus)>1) {
    RegData$UnitId[RegData$UnitId %in% valgtShus] <- 99
    shtxt <- 'Ditt utvalg'
    reshID <- 99
  }

  #Hvis man ikke skal sammenligne, får man ut resultat for eget sykehus
  if (enhetsUtvalg == 2) {RegData <- RegData[which(RegData$UnitId == reshID), ]}

  ## Preparer variabler for fremstilling i figur
  PlotParams <- norvasPrepvar(RegData=RegData, valgtVar=valgtVar)
  RegData <- PlotParams$RegData
  PlotParams$RegData <- NA

  # Initialiserer nødvendige størrelser
  Andeler <- list(Hoved = 0, Rest =0)
  ind <- list(Hoved=which(RegData$UnitId == reshID), Rest=which(RegData$UnitId != reshID))
  Nrest <- 0

  if (enhetsUtvalg==1) {
    AntHoved <- table(RegData$VariabelGr[ind$Hoved])
    NHoved <- sum(AntHoved)
    Andeler$Hoved <- 100*AntHoved/NHoved
    AntRest <- table(RegData$VariabelGr[ind$Rest])
    Nrest <- sum(AntRest)	#length(indRest)- Kan inneholde NA
    Andeler$Rest <- 100*AntRest/Nrest
  } else {
    AntHoved <- table(RegData$VariabelGr)
    NHoved <- sum(AntHoved)
    Andeler$Hoved <- 100*AntHoved/NHoved
  }


  ##-----------Figur---------------------------------------
  tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt;
  retn <- PlotParams$retn; cexgr <- PlotParams$cexgr;

  # AggVerdier <- list(Hoved=)

  rapFigurer::FigFordeling(AggVerdier=Andeler, tittel=tittel, hovedgrTxt='Hele landet', smltxt='',
                           N=list(Hoved=NHoved, Rest=Nrest), retn=retn, subtxt='', utvalgTxt=utvalgTxt, grtxt=grtxt, grtxt2='',
                           medSml=0, antDes=1, fargepalett='BlaaOff', outfile=outfile, inkl_N=inkl_N)


  # FigTypUt <- figtype(outfile=outfile, fargepalett=norvasUtvalg$fargepalett, pointsizePDF=12)

  # #Hvis for få observasjoner..
  # if (NHoved < 5 | (Nrest<5 & enhetsUtvalg==1)) {
  #   #-----------Figur---------------------------------------
  #   NutvTxt <- length(utvalgTxt)
  #   par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt-1)))  #Har alltid datoutvalg med
  #   plot.new()
  #   text(0.5, 0.6, 'Færre enn 5 registreringer i egen- eller sammenlikningsgruppa', cex=1.2)
  #
  #
  # } else {
  #
  #   #Plottspesifikke parametre:
  #
  #   farger <- FigTypUt$farger
  #   NutvTxt <- length(utvalgTxt)
  #   grtxtpst <- rev(grtxt)
  #   # if (incl_pst) {grtxtpst <- paste(rev(grtxt), ' (', rev(sprintf('%.1f', Andeler$Hoved)), '%)', sep='')}
  #   # if (incl_N) {grtxtpst <- paste(rev(grtxt), ' (n=', rev(sprintf('%.0f', Andeler$Hoved*NHoved/100)), ')', sep='')}
  #   vmarg <- switch(retn, V=0, H=max(0, strwidth(grtxtpst, units='figure', cex=cexgr)*0.8))
  #   par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))  #Har alltid datoutvalg med
  #   grtxt2 <- paste0(sprintf('%.1f',Andeler$Hoved), '%')
  #
  #   fargeHoved <- farger[1]
  #   fargeRest <- farger[3]
  #   antGr <- length(grtxt)
  #   lwdRest <- 3	#tykkelse på linja som repr. landet
  #
  #   if (retn == 'V' ) {
  #     #Vertikale søyler
  #     ymax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
  #     ylabel <- "Andel pasienter"
  #     pos <- barplot(as.numeric(Andeler$Hoved), beside=TRUE, las=1, ylab=ylabel,  #main=tittel,
  #                    cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, #names.arg=grtxt, cex.names=cexgr,sub=subtxt,
  #                    col=fargeHoved, border='white', ylim=c(0, ymax))	#farger[c(1,3)]
  #     mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
  #     mtext(at=pos, grtxt2, side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
  #     if (enhetsUtvalg == 1) {
  #       points(pos, as.numeric(Andeler$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
  #       legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
  #              border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2, lty=c(NA,NA),
  #              lwd=lwdRest, ncol=1, cex=cexgr)
  #     } else {
  #       legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
  #              border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexgr)
  #     }
  #   }
  #
  #
  #   if (retn == 'H') {
  #     #Horisontale søyler
  #     ymax <- antGr*1.4
  #     xmax <- min(max(c(Andeler$Hoved, Andeler$Rest),na.rm=T)*1.25, 100)
  #     xlabel <- "Andel pasienter (%)"
  #
  #     pos <- barplot(rev(as.numeric(Andeler$Hoved)), horiz=TRUE, beside=TRUE, las=1, xlab=xlabel, #main=tittel,
  #                    col=fargeHoved, border='white', font.main=1, xlim=c(0, xmax), ylim=c(0.05,1.4)*antGr)	#
  #     mtext(at=pos+0.05, text=grtxtpst, side=2, las=1, cex=cexgr, adj=1, line=0.25)
  #
  #     if (enhetsUtvalg == 1) {
  #       points(as.numeric(rev(Andeler$Rest)), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
  #       legend('top', c(paste(shtxt, ' (N=', NHoved,')', sep=''), paste('Landet forøvrig (N=', Nrest,')', sep='')),
  #              border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
  #              lwd=lwdRest,	lty=NA, ncol=1, cex=cexgr)
  #     } else {
  #       legend('top', paste(shtxt, ' (N=', NHoved,')', sep=''),
  #              border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexgr)
  #     }
  #
  #
  #   }
  #
  #
  # }
  #
  # krymp <- .9
  # title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
  # mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))
  #
  # par('fig'=c(0, 1, 0, 1))
  #
  # if ( outfile != '') {dev.off()}

}

