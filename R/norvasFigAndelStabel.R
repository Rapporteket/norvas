#' Lag søylediagram eller stabelplott som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram eller stabelplot som viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @param RegData En dataramme med alle nodvendige variabler fra registeret
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

norvasFigFordelingGr  <- function(RegData=0, valgtVar='ErMann', datoFra='2014-01-01', datoTil='2050-12-31',
                              minald=0, maxald=130, erMann=99, outfile='', reshID, enhetsUtvalg=0, preprosess=F,
                              valgtShus=c(''),hentData=F, datovar='InklusjonDato', aldervar='PatientAge', diag_gruppe=99)
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

  #-------------------------Beregninger-----------------------------------------

  if (PlotParams$flerevar == 0) {
    grtxt <- levels(RegData$Gr)
    stabeltxt <- levels(RegData$VariabelGr)
    NVarGr <- ftable(RegData[ , c('VariabelGr','Gr')])	#ftable(list(RegData$Var, RegData$Gr))
    NGr <- colSums(NVarGr)
    AndelVar <- prop.table(NVarGr,2)*100
  }

  if (PlotParams$flerevar == 1) {
    grtxt <- levels(RegData$Gr)
    tmp <- tidyr::gather(PlotParams$AntVar, VariabelGr, Antall, -Group.1)
    AndelVar <- tidyr::spread(tmp, Group.1, Antall)
    stabeltxt <- AndelVar$VariabelGr
    AndelVar <- AndelVar[,-1]/t(matrix(PlotParams$NVar, nrow = length(grtxt), ncol = length(stabeltxt)))*100
    NGr <- PlotParams$NVar
  }


  ##-----------Figur---------------------------------------
  # tittel <- PlotParams$tittel; grtxt <- PlotParams$grtxt;
  retn <- PlotParams$retn; #cexgr <- PlotParams$cexgr;



  # FigFordelingGr(AggVerdier=Andeler, tittel=PlotParams$tittel, hovedgrTxt='Hele landet', smltxt='',
  #              N=list(Hoved=NHoved, Rest=Nrest), retn='H', subtxt='', utvalgTxt=utvalgTxt, grtxt=grtxt, grtxt2='',
  #              medSml=0, antDes=1, fargepalett='BlaaOff', outfile=outfile)

  tittel <- PlotParams$tittel; stabel <- PlotParams$stabel;
  subtxt <- PlotParams$subtxt; cexgr <- PlotParams$cexgr;
  FigTypUt <- rapFigurer::figtype(outfile=outfile, fargepalett=norvasUtvalg$fargepalett, pointsizePDF=12)

  farger <- FigTypUt$farger
  NutvTxt <- length(utvalgTxt)
  par('fig'=c(0, 1, 0, 1-0.02*(NutvTxt-1)))  #Har alltid datoutvalg med

  if (length(stabeltxt) == 2 & !stabel){
    ymax <- min(1.1*max(AndelVar[1,]), 100)
    ylabel <- "Andel pasienter"
    pos <- barplot(AndelVar[1,], beside=TRUE, las=1, ylab=ylabel,  #main=tittel,
                   sub=subtxt, cex.axis=cexgr, cex.sub=cexgr,	cex.lab=cexgr, #names.arg=grtxt, cex.names=cexgr,
                   col=farger[1], border='white', ylim=c(0, ymax))	#farger[c(1,3)]
    mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
    text(pos, AndelVar[1,]+1, paste('N=',NGr,sep=''), cex=0.75)
  } else {
    if (stabel==1){
      koord <- barplot(AndelVar, beside=F, las=1, #names.arg=grtxt, cex.names=0.95,
                       col=farger, ylab="Andel (%)", ylim=c(0,132),	 #xlim=c(0, length(grtxt)*1.2),
                       cex.main=1, font.main=1, axes=F, cex.axis=.9, cex.lab=.95, space=.25, border=NA) #
      axis(side=2, at=c(0,20,40,60,80,100))
      legend('top', legend=rev(stabeltxt), bty='n', cex=.8, 	#max(koord+0.5)*1.35, y=80,
             xjust=0.5, fill=farger[length(stabeltxt):1], border=farger[length(stabeltxt):1], ncol=1)
      mtext(at=koord, cex=0.9, side=1, line=0, adj=0.5, grtxt)	#
      text(koord, 102.7, paste('N=',NGr,sep=''), cex=0.75)
    } else {
      koord <- barplot(as.matrix(AndelVar), beside=T, las=2, #names.arg=grtxt, cex.names=0.95,
                       col=farger[1:length(stabeltxt)], ylab="Andel (%)", ylim=c(0,max(AndelVar)*1.2),	 #xlim=c(0, length(grtxt)*1.2),
                       cex.main=1, font.main=1, axes=F, cex.axis=.9, cex.lab=.95, border=NA, xaxt='n', ann=FALSE) #
      axis(side=2, at=c(0,20,40,60,80,100))
      legend('top', legend=rev(stabeltxt), bty='n', cex=.8, 	#max(koord+0.5)*1.35, y=80,
             xjust=0.5, fill=farger[length(stabeltxt):1], border=farger[length(stabeltxt):1], ncol=1)
      mtext(at=colMeans(koord), grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)
      mtext(at=colMeans(koord), paste('N=',NGr,sep=''), side=1, las=1, cex=cexgr, adj=0.5, line=1.5)
    }
  }

  krymp <- .9
  title(main = tittel, line=1, font.main=1, cex.main=1.3*cexgr)
  mtext(utvalgTxt, side=3, las=1, cex=krymp*cexgr, adj=0, col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))

  par('fig'=c(0, 1, 0, 1))

  if ( outfile != '') {dev.off()}

}

