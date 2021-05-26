#' Fordelingsfigur
#'
#' Denne funksjonen lager et horisontalt eller vertikalt søylediagram som viser andeler (fordeling)
#' av en gitt variabel, basert på de aggregerte verdier som gis inn til funksjonen.

#'
#' @param AggVerdier Aggregerte verdier (andeler) som skal visualiseres. Liste med to vektorer Hoved og Rest
#' @param tittel Figurtittel
#' @param hovedgrTxt hovedgruppe, eks. hele landet eller UNN, Tromsø
#' @param smltxt sammenligningstekst, eks. landet forøvrig
#' @param N Totalt antall. Listevariabel: N$Hoved/Rest - antall i hovedgruppa/sammenligningsgruppa
#' @param retn V-vertikale søyler. H-horisontale søyler
#' @param utvalgTxt Tekst over flere linjer som angir hvilket utvalg som er gjort
#' @param grtxt Tekst som angir navn på hver av gruppene i fordelinga.Vektor med lenge lik antall grupper/søyler
#' @param grtxt2 Evt. undertekst til hver av gruppene. Vektor med lenge lik antall grupper/søyler
#' @param medSml 0-med sammenlikningsgruppe, 1-uten sammenlikningsgruppe
#' @param fargepalett hvilken fargepalett fra "rapfig::figfiltype" som skal benyttes
#' @param antDes antall desimaler i visning av andel som prosent
#' @param outfile hvordan figuren skal vises: ''-på Skjerm, "filnavn.pdf/png/.." - gir fil av angitt type.
#'
#' @return Søylediagram (fordeling).
#' @export
#'
#' @examples Eksempel kommer
generellFigFordeling <- function(AggVerdier, tittel='mangler tittel', hovedgrTxt='', smltxt='',
                         N, retn='H', subtxt='', utvalgTxt='', grtxt, grtxt2='',
                         medSml=0, antDes=1, fargepalett='BlaaOff', outfile='', inkl_N=F
                         #medKI=0, KImaal = NA, KImaaltxt = '', Ngr, cexgr=1, grTypeTxt='', pstTxt=list(Hoved='', Rest=''),
) {


  #Hvis for få observasjoner..

  if ((N$Hoved < 5) | (sum(N$Hoved+N$Rest)<11)){
    #-----------Figur---------------------------------------
    FigTypUt <- rapFigurer::figtype(outfile)  #FigTypUt <- figtype(outfile)
    farger <- FigTypUt$farger
    plot.new()
    title(tittel)	#, line=-6)
    legend('topleft',legend=utvalgTxt, bty='n', cex=0.9, text.col=farger[1])
    tekst <- 'For få registreringer i egen eller sammenligningsgruppe'
    text(0.5, 0.6, tekst, cex=1.2)
    if ( outfile != '') {dev.off()}

  } else {


    #Plottspesifikke parametre:
    cexgr <- 1-ifelse(length(grtxt)>20, 0.25*length(grtxt)/60, 0)

    # funksjon for tekstbryting
    # wrap.it <- function(x, len)
    # {
    #   sapply(x, function(y) paste(strwrap(y, len),
    #                               collapse = "\n"),
    #          USE.NAMES = FALSE)
    # }

    grtxt <- muskel::wrap.it(grtxt, 28)

    #Høyde må avhenge av antall grupper
    hoyde <- ifelse(length(AggVerdier$Hoved)>20, 3*800, 3*600)
    FigTypUt <- rapFigurer::figtype(outfile, height=hoyde, fargepalett=fargepalett)
    #Tilpasse marger for å kunne skrive utvalgsteksten
    NutvTxt <- length(utvalgTxt)
    vmarg <- switch(retn, V=0.05, H=min(1,max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75)))
    #NB: strwidth oppfører seg ulikt avh. av device...
    par('fig'=c(vmarg, 1, 0, 1-0.02*(NutvTxt-1)))	#Har alltid datoutvalg med

    farger <- FigTypUt$farger
    fargeHoved <- farger[1]
    fargeRest <- farger[3]
    graa <- c('#4D4D4D','#737373','#A6A6A6','#DADADA')  #Mørk til lys          																# Fire graatoner
    #antGr <- length(grtxt)
    cexleg <- 0.9	#Størrelse på legendtekst
    pstTxt <- list('Hoved' = paste0(sprintf(paste0('%.', antDes, 'f'), AggVerdier$Hoved), '%'),
                   'Rest' = paste0(sprintf(paste0('%.', antDes, 'f'), AggVerdier$Rest), '%'))

    #Horisontale søyler
    if (retn == 'H') {
      #Definerer disse i beregningsfunksjonen?
      xmax <- max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.2
      xmax <- min(xmax, 100)
      ymin <- 0.3 #0.5/cexgr^4	#0.05*antGr #Fordi avstand til x-aksen av en eller annen grunn øker når antall sykehus øker
      ymax <- 0.4+1.25*length(AggVerdier$Hoved) #c(0.3/xkr^4,  0.3+1.25*length(Midt)), 0.2+1.2*length(AggVerdier$Hoved)

      pos <- rev(barplot(rev(as.numeric(AggVerdier$Hoved)), xlim=c(0,xmax), ylim=c(ymin, ymax), #, plot=FALSE)
                         xlab="Andel pasienter (%)", horiz=T, border=NA, col=fargeHoved)) #, col.axis='white', col='white'))
      indOK <- which(AggVerdier$Hoved>=0)
      posOK <- pos[indOK]
      posOver <- max(pos)+0.35*log(max(pos))
      posDiff <- 1.2*(pos[1]-pos[2])
      posOK <- pos[indOK]
      #minpos <- min(posOK)-0.7
      #maxpos <- max(posOK)+0.7
      pstHS <- pstTxt$Hoved #Angi prosentandeler på høyre side
      txtOver <- ifelse(pstHS=='', '', ifelse(medSml==0, '', 'Egen/Resten'))

      if (inkl_N) {
        text(x=AggVerdier$Hoved, y=pos, labels = round(AggVerdier$Hoved*N$Hoved/100), adj = 0, cex=cexgr*0.7, col=graa[2])
      }

      if (medSml == 1) {
        pstHS <- paste0(pstTxt$Hoved,'  ', pstTxt$Rest)
        legend(xmax/4, posOver+0.6*posDiff,
               c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18),
               pt.cex=2, ncol=1) #lwd=lwdRest, lty=NA,
      } else {
        legend(xmax/4, posOver+0.6*posDiff, paste0(hovedgrTxt, ' (N=', N$Hoved,')'),
               border=NA, fill=fargeHoved, bty='n', ncol=1)
      }

      #Legge på gruppe/søylenavn og evt. prosentangivelser
      #grtxt <- paste0(grtxt, ifelse(length(grtxt) < 11, ' \n(', ' (' ), pstTxt, ')') #med andel
      mtext(at=pos+0.00, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25)
      mtext(at=pos+0.05, text=pstHS, side=4, las=1, cex=0.8*cexgr, adj=0.5, line=-1, col=graa[2])
      mtext(at=posOver, text=txtOver, side=4, las=1, cex = 0.8*cexgr, adj=0.2, line= -2, col = graa[2])

      if (medSml == 1) { #Legge på prikker for sammenlikning
        points(as.numeric(AggVerdier$Rest), pos, col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
      }
    }		#Slutt horisontale søyler



    if (retn == 'V' ) {
      #Vertikale søyler. Det er bare andeler som har vertikale søyler.
      ymax <- min(max(c(AggVerdier$Hoved, AggVerdier$Rest),na.rm=T)*1.25, 115)
      pos <- barplot(as.numeric(AggVerdier$Hoved), beside=TRUE, las=1, ylab="Andel pasienter (%)",
                     sub=subtxt,	col=fargeHoved, border='white', ylim=c(0, ymax))
      mtext(at=pos, grtxt, side=1, las=1, cex=cexgr, adj=0.5, line=0.5)

      mtext(at=pos, paste0(sprintf(paste0('%.', antDes, 'f'), AggVerdier$Hoved), '%'),
            side=1, las=1, cex=0.8*cexgr, adj=0.5, line=1.5, col=graa[2])
      mtext(at=0,  paste0(hovedgrTxt,': '), side=1, cex=0.8*cexgr, adj=0.9, line=1.5, col=graa[2])

      if (medSml == 1) {
        mtext(at=pos, paste0(sprintf(paste0('%.', antDes, 'f'), AggVerdier$Rest), '%'),
              side=1, las=1, cex=0.8*cexgr, adj=0.5, line=2.5, col=graa[2])
        mtext(at=0,  paste0(smltxt,': '), side=1, cex=0.8*cexgr, adj=0.9, line=2.5, col=graa[2])
        points(pos, as.numeric(AggVerdier$Rest), col=fargeRest,  cex=2, pch=18) #c("p","b","o"),
        legend('top', legend=c(paste0(hovedgrTxt, ' (N=', N$Hoved,')'), paste0(smltxt, ' (N=', N$Rest,')')),
               border=c(fargeHoved,NA), col=c(fargeHoved,fargeRest), bty='n', pch=c(15,18), pt.cex=2,
               ncol=2, cex=cexleg) #, lwd=lwdRest, lty=c(NA,NA),
      } else {
        legend('top', legend=paste0(hovedgrTxt, ' (N=', N$Hoved,')'),
               border=NA, fill=fargeHoved, bty='n', ncol=1, cex=cexleg)
      }
    }

    title(tittel, line=1.5) #cex.main=1.3)

    #Tekst som angir hvilket utvalg som er gjort
    mtext(utvalgTxt, side=3, las=1, cex=0.9, adj=0, col=farger[1], line=c(3+0.8*((NutvTxt-1):0)))

    par('fig'=c(0, 1, 0, 1))
    if ( outfile != '') {dev.off()}
  } #Nok observasjoner
}
