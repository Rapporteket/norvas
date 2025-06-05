#' Lag gruppert andelsplot
#'
#' Denne funksjonen lager et søylediagram over valgt dikotom variabel
#'
#' @inheritParams norvasFigAndeler
#'
#' @return En figur med søylediagram over valgt variabel
#' @param RegData En dataramme med kolonne Sykehusnavn, Aar, Teller og Nevner
#'
#' @export
#'

norvasFigAndelGruppert <- function(RegData,
                                   var=NA,
                                   grvar=NA,
                                   tittel='',
                                   skriftStr=1.3,
                                   decreasing=F,
                                   outfile = '',
                                   width=800,
                                   height=700,
                                   mrom=0.3,
                                   datoFra="2000-01-01",
                                   datoTil="2050-01-01",
                                   datovar="InklusjonDato",
                                   minald=0,
                                   maxald=130,
                                   aldervar="PatientAge")
# RegData<-Inklusjon
# decreasing=F
# outfile = ''
# height=700
# var="ErKvinne"
# grvar="Diagnose"
# tittel='Andel kvinner'
# skriftStr = 1.1
# outfile = outfile
# width = 1000
# mrom=0.5
# datoFra="2014-01-01"
# datoTil="2050-01-01"
# datovar="InklusjonDato"
# minald=0
# maxald=120
# aldervar="PatientAge"

{
  if (is.na(var) | is.na(grvar)) {break}

  ## Gjør utvalg basert på brukervalg (LibUtvalg)
  norvasUtvalg <- norvasUtvalg(RegData=RegData,
                               datoFra=datoFra,
                               datoTil=datoTil,
                               datovar=datovar,
                               minald=minald,
                               maxald=maxald,
                               aldervar=aldervar)
  RegData <- norvasUtvalg$RegData
  utvalgTxt <- norvasUtvalg$utvalgTxt

  RegData$var <- RegData[, var]
  RegData$grvar <- RegData[, grvar]

  Tabell <- RegData |>
    dplyr::filter(!is.na(var),
                  !is.na(grvar)) |>
    dplyr::summarise(Antall = sum(var),
                     N = dplyr::n(),
                     .by = grvar) |>
    janitor::adorn_totals(name = "Samlet") |>
    dplyr::mutate(Andel = Antall/N*100) |>
    dplyr::arrange(Andel)

  andeler <- Tabell[, c("grvar", "Andel")]
  N <- Tabell[, c("grvar", "N")]

  pst_txt <- paste0(sprintf(
    '%.0f', purrr::as_vector(andeler[, dim(andeler)[2]])), ' %')

  FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width,
                                  height=height, pointsizePDF=11,
                                  fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], dim(andeler)[1])
  soyleFarger[which(andeler$grvar=='Samlet')] <- farger[4]
  grtxt <- norvas::wrap.it(andeler$grvar, len = 27)

  par('mar'=c(5.1, 12.1, 3.1+length(utvalgTxt), 5.1))
  xmax <- min(100, 1.15*max(andeler[,-1], na.rm = T))

  ypos <- barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0, mrom),
                   col=soyleFarger, border=NA, xlab = 'Andel (%)')

  mtext(grtxt, side=2, line=0.2, las=1, at=ypos, col=1, cex=skriftStr)

  title(main = tittel)
  ypos <- as.numeric(ypos)
  axis(1,cex.axis=0.9)

  mtext( c(purrr::as_vector(N[,2]), names(N)[2]),
         side=4, line=2.5, las=1,
         at=c(ypos, max(ypos) + ypos[2]-ypos[1]), col=1,
         cex=skriftStr*.7, adj = 1)

  text(x=0, y=ypos, labels = pst_txt, cex=0.75, pos=4)#
  mtext(utvalgTxt, side=3, las=1, cex=0.8*skriftStr, adj=0,
        col=FigTypUt$farger[1], line=c(3+0.8*((length(utvalgTxt) -1):0)))


  if ( outfile != '') {dev.off()}



}
