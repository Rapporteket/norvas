#' Lag indikatorfigur
#'
#' Denne funksjonen lager et søylediagram over valgt indikator
#'
#' @inheritParams norvasFigAndeler
#'
#' @return En figur med søylediagram over valgt indikator
#' @param indikatordata En dataramme med kolonnene Sykehusnavn, Aar, Teller og Nevner
#'
#' @export
#'

norvasIndikator <- function(indikatordata, tittel='', terskel=5, minstekrav = NA, maal = NA, skriftStr=1.3, pktStr=1.4,
                            legPlass='top', minstekravTxt='Min.', maalTxt='Mål', graaUt=NA, decreasing=F, outfile = '',
                            lavDG=NA, width=800, height=700, inkl_konf=F, maalretn='hoy', xmax=NA)
{
  # indikatordata=figurdata; tittel='testtittel'; terskel=5; minstekrav = NA; maal = 95; skriftStr=1.3; pktStr=1.4;
  # legPlass='top'; minstekravTxt='Min.'; maalTxt='Mål'; graaUt=NA; decreasing=F; outfile = '';
  # lavDG=NA; width=800; height=700; inkl_konf=F; maalretn='hoy'
  # tittel = c("Andelen med minimum 2 oppfølginger pr. år")
  # maal = 80; minstekrav = 40; xmax = 100

  Tabell <- indikatordata %>%
    dplyr::group_by(Sykehusnavn) %>%
    dplyr::summarise(Antall = sum(Teller), N = n())%>%
    mutate(Sykehusnavn=as.character(Sykehusnavn))

  Tabell <- bind_rows(Tabell, tibble(Sykehusnavn="Nasjonalt", Antall=sum(Tabell$Antall), N=sum(Tabell$N))) %>%
    mutate(Andel = Antall/N*100)

  andeler <- Tabell[, c("Sykehusnavn", "Andel")]
  N <- Tabell[, c("Sykehusnavn", "N")]

  N[is.na(N)] <- 0

  # Fjern år med færre registreringer enn terskelverdi og sykehus med for lav dekningsgrad
  andeler[N < terskel] <- NA
  andeler[andeler$Sykehusnavn %in% lavDG, -1] <- NA

  # Ordne rekkefølge, stigende eller synkende
  rekkefolge <- order(andeler$Andel, decreasing = decreasing, na.last = F)

  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]

  pst_txt <- paste0(sprintf('%.0f', purrr::as_vector(andeler[, dim(andeler)[2]])), ' %')
  # pst_txt[is.na(andeler[, dim(andeler)[2]])] <- paste0('N<', terskel, ' eller dekningsgrad mindre en 60 pst.')
  pst_txt[N[, dim(andeler)[2]]<terskel] <- paste0('N<', terskel)
  pst_txt[andeler$Sykehusnavn %in% lavDG] <- 'Dekningsgrad < 60 %'
  pst_txt <- c(NA, pst_txt, NA, NA)

  FigTypUt <- rapFigurer::figtype(outfile=outfile, width=width, height=height, pointsizePDF=11, fargepalett='BlaaOff')
  farger <- FigTypUt$farger
  soyleFarger <- rep(farger[3], dim(andeler)[1])
  soyleFarger[which(andeler$Sykehusnavn=='Nasjonalt')] <- farger[4]
  if (!is.na(graaUt[1])) {soyleFarger[which(andeler$Sykehusnavn %in% graaUt)] <- 'gray88'}
  soyleFarger <- c(NA, soyleFarger)

  # Lagre parameterverdier
  oldpar_mar <- par()$mar
  oldpar_fig <- par()$fig
  oldpar_oma <- par()$oma

  cexgr <- skriftStr


  andeler <- rbind(andeler, c(NA,NA))
  andeler$Sykehusnavn[dim(andeler)[1]] <- ''

  andeler <- rbind(c(NA,NA), andeler, c(NA,NA))
  andeler$Sykehusnavn[dim(andeler)[1]] <- ''
  andeler$Sykehusnavn[1] <- ''

  vmarg <- max(0, strwidth(andeler$Sykehusnavn, units='figure', cex=cexgr)*0.75)
  # par('fig'=c(vmarg, 1, 0, 1))
  # x11()
  par('mar'=c(5.1, 10.1, 4.1, 5.1))

  if (is.na(xmax)){
    xmax <- min(100, 1.15*max(andeler[,-1], na.rm = T))
  }

  ypos <- barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
                   xlim=c(0,xmax),
                   names.arg=rep('',dim(andeler)[1]),
                   horiz=T, axes=F, space=c(0,0.3),
                   col=soyleFarger, border=NA, xlab = 'Andel (%)')

  fargerMaalNiva <-  c('aquamarine3','#fbf850', 'red')

  if (maal > minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=minstekrav, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (maal < minstekrav & !is.na(maal) & !is.na(minstekrav)) {
    rect(xleft=maal, ybottom=1, xright=minstekrav, ytop=max(ypos)-1.6, col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='lav') {
    # rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+0.4, col = fargerMaalNiva[2], border = NA)
    rect(xleft=0, ybottom=1, xright=maal, ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}
  if (!is.na(maal) & is.na(minstekrav) & maalretn=='hoy') {
    # rect(xleft=maal, ybottom=0, xright=minstekrav, ytop=max(ypos)+0.4, col = fargerMaalNiva[2], border = NA)
    rect(xleft=maal, ybottom=1, xright=min(xmax, 100), ytop=max(ypos)-1.6, col = fargerMaalNiva[1], border = NA)}

  barplot( t(andeler[,dim(andeler)[2]]), beside=T, las=1,
           names.arg=rep('',dim(andeler)[1]),
           horiz=T, axes=F, space=c(0,0.3),
           col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)

  title(main = tittel)
  ypos <- as.numeric(ypos) #as.vector(ypos)
  yposOver <- max(ypos)-2 + 0.5*diff(ypos)[1]
  if (!is.na(minstekrav)) {
    lines(x=rep(minstekrav, 2), y=c(-1, yposOver), col=fargerMaalNiva[2], lwd=2)
    par(xpd=TRUE)
    text(x=minstekrav, y=yposOver, labels = minstekravTxt,
         pos = 4, cex=cexgr*0.65, srt = 90)
    par(xpd=FALSE)
  }
  if (!is.na(maal)) {
    lines(x=rep(maal, 2), y=c(-1, yposOver), col=fargerMaalNiva[1], lwd=2)
    barplot( t(andeler[, dim(andeler)[2]]), beside=T, las=1,
             names.arg=rep('',dim(andeler)[1]),
             horiz=T, axes=F, space=c(0,0.3),
             col=soyleFarger, border=NA, xlab = 'Andel (%)', add=TRUE)
    par(xpd=TRUE)
    text(x=maal, y=yposOver, labels = maalTxt, pos = 4, cex=cexgr*0.65, srt = 90) #paste0(maalTxt,maal,'%')
    par(xpd=FALSE)
  }

  axis(1,cex.axis=0.9)
  mtext( andeler$Sykehusnavn, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  antAar <- dim(andeler)[2]-1
  mtext( c(NA, purrr::as_vector(N[,2]), names(N)[2], NA, NA), side=4, line=2.5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)


  text(x=0, y=ypos, labels = pst_txt, cex=0.75, pos=4)#
  if ( outfile != '') {dev.off()}



}
