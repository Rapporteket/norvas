#' Gi en visuell fremstilling av registerets indikatorer over tid
#'
#' @param indikatordata En dataramme med følgende kolonner:
#'                 - Aar
#'                 - Teller
#'                 - Sykehusnavn
#'
#' @export
#'
norvasFigIndikatorAar <- function(indikatordata,
                                  tittel='',
                                  terskel=5,
                                  minstekrav = NA,
                                  maal = NA,
                                  skriftStr=1.3,
                                  pktStr=1.4,
                                  legPlass='top',
                                  minstekravTxt='Min.',
                                  maalTxt='Mål',
                                  graaUt=NA,
                                  decreasing=F,
                                  outfile = '',
                                  lavDG=NA,
                                  width=800,
                                  height=700,
                                  inkl_konf=F,
                                  maalretn='hoy',
                                  xmax=NA)
{

  # terskel=5; skriftStr=1.3; pktStr=1.4;
  # legPlass='top'; minstekravTxt='Min.'; maalTxt='Mål'; graaUt=NA; decreasing=F; outfile = '';
  # lavDG=NA; width=800; height=700; inkl_konf=F; maalretn='hoy'; xmax=100

  indikatordata <- indikatordata[indikatordata$Aar > max(indikatordata$Aar)-3, ] # behold bare siste 3 år

  Tabell <- indikatordata %>% dplyr::group_by(Sykehusnavn, Aar) %>%
    dplyr::summarise(Antall = sum(Teller),
                     N = dplyr::n(),
                     Andel = Antall/N*100)


  AntTilfeller <- tidyr::spread(Tabell[, -c(4,5)], 'Aar', 'Antall') %>%
    janitor::adorn_totals(name = "Nasjonalt") %>% as_tibble()

  N <- tidyr::spread(Tabell[, -c(3,5)], 'Aar', 'N') %>%
    janitor::adorn_totals(name = "Nasjonalt") %>% as_tibble()
  N[is.na(N)] <- 0

  # Andeler, inkludert nasjonalt
  andeler <- dplyr::bind_cols(AntTilfeller[,1], AntTilfeller[,-1]/N[,-1] * 100)

  # Fjern år med færre registreringer enn terskelverdi og sykehus med for lav dekningsgrad
  # andeler[N < terskel] <- NA
  andeler[,-1][N[, -1] < terskel] <- NA
  andeler[andeler$Sykehusnavn %in% lavDG, -1] <- NA

  # Ordne rekkefølge, stigende eller synkende
  rekkefolge <- order(andeler[[dim(andeler)[2]]], decreasing = decreasing, na.last = F)

  andeler <- andeler[rekkefolge, ]
  N <- N[rekkefolge, ]

  # Skjul også tidligere år hvis siste år er sensurert pga. for få reg.
  # andeler[as.vector(N[, dim(andeler)[2]]<terskel), 2:3] <- NA
  andeler[as.vector(N[, dim(andeler)[2]]<terskel), 2:(dim(andeler)[2]-1)] <- NA


  # Beregn konfidensintervaller
  KI <- binomkonf(purrr::as_vector(AntTilfeller[rekkefolge, dim(andeler)[2]]),
                  purrr::as_vector(N[, dim(andeler)[2]]))*100
  KI[, is.na(andeler[, dim(andeler)[2]])] <- NA

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
  if (inkl_konf) {
    andeler$Sykehusnavn <- paste0(andeler$Sykehusnavn, ' (', purrr::as_vector(N[, dim(N)[2]]), ')')
    andeler <- rbind(andeler, c(NA,NA,NA))
    andeler$Sykehusnavn[dim(andeler)[1]] <- paste0('(N, ', names(andeler)[dim(andeler)[2]], ')')
    KI <- cbind(c(NA, NA), KI, c(NA, NA))
  } else {
    andeler <- rbind(andeler, NA)
    # andeler$Sykehusnavn[dim(andeler)[1]] <- ''
  }

  andeler <- rbind(NA, andeler, NA)
  # andeler$Sykehusnavn[dim(andeler)[1]] <- ''
  # andeler$Sykehusnavn[1] <- ' '

  vmarg <- max(0, strwidth(andeler$Sykehusnavn, units='figure', cex=cexgr)*0.75)
  # par('fig'=c(vmarg, 1, 0, 1))
  # x11()
  par('mar'=c(5.1, 10.1, 5.1, 9.1))
  # par('oma'=c(0,1,0,0))

  if (is.na(xmax)){
    if (inkl_konf) {
      par('mar'=c(5.1, 4.1, 5.1, 2.1))
      xmax <- min(max(KI, na.rm = T)*1.15,100)
    } else {
      xmax <- min(100, 1.15*max(andeler[,-1], na.rm = T))
    }
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
  if (inkl_konf){
    arrows(x0 = KI[1,], y0 = ypos, x1 = KI[2,], y1 = ypos,
           length=0.5/max(ypos), code=3, angle=90, lwd=1.8, col='gray') #, col=farger[1])
    legend('bottom', cex=0.9*cexgr, bty='n',
           lwd=1.8, lty = 1, pt.cex=1.8, col='gray',
           legend=paste0('Konfidensintervall ', names(N)[dim(N)[2]]))
  }

  axis(1,cex.axis=0.9)
  mtext( andeler$Sykehusnavn, side=2, line=0.2, las=1, at=ypos, col=1, cex=cexgr)
  antAar <- dim(andeler)[2]-1
  if (dim(N)[2] == 4) {
    mtext( c(NA, purrr::as_vector(N[,2]), names(N)[2], NA, NA), side=4,
           line=2.5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
    mtext( c(NA, purrr::as_vector(N[,3]), names(N)[3], NA, NA), side=4,
           line=5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
    mtext( c(NA, purrr::as_vector(N[,4]), names(N)[4], NA, NA), side=4,
           line=7.5, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
    mtext( 'N', side=4, line=5.0, las=1, at=max(ypos), col=1, cex=cexgr*.7, adj = 1)
  }
  if (dim(N)[2] == 3) {
    mtext( c(NA, purrr::as_vector(N[,2]), names(N)[2], NA, NA), side=4,
           line=3, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
    mtext( c(NA, purrr::as_vector(N[,3]), names(N)[3], NA, NA), side=4,
           line=6, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
    mtext( 'N', side=4, line=4.0, las=1, at=max(ypos), col=1, cex=cexgr*.7, adj = 1)
  }
  if (dim(N)[2] == 2) {
    mtext( c(NA, purrr::as_vector(N[,2]), names(N)[2], NA, NA), side=4,
           line=3, las=1, at=ypos, col=1, cex=cexgr*.7, adj = 1)
    mtext( 'N', side=4, line=3, las=1, at=max(ypos), col=1, cex=cexgr*.7, adj = 1)
  }

  par(xpd=TRUE)
  if (dim(N)[2] == 4) {
    points(y=ypos, x=purrr::as_vector(andeler[,2]),cex=pktStr) #'#4D4D4D'
    points(y=ypos, x=purrr::as_vector(andeler[,3]),cex=pktStr,pch= 19)
    par(xpd=FALSE)
    if (legPlass=='nede'){
      legend(x=82, y=ypos[2]+1 ,xjust=0, cex=cexgr, bty='n', #bg='white', box.col='white',
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
             legend=names(N) )}
    if (legPlass=='top'){
      legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
             legend=names(N[,-1]), ncol = dim(andeler)[2]-1)
    }
  }
  if (dim(N)[2] == 3) {
    points(y=ypos, x=purrr::as_vector(andeler[,2]),cex=pktStr,pch= 19) #'#4D4D4D'
    # points(y=ypos, x=purrr::as_vector(andeler[,3]),cex=pktStr,pch= 19)
    par(xpd=FALSE)
    # if (legPlass=='nede'){
    #   legend(x=82, y=ypos[2]+1 ,xjust=0, cex=cexgr, bty='n', #bg='white', box.col='white',
    #          lwd=c(NA,NA,NA), pch=c(1,19,15), pt.cex=c(1.2,1.2,1.8), col=c('black','black',farger[3]),
    #          legend=names(N) )}
    if (legPlass=='top'){
      legend('top', cex=0.9*cexgr, bty='n', #bg='white', box.col='white',y=max(ypos),
             lwd=c(NA,NA), pch=c(19,15), pt.cex=c(1.2,1.8), col=c('black',farger[3]),
             legend=names(N[,-1]), ncol = dim(andeler)[2]-1)
    }
  }

  text(x=0, y=ypos, labels = pst_txt, cex=0.75, pos=4)#
  if ( outfile != '') {dev.off()}

}
