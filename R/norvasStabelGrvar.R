#' Plot stablede andeler per grupperingsvariabel
#'
#' Stabelplot
#'
#' @export
norvarStabelGrvar <- function(plotMatrise,
                              grtxt = "",
                              outfile = "",
                              legendTxt = NA,
                              tittel = "",
                              xlab =" Antall pasienter",
                              cexgr = 1,
                              fargepalett = "BlaaOff",
                              beside = F,
                              revcol = F,
                              revcol_legend = F) {
  figinfo <- rapFigurer::figtype(outfile = outfile, fargepalett=fargepalett)
  farger <- if (dim(plotMatrise)[1]==2) {
    figinfo$farger[c(3,1)]
  } else {
    rev(figinfo$farger[1:dim(plotMatrise)[1]])
  }
  if (revcol) {farger <- rev(farger)}
  xmax <- if (beside) {min(100, 1.15*max(colSums(plotMatrise)), na.rm=T)} else {1.15*max(colSums(plotMatrise))}
  vmarg <- min(1,max(0, strwidth(grtxt, units='figure', cex=cexgr)*0.75))
  par('fig'=c(vmarg, 1, 0, 1))
  pos <- barplot(plotMatrise, beside = beside, horiz = T,
                 col = farger,
                 border=NA, xlab=xlab,
                 xlim = c(0, xmax))
  if (beside) {
    mtext(at=colMeans(pos)+0.00, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25)
  } else {
    mtext(at=pos+0.00, text=grtxt, side=2, las=1, cex=cexgr, adj=1, line=0.25)
  }
  if (!is.na(legendTxt[1])) {
    legend('bottomright', legend = legendTxt, col = if (revcol_legend) {rev(farger)} else {farger},
           pch = 15, border = NA, bty='n')
  }
  title(main = tittel)
  if ( outfile != '') {dev.off()}

}
