#' Beregn konfidensintervall for binomisk data med n suksess av N forsøk
#'
#' Denne funksjonen beregner konfidensintervall for binomisk fordelt data
#'
#' @param n - Antall suksess, skalar eller vektor
#' @param N - Antall forsøk, skalar eller vektor av samme lengde som n
#' @param konfnivaa - Konfidensnivået til test (Default 0.95)
#'
#' @return Nedre og øvre grense til konfidensintervallet
#'
#' @export

binomkonf <- function(n, N, konfnivaa=0.95)
  {
  binkonf <- matrix(nrow=2, ncol = length(n))
  for (i in 1:length(n)) {
    if (N[i]>0) {
      binkonf[,i] <- binom.test(n[i],N[i], alternative = 'two.sided', conf.level = konfnivaa)$conf.int[1:2]
    } else {
      binkonf[,i] <- c(0,0)
    }
  }
  return(invisible(binkonf))
}
