#' Lag søylediagram eller stabelplott som viser andeler av ulike variabler
#'
#' Denne funksjonen lager et søylediagram eller stabelplot som viser andeler av valgt variabel
#' filtrert på de utvalg som er gjort.
#'
#' @inheritParams  RegData En dataramme med alle nodvendige variabler fra registeret
#'
#' @return En figur med søylediagram eller et stabelplot av ønsket variabel
#'
#' @export
#'

norvasIndikatorOppf  <- function(RegData=0, valgtVar='Inklusjonsalder', datoFra='2014-01-01', datoTil='2050-12-31',
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


  ####################  Andel oppfølginger med BVAS (ANCA + andre) eller KERR (Storkarsvaskulitt) #####################

  ## Lag indikatorfigur

  tmp2 <- merge(Oppfolging, BVAS[, c("bvas_samlet", "PasientGUID", "BVAS_Dato")], by.x = c('PasientGUID','OppfolgingsDato'),
                by.y = c('PasientGUID','BVAS_Dato'), all.x = T)
  tmp2 <- tmp2[tmp2$Diag_gr_nr %in% 2:3, ]
  sum(!is.na(tmp2$bvas_samlet))/dim(tmp2)[1]*100

  tmp3 <- merge(Oppfolging, KERR[, c("KerrsKriterier_Dato", "PasientGUID", "KerrsKriterierScore")], by.x = c('PasientGUID','OppfolgingsDato'),
                by.y = c('PasientGUID','KerrsKriterier_Dato'), all.x = T)
  tmp3 <- tmp3[tmp3$Diag_gr_nr == 1, ]
  sum(!is.na(tmp3$KerrsKriterierScore))/dim(tmp3)[1]*100

}
