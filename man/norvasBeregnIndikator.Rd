% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/norvasBeregnIndikator.R
\name{norvasBeregnIndikator}
\alias{norvasBeregnIndikator}
\title{Beregn indikatorer}
\usage{
norvasBeregnIndikator(
  norvasdata,
  ind_id = "norvas_anca_ved_debut",
  rap_aar = lubridate::year(Sys.Date())
)
}
\arguments{
\item{norvasdata}{liste med alle relevante tabeller fra Norvas}

\item{ind_id}{id til indikator som skal beregnes}

\item{rap_aar}{til og med hvilket år}
}
\description{
Beregn Norvas sine indikatorer
}
