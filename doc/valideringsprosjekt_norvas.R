library(norvas)
library(dplyr)
rm(list = ls())

Inklusjon_validering <- read.table(
  "C:/regdata/norvas/validering_helseplattform/DataDump_MRS-PROD_Inklusjonskjema_2025-11-17_0943.csv",
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
hjelpenr <- openxlsx::read.xlsx("C:/regdata/norvas/validering_helseplattform/til_validering_m_hjelpenr.xlsx") |>
  dplyr::filter(hjelpenr %in% Inklusjon_validering$PasientId)

Inklusjon <- read.table(
  "C:/regdata/norvas/datadump/DataDump_MRS-PROD_Inklusjonskjema_2025-10-23_1539.csv",
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  dplyr::filter(UnitId == 104579) |>
  dplyr::filter(PasientGUID %in% hjelpenr$PasientGUID) |>
  merge(hjelpenr[, c("PasientGUID", "hjelpenr")], by = "PasientGUID") |>
  dplyr::relocate(hjelpenr) |>
  dplyr::rename(PasientId = hjelpenr)
Oppfolging <- read.table(
  'C:/regdata/norvas/datadump/DataDump_MRS-PROD_OppfølgingSkjema_2025-10-23_1540.csv',
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  dplyr::filter(UnitId == 104579) |>
  norvas::norvasPreprosess() |>
  dplyr::filter(HovedskjemaGUID %in% Inklusjon$SkjemaGUID)
Diagnoser <- read.table(
  'C:/regdata/norvas/datadump/DataDump_MRS-PROD_DiagnoseSkjema_2025-10-23_1539.csv',
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  dplyr::filter(UnitId == 104579) |>
  dplyr::filter(HovedskjemaGUID %in% Inklusjon$SkjemaGUID)

Inklusjon_validering <- read.table(
  "C:/regdata/norvas/validering_helseplattform/DataDump_MRS-PROD_Inklusjonskjema_2025-11-17_0943.csv",
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') # |>


tmp <- merge(Inklusjon |> select(PasientId, InklusjonDato),
             Inklusjon_validering |> select(PasientId, InklusjonDato),
             by = "PasientId", suffixes = c("", "_validering")) |>
  mutate(
    InklusjonDato = as.Date(InklusjonDato, format="%d.%m.%Y"),
    InklusjonDato_validering = as.Date(InklusjonDato_validering, format="%d.%m.%Y")) |>
  filter(InklusjonDato != InklusjonDato_validering)





















