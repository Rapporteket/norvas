library(norvas)
library(dplyr)
rm(list = ls())

Inklusjon_validering <- read.table(
  "C:/Users/kth200/regdata/norvas/validering_helseplattform/DataDump_MRS-PROD_Inklusjonskjema_2026-02-12_0914_sensitiv.csv",
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Oppfolging_validering <- read.table(
  "C:/Users/kth200/regdata/norvas/validering_helseplattform/DataDump_MRS-PROD_OppfølgingSkjema_2026-02-12_0916.csv",
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  merge(Inklusjon_validering[, c("SkjemaGUID", "PasientId")],
        by.x = "HovedskjemaGUID", by.y = "SkjemaGUID") |>
  dplyr::relocate(PasientId) |>
  mutate(OppfolgingsDato = as.Date(OppfolgingsDato, format = "%d.%m.%Y"))
Diagnoser_validering <- read.table(
  "C:/Users/kth200/regdata/norvas/validering_helseplattform/DataDump_MRS-PROD_DiagnoseSkjema_2026-02-12_0917.csv",
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  merge(Inklusjon_validering[, c("SkjemaGUID", "PasientId")],
        by.x = "HovedskjemaGUID", by.y = "SkjemaGUID") |>
  dplyr::relocate(PasientId)|>
  mutate(Diagnose_Klinisk_Dato =
           as.Date(Diagnose_Klinisk_Dato, format = "%d.%m.%Y"))

Inklusjon_validering <- merge(
  Inklusjon_validering,
  Diagnoser_validering |> select(PasientId, Diagnosegruppe),
  by = "PasientId"
)
Oppfolging_validering <- merge(
  Oppfolging_validering,
  Diagnoser_validering |> select(PasientId, Diagnosegruppe),
  by = "PasientId"
)

hjelpenr <- openxlsx::read.xlsx("C:/Users/kth200/regdata/norvas/validering_helseplattform/til_validering_m_hjelpenr.xlsx") |>
  dplyr::filter(hjelpenr %in% Inklusjon_validering$PasientId)

Inklusjon <- read.table(
  "C:/Users/kth200/regdata/norvas/datadump/DataDump_MRS-PROD_Inklusjonskjema_2025-10-23_1539.csv",
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  dplyr::filter(UnitId == 104579) |>
  dplyr::filter(PasientGUID %in% hjelpenr$PasientGUID) |>
  merge(hjelpenr[, c("PasientGUID", "hjelpenr")], by = "PasientGUID") |>
  dplyr::relocate(hjelpenr) |>
  dplyr::rename(PasientId = hjelpenr) |>
  mutate(InklusjonDato =
           as.Date(InklusjonDato, format = "%d.%m.%Y"))
Oppfolging <- read.table(
  'C:/Users/kth200/regdata/norvas/datadump/DataDump_MRS-PROD_OppfølgingSkjema_2025-10-23_1540.csv',
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  dplyr::filter(UnitId == 104579) |>
  norvas::norvasPreprosess() |>
  dplyr::filter(HovedskjemaGUID %in% Inklusjon$SkjemaGUID) |>
  merge(hjelpenr[, c("PasientGUID", "hjelpenr")], by = "PasientGUID") |>
  dplyr::relocate(hjelpenr) |>
  dplyr::rename(PasientId = hjelpenr)
Diagnoser <- read.table(
  'C:/Users/kth200/regdata/norvas/datadump/DataDump_MRS-PROD_DiagnoseSkjema_2025-10-23_1539.csv',
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  dplyr::filter(UnitId == 104579) |>
  dplyr::filter(HovedskjemaGUID %in% Inklusjon$SkjemaGUID) |>
  merge(hjelpenr[, c("PasientGUID", "hjelpenr")], by = "PasientGUID") |>
  dplyr::relocate(hjelpenr) |>
  dplyr::rename(PasientId = hjelpenr)|>
  mutate(Diagnose_Klinisk_Dato =
           as.Date(Diagnose_Klinisk_Dato, format = "%d.%m.%Y"))

duplikat <- Oppfolging[
  duplicated(Oppfolging[c("PasientId", "OppfolgingsDato")]) |
    duplicated(Oppfolging[c("PasientId", "OppfolgingsDato")],
               fromLast = TRUE), ]|>
  dplyr::relocate(PasientId, OppfolgingsDato) |>
  dplyr::arrange(PasientId, OppfolgingsDato)

Oppfolging_unik <- Oppfolging |>
  mutate(na_count = rowSums(is.na(across(everything())))) |>
  arrange(PasientId, OppfolgingsDato, na_count) |>      # choose grouping columns here
  slice(1, .by = c(PasientId, OppfolgingsDato)) |>      # keep row with fewest NAs
  select(-na_count) |>
  merge(Inklusjon |> select(PasientId, InklusjonDato),
        by = "PasientId", all.x = TRUE)|>
  filter(OppfolgingsDato >= InklusjonDato,
         OppfolgingsDato >= "2022-11-12",
         OppfolgingsDato <= "2025-10-31")


inkl_og_oppf_samme_dato <- merge(
  Inklusjon, Oppfolging_unik,
  by.x = c("PasientId", "InklusjonDato"),
  by.y = c("PasientId", "OppfolgingsDato"),
)

Oppfolging_unik <- anti_join(
  Oppfolging_unik,
  inkl_og_oppf_samme_dato[, c("PasientId", "InklusjonDato")],
  by = join_by("PasientId" == "PasientId",
               "OppfolgingsDato" == "InklusjonDato"))

table(Inklusjon_validering$Diagnosegruppe)




Diagnoser_alle <- read.table(
  'C:/Users/kth200/regdata/norvas/datadump/DataDump_MRS-PROD_DiagnoseSkjema_2025-10-23_1539.csv',
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  merge(hjelpenr[, c("PasientGUID", "hjelpenr")],
        by = "PasientGUID",
        all.x = TRUE) |>
  dplyr::relocate(hjelpenr) |>
  dplyr::rename(PasientId = hjelpenr)


Oppfolging_pr_pas <- Oppfolging |>
  summarise(Oppfolginger_norvas = paste0(sort(OppfolgingsDato),
                                         collapse = ", "),
            .by = PasientId)
Oppfolging_pr_pas_val <- Oppfolging_validering |>
  summarise(Oppfolginger_validering = paste0(sort(OppfolgingsDato),
                                             collapse = ", "),
            .by = PasientId)

Oppsum_oppf <- merge(Oppfolging_pr_pas, Oppfolging_pr_pas_val,
                     by = "PasientId", all = TRUE)

# openxlsx::write.xlsx(
#   Oppsum_oppf,
#   "C:/Users/kth200/regdata/norvas/validering_helseplattform/oppfolgingsdatoer.xlsx")


var_inklusjon <- c(
  "InklusjonDato", "Jobbsituasjon", "RoykeStatus", "SnuseStatus", "ErGravid",
  "Pasientsmerter", "Tretthet", "PasientGlobalSykdomsaktivitet", "Rand12Q01",
  "Rand12Q02", "Rand12Q03", "Rand12Q04", "Rand12Q05", "Rand12Q06", "Rand12Q07",
  "Rand12Q08", "Rand12Q09", "Rand12Q10", "Rand12Q11", "Rand12Q12", "Rand12ScorePf",
  "Rand12ScoreRp", "Rand12ScoreBp", "Rand12ScoreGh", "Rand12ScoreVt",
  "Rand12ScoreSf", "Rand12ScoreRe", "Rand12ScoreMh"
)
var_oppf <- c(
  "OppfolgingsDato", "IngenEndringerMedisinering", "DatoIngenEndringerMedisinering",
  "EksklusjonsDato", "EksklusjonsArsak", "ArsakDod", "Avsluttet", "Hoyde",
  "Vekt", "RoykeStatus", "SnuseStatus", "ErGravid", "Pasientsmerter", "Tretthet",
  "PasientGlobalSykdomsaktivitet", "Rand12Q01", "Rand12Q02", "Rand12Q03",
  "Rand12Q04", "Rand12Q05", "Rand12Q06", "Rand12Q07", "Rand12Q08", "Rand12Q09",
  "Rand12Q10", "Rand12Q11", "Rand12Q12", "Rand12ScoreSf", "Rand12ScoreVt",
  "Rand12ScoreGh", "Rand12ScorePf", "Rand12ScoreBp", "Rand12ScoreRp",
  "Rand12ScoreMh", "Rand12ScoreRe")


sammenlign_inkl <- merge(
  Inklusjon |> select(PasientId, all_of(var_inklusjon)) |>
    mutate(InklusjonDato = as.Date(InklusjonDato, format="%d.%m.%Y")),
  Inklusjon_validering |> select(PasientId, all_of(var_inklusjon)) |>
    mutate(InklusjonDato = as.Date(InklusjonDato, format="%d.%m.%Y")),
  by = "PasientId", suffixes = c("_reg", "_gold")) %>%
  relocate(sort(names(.))) |>
  relocate(PasientId)

sammenlign_oppf <- merge(
  Oppfolging_unik |> select(PasientId, all_of(var_oppf)) |>
    mutate(OppfolgingsDato = as.Date(OppfolgingsDato, format="%d.%m.%Y")),
  Oppfolging_validering |> select(PasientId, all_of(var_oppf)) |>
    mutate(OppfolgingsDato = as.Date(OppfolgingsDato, format="%d.%m.%Y")),
  by = c("PasientId", "OppfolgingsDato"), suffixes = c("_reg", "_gold")) %>%
  relocate(sort(names(.))) |>
  relocate(PasientId, OppfolgingsDato) |>
  arrange(PasientId, OppfolgingsDato)




#####

KERR <- read.table(
  'C:/Users/kth200/regdata/norvas/validering_helseplattform//DataDump_MRS-PROD_KerrsKriterierSkjema_2026-02-12_0917.csv',
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  merge(Inklusjon_validering[, c("SkjemaGUID", "PasientId")],
        by.x = "HovedskjemaGUID", by.y = "SkjemaGUID") |>
  dplyr::relocate(PasientId) |>
  mutate(KerrsKriterier_Dato = as.Date(KerrsKriterier_Dato, format = "%d.%m.%Y"))

tmp <- Oppfolging |> filter(PasientId == 80001174693, OppfolgingsDato == "2025-08-06")
tmp2 <- KERR |> filter(PasientId == 80001174693, OppfolgingsDato == "2025-08-06")




# c("SvarDato",
#   "Hoyde",
#   "Hoyde_IngenEndring",
#   "Vekt",
#   "Vekt_IngenEndring",
#   "ArPaSkole",
#   "ArPaSkole_IngenEndring",
#   "Jobbsituasjon",
#   "RoykeStatus",
#   "SnuseStatus",
#   "ErGravid",
#   "UkerGravid",
#   "Prom_PasientSmerter",
#   "Prom_Tretthet",
#   "Prom_PasientGlobalSykdomsaktivitet",
#   "Rand12Q01",
#   "Rand12Q02",
#   "Rand12Q03",
#   "Rand12Q04",
#   "Rand12Q05",
#   "Rand12Q06",
#   "Rand12Q07",
#   "Rand12Q08",
#   "Rand12Q09",
#   "Rand12Q10",
#   "Rand12Q11",
#   "Rand12Q12")

# sammenlign_oppf2 <- merge(
#   Oppfolging_unik |> select(PasientId, all_of(var_oppf)) |>
#     mutate(OppfolgingsDato = as.Date(OppfolgingsDato, format="%d.%m.%Y")),
#   Oppfolging_validering |> select(PasientId, all_of(var_oppf)) |>
#     mutate(OppfolgingsDato = as.Date(OppfolgingsDato, format="%d.%m.%Y")),
#   by = c("PasientId", "OppfolgingsDato"), suffixes = c("_reg", "_gold"),
#   all = TRUE) %>%
#   relocate(sort(names(.))) |>
#   relocate(PasientId, OppfolgingsDato) |>
#   arrange(PasientId, OppfolgingsDato)
#
# utforsk <- Oppfolging_validering |>
#   filter(PasientId %in% sammenlign_oppf2$PasientId) |>
#   relocate(PasientId, OppfolgingsDato) |>
#   arrange(PasientId, OppfolgingsDato)

# tmp <- Oppfolging_unik |> filter(OppfolgingsDato < InklusjonDato) |>
#   select(PasientId, OppfolgingsDato, InklusjonDato) |>
#   arrange(PasientId, OppfolgingsDato)



# sammenlign_inkl <- merge(Inklusjon,
#                          Inklusjon_validering,
#                          by = "PasientId", suffixes = c("", "_validering")) %>%
#   relocate(sort(names(.)))
#
# Diagnoser_sml1 <- merge(Diagnoser,
#                         Diagnoser_validering,
#                         by = c("PasientId", "Diagnose_Klinisk_Dato"),
#                         suffixes = c("", "_validering")) %>%
#   relocate(sort(names(.))) |>
#   relocate(PasientId, Diagnose_Klinisk_Dato)
# Diagnoser_sml2 <- merge(Diagnoser,
#                         Diagnoser_validering,
#                         by = c("PasientId", "Diagnose_Klinisk_Dato"),
#                         suffixes = c("", "_validering"),
#                         all = TRUE) %>%
#   relocate(sort(names(.))) |>
#   relocate(PasientId, Diagnose_Klinisk_Dato)


#
# Diagnoser |> summarise(
#   Diagnose_Klinisk_Dato = paste0(sort(Diagnose_Klinisk_Dato), collapse = ","),
#   N = n(),
#   .by = PasientId) |>
#   filter(N>1) |>
#   select(-N) |>
#   merge(Diagnoser_validering |> select(PasientId, Diagnose_Klinisk_Dato),
#         by = "PasientId", suffixes = c("", "_validering"))
#
# setdiff(Diagnoser_validering$PasientId, Diagnoser$PasientId)
# table(Diagnoser_alle$Icd_IcdDataDump, useNA = 'ifany')
# setdiff(Diagnoser_validering$PasientId, Diagnoser_alle$PasientId)
