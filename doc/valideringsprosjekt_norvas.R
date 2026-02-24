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
  # dplyr::filter(UnitId == 104579) |>
  dplyr::filter(HovedskjemaGUID %in% Inklusjon$SkjemaGUID) |>
  merge(hjelpenr[, c("PasientGUID", "hjelpenr")], by = "PasientGUID") |>
  dplyr::relocate(hjelpenr) |>
  dplyr::rename(PasientId = hjelpenr)|>
  mutate(Diagnose_Klinisk_Dato =
           as.Date(Diagnose_Klinisk_Dato, format = "%d.%m.%Y"))


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
  by.y = c("PasientId", "OppfolgingsDato")
)

Oppfolging_unik <- anti_join(
  Oppfolging_unik,
  inkl_og_oppf_samme_dato[, c("PasientId", "InklusjonDato")],
  by = join_by("PasientId" == "PasientId",
               "OppfolgingsDato" == "InklusjonDato"))

# Inklusjon |> select(PasientId, Diag)
# table(Inklusjon$Diagnosegruppe)

Diagnoser_alle <- read.table(
  'C:/Users/kth200/regdata/norvas/datadump/DataDump_MRS-PROD_DiagnoseSkjema_2025-10-23_1539.csv',
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') |>
  mutate(na_count = rowSums(is.na(across(everything())))) |>
  arrange(PasientGUID, Diagnose_Klinisk_Dato, na_count) |>      # choose grouping columns here
  slice(1, .by = c(PasientGUID, Diagnose_Klinisk_Dato)) |>      # keep row with fewest NAs
  select(-na_count) |>
  merge(hjelpenr[, c("PasientGUID", "hjelpenr")],
        by = "PasientGUID",
        all.x = TRUE) |>
  dplyr::relocate(hjelpenr) |>
  dplyr::rename(PasientId = hjelpenr)

Inklusjon <- Inklusjon |>
  merge(Diagnoser_validering |> select(PasientId, Diagnosegruppe),
        by = "PasientId")

tmp <- Inklusjon |> select(PasientId, InklusjonDato, Diagnosegruppe) |>
  merge(
    Oppfolging_unik |>
      mutate(Fulgt_opp = 1) |>
      mutate(ant_oppf = sum(Fulgt_opp), .by = PasientId) |>
      arrange(PasientId, OppfolgingsDato) |>      # choose grouping columns here
      slice(1, .by = c(PasientId)) |>
      select(PasientId, Fulgt_opp, ant_oppf),
    by = "PasientId", all.x = TRUE
  ) |>
  mutate(Fulgt_opp = ifelse(is.na(Fulgt_opp), 0, Fulgt_opp),
         ant_oppf = ifelse(is.na(ant_oppf), 0, ant_oppf)) |>
  merge(
    Oppfolging_validering |>
      mutate(Fulgt_opp_val = 1) |>
      mutate(ant_oppf_val = sum(Fulgt_opp_val), .by = PasientId) |>
      arrange(PasientId, OppfolgingsDato) |>      # choose grouping columns here
      slice(1, .by = c(PasientId)) |>
      select(PasientId, Fulgt_opp_val, ant_oppf_val),
    by = "PasientId", all.x = TRUE
  ) |>
  mutate(Fulgt_opp_val = ifelse(is.na(Fulgt_opp_val), 0, Fulgt_opp_val),
         ant_oppf_val = ifelse(is.na(ant_oppf_val), 0, ant_oppf_val)) |>
  summarise(ant_inkl = n(),
            ant_ingen_oppf = sum(Fulgt_opp == 0),
            ant_ingen_oppf_val = sum(Fulgt_opp_val == 0),
            ant_oppf = sum(ant_oppf),
            ant_oppf_val = sum(ant_oppf_val),
            .by = Diagnosegruppe)



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

tmp <- Oppfolging |> filter(
  PasientId == 80001174693,
  OppfolgingsDato == "2025-08-06")
tmp2 <- KERR |> filter(PasientId == 80001174693,
                       OppfolgingsDato == "2025-08-06")



# duplikat <- Oppfolging[
#   duplicated(Oppfolging[c("PasientId", "OppfolgingsDato")]) |
#     duplicated(Oppfolging[c("PasientId", "OppfolgingsDato")],
#                fromLast = TRUE), ]|>
#   dplyr::relocate(PasientId, OppfolgingsDato) |>
#   dplyr::arrange(PasientId, OppfolgingsDato)

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



# var_inklusjon <- data.frame(
#   variabel= var_inklusjon,
#   felttype = kodebok_norvas$Felttype[
#     match(var_inklusjon, kodebok_norvas$Variabelnavn)])
#
# date_vars    <- var_inklusjon$variabel[var_inklusjon$felttype == "Dato/tid"]
# nominal_vars <- var_inklusjon$variabel[var_inklusjon$felttype == "Enkeltvalg"]
# numeric_vars <- var_inklusjon$variabel[var_inklusjon$felttype == "Tall"]
#
# library(lubridate)
# library(psych)      # for Cohen's kappa
# library(ggplot2)
#
#
# ### --- DATE VARIABLES ----------------------------------------------------
#
# analyze_dates <- function(df, vars) {
#   results <- lapply(vars, function(v) {
#     reg  <- df[[paste0(v, "_reg")]]
#     gold <- df[[paste0(v, "_gold")]]
#
#     diff_days <- as.numeric(difftime(reg, gold, units = "days"))
#
#     data.frame(
#       variable = v,
#       n = sum(!is.na(diff_days)),
#       mean_diff_days = mean(diff_days, na.rm = TRUE),
#       median_diff_days = median(diff_days, na.rm = TRUE),
#       sd_diff_days = sd(diff_days, na.rm = TRUE)
#     )
#   })
#   bind_rows(results)
# }
#
# date_results <- analyze_dates(sammenlign_inkl, date_vars)
# print(date_results)
#
# ### --- NOMINAL VARIABLES ----------------------------------------------------
#
# analyze_nominal <- function(df, vars) {
#   results <- lapply(vars, function(v) {
#     reg  <- df[[paste0(v, "_reg")]]
#     gold <- df[[paste0(v, "_gold")]]
#
#     tab <- table(reg, gold)
#     kappa <- cohen.kappa(tab)
#
#     data.frame(
#       variable = v,
#       percent_agreement = sum(reg == gold, na.rm = TRUE) / sum(!is.na(reg) & !is.na(gold)),
#       kappa = ifelse(is.null(kappa$kappa), NA, kappa$kappa),
#       kappa_ci_low = ifelse(is.null(kappa$confid[1]), NA, kappa$confid[1]),
#       kappa_ci_high = ifelse(is.null(kappa$confid[2]), NA, kappa$confid[2])
#     )
#   })
#   bind_rows(results)
# }
#
# nominal_results <- analyze_nominal(sammenlign_inkl, nominal_vars[-4])
# print(nominal_results)
#
# ### --- NUMERIC VARIABLES ----------------------------------------------------
#
# analyze_numeric <- function(df, vars) {
#   results <- lapply(vars, function(v) {
#     reg  <- df[[paste0(v, "_reg")]]
#     gold <- df[[paste0(v, "_gold")]]
#
#     diff <- reg - gold
#     mean_diff <- mean(diff, na.rm = TRUE)
#     sd_diff <- sd(diff, na.rm = TRUE)
#
#     loa_low  <- mean_diff - 1.96 * sd_diff
#     loa_high <- mean_diff + 1.96 * sd_diff
#
#     data.frame(
#       variable = v,
#       n = sum(!is.na(diff)),
#       mean_diff = mean_diff,
#       sd_diff = sd_diff,
#       loa_low = loa_low,
#       loa_high = loa_high,
#       correlation = cor(reg, gold, use = "complete")
#     )
#   })
#   bind_rows(results)
# }
#
# tmp <- sammenlign_inkl |> select(c(paste0(numeric_vars, "_gold"), paste0(numeric_vars, "_reg"))) %>%
#   relocate(sort(names(.)))
#
# numeric_results <- analyze_numeric(sammenlign_inkl, numeric_vars)
# print(numeric_results)
#
# ### --- OPTIONAL: Bland-Altman PLOT for any numeric variable --------------
#
# plot_bland_altman <- function(df, var) {
#   reg  <- df[[paste0(var, "_reg")]]
#   gold <- df[[paste0(var, "_gold")]]
#
#   avg <- (reg + gold) / 2
#   diff <- reg - gold
#
#   mean_diff <- mean(diff, na.rm = TRUE)
#   sd_diff <- sd(diff, na.rm = TRUE)
#
#   ggplot(data.frame(avg, diff), aes(x = avg, y = diff)) +
#     geom_point(alpha = 0.4) +
#     geom_hline(yintercept = mean_diff, color = "blue", linetype = "dashed") +
#     geom_hline(yintercept = mean_diff + 1.96*sd_diff, color = "red", linetype = "dotted") +
#     geom_hline(yintercept = mean_diff - 1.96*sd_diff, color = "red", linetype = "dotted") +
#     labs(title = paste("Bland–Altman plot for", var),
#          x = "Average of registry & gold",
#          y = "Difference (reg - gold)") +
#     theme_minimal()
# }
#
# # Example:
# # plot_bland_altman(df, "age")
