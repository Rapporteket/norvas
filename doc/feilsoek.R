rm(list = ls())
library(dplyr)
library(norvas)
Medisiner <- read.table(
  'C:/regdata/norvas/datadump/DataDump_MRS-PROD_MedisineringSkjema_2025-08-29_0950.csv',
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
MedisinerHistorisk <- read.table(
  'C:/regdata/norvas/datadump/DataDump_MRS-PROD_MedisineringHistoriskDoseSkjema_2025-08-29_0951.csv',
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
MedisinerInfusjonslogg <- read.table(
  'C:/regdata/norvas/datadump/DataDump_MRS-PROD_MedisineringInfusjonsLoggSkjema_2025-08-29_0951.csv',
  header=TRUE, sep=";",
  stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

# MedisinerInfusjonslogg <- MedisinerInfusjonslogg |>
#   mutate(Legemiddel = Infusjon_LegemiddelType2023,
#          Legemiddel = ifelse(Legemiddel == "", Infusjon_LegemiddelType2022, Legemiddel),
#          Legemiddel = ifelse(Legemiddel == "", Infusjon_LegemiddelType2020, Legemiddel),
#          Legemiddel = ifelse(Legemiddel == "", Infusjon_LegemiddelType2019, Legemiddel))
#
# tmp <- merge(Medisiner, MedisinerInfusjonslogg,
#              by.x = c("HovedskjemaGUID"),
#              by.y = c("HovedskjemaGUID")
# )


etter_oppdatering <- openxlsx::read.xlsx(
  "C:/Users/kth200/OneDrive - Helse Nord RHF/Dokumenter/regdata/norvas/feilsøking_bergen/DataDump Bergen medisinering 010114-280825 etter full import.xlsx",
  sheet = 1) |> select(-2)

foer_oppdatering <- openxlsx::read.xlsx(
  "C:/Users/kth200/OneDrive - Helse Nord RHF/Dokumenter/regdata/norvas/feilsøking_bergen/DataDump Bergen medisinering 010114-140825.xlsx",
  sheet = 1) |> select(-2)

varnavn <- kodebok_norvas[which(!is.na(kodebok_norvas$Variabelnavn)),
                          c("Variabelnavn", "skjema")]
indekser_kodebok <-
  which(kodebok_norvas$Variabelnavn == 'LegemiddelType2023' &
          kodebok_norvas$skjema == 'MedisineringSkjema'):
  (which(kodebok_norvas$Variabelnavn ==
           varnavn$Variabelnavn[which(varnavn$Variabelnavn=='LegemiddelType2023' &
                                        varnavn$skjema == 'MedisineringSkjema')+1])-1)
kobl_generisknavn_kode <- data.frame(
  kode=as.numeric(kodebok_norvas$kode[c(indekser_kodebok[-1], indekser_kodebok[1])]),
  label=kodebok_norvas$label[c(indekser_kodebok[-1], indekser_kodebok[1])])


Medisiner_foer <- foer_oppdatering |>
  filter(Skjematype == "MedisineringSkjema")
Medisiner_etter <- etter_oppdatering |>
  filter(Skjematype == "MedisineringSkjema")
Infusjonslogg_foer <- foer_oppdatering |>
  filter(Skjematype == "MedisineringInfusjonsLoggSkjema")
Infusjonslogg_etter <- etter_oppdatering |>
  filter(Skjematype == "MedisineringInfusjonsLoggSkjema")

Medisiner_foer <- Medisiner_foer |>
  filter(SkjemaGUID %in% intersect(SkjemaGUID, Medisiner_etter$SkjemaGUID))|>
  select(intersect(names(Medisiner), names(Medisiner_foer)))

Medisiner_etter <- Medisiner_etter |>
  filter(SkjemaGUID %in% intersect(SkjemaGUID, Medisiner_foer$SkjemaGUID))|>
  select(intersect(names(Medisiner), names(Medisiner_etter)))

tom <- etter_oppdatering |> filter(LegemiddelNr == 0)
tom2 <- foer_oppdatering |> filter(LegemiddelNr == 0)



Medisiner <- Medisiner |> filter(SkjemaGUID %in% Medisiner_foer$SkjemaGUID)
Medisiner_foer <- Medisiner_foer |>
  filter(SkjemaGUID %in% intersect(SkjemaGUID, Medisiner$SkjemaGUID))
Medisiner_etter <- Medisiner_etter |>
  filter(SkjemaGUID %in% intersect(SkjemaGUID, Medisiner$SkjemaGUID))

Medisiner_alle <- norvasPreprosess(Medisiner)

Medisiner_foer <- Medisiner_foer |>
  select(intersect(names(Medisiner), names(Medisiner_foer))) |>
  arrange(SkjemaGUID)
Medisiner_etter <- Medisiner_etter |>
  select(intersect(names(Medisiner), names(Medisiner_etter))) |>
  arrange(SkjemaGUID)
Medisiner <- Medisiner |>
  select(intersect(names(Medisiner), names(Medisiner_etter))) |>
  arrange(SkjemaGUID)

Medisiner_foer_vasket <- norvasPreprosess(Medisiner_foer)
Medisiner_etter_vasket <- norvasPreprosess(Medisiner_etter)

nyvask <- Medisiner_alle |>
  summarise(LegemiddelType2019 = paste0(unique(LegemiddelType2019), collapse = ","),
            LegemiddelType2020 = paste0(unique(LegemiddelType2020), collapse = ","),
            LegemiddelType2022 = paste0(unique(LegemiddelType2022), collapse = ","),
            LegemiddelType2023 = paste0(unique(LegemiddelType2023), collapse = ","),
            LegemiddelGenerisk = paste0(unique(LegemiddelGenerisk), collapse = ","),
            N = n(),
            .by = LegemiddelNr) |>
  arrange(LegemiddelNr)



Medisiner_vasket |>
  filter(LegemiddelType2023 != "") |>
  summarise(LegemiddelType2019 = paste0(unique(LegemiddelType2019), collapse = ","),
            LegemiddelType2020 = paste0(unique(LegemiddelType2020), collapse = ","),
            LegemiddelType2022 = paste0(unique(LegemiddelType2022), collapse = ","),
            LegemiddelType2023 = paste0(unique(LegemiddelType2023), collapse = ","),
            N = n(),
            .by = LegemiddelNr) |>
  arrange(LegemiddelNr)


foer <- merge(aux2023_foer,
              kobl_generisknavn_kode, by.x = "LegemiddelNr",
              by.y = "kode", all.x = T)

etter <- merge(aux2023_etter,
               kobl_generisknavn_kode, by.x = "LegemiddelNr",
               by.y = "kode", all.x = T)






Medisiner_foer <- foer_oppdatering |> filter(Skjematype == "MedisineringInfusjonsLoggSkjema")
Medisiner_etter <- etter_oppdatering |> filter(Skjematype == "MedisineringInfusjonsLoggSkjema")

Medisiner_foer <- Medisiner_foer |>
  filter(SkjemaGUID %in% intersect(SkjemaGUID, Medisiner_etter$SkjemaGUID)) |>
  select(intersect(names(Medisiner), names(Medisiner_foer))) |>
  arrange(SkjemaGUID)
Medisiner_etter <- Medisiner_etter |>
  filter(SkjemaGUID %in% intersect(SkjemaGUID, Medisiner_foer$SkjemaGUID)) |>
  select(intersect(names(Medisiner), names(Medisiner_foer))) |>
  arrange(SkjemaGUID)




