library(norvas)
# library(lubridate)
library(dplyr)
rm(list = ls())

# Inklusjon_mai2024 <- read.table(
#   '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_Inklusjonskjema_2024-05-15_0822.csv',
#   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Inklusjon_mai2024 <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_Inklusjonskjema_2024-05-15_0822.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>%
  mutate(InklusjonDato = as.Date(InklusjonDato, format="%d.%m.%Y"),
         Aar = as.numeric(format(InklusjonDato, format = '%Y')))
Diagnoser_mai2024 <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_DiagnoseSkjema_2024-05-15_0823.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Inklusjon_juni2024 <- read.table(
#   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_Inklusjonskjema_2024-06-11_1506.csv',
#   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Inklusjon_aug2024 <- read.table(
#   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_Inklusjonskjema_2024-08-02_1505.csv',
#   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Inklusjon_aug2024 <- read.table(
  '~/softlinks/mydata/norvas/DataDump_MRS-PROD_Inklusjonskjema_2024-08-02_1505.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>%
  mutate(InklusjonDato = as.Date(InklusjonDato, format="%d.%m.%Y"),
         Aar = as.numeric(format(InklusjonDato, format = '%Y')))
Diagnoser_aug2024 <- read.table(
  '~/softlinks/mydata/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2024-08-02_1506.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

mapEnhet <- data.frame(
  UnitId = c(102977, 104579, 105274, 106841, 601159, 700701, 105776, 4210431,
             103725, 104092, 104209, 110353, 110629, 102708, 4210614, 108054,
             701344, 103300),
  Sykehusnavn = c('Haukeland', 'St. Olavs', 'Førde', 'Haugesund', 'UNN',
                  'Nordlandsykehuset', 'Levanger', 'Rikshospitalet',
                  'Drammen', 'Kristiansand', 'Betanien', 'Lillehammer',
                  'Martina Hansen', 'Ålesund', 'Helgelandssykehuset',
                  'Moss', 'Stavanger', 'Drammen'))

Inklusjon_mai2024$Sykehusnavn <-
  mapEnhet$Sykehusnavn[match(Inklusjon_mai2024$UnitId, mapEnhet$UnitId)]
Inklusjon_aug2024$Sykehusnavn <-
  mapEnhet$Sykehusnavn[match(Inklusjon_aug2024$UnitId, mapEnhet$UnitId)]

Inklusjon_mai2024_oppsummert <- Inklusjon_mai2024 %>%
  dplyr::summarise(N = n(),
                   shus = paste(Sykehusnavn, collapse = ","),
                   .by = PasientGUID) %>%
  dplyr::filter(N > 1)
Inklusjon_aug2024_oppsummert <- Inklusjon_aug2024 %>%
  dplyr::summarise(N = n(),
                   shus = paste(Sykehusnavn, collapse = ","),
                   .by = PasientGUID) %>%
  dplyr::filter(N > 1)

inkl_stolavs <- Inklusjon_mai2024 %>%
  filter(Sykehusnavn == "St. Olavs") %>%
  select(PasientGUID, InklusjonDato)
inkl_aalesund <- Inklusjon_mai2024 %>%
  filter(Sykehusnavn == "Ålesund") %>%
  select(PasientGUID, InklusjonDato)

overlapp_mai2024 <- merge(inkl_stolavs, inkl_aalesund,
                          by = "PasientGUID",
                          suffixes = c("_stolav", "_aalesund"))

inkl_stolavs <- Inklusjon_aug2024 %>%
  filter(Sykehusnavn == "St. Olavs") %>%
  select(PasientGUID, InklusjonDato)
inkl_aalesund <- Inklusjon_aug2024 %>%
  filter(Sykehusnavn == "Ålesund") %>%
  select(PasientGUID, InklusjonDato)

overlapp_aug2024 <- merge(inkl_stolavs, inkl_aalesund,
                           by = "PasientGUID",
                           suffixes = c("_stolav", "_aalesund"))



tmptabellmai <- Inklusjon_mai2024 %>%
  filter(Sykehusnavn %in% c("St. Olavs", "Ålesund")) %>%
  summarise(N = n(),
            .by = c(Sykehusnavn, Aar)) %>%
  tidyr::pivot_wider(names_from = Sykehusnavn, values_from = N, values_fill = 0) %>%
  arrange(Aar) %>%
  dplyr::rename("St. Olavs mai"= "St. Olavs",
                "Ålesund mai" = "Ålesund")

tmptabellaugust <- Inklusjon_aug2024 %>%
  filter(Sykehusnavn %in% c("St. Olavs", "Ålesund")) %>%
  summarise(N = n(),
            .by = c(Sykehusnavn, Aar)) %>%
  tidyr::pivot_wider(names_from = Sykehusnavn, values_from = N, values_fill = 0) %>%
  arrange(Aar) %>%
  dplyr::rename("St. Olavs aug"= "St. Olavs",
                "Ålesund aug" = "Ålesund")

samlet <- merge(tmptabellmai, tmptabellaugust, by="Aar") %>%
  dplyr::select("Aar", "St. Olavs mai", "St. Olavs aug", "Ålesund mai", "Ålesund aug")

# Inklusjon_okt2022 <- read.table(
#   '~/softlinks/mydata/norvas/oktober2022/DataDump_MRS-PROD_Inklusjonskjema_2022-10-20_1507.csv',
#   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Inklusjon_des2022 <- read.table(
#   '~/softlinks/mydata/norvas/desember2022/DataDump_MRS-PROD_Inklusjonskjema_2022-12-07_0919.csv',
#   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Inklusjon_mai2023 <- read.table(
#   '~/softlinks/mydata/norvas/DataDump_MRS-PROD_Inklusjonskjema_2023-05-30_1403.csv',
#   header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# Inklusjon_okt2022$Sykehusnavn <-
#   mapEnhet$Sykehusnavn[match(Inklusjon_okt2022$UnitId, mapEnhet$UnitId)]
# Inklusjon_des2022$Sykehusnavn <-
#   mapEnhet$Sykehusnavn[match(Inklusjon_des2022$UnitId, mapEnhet$UnitId)]
# Inklusjon_mai2023$Sykehusnavn <-
#   mapEnhet$Sykehusnavn[match(Inklusjon_mai2023$UnitId, mapEnhet$UnitId)]
# Inklusjon_okt2022_oppsummert <- Inklusjon_okt2022 %>%
#   # dplyr::filter(Sykehusnavn %in% )
#   dplyr::summarise(N = n(),
#                    shus = paste(Sykehusnavn, collapse = ","),
#                    .by = PasientGUID) %>%
#   dplyr::filter(N > 1)
# Inklusjon_des2022_oppsummert <- Inklusjon_des2022 %>%
#   # dplyr::filter(Sykehusnavn %in% )
#   dplyr::summarise(N = n(),
#                    shus = paste(Sykehusnavn, collapse = ","),
#                    .by = PasientGUID) %>%
#   dplyr::filter(N > 1)
# Inklusjon_mai2023_oppsummert <- Inklusjon_mai2023 %>%
#   # dplyr::filter(Sykehusnavn %in% )
#   dplyr::summarise(N = n(),
#                    shus = paste(Sykehusnavn, collapse = ","),
#                    .by = PasientGUID) %>%
#   dplyr::filter(N > 1)
# tmp1 <- Inklusjon_okt2022 %>% filter(Sykehusnavn == "St. Olavs") %>%
#   select(InklusjonDato, PasientGUID)
# tmp2 <- Inklusjon_des2022 %>% filter(Sykehusnavn == "St. Olavs") %>%
#   select(InklusjonDato, PasientGUID)
# tmp3 <- Inklusjon_mai2023 %>% filter(Sykehusnavn == "St. Olavs") %>%
#   select(InklusjonDato, PasientGUID)
# tmp4 <- Inklusjon_mai2024 %>% filter(Sykehusnavn == "St. Olavs") %>%
#   select(InklusjonDato, PasientGUID)
# tmp5 <- Inklusjon_juni2024 %>% filter(Sykehusnavn == "St. Olavs") %>%
#   select(InklusjonDato, PasientGUID)
#
# Inklusjonsdatoer_stolavs <- merge(tmp1, tmp2, by = "PasientGUID", suffixes = c("", "_des2022")) %>%
#   merge(tmp3, by = "PasientGUID", suffixes = c("", "_mai2023")) %>%
#   merge(tmp4, by = "PasientGUID", suffixes = c("", "_mai2024")) %>%
#   merge(tmp5, by = "PasientGUID", suffixes = c("", "_juni2024")) %>%
#   mutate(indikator = (InklusjonDato!=InklusjonDato_des2022 |
#                         InklusjonDato!=InklusjonDato_mai2023 |
#                         InklusjonDato!=InklusjonDato_mai2024 |
#                         InklusjonDato!=InklusjonDato_juni2024 |
#                         InklusjonDato_des2022!=InklusjonDato_mai2023 |
#                         InklusjonDato_des2022!=InklusjonDato_mai2024 |
#                         InklusjonDato_des2022!=InklusjonDato_juni2024 |
#                         InklusjonDato_mai2023!=InklusjonDato_mai2024 |
#                         InklusjonDato_mai2023!=InklusjonDato_juni2024 |
#                         InklusjonDato_mai2024!=InklusjonDato_juni2024
#   )
#   )



