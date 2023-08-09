library(dplyr)
library(norvas)
rm(list = ls())

rap_aar <- 2022
aarrappdata <- norvas::lesogprosesser(rap_aar = rap_aar)
Inklusjon <- aarrappdata$Inklusjon

kobl_unitid_shusnavn_norvas <- Inklusjon[match(unique(Inklusjon$UnitId),
                                               Inklusjon$UnitId), c("UnitId", "Sykehusnavn")]
orgnr_table <-
  tibble::tribble(
    ~UnitId,   ~orgnr,
    104579, 974749025,
    601159, 974795787,
    700701, 974795361,
    4210614, 974795515,
    104209, 873255102,
    110353, 874632562,
    4210431, 874716782,
    110629, 974116588,
    103725, 974631326,
    108054, 974633698,
    701344, 974703300,
    106841, 974724774,
    104092, 974733013,
    105274, 974744570,
    102708, 974747138,
    105776, 974754118,
    102977, 974557746
  )
kobl_unitid_shusnavn_norvas <- merge(kobl_unitid_shusnavn_norvas, orgnr_table,
                                     by = "UnitId", all.x = TRUE)

## DG 2019-2020 ##################################################################################

kobl_npr <- read.csv2("~/softlinks/mydata/norvas/koblingsfil.csv", fileEncoding = "Latin1")

dg_D690_andre <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_D690.csv",
                            sep = ";", fileEncoding = "Latin1", header = T)
dg_D891_andre <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_D891.csv",
                            sep = ";", fileEncoding = "Latin1", header = T)
dg_I776_storkar <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_I776.csv",
                              sep = ";", fileEncoding = "Latin1", header = T)
dg_M300_andre <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_M300.csv",
                            sep = ";", fileEncoding = "Latin1", header = T)
dg_M301_anca <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_M301.csv",
                           sep = ";", fileEncoding = "Latin1", header = T)
dg_M313_anca <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_M313.csv",
                           sep = ";", fileEncoding = "Latin1", header = T)
dg_M314_storkar <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_M314.csv",
                              sep = ";", fileEncoding = "Latin1", header = T)
dg_M315_storkar <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_M315_M316.csv",
                              sep = ";", fileEncoding = "Latin1", header = T)
dg_M317_anca <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_M317.csv",
                           sep = ";", fileEncoding = "Latin1", header = T)
dg_M319_andre <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_M319.csv",
                            sep = ";", fileEncoding = "Latin1", header = T)
dg_M352_andre <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_hf_M352.csv",
                            sep = ";", fileEncoding = "Latin1", header = T)
dg_landet <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_landet_hf.csv",
                        sep = ";", fileEncoding = "Latin1", header = T)
dg_landet_innrap <- read.table("~/softlinks/mydata/norvas/DG_2019_2020/DGA_resultat_landet_hf_innrapp.csv",
                               sep = ";", fileEncoding = "Latin1", header = T)

dg_andre <- merge(dg_D690_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                  dg_D891_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                  by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
  merge(dg_M300_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3")) %>%
  merge(dg_M319_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".4")) %>%
  merge(dg_M352_andre[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".5"))
dg_andre[is.na(dg_andre)] <- 0
dg_andre$norvas <- rowSums(dg_andre[, c("Begge.1", "Kun_norvas.1", "Begge.2", "Kun_norvas.2", "Begge", "Kun_norvas", "Begge.4", "Kun_norvas.4",
                                        "Begge.5", "Kun_norvas.5")])
dg_andre$totalt <- rowSums(dg_andre[, c("Total.1", "Total.2", "Total", "Total.4", "Total.5")])
dg_andre$DG <- dg_andre$norvas/dg_andre$totalt*100

dg_storkar <- merge(dg_I776_storkar[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                    dg_M314_storkar[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                    by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
  merge(dg_M315_storkar[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3"))
dg_storkar[is.na(dg_storkar)] <- 0
dg_storkar$norvas <- rowSums(dg_storkar[, c("Begge.1", "Kun_norvas.1", "Begge.2", "Kun_norvas.2", "Begge", "Kun_norvas")])
dg_storkar$totalt <- rowSums(dg_storkar[, c("Total.1", "Total.2", "Total")])
dg_storkar$DG <- dg_storkar$norvas/dg_storkar$totalt*100

dg_anca <- merge(dg_M301_anca[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                 dg_M313_anca[,c("hf_standard", "Begge", "Kun_norvas", "Total")],
                 by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
  merge(dg_M317_anca[,c("hf_standard", "Begge", "Kun_norvas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3"))
dg_anca[is.na(dg_anca)] <- 0
dg_anca$norvas <- rowSums(dg_anca[, c("Begge.1", "Kun_norvas.1", "Begge.2", "Kun_norvas.2", "Begge", "Kun_norvas")])
dg_anca$totalt <- rowSums(dg_anca[, c("Total.1", "Total.2", "Total")])
dg_anca$DG <- dg_anca$norvas/dg_anca$totalt*100

dg_landet_innrap$norvas <- rowSums(dg_landet_innrap[, c("Begge", "Kun_norvas")])
dg_landet_innrap$totalt <- dg_landet_innrap$Total

dg_anca$ind_id <- "norvas_dg_anca"
dg_storkar$ind_id <- "norvas_dg_storkar"
dg_andre$ind_id <- "norvas_dg_andre"
dg_landet_innrap$ind_id <- "norvas_dg_alle"

dg_samlet <- bind_rows(dg_anca[, c("hf_standard", "norvas", "totalt", "ind_id")],
                       dg_storkar[, c("hf_standard", "norvas", "totalt", "ind_id")],
                       dg_andre[, c("hf_standard", "norvas", "totalt", "ind_id")],
                       dg_landet_innrap[, c("hf_standard", "norvas", "totalt", "ind_id")])

dg_samlet$UnitId <- kobl_npr$UnitId[match(dg_samlet$hf_standard, kobl_npr$hf_standard)]
dg_samlet$UnitId[dg_samlet$hf_standard == "Betanien, Skien"] <- 104209
dg_samlet$orgnr <- kobl_unitid_shusnavn_norvas$orgnr[match(dg_samlet$UnitId, kobl_unitid_shusnavn_norvas$UnitId)]

dg_samlet$orgnr[dg_samlet$hf_standard == "Finnmarkssykehuset HF"] <- 983974880
dg_samlet$orgnr[dg_samlet$hf_standard == "Akershus universitetssykehus HF"] <- 983971636

dg_samlet <- dg_samlet[!is.na(dg_samlet$orgnr), ] %>%
  filter(ind_id != "norvas_dg_andre")

names(dg_samlet)[match(c("norvas", "totalt"), names(dg_samlet))] <- c("var", "denominator")
dg_samlet$year <- 2019
dg_samlet$context <- "caregiver"
dg_2019 <- dg_samlet[, c("orgnr", "year", "var", "denominator", "ind_id", "context")]
dg_2020 <- dg_2019
dg_2020$year <- 2020


### DG 2017

dg2017 <- readxl::read_xlsx("~/softlinks/mydata/norvas/DGA_NorVas_2017.xlsx", sheet = 1) %>%
  mutate(var = Begge + Kun_NorVas) %>%
  rename(denominator = Total,
         hf_standard = `HF/ideelt sykehus`) %>%
  filter(hf_standard != "Totalt") %>%
  mutate(ind_id = "norvas_dg_alle",
         context = "caregiver",
         year = 2017,
         orgnr = kobl_npr$UnitId[match(hf_standard, kobl_npr$hf_standard)]) %>%
  mutate(orgnr = kobl_unitid_shusnavn_norvas$orgnr[match(orgnr, kobl_unitid_shusnavn_norvas$UnitId)]) %>%
  mutate(orgnr = ifelse(hf_standard == "Revmatismesykehuset, NKS. Lillehammer", 973254979, orgnr)) %>%
  select("orgnr", "year", "var", "denominator", "ind_id", "context")

dg2018 <- readxl::read_xlsx("~/softlinks/mydata/norvas/DGA_NorVas_2018.xlsx", sheet = 1) %>%
  mutate(var = Begge + Kun_NorVas) %>%
  rename(denominator = Total,
         hf_standard = `HF/ideelt sykehus`) %>%
  filter(hf_standard != "Totalt") %>%
  mutate(ind_id = "norvas_dg_alle",
         context = "caregiver",
         year = 2018,
         orgnr = kobl_npr$UnitId[match(hf_standard, kobl_npr$hf_standard)]) %>%
  mutate(orgnr = kobl_unitid_shusnavn_norvas$orgnr[match(orgnr, kobl_unitid_shusnavn_norvas$UnitId)]) %>%
  mutate(orgnr = ifelse(hf_standard == "Revmatismesykehuset, NKS. Lillehammer", 973254979, orgnr)) %>%
  select("orgnr", "year", "var", "denominator", "ind_id", "context")

###### DG 2022 ##################################################3

dg_I776_storkar <- read.table("~/softlinks/mydata/norvas/DG_2022/DGA_resultat_NorVas/DGA_resultat_hf_I776.csv",
                           sep = ";", fileEncoding = "Latin1", header = T)
dg_M301_anca <- read.table("~/softlinks/mydata/norvas/DG_2022/DGA_resultat_NorVas/DGA_resultat_hf_M301.csv",
                           sep = ";", fileEncoding = "Latin1", header = T)
dg_M313_anca <- read.table("~/softlinks/mydata/norvas/DG_2022/DGA_resultat_NorVas/DGA_resultat_hf_M313.csv",
                           sep = ";", fileEncoding = "Latin1", header = T)
dg_M314_storkar <- read.table("~/softlinks/mydata/norvas/DG_2022/DGA_resultat_NorVas/DGA_resultat_hf_M314.csv",
                              sep = ";", fileEncoding = "Latin1", header = T)
dg_M315_storkar <- read.table("~/softlinks/mydata/norvas/DG_2022/DGA_resultat_NorVas/DGA_resultat_hf_M315_M316.csv",
                              sep = ";", fileEncoding = "Latin1", header = T)
dg_M317_anca <- read.table("~/softlinks/mydata/norvas/DG_2022/DGA_resultat_NorVas/DGA_resultat_hf_M317.csv",
                           sep = ";", fileEncoding = "Latin1", header = T)

dg_landet_innrap <- read.table("~/softlinks/mydata/norvas/DG_2022/DGA_resultat_NorVas/DGA_resultat_hf.csv",
                               sep = ";", fileEncoding = "Latin1", header = T)

dg_storkar <- merge(dg_I776_storkar[,c("hf_standard", "Begge", "Kun_NorVas", "Total")],
                    dg_M314_storkar[,c("hf_standard", "Begge", "Kun_NorVas", "Total")],
                    by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
  merge(dg_M315_storkar[,c("hf_standard", "Begge", "Kun_NorVas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3"))
dg_storkar[is.na(dg_storkar)] <- 0
dg_storkar$norvas <- rowSums(dg_storkar[, c("Begge.1", "Kun_NorVas.1", "Begge.2", "Kun_NorVas.2", "Begge", "Kun_NorVas")])
dg_storkar$totalt <- rowSums(dg_storkar[, c("Total.1", "Total.2", "Total")])
dg_storkar$DG <- dg_storkar$norvas/dg_storkar$totalt*100

dg_anca <- merge(dg_M301_anca[,c("hf_standard", "Begge", "Kun_NorVas", "Total")],
                 dg_M313_anca[,c("hf_standard", "Begge", "Kun_NorVas", "Total")],
                 by = "hf_standard", all = T, suffixes = c(".1", ".2")) %>%
  merge(dg_M317_anca[,c("hf_standard", "Begge", "Kun_NorVas", "Total")], by = "hf_standard", all = T, suffixes = c("", ".3"))
dg_anca[is.na(dg_anca)] <- 0
dg_anca$norvas <- rowSums(dg_anca[, c("Begge.1", "Kun_NorVas.1", "Begge.2", "Kun_NorVas.2", "Begge", "Kun_NorVas")])
dg_anca$totalt <- rowSums(dg_anca[, c("Total.1", "Total.2", "Total")])
dg_anca$DG <- dg_anca$norvas/dg_anca$totalt*100

dg_landet_innrap$norvas <- rowSums(dg_landet_innrap[, c("Begge", "Kun_NorVas")])
dg_landet_innrap$totalt <- dg_landet_innrap$Total

dg_anca$ind_id <- "norvas_dg_anca"
dg_storkar$ind_id <- "norvas_dg_storkar"
dg_landet_innrap$ind_id <- "norvas_dg_alle"

dg_samlet <- bind_rows(dg_anca[, c("hf_standard", "norvas", "totalt", "ind_id")],
                       dg_storkar[, c("hf_standard", "norvas", "totalt", "ind_id")],
                       dg_landet_innrap[, c("hf_standard", "norvas", "totalt", "ind_id")])

dg_samlet$UnitId <- kobl_npr$UnitId[match(dg_samlet$hf_standard, kobl_npr$hf_standard)]
dg_samlet$orgnr <- kobl_unitid_shusnavn_norvas$orgnr[match(dg_samlet$UnitId, kobl_unitid_shusnavn_norvas$UnitId)]

dg_samlet$orgnr[dg_samlet$hf_standard == "Finnmarkssykehuset HF"] <- 983974880
dg_samlet$orgnr[dg_samlet$hf_standard == "Akershus universitetssykehus HF"] <- 983971636

dg_samlet <- dg_samlet[!is.na(dg_samlet$orgnr), ]

names(dg_samlet)[match(c("norvas", "totalt"), names(dg_samlet))] <- c("var", "denominator")
dg_samlet$year <- 2022
dg_samlet$context <- "caregiver"
dg_2022 <- dg_samlet[, c("orgnr", "year", "var", "denominator", "ind_id", "context")]

dg_norvas_tom2022 <- dplyr::bind_rows(dg2017,
                                      dg2018,
                                      dg_2019,
                                      dg_2020,
                                      dg_2022)

write.csv2(dg_norvas_tom2022, "~/GIT/norvas/doc/dg_norvas_tom2022_v2.csv",
           row.names = F)


