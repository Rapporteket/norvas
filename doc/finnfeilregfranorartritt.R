library(norvas)
library(dplyr)
rm(list = ls())

aktuelle_avdelinger <- c(601159, 700701, 4210614)


feilreg_julianne <- xlsx::read.xlsx("~/GIT/norvas/doc/Feilimport 22.04.24.xlsx", sheetIndex = 1)
diagnoser_julianne <- read.csv2("~/GIT/norvas/doc/diagnoser_norvas_julianne_v2.csv",
                                fileEncoding = "Latin1")

Inklusjon <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_Inklusjonskjema_2024-04-29_1113.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>%
  filter(UnitId %in% aktuelle_avdelinger)
Diagnoser <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_DiagnoseSkjema_2024-04-29_1114.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>%
  filter(UnitId %in% aktuelle_avdelinger)

diagliste <- Diagnoser %>% summarise(N=n(),
                                     icd = paste(unique(Icd_IcdDataDump), collapse = ", "),
                                     DiagnoseNr = paste(unique(DiagnoseNr), collapse = ", "),
                                     .by = Diagnose) %>%
  arrange(-N)
#
# write.csv2(diagliste,  "~/GIT/norvas/doc/diagnoser_norvas.csv",
#            fileEncoding = "UTF-8", row.names = F)
Diagnoser$diagnoseskjema <- 1
Inklusjon$inklusjonsskjema <- 1

Inklusjon <- Inklusjon %>% rename(PasientGUID = Fødselsnummer)
Diagnoser <- Diagnoser %>% rename(PasientGUID = Fødselsnummer)

Inklusjon_m_diagnoser <-
  merge(Inklusjon[,c("SkjemaGUID", "PasientGUID", "InklusjonDato",
                     "ImportTidspunkt", "FormDate", "FormStatus",
                     "UnitId", "inklusjonsskjema")],
        Diagnoser[,c("HovedskjemaGUID", "Diagnose", "Diagnose_Klinisk_Dato",
                     "DiagnoseNr", "Icd_IcdDataDump", "SkjemaGUID",
                     "FormDate", "FormStatus",
                     "UnitId", "PasientGUID", "diagnoseskjema")],
        by.x = "SkjemaGUID", by.y = "HovedskjemaGUID",
        suffixes = c("", "_diagskjema"),
        all = TRUE) %>%
  dplyr::mutate_at(c("InklusjonDato", "ImportTidspunkt", "FormDate",
                     "Diagnose_Klinisk_Dato", "FormDate_diagskjema"),
                   function(x){as.Date(x, format="%d.%m.%Y")}) %>%
  mutate(PasientGUID = ifelse(is.na(PasientGUID), PasientGUID_diagskjema, PasientGUID),
         UnitId = ifelse(is.na(UnitId), UnitId_diagskjema, UnitId))

ikkenorvas <- diagnoser_julianne$Diagnose[diagnoser_julianne$Skal.ikke.være.i.NorVas == "x"]

feilreg <- Inklusjon_m_diagnoser %>% filter(Diagnose %in% ikkenorvas | is.na(Diagnose)) %>%
  select(SkjemaGUID, SkjemaGUID_diagskjema, PasientGUID, InklusjonDato, UnitId, Diagnose,
         Diagnose_Klinisk_Dato, inklusjonsskjema, diagnoseskjema, FormStatus, FormStatus_diagskjema)

write.csv2(feilreg,  "~/GIT/norvas/doc/feilreg_norvas_20240429.csv",
           fileEncoding = "UTF-8", row.names = F, na = "")


Inklusjon_unn <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_Inklusjonskjema_UNN_2024-04-29_1322.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>%
  filter(UnitId %in% aktuelle_avdelinger)
Diagnoser_unn <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_DiagnoseSkjema_UNN_2024-04-29_1323.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>%
  filter(UnitId %in% aktuelle_avdelinger)
Inklusjon_NLSH <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_Inklusjonskjema_NLSH_2024-04-29_1327.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>%
  filter(UnitId %in% aktuelle_avdelinger)
Diagnoser_NLSH <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_DiagnoseSkjema_NLSH_2024-04-29_1328.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>%
  filter(UnitId %in% aktuelle_avdelinger)
Inklusjon_Helgeland <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_Inklusjonskjema_Helgeland_2024-04-29_1318.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>%
  filter(UnitId %in% aktuelle_avdelinger)
Diagnoser_Helgeland <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_DiagnoseSkjema_Helgeland_2024-04-29_1319.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM') %>%
  filter(UnitId %in% aktuelle_avdelinger)

dim(Inklusjon_Helgeland)[1] + dim(Inklusjon_NLSH)[1] + dim(Inklusjon_unn)[1]
dim(Diagnoser_Helgeland)[1] + dim(Diagnoser_NLSH)[1] + dim(Diagnoser_unn)[1]



########### VERIFISER SLETTING #################################################

feilreg <- read.csv2("~/GIT/norvas/doc/feilreg_norvas_20240429.csv")

inklusjon <- read.table(
  '~/softlinks/mydata/norvas/prod_2024/DataDump_MRS-PROD_Inklusjonskjema_2024-05-06_1236.csv',
  header=TRUE, sep=";", stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')


fellesskjema <- intersect(feilreg$SkjemaGUID, inklusjon$SkjemaGUID)


feilreg %>% dplyr::filter(SkjemaGUID %in% fellesskjema)
inklusjon %>% dplyr::filter(SkjemaGUID %in% fellesskjema)






ikkenorvas <- c('Psoriasisartritt',
                'Non-radiografisk aksial spondyloartritt',
                'Ankyloserende Spondylitt',
                'Juvenil Idiopatisk Artritt (JIA)',
                'Psoriasisartritt med aksial affeksjon',
                'Spondyloartritt',
                'Reaktiv Artritt',
                'IBD relatert Artritt',
                'Leddsykdom',
                'Perifer spondyloartritt',
                'JIA Entesitt-relatert Artritt',
                'Polymyalgia Rheumatica')







mangler_inklusjon_pguid <- setdiff(Diagnoser$PasientGUID, Inklusjon$PasientGUID)
mangler_inklusjon_hskjemaguid <- setdiff(Diagnoser$HovedskjemaGUID, Inklusjon$SkjemaGUID)

mangler_diagnose_pguid <- setdiff(Inklusjon$PasientGUID, Diagnoser$PasientGUID)
mangler_diagnose_hskjemaguid <- setdiff(Inklusjon$SkjemaGUID, Diagnoser$HovedskjemaGUID)

mangler_inklusjon1 <- Diagnoser %>% filter(HovedskjemaGUID %in% mangler_inklusjon_hskjemaguid) %>%
  summarise(N=n(),
            .by = Diagnose) %>%
  arrange(-N)
mangler_inklusjon2 <- Diagnoser %>% filter(PasientGUID %in% mangler_inklusjon_pguid) %>%
  summarise(N=n(),
            .by = Diagnose) %>%
  arrange(-N)


Diagnoser %>% summarise(N=n(),
                        diagnoser = paste(unique(Diagnose), collapse = ", "),
                        unitid = paste(unique(UnitId), collapse = ", "),
                        .by = HovedskjemaGUID) %>%
  filter(N>1) %>% arrange(-N)


norvasdiagnoser <- c("Kjempecelle Arteritt",
                     "Granulomatøs Polyangitt (Wegener’s)",
                     "Mikroskopisk Polyangiitis",
                     "Aortitt INA",
                     "Takayasu Arteritt",
                     "Eosinofilisk Granulomatøs Polyangitt (Churg-Strauss)",
                     "Kjempcellearteritt  (GCA)",
                     "Granulomatose med polyangiitt (GPA)(Wegeners granulomatose)",
                     "Behcets sykdom",
                     "Granulomatose med polyangiitt",
                     "Takayasus sykdom (TAK)",
                     "Uspesifisert nekrotiserende vaskulitt",
                     "Systemisk Vaskulitt sykdom",
                     "Kjempecellearteritt med polymyalgia rheumatica",
                     "Eosinofil granulomatose med polyangiitt  (EGPA)(Churg Strauss sykdom)",
                     "Mikroskopisk polyangiitt (MPA)",
                     "Behcets sykdom (BS)",
                     "Eosinofil granulomatose med polyangiitt",
                     "Polyarteritis Nodosa",
                     "Takayasus arteritt",
                     "Vaskulitt",
                     "Kjempecellearteritt med polymyalgia rheumatica (GCA2)",
                     "Mikroskopisk polyangiitt",
                     "Kjempecellearteritt uten polymyalgia rheumatica",
                     "Aortitis INA",
                     "Behçets syndrom",
                     "Aortitt",
                     "Polyarteritis nodosa",
                     "IgA Vaskulitt (Henoch-Schoenlein)",
                     "Primær nekrotiserende systemisk vaskulitt",
                     "Takayasus sykdom",
                     "IgA vaskulitt  (IgAV)(Henoch Schønleins purpura)",
                     "Kawasakis syndrom")



ikkenorvas <- c("Psoriasisartritt",
                "Non-radiografisk aksial spondyloartritt",
                "Ankyloserende Spondylitt",
                "Juvenil Idiopatisk Artritt (JIA)",
                "Psoriasisartritt med aksial affeksjon",
                "Spondyloartritt",
                "Temporalisarteritt",
                "Polymyalgia Rheumatica",
                "Reaktiv Artritt",
                "IBD relatert Artritt",
                "Leddsykdom",
                "Perifer spondyloartritt",
                "Juvenil temporalisarteritt",
                "Periarteritt",
                "Arteritt",
                "Nekrotiserende arteritt",
                "JIA Entesitt-relatert Artritt")

