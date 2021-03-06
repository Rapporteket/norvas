######## Last data ########################################
library(norvas)
library(tidyverse)
library(kableExtra)
library(DT)
library(shiny)
library(shinyjs)
library(lubridate)

# context <- Sys.getenv("R_RAP_INSTANCE") #Blir tom hvis jobber lokalt
# onServer <- context == "TEST" | context == "QA" | context == "PRODUCTION"
# if (onServer) {
#   # RegData <- norvasHentRegData()
#   # skjemaoversikt <- NorvasHentSkjemaOversikt()
# } else {
  Inklusjon <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-03-08_0852.csv', header=TRUE, sep=";",
                          stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Inklusjon_pguid <- read.table('I:/norvas/DataDump_MRS-PROD_Inklusjonskjema_2021-03-08_0854.csv', header=TRUE, sep=";",
                                stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  # Inklusjon$PasientGUID <- Inklusjon_pguid$PasientGUID
  Inklusjon <- merge(Inklusjon, Inklusjon_pguid[, c("SkjemaGUID", "PasientGUID")], by ='SkjemaGUID')
  Oppfolging <- read.table('I:/norvas/DataDump_MRS-PROD_OppfølgingSkjema_2021-03-08_1409.csv', header=TRUE, sep=";",
                           stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Diagnoser <- read.table('I:/norvas/DataDump_MRS-PROD_DiagnoseSkjema_2021-03-08_0858.csv', header=TRUE, sep=";",
                          stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Medisiner <- read.table('I:/norvas/DataDump_MRS-PROD_MedisineringSkjema_2021-03-08_1410.csv', header=TRUE, sep=";",
                          stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  BVAS <- read.table('I:/norvas/DataDump_MRS-PROD_BvasSkjema_2021-03-08_1421.csv', header=TRUE, sep=";",
                     stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  KERR <- read.table('I:/norvas/DataDump_MRS-PROD_KerrsKriterierSkjema_2021-03-08_1426.csv', header=TRUE, sep=";",
                     stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  VDI <- read.table('I:/norvas/DataDump_MRS-PROD_VdiSkjema_2021-03-08_1420.csv', header=TRUE, sep=";",
                    stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Alvorlig_infeksjon <- read.table('I:/norvas/DataDump_MRS-PROD_SelvrapportertAlvorligInfeksjonSkjema_2021-03-08_1426.csv', header=TRUE, sep=";",
                                   stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Utredning <- read.table('I:/norvas/DataDump_MRS-PROD_Utredning_2021-03-08_1425.csv', header=TRUE, sep=";",
                          stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  Labskjema <- read.table('I:/norvas/DataDump_MRS-PROD_BlodprøvesvarSkjema_2021-03-08_1425.csv', header=TRUE, sep=";",
                          stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
# }

Inklusjon <- norvasPreprosess(Inklusjon)
Oppfolging <- norvasPreprosess(Oppfolging)
Diagnoser <- norvasPreprosess(Diagnoser)
Medisiner <- norvasPreprosess(Medisiner)
BVAS <- norvasPreprosess(BVAS)
KERR <- norvasPreprosess(KERR)
VDI <- norvasPreprosess(VDI)
Alvorlig_infeksjon <- norvasPreprosess(Alvorlig_infeksjon)
Utredning <- norvasPreprosess(Utredning)
Labskjema <- norvasPreprosess(Labskjema)

Inklusjon <- Inklusjon[order(Inklusjon$InklusjonDato), ]
Inklusjon <- Inklusjon[match(unique(Inklusjon$Fødselsnummer), Inklusjon$Fødselsnummer), ]

Diagnoser <- Diagnoser[order(Diagnoser$Diagnose_Klinisk_Dato, decreasing = T), ]
Diagnoser <- Diagnoser[match(unique(Diagnoser$PasientGUID), Diagnoser$PasientGUID), ]

Inklusjon <- merge(Inklusjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr", "Diag_gr")],
                   by.x = 'SkjemaGUID', by.y = 'HovedskjemaGUID', all.x = T)
BVAS <- merge(BVAS, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
BVAS <- merge(BVAS, Inklusjon[, c('SkjemaGUID', "InklusjonDato")], by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID', all.x = T)
KERR <- merge(KERR, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
VDI <- merge(VDI, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
Medisiner <- merge(Medisiner, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
Alvorlig_infeksjon <- merge(Alvorlig_infeksjon, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)
Oppfolging <- merge(Oppfolging, Diagnoser[, c('HovedskjemaGUID', 'Diagnose_Klinisk_Dato', "Diag_gr_nr")], by = 'HovedskjemaGUID', all.x = T)

Inklusjon$Diagnose_ny <- NA
Inklusjon$Diagnose_ny[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) <= 90] <- 1
Inklusjon$Diagnose_ny[abs(difftime(Inklusjon$Diagnose_Klinisk_Dato, Inklusjon$InklusjonDato, units = 'days')) > 90] <- 0

Inklusjon$Fodselsdato <- personnr2fodselsdato(Inklusjon$Fødselsnummer)
Diagnoser <- merge(Diagnoser, Inklusjon[, c("SkjemaGUID", "Fodselsdato", "InklusjonDato")],
                   by.x = 'HovedskjemaGUID', by.y = 'SkjemaGUID')

Inklusjon$Inklusjonsaar <- as.numeric(format(Inklusjon$InklusjonDato, format = '%Y'))
Oppfolging$Oppfolgingsaar <- as.numeric(format(Oppfolging$OppfolgingsDato, format = '%Y'))
Inklusjon$Inklusjonsalder <- age(Inklusjon$Fodselsdato, Inklusjon$InklusjonDato)
Diagnoser$DiagnoseAlder <- age(Diagnoser$Fodselsdato, Diagnoser$Diagnose_Klinisk_Dato)

varvalg <- c('Inklusjonsalder', 'DiagnoseAlder', 'Diag_gr', 'Navn', 'ErMann', 'bvas_samlet', 'bvas_samlet_nyeste', 'KerrsKriterierScore', 'KerrsKriterierScore_nyeste',
             'Legemiddelgruppe', 'LegemiddelGenerisk', 'VdiTotal', 'Sykdomsvurdering', 'Sykdomsvurdering_nyeste',
             'AntallInfeksjoner', 'AntallInfeksjoner_kummulativ', 'tid_symp_diagnose', 'tid_til_remisjon')
names(varvalg) <- c('Inklusjonsalder', 'Diagnosealder', 'Diagnosegruppe', 'Diagnoser', 'Kjønn', 'BVAS', 'BVAS (kun siste registrering)', 'KerrsKriterierScore', 'KerrsKriterierScore (kun siste registrering)',
                    'Legemiddelgruppe', 'Legemiddel', 'Vdi-skår', 'Sykdomsvurdering', 'Sykdomsvurdering (kun siste registrering)',
                    'Antall infeksjoner', 'Sum antall infeksjoner', 'Tid fra symptom til diagnose', 'Tid til remisjon')
varvalg_andel <- c('Andel_LVV', 'Andel_ANCA', 'Andel_remisjon', 'erMann')

######################################################################

# Define UI for application
ui <- navbarPage(title = "RAPPORTEKET NORVAS", theme = "bootstrap.css",
                 tabPanel("Fordelingsfigurer",
                          # sidebarLayout(
                          sidebarPanel(
                            shinyjs::useShinyjs(),
                            selectInput(inputId = "valgtVar", label = "Velg variabel",
                                        choices = varvalg),
                            dateRangeInput(inputId="datovalg", label = "Dato fra og til", min = '1920-01-01',
                                           max = Sys.Date(), start  = '2010-01-01', end = Sys.Date(), language = "nb", separator = " til "),
                            selectInput(inputId = "enhetsUtvalg", label = "Kjør rapport for",
                                        choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1, 'Egen avd.'=2)),
                            selectInput(inputId = "diag_gruppe", label = "Diagnosegruppe",
                                        choices = c('Alle'=99, 'Storkarsvaskulitt'=1, 'ANCA assosiert vaskulitt'=2,
                                                    'Andre'=3)),
                            selectInput(inputId = "erMann", label = "Kjønn",
                                        choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1))
                          ),
                          mainPanel(tabsetPanel(
                            tabPanel("Figur",
                                     plotOutput("Figur1", height="auto"), downloadButton("lastNedBilde", "Last ned bilde")
                                     ),
                            tabPanel("Tabell"#,
                                     # uiOutput("utvalg"),
                                     # tableOutput("Tabell1"), downloadButton("lastNed", "Last ned tabell")
                                     )
                          )
                          )
                 ),
                 tabPanel("Andelsfigurer",
                          # sidebarLayout(
                          sidebarPanel(
                            shinyjs::useShinyjs(),
                            selectInput(inputId = "valgtVar_andel", label = "Velg variabel",
                                        choices = varvalg_andel),
                            dateRangeInput(inputId="datovalg_andel", label = "Dato fra og til", min = '1920-01-01',
                                           max = Sys.Date(), start  = '2010-01-01', end = Sys.Date(), language = "nb", separator = " til "),
                            selectInput(inputId = "enhetsUtvalg_andel", label = "Kjør rapport for",
                                        choices = c('Hele landet'=0, 'Egen avd. mot landet forøvrig'=1, 'Egen avd.'=2)),
                            selectInput(inputId = "diag_gruppe_andel", label = "Diagnosegruppe",
                                        choices = c('Alle'=99, 'Storkarsvaskulitt'=1, 'ANCA assosiert vaskulitt'=2,
                                                    'Andre'=3)),
                            selectInput(inputId = "erMann_andel", label = "Kjønn",
                                        choices = c('Begge'=99, 'Kvinne'=0, 'Mann'=1))
                          ),
                          mainPanel(tabsetPanel(
                            tabPanel("Figur" #,
                                     #plotOutput("Figur_andel", height="auto"), downloadButton("lastNedBilde", "Last ned bilde")
                            ),
                            tabPanel("Tabell"#,
                                     # uiOutput("utvalg"),
                                     # tableOutput("Tabell1"), downloadButton("lastNed", "Last ned tabell")
                            )
                          )
                          )
                 )
)


server <- function(input, output, session) {


  reshID <- 601159

  output$Figur1 <- renderPlot({
    if (input$valgtVar %in% c('Inklusjonsalder', 'ErMann')) {
      RegData=Inklusjon
      datovar='InklusjonDato'
    }

    if (input$valgtVar %in% c('DiagnoseAlder', 'Navn', 'Diag_gr', 'tid_symp_diagnose')) {
      RegData=Diagnoser
      datovar='Diagnose_Klinisk_Dato'
    }

    if (input$valgtVar %in% c('Legemiddelgruppe', 'LegemiddelGenerisk')) {
      RegData=Medisiner
      datovar='StartDato'
    }

    if (input$valgtVar %in% c('bvas_samlet', 'bvas_samlet_nyeste', 'Sykdomsvurdering', 'Sykdomsvurdering_nyeste')) {
      RegData=BVAS
      datovar='BVAS_Dato'
    }

    if (input$valgtVar %in% c('tid_til_remisjon')) {
      RegData=BVAS
      datovar='InklusjonDato'
    }


    if (input$valgtVar %in% c('KerrsKriterierScore', 'KerrsKriterierScore_nyeste')) {
      RegData=KERR
      datovar='KerrsKriterier_Dato'
    }

    if (input$valgtVar %in% c('VdiTotal')) {
      RegData=VDI
      datovar='VDI_Dato'
    }

    if (input$valgtVar %in% c('AntallInfeksjoner', 'AntallInfeksjoner_kummulativ')) {
      RegData=Alvorlig_infeksjon
      datovar='SelvrapportertAlvorligInfeksjon_Registrert_Dato'
    }
    norvasFigAndeler(RegData=RegData, valgtVar=input$valgtVar, datoFra = input$datovalg[1], datoTil = input$datovalg[2],
                     erMann=99, outfile='', reshID=reshID, enhetsUtvalg = input$enhetsUtvalg, preprosess=F,
                     valgtShus=c(''),hentData=F, datovar=datovar, aldervar='PatientAge',
                     diag_gruppe=as.numeric(input$diag_gruppe))
  }, width = 700, height = 700)





}


# Run the application
shinyApp(ui = ui, server = server)
