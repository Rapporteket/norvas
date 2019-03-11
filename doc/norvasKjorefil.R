setwd("C:/GIT/norvas/doc/")
rm(list = ls())

lagpdf <- T

Inklusjon <- read.table('I:/norvas/DataDump_Prod_Inklusjonskjema_2017-11-20.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Diagnoser <- read.table('I:/norvas/DataDump_Prod_DiagnoseSkjema_2017-11-20.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
Medisiner <- read.table('I:/norvas/DataDump_Prod_MedisineringSkjema_2017-11-20.csv', header=TRUE, sep=";",
                        stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')

Inklusjon$FormDate <- as.POSIXlt(Inklusjon$FormDate, format = "%d.%m.%Y")
Inklusjon$Importdato <- as.POSIXlt(Inklusjon$Importdato, format = "%d.%m.%Y")
Inklusjon$LastUpdate <- as.POSIXlt(Inklusjon$LastUpdate, format = "%d.%m.%Y")
Inklusjon$Inklusjon.dato <- as.POSIXlt(Inklusjon$Inklusjon.dato, format = "%d.%m.%Y")

##############################################################
########### Diagnoser  #####################################
##############################################################


antall <- sort(table(Diagnoser$Diagnose.term, useNA = 'ifany'), decreasing = T)
grtxt <- names(antall)

if (lagpdf) {
  outfile <- 'diagnoser.pdf'
} else {
  outfile <- ''
}

windows()

FigTypUt <- rapbase::figtype(outfile='', fargepalett='BlaaOff', pointsizePDF=12)
farger <- FigTypUt$farger
vmarg <- max(0, strwidth(grtxt, units='figure', cex=1)*.8)
par('fig'=c(vmarg, 1, 0, 1))

pos <- barplot(rev(as.numeric(antall)), horiz=TRUE, beside=TRUE, las=1, xlab='Antall', main='Diagnoser',
               col=farger[1], border='white', font.main=1, xpd=TRUE, xlim = c(0,max(antall)*1.2))	#
mtext(at=pos+0.05, text=rev(grtxt), side=2, las=1, cex=1, adj=1, line=0.25)
text(rev(as.numeric(antall)), pos, labels=rev(as.numeric(antall)), adj = 0)

if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}
# dev.off()

##############################################################
########### Alder og kjønn ###################################
##############################################################

aldersgr <- c(0, seq(19,79,by=10), 140)

# table(Inklusjon$PatientAge, useNA = 'ifany')
Inklusjon$aldersgr <- cut(Inklusjon$PatientAge, breaks = aldersgr, include.lowest = T)

antall <- table(Inklusjon$aldersgr, useNA = 'ifany')
grtxt <- c('<20', '20-29', '30-39', '40-49', '50-59', '60-69', '70-79', '>80')

if (lagpdf) {
  outfile <- 'aldersfordeling.pdf'
} else {
  outfile <- ''
}


windows()

FigTypUt <- rapbase::figtype(outfile='', fargepalett='BlaaOff', pointsizePDF=12)
farger <- FigTypUt$farger
# vmarg <- max(0, strwidth(grtxt, units='figure', cex=1)*.8)
# par('fig'=c(vmarg, 1, 0, 1))

pos <- barplot(as.numeric(antall), horiz=F, beside=TRUE, las=1, xlab='Antall', main='Aldersfordeling',
               col=farger[1], border='white', font.main=1, xpd=TRUE, ylim = c(0,max(antall)*1.2))	#
# mtext(at=pos+0.05, text=rev(grtxt), side=2, las=1, cex=1, adj=1, line=0.25)
mtext(at=pos, grtxt, side=1, las=1, cex=1, adj=0.5, line=0.5)
text(pos, as.numeric(antall), labels=as.numeric(antall), adj = c(.5, 0))
# text(rev(as.numeric(antall)), pos, labels=rev(as.numeric(antall)), adj = 0)

if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}
# dev.off()


table(Inklusjon$PatientGender, useNA = 'ifany')
# table(Medisiner$Legemiddel, useNA = 'ifany')







##############################################################
########### Medikamenter #####################################
##############################################################
Medisiner <- Medisiner[Medisiner$Legemiddel != "Folsyre", ]
Medisiner <- Medisiner[Medisiner$Legemiddel != "Salazopyrin", ]


handelsnavn <- c("Arava", "CellCept", "Cimzia", "Enbrel", "Everolimus", "Folsyre",
                 "HumantImmunoglobulinGiv", "HumantImmunoglobulinGsc", "Imurel", "Inflektra",
                 "Lodotra", "MabThera", "MethotrexateImSc", "Prednisolon", "Remicade", "Remsima",
                 "RoActemraInfusjon", "Sandimmun", "SendoxanIv", "Tacrolimus", "AnnetImportert")
kode <- c(15, 16, 1, 2, 31, 17, 36, 37, 18, 5, 19, 7, 20, 23, 10, 11, 12, 25, 26, 34, 999)

kobl_hnavn_kode <- data.frame(kode, handelsnavn)

generisknavn <- c("Cetolizumab", "Etanercept", "Etanercept", "Adalimumab", "Infliximab", "Infliximab",
  "Infliximab", "Golimumab", "Anakinra", "Canakinumab", "Tocilizumab", "Tocilizumab",
  "Ustekinumab", "Sekunikumab", "Rituximab", "Abatacept", "Abatacept", "Leflunomid",
  "Mycofenolatmofetil", "Mycofenolsyre", "Azatioprin", "Methotrexat", "Methotrexat",
  "Hydroxychlochin", "Sulfasalazin", "Everolimus", "Sirolimus", "Tallidomid", "Ciclosporin A",
  "Tacrolimus", "Prednison", "Prednisolon", "Methylprednisolon", "Methylprednisolon",
  "Humant immunglobulin", "Humant immunglobulin", "Cyclofosfamid", "Cyclofosfamid", "Folsyre", "Methylprednisolon ")

kode2 <- c(1, 2, 29, 3, 5, 10, 11, 14, 6, 4, 12, 13, 28, 30, 7, 8, 9, 15, 16, 32,
           18, 20, 21, 22, 24, 31, 33, 35, 25, 34, 19, 23, 38, 39, 36, 37, 26, 27, 17, 999)

kobl_gennavn_kode <- data.frame(kode2, generisknavn)


Medisiner$LegemiddelKode <- kobl_hnavn_kode$kode[match(Medisiner$Legemiddel, kobl_hnavn_kode$handelsnavn)]

Medisiner$LegemiddelGenerisk <- kobl_gennavn_kode$generisknavn[match(Medisiner$LegemiddelKode, kobl_gennavn_kode$kode2)]

antall <- sort(table(as.character(Medisiner$LegemiddelGenerisk), useNA = 'ifany'), decreasing = T)
grtxt <- names(antall)

if (lagpdf) {
  outfile <- 'medisiner.pdf'
} else {
  outfile <- ''
}
windows()

FigTypUt <- rapbase::figtype(outfile='', fargepalett='BlaaOff', pointsizePDF=12)
farger <- FigTypUt$farger
vmarg <- max(0, strwidth(grtxt, units='figure', cex=1)*.8)
par('fig'=c(vmarg, 1, 0, 1))

pos <- barplot(rev(as.numeric(antall)), horiz=TRUE, beside=TRUE, las=1, xlab='Antall', main='Medisiner',
               col=farger[1], border='white', font.main=1, xpd=TRUE, xlim = c(0,max(antall)*1.2))	#
mtext(at=pos+0.05, text=rev(grtxt), side=2, las=1, cex=1, adj=1, line=0.25)
text(rev(as.numeric(antall)), pos, labels=rev(as.numeric(antall)), adj = 0)

if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}
# dev.off()




Legemiddelgruppe <- c("Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                      "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                      "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                      "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                      "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                      "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                      "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                      "Biologiske legemidler \n (Rituximab ekskludert)", "Biologiske legemidler \n (Rituximab ekskludert)",
                      "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD",
                      "DMARD", "DMARD", "DMARD", "Kortikosteroider", "Kortikosteroider", "Kortikosteroider",
                      "Kortikosteroider", "Cytostatika", "Cytostatika", "Rituximab", "Immunglob.G", "Immunglob.G",
                      "Annet", "Kortikosteroider")

# kode3 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 28, 29, 30, 15, 16, 18, 20, 21, 22, 24, 25, 31, 32, 33, 34,
           # 35, 38, 39, 19, 23, 26, 27, 36, 37, 17, 999)
kode3 <- c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 28, 29, 30, 15, 16, 18, 20, 21, 22, 24, 25, 31, 32, 33, 34,
           35, 38, 39, 19, 23, 26, 27, 7, 36, 37, 17, 999)

kobl_gruppe_kode <- data.frame(kode3, Legemiddelgruppe)

Medisiner$Legemiddelgruppe <- kobl_gruppe_kode$Legemiddelgruppe[match(Medisiner$LegemiddelKode, kobl_gruppe_kode$kode3)]
Medisiner$Legemiddel[which(is.na(Medisiner$Legemiddelgruppe))]

antall <- sort(table(Medisiner$Legemiddelgruppe, useNA = 'ifany'), decreasing = T)
grtxt <- names(antall)

if (lagpdf) {
  outfile <- 'medisingruppe.pdf'
} else {
  outfile <- ''
}
windows()

FigTypUt <- rapbase::figtype(outfile='', fargepalett='BlaaOff', pointsizePDF=12)
farger <- FigTypUt$farger
vmarg <- max(0, strwidth(grtxt, units='figure', cex=1)*.8)
par('fig'=c(vmarg, 1, 0, 1))

pos <- barplot(rev(as.numeric(antall)), horiz=TRUE, beside=TRUE, las=1, xlab='Antall', main='Medisingrupper',
               col=farger[1], border='white', font.main=1, xpd=TRUE, xlim = c(0,max(antall)*1.2))	#
mtext(at=pos+0.05, text=rev(grtxt), side=2, las=1, cex=1, adj=1, line=0.25)
text(rev(as.numeric(antall)), pos, labels=rev(as.numeric(antall)), adj = 0)

if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}


# tapply(Medisiner$PasientGUID, Medisiner$Legemiddel, length)

antall <- sort(tapply(Medisiner$PasientGUID, as.character(Medisiner$LegemiddelGenerisk), function(x){length(unique(x))}), decreasing = T)
grtxt <- names(antall)

if (lagpdf) {
  outfile <- 'antall_pasienter_per_medisin.pdf'
} else {
  outfile <- ''
}
windows()

FigTypUt <- rapbase::figtype(outfile='', fargepalett='BlaaOff', pointsizePDF=12)
farger <- FigTypUt$farger
vmarg <- max(0, strwidth(grtxt, units='figure', cex=1)*.8)
par('fig'=c(vmarg, 1, 0, 1))

pos <- barplot(rev(as.numeric(antall)), horiz=TRUE, beside=TRUE, las=1, xlab='Antall', main='Antall pasienter per medisin',
               col=farger[1], border='white', font.main=1, xpd=TRUE, xlim = c(0,max(antall)*1.2))	#
mtext(at=pos+0.05, text=rev(grtxt), side=2, las=1, cex=1, adj=1, line=0.25)
text(rev(as.numeric(antall)), pos, labels=rev(as.numeric(antall)), adj = 0)

if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}


# tapply(Medisiner$PasientGUID, Medisiner$Legemiddel, length)

antall <- sort(tapply(Medisiner$PasientGUID, as.character(Medisiner$Legemiddelgruppe), function(x){length(unique(x))}), decreasing = T)
grtxt <- names(antall)

if (lagpdf) {
  outfile <- 'antall_pasienter_per_medisingruppe.pdf'
} else {
  outfile <- ''
}
windows()

FigTypUt <- rapbase::figtype(outfile='', fargepalett='BlaaOff', pointsizePDF=12)
farger <- FigTypUt$farger
vmarg <- max(0, strwidth(grtxt, units='figure', cex=1)*.8)
par('fig'=c(vmarg, 1, 0, 1))

pos <- barplot(rev(as.numeric(antall)), horiz=TRUE, beside=TRUE, las=1, xlab='Antall', main='Antall pasienter per medisingruppe',
               col=farger[1], border='white', font.main=1, xpd=TRUE, xlim = c(0,max(antall)*1.2))	#
mtext(at=pos+0.05, text=rev(grtxt), side=2, las=1, cex=1, adj=1, line=0.25)
text(rev(as.numeric(antall)), pos, labels=rev(as.numeric(antall)), adj = 0)

if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}




# Legemiddelgruppe <- c("Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler",
#                       "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler",
#                       "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler",
#                       "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler",
#                       "Biologiske legemidler", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD",
#                       "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "Methylprednisolon", "Methylprednisolon", "Prednisolon",
#                       "Prednisolon", "Cyclofosfamid", "Cyclofosfamid", "Rituximab", "Immunglob.G", "Immunglob.G", "Annet", "AnnetImportert")
#
# kode3 <- c(1, 2, 3, 4, 5, 6, 8, 9, 10, 11, 12, 13, 14, 28, 29, 30, 15, 16, 18, 20, 21, 22, 24, 25, 31, 32, 33, 34,
#            35, 38, 39, 19, 23, 26, 27, 7, 36, 37, 17, 999)
#
# kobl_gruppe_kode <- data.frame(kode3, Legemiddelgruppe)
#
# Medisiner$Legemiddelgruppe <- kobl_gruppe_kode$Legemiddelgruppe[match(Medisiner$LegemiddelKode, kobl_gruppe_kode$kode3)]
# Medisiner$Legemiddel[which(is.na(Medisiner$Legemiddelgruppe))]
#
# antall <- sort(table(Medisiner$Legemiddelgruppe, useNA = 'ifany'), decreasing = T)
# grtxt <- names(antall)
#
# if (lagpdf) {
#   outfile <- 'medisingruppe3.pdf'
# } else {
#   outfile <- ''
# }
# windows()
#
# FigTypUt <- rapbase::figtype(outfile='', fargepalett='BlaaOff', pointsizePDF=12)
# farger <- FigTypUt$farger
# vmarg <- max(0, strwidth(grtxt, units='figure', cex=1)*.8)
# par('fig'=c(vmarg, 1, 0, 1))
#
# pos <- barplot(rev(as.numeric(antall)), horiz=TRUE, beside=TRUE, las=1, xlab='Antall', main='Medisingrupper',
#                col=farger[1], border='white', font.main=1, xpd=TRUE, xlim = c(0,max(antall)*1.2))	#
# mtext(at=pos+0.05, text=rev(grtxt), side=2, las=1, cex=1, adj=1, line=0.25)
# text(rev(as.numeric(antall)), pos, labels=rev(as.numeric(antall)), adj = 0)
#
# if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}
#
#
# Legemiddelgruppe <- c("Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler",
#                       "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler",
#                       "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler",
#                       "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler", "Biologiske legemidler",
#                       "Biologiske legemidler", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "DMARD",
#                       "DMARD", "DMARD", "DMARD", "DMARD", "DMARD", "Kortikosteroider", "Kortikosteroider", "Kortikosteroider",
#                       "Kortikosteroider", "Cytostatika", "Cytostatika", "Immunglob.G", "Immunglob.G", "Annet", "AnnetImportert")
#
# kode3 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 28, 29, 30, 15, 16, 18, 20, 21, 22, 24, 25, 31, 32, 33, 34,
#            35, 38, 39, 19, 23, 26, 27, 36, 37, 17, 999)
#
# kobl_gruppe_kode <- data.frame(kode3, Legemiddelgruppe)
#
# Medisiner$Legemiddelgruppe <- kobl_gruppe_kode$Legemiddelgruppe[match(Medisiner$LegemiddelKode, kobl_gruppe_kode$kode3)]
# Medisiner$Legemiddel[which(is.na(Medisiner$Legemiddelgruppe))]
#
# antall <- sort(table(Medisiner$Legemiddelgruppe, useNA = 'ifany'), decreasing = T)
# grtxt <- names(antall)
#
# if (lagpdf) {
#   outfile <- 'medisingruppe2.pdf'
# } else {
#   outfile <- ''
# }
# windows()
#
# FigTypUt <- rapbase::figtype(outfile='', fargepalett='BlaaOff', pointsizePDF=12)
# farger <- FigTypUt$farger
# vmarg <- max(0, strwidth(grtxt, units='figure', cex=1)*.8)
# par('fig'=c(vmarg, 1, 0, 1))
#
# pos <- barplot(rev(as.numeric(antall)), horiz=TRUE, beside=TRUE, las=1, xlab='Antall', main='Medisingrupper',
#                col=farger[1], border='white', font.main=1, xpd=TRUE, xlim = c(0,max(antall)*1.2))	#
# mtext(at=pos+0.05, text=rev(grtxt), side=2, las=1, cex=1, adj=1, line=0.25)
# text(rev(as.numeric(antall)), pos, labels=rev(as.numeric(antall)), adj = 0)
#
# if (outfile != '') {savePlot(outfile, type=substr(outfile, nchar(outfile)-2, nchar(outfile)))}

pasguid <- c("ef4371c3-729c-e711-80db-00155d0b480b" ,"38d794af-729c-e711-80db-00155d0b480b" ,
             "48ddfeb5-729c-e711-80db-00155d0b480b","36d794af-729c-e711-80db-00155d0b480b" ,"d3e3c8cb-709c-e711-80db-00155d0b480b")

Inklusjon$SkjemaGUID[match(pasguid, Inklusjon$PasientGUID)]

