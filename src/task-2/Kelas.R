library(shiny)
library(DT)

masterData <- read.csv2("./data/siswa.csv")

karyawanClass <- subset(masterData, masterData[3] == "Karyawan")[1:251, 4]
karyawanClass <- karyawanClass[!is.na(karyawanClass)]
regulerClass <- subset(masterData, masterData[3] == "Reguler")[1:251, 4]
regulerClass <- regulerClass[!is.na(regulerClass)]
internationalClass <- subset(masterData, masterData[3] == "Internasional")[1:251, 4]
internationalClass <- internationalClass[!is.na(internationalClass)]

karyawanClass

#max data need to calculate
maxData = 250



Label = c("Karyawan", "Reguler", "Internasional")
Frekwensi = c(length(karyawanClass),
              length(regulerClass),
              length(internationalClass))
Presentase = c(length(karyawanClass) * 100 / maxData,
               length(regulerClass) * 100 / maxData,
               length(internationalClass) * 100 / maxData)
RataRataIPK = c(
              round(mean(karyawanClass), digits = 1),
              round(mean(regulerClass), digits = 1),
              round(mean(internationalClass), digits = 1)
)


tableData <- data.frame(Label, Frekwensi, Presentase, RataRataIPK)


datatable(tableData, options= list(
  pageLength = 20
))
