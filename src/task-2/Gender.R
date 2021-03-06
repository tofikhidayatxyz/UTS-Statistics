  library(shiny)
  library(DT)
  library(ggplot2)
  library(scales)
  
  masterData <- read.csv2("./data/siswa.csv")
  
  manData <- subset(masterData, masterData[2] == "L")[1:252, 4]
  manData <- manData[!is.na(manData)]
  womanData <- subset(masterData, masterData[2] == "P")[1:252, 4]
  womanData <- womanData[!is.na(womanData)]
  noAnswerData <- subset(masterData, masterData[2] == "Tidak Menjawab")[1:252, 4]
  noAnswerData <- noAnswerData[!is.na(noAnswerData)]
  
  
  #max data need to calculate
  maxData = 250
  
  
  Label = c("Laki Laki", "Perempuan", "Tidak Menjawab")
  Frekwensi = c(length(manData),
                length(womanData),
                length(noAnswerData))
  Presentase = c(length(manData) * 100 / maxData,
                 length(womanData) * 100 / maxData,
                 length(noAnswerData) * 100 / maxData)
  RataRataIPK = c(
                round(mean(manData), digits = 1),
                round(mean(womanData), digits = 1),
                round(mean(noAnswerData), digits = 1)
  )
  
  
  pieData <- data.frame(value = Frekwensi, group = c("Laki Laki", "Perempuan", "Tidak Menjawab"))
  
  ggplot(pieData, aes(x="", y=value, fill=paste(group, value * 100 / 250 , "%"))) +
    geom_bar(stat="identity", width=1, color="white") + 
    coord_polar("y", start=0)
    theme_void()
  
  
  tableData <- data.frame(Label, Frekwensi, Presentase, RataRataIPK)
  
  
  #datatable(tableData, options= list(
  #  pageLength = 20
  #))
