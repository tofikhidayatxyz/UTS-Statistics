library(ggplot2)

masterData <- read.csv2("./data/siswa.csv")


colorsData = c(
  "#f0c929",
  "#ff8882",
  "#29bb89",
  "#93329e",
  "#f5c0c0",
  "#7868e6"
)



karyawanData <- subset(masterData, masterData[3] == "Karyawan")[1:251, 4]
karyawanData <- karyawanData[!is.na(karyawanData)]

internasionalData <- subset(masterData, masterData[3] == "Internasional")[1:251, 4]
internasionalData <- internasionalData[!is.na(internasionalData)]

regulerData <- subset(masterData, masterData[3] == "Reguler")[1:251, 4]
regulerData <- regulerData[!is.na(regulerData)]

## box plot

plotData <- rbind.fill(
  data.frame(Karyawan = karyawanData),
  data.frame(Internasional = internasionalData),
  data.frame(Reguler = regulerData)
)

boxplot(plotData,
        col=colorsData[3:6],
        #notch = TRUE,
        main = "Boxplot")


## Mean

overideName = c("Karyawan", "Internasional", "Reguler")

dataMean <- data.frame(Mean = c(round(mean(karyawanData),digits = 2),
                                round(mean(internasionalData), digits = 2),
                                round(mean(regulerData), digits=2)),
                       x = overideName)

ggplot(dataMean, aes(x = x, y = Mean)) + 
  geom_bar(stat = "identity",
           color = colorsData[1],
           fill = colorsData[1], 
           width = 0.7
  ) + 
  geom_text(aes(label = Mean, y = Mean), size = 3)


# Median
dataMedian <- data.frame(Median = c(round(median(karyawanData),digits = 2),
                                round(median(internasionalData), digits = 2),
                                round(median(regulerData), digits=2)),
                       x = overideName)

ggplot(dataMedian, aes(x = x, y = Median)) + 
  geom_bar(stat = "identity",
           color = colorsData[2],
           fill = colorsData[2], 
           width = 0.7
  ) +
  geom_text(aes(label = Median, y = Median), size = 3)

# devisiation
dataDevisiation <- data.frame(Devisiation = c(sd(karyawanData),
                                              sd(internasionalData),
                                              sd(regulerData)),
                         x = overideName)

ggplot(dataDevisiation, aes(x = x, y = Devisiation)) + 
  geom_bar(stat = "identity",
           color = colorsData[5],
           fill = colorsData[5], 
           width = 0.7
  ) +
  geom_text(aes(label = round(Devisiation, digits=2), y = Devisiation), size = 3)


# variant

dataVariant <- data.frame(variant = c(var(karyawanData),
                                              var(internasionalData),
                                              var(regulerData)),
                              x = overideName)

ggplot(dataVariant, aes(x = x, y = Variant)) + 
  geom_bar(stat = "identity",
           color = colorsData[5],
           fill = colorsData[5], 
           width = 0.7
  ) +
  geom_text(aes(label = round(Variant, digits=2), y = Variant), size = 3)


# range

dataRange <- data.frame(Range = c(range(karyawanData),
                                     range(internasionalData),
                                     range(regulerData)),
                           x = overideName)

ggplot(dataRange, aes(x = x, y = Range)) + 
  geom_linerange(stat = "identity",
                 color = colorsData[6],
                 fill = NA, 
                 width = 2,
                 aes(ymin = min(Range), ymax = max(Range))
  ) + geom_point(col=colorsData[1], size=6) + 
  geom_text(aes(label = Range, y = Range), size = 3)




## implement as table means etc

classData <- c("Karyawan", "Reguler", "Internasional")

meanData <- c()
rangeData <- c()
medianData <- c()
devisiationData <- c()
variantData <- c()


for (class in classData) {
  dataObj <- subset(masterData, masterData[3] == class)[1:251, 4]
  dataObj <- dataObj[!is.na(dataObj)]
  
  meanObj <- round(mean(dataObj), digits = 2)
  rangeObj <- range(dataObj)
  medianObj <- median(dataObj)
  sdObj <- round(sd(dataObj), digits = 2)
  variantObj <- round(var(dataObj), digits = 2)
  
  meanData <- append(meanData, meanObj)
  rangeData <- append(rangeData, paste(rangeObj[1], "-", rangeObj[2]))
  medianData <- append(medianData, medianObj)
  devisiationData <- append(devisiationData, sdObj)
  variantData <- append(variantData, variantObj)
}



plotData <- data.frame(
  Kelas = classData,
  RataRata = meanData,
  Range = rangeData,
  Median = medianData,
  Devisiation = devisiationData,
  Varian = variantData
)



datatable(plotData, options= list(
  pageLength = 20
))




