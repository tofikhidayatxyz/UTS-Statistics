library(ggplot2)
library(glue)
library(plyr)
library(DT)

masterData <- read.csv2("./data/siswa.csv")


colorsData = c(
  "#f0c929",
  "#ff8882",
  "#29bb89",
  "#93329e",
  "#f5c0c0",
  "#7868e6"
)



minColumnScore = 5
maxColumnScore = 19


dataLayer = c()

## extract data from function

extractData <- function(from, to, col) {
  return(unclass(factor(masterData[from:to, col])))
}

#print(summary(masterData[2:251, 5]))


#dataLayer <- data.frame(indexData = masterData[2:251, 5])

plotData <- rbind.fill(
  data.frame(Per1 = extractData(1, 251, 5)),
  data.frame(Per2 = extractData(1, 251, 6)),
  data.frame(Per3 = extractData(1, 251, 7)),
  data.frame(Per4 = extractData(1, 251, 8)),
  data.frame(Per5 = extractData(1, 251, 9)),
  data.frame(Per6 = extractData(1, 251, 10)),
  data.frame(Per7 = extractData(1, 251, 11)),
  data.frame(Per8 = extractData(1, 251, 12)),
  data.frame(Per9 = extractData(1, 251, 13)),
  data.frame(Per10 = extractData(1, 251, 14)),
  data.frame(Per11 = extractData(1, 251, 15)),
  data.frame(Per12 = extractData(1, 251, 16)),
  data.frame(Per13 = extractData(1, 251, 17)),
  data.frame(Per14 = extractData(1, 251, 18)),
  data.frame(Per15 = extractData(1, 251, 19))
)



plotObj = boxplot(plotData,
                  col=colorsData[3:6],
                  #notch = TRUE,
                  main = "Boxplot")


# mean
boxPlotData = c()
meanData = c()
rangeData = c()
medianData = c()
devisiation = c()
varian = c()


for(i in 5:19) {
  boxPlotData <- append(boxPlotData, extractData(1, 251, i))
  meanData <- append(meanData, mean(extractData(1, 251, i)))
  rangeData <- append(rangeData, range(extractData(1, 251, i)))
  medianData <- append(medianData, median(extractData(1, 251, i)))
  devisiation <- append(devisiation, sd(extractData(1, 251, i)))
  variant <- append(varian, var(extractData(1, 251, i)))
}

#barplot(meanData, main="Car Distribution",
#        xlab="Number of Gears", 
#        legend = rownames(c(1:15)),
#        beside = TRUE,
#        col = colorsData[2]
#       )

#print(meanData ? Nomong opo neng ?)

dataBoxPlot <- data.frame(Mean = boxPlotData, PerSection = c(1:15))
dataMean <- data.frame(RataRata = meanData, PerSection = c(1:15))
dataRange <- data.frame(Range = rangeData, PerSection = c(1:15))
dataMedian <- data.frame(Median = medianData, PerSection = c(1:15))
dataDevisiation <- data.frame(Devisiation = devisiation, PerSection = c(1:15))
dataVariant <- data.frame(Variant = variant, PerSection = c(1:15))

# mean
ggplot(dataMean, aes(x = PerSection, y = RataRata)) + 
  geom_bar(stat = "identity",
           color = colorsData[3],
           fill = colorsData[3], 
           width = 0.7
  ) + 
  geom_text(aes(label = RataRata, y = RataRata), size = 3)

# Median
ggplot(dataMedian, aes(x = PerSection, y = Median)) + 
  geom_bar(stat = "identity",
           color = colorsData[4],
           fill = colorsData[4], 
           width = 0.7
  ) +
  geom_text(aes(label = Median, y = Median), size = 3)

# devisiation
ggplot(dataDevisiation, aes(x = PerSection, y = Devisiation)) + 
  geom_bar(stat = "identity",
           color = colorsData[5],
           fill = colorsData[5], 
           width = 0.7
  ) +
  geom_text(aes(label = round(Devisiation, digits=2), y = Devisiation), size = 3)

#variant

ggplot(dataVariant, aes(x = PerSection, y = Variant)) + 
  geom_bar(stat = "identity",
           color = colorsData[5],
           fill = colorsData[5], 
           width = 0.7
  ) +
  geom_text(aes(label = round(Variant, digits=2), y = Variant), size = 3)

#range

ggplot(dataRange, aes(x = PerSection, y = Range)) + 
  geom_linerange(stat = "identity",
                 color = colorsData[6],
                 fill = NA, 
                 width = 2,
                 aes(ymin = min(Range), ymax = max(Range))
  ) + geom_point(col=colorsData[1], size=6) + 
  geom_text(aes(label = Range, y = Range), size = 3)

#rangeTable <- table(dataRange$Range, dataRange$PerSection)
# boxplot
#+geom_text(aes(label = round(Mean, digits=2), y = Mean), size = 3)

#print(range(extractData(2, 251, 7))[1])

#barplot(rangeData, main="Range",
#        xlab="Per Section", col= colorsData[1],
#        legend = rownames(rangeData), 
#        names =  c(1:15)
#        )


## boxplot total data 
plotData <- c(
  mean(extractData(1, 251, 6)),
  mean(extractData(1, 251, 6)),
  mean(extractData(1, 251, 7)),
  mean(extractData(1, 251, 8)),
  mean(extractData(1, 251, 9)),
  mean(extractData(1, 251, 10)),
  mean(extractData(1, 251, 11)),
  mean(extractData(1, 251, 12)),
  mean(extractData(1, 251, 13)),
  mean(extractData(1, 251, 14)),
  mean(extractData(1, 251, 15)),
  mean(extractData(1, 251, 16)),
  mean(extractData(1, 251, 17)),
  mean(extractData(1, 251, 18)),
  mean(extractData(1, 251, 19))
)

plotObj = boxplot(plotData,
                  col=colorsData[3:6],
                  #notch = TRUE,
                  main = "Boxplot")



