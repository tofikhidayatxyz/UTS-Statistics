library(sjPlot)
library(ggrepel)
library(sjmisc)
library(cowplot)

masterData <- read.csv2("./data/siswa.csv")


colorsData = c(
  "#f0c929",
  "#ff8882",
  "#29bb89",
  "#93329e",
  "#f5c0c0",
  "#7868e6"
)


extractData <- function(from, to, col) {
  pointData <- strtoi(masterData[from:to, col])
  pointData <- pointData[!is.na(pointData)]
  return(pointData)
}


motivation <- data.frame(
  P1 = extractData(1, 251, 5),
  P2 = extractData(1, 251, 6),
  P3 = extractData(1, 251, 7),
  P4 = extractData(1, 251, 8),
  P5 = extractData(1, 251, 9),
  P6 = extractData(1, 251, 10),
  P7 = extractData(1, 251, 11),
  P8 = extractData(1, 251, 12),
  P9 = extractData(1, 251, 13),
  P10 = extractData(1, 251, 14),
  P11 = extractData(1, 251, 15),
  P12 = extractData(1, 251, 16),
  P13 = extractData(1, 251, 17),
  P14 = extractData(1, 251, 18),
  P15 = extractData(1, 251, 19)
)

motivationLabel = c("Sangat Tidak Sesuai", "Tidak Sesuai", "Kurang Sesuai", "Sesuai", "Sangat Sesuai", "-")

plot_likert(motivation,
            legend.labels = motivationLabel,
            reverse.scale = TRUE,
            geom.colors = colorsData,
            legend.pos = "all",
            title = "Motivasi belajar",
            group.legend.options = list(nrow = 1))

