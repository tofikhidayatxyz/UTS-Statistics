library(DT)

masterData <- read.csv2("./data/siswa.csv")

baseValue <- masterData[1:251, 4]

baseValue <- baseValue[!is.na(baseValue)] 

alpha = 1/10

rangeData = max(baseValue) - min(baseValue)
lengthOfClass = 1 + 3.3 * log(length(baseValue))
maxLengthClass = round(rangeData / lengthOfClass, digits = 2)

print(alpha)

intervalRange = c(floor(min(baseValue)))

agregation = c()
valueOfAgregate = c()
dataValues = c()
medianValues = c()
calculatedValues = c()


while(intervalRange[length(intervalRange)] < max(baseValue)) {
  intervalRange <- append(intervalRange,
                          round((intervalRange[length(intervalRange)] + maxLengthClass + alpha), digits=2))
}


agr <- 0
loopIndex <- 0


obj <- c()
arg <- c()
for (ub in unique(baseValue)) {
  arg <- append(arg, length(baseValue[baseValue == ub]))
  obj <- append(obj, baseValue[baseValue == ub])
}



for (itm in intervalRange) {
  loopIndex <- loopIndex + 1
  maxRange = itm + maxLengthClass + alpha - alpha / 100
  minRange = itm
  agregate = baseValue[baseValue >= minRange & baseValue < maxRange]
  agr <- agr + length(agregate)
  
  if(length(agregate) > 0) {
    agregation <- append(agregation,
                         paste(itm, " - ", itm + maxLengthClass + alpha),
                         after = length(agregation)) 
    
    valueOfAgregate <- append(valueOfAgregate,
                              length(agregate),
                              after = length(valueOfAgregate))
    
    dataValues <- append(dataValues,
                         toString(sort.int(agregate))
    )
    
    medianValues <- append(medianValues,
                           median(agregate)
    )
    
    calculatedValues <- append(
      calculatedValues,
      median(agregate) * length(agregate)
    ) 
  }
}

tableData <- data.frame(Interval = agregation,
                        Median = medianValues,
                        Jumlah = valueOfAgregate,
                        Total = calculatedValues,
                        Data_Sebaran = dataValues
)

print(agr)

datatable(tableData, options= list(
  pageLength = 20
))