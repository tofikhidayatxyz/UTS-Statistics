install.packages(rjson)

library(rjson)

baseSample <- sample(00000:99999, 250)


sampleString = c()

for(itm in baseSample) {
  insertZero <- 5 - nchar(toString(itm))
  
  zeroText = gsub(", ", "", toString(rep("0", insertZero)))
  sampleString <- append(sampleString,
                         paste(zeroText, itm, sep = ""))
}

jsonData <- toJSON(sampleString)
print(sampleString)
write(jsonData, "./data/tar.json")