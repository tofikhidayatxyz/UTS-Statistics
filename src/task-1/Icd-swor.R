library(rjson)

jsonData <- fromJSON(file = "./data/tar.json")

maxSample = 154
maxDataNum = 250

sampleResultSWOR =  c() # Canot duplidate


currentLoop = 1
while (length(sampleResultSWOR) < maxSample) {
  for(itm in jsonData) {
    if(length(sampleResultSWOR) < maxSample) {
      currentNum = strtoi(substr(itm, currentLoop, currentLoop + 2))
      if(!is.na(currentNum)) {
        if(currentNum <= maxDataNum && currentNum > 0) {
          if(!is.element(currentNum, sampleResultSWOR)) {
            sampleResultSWOR <- append(sampleResultSWOR, currentNum)    
          }
        } else if(currentNum <= 0) {
          if(!is.element(maxDataNum, sampleResultSWOR)) {
            sampleResultSWOR <- append(sampleResultSWR, maxDataNum)    
          }
        }
      }
    }
  }
  currentLoop = currentLoop + 1
}



print(sampleResultSWOR)

jsonResult <- toJSON(sampleResultSWOR)

write(jsonResult, "./data/swor.json")


