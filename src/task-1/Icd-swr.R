library(rjson)

jsonData <- fromJSON(file = "./data/tar.json")

maxSample = 154
maxDataNum = 250

sampleResultSWR =  c() # Can duplidate


currentLoop = 1
while (length(sampleResultSWR) < maxSample) {
  for(itm in jsonData) {
    if(length(sampleResultSWR) < maxSample) {
      currentNum = strtoi(substr(itm, currentLoop, currentLoop + 2))
      if(!is.na(currentNum)) {
        resultNum <- currentNum %% maxDataNum
        if(resultNum <= maxDataNum && resultNum > 0) {
          if(resultNum == 0) {
            sampleResultSWR <- append(sampleResultSWR, maxDataNum)     
          } else {
            sampleResultSWR <- append(sampleResultSWR, resultNum)     
          }
        } else if(currentNum <= 0) {
          if(!is.element(maxDataNum, sampleResultSWR)) {
            sampleResultSWR <- append(sampleResultSWR, maxDataNum)    
          }
        }
      }
    }
  }
  currentLoop = currentLoop + 1
}

print(sampleResultSWR)

jsonResult <- toJSON(sampleResultSWR)

write(jsonResult, "./data/swr.json")


