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
        if(currentNum <= maxDataNum && currentNum > 0) {
          sampleResultSWR <- append(sampleResultSWR, currentNum)   
        } else if(currentNum <= 0) {
          sampleResultSWR <- append(sampleResultSWR, maxDataNum)   
        }
      }
    }
  }
  currentLoop = currentLoop + 1
}

print(sampleResultSWR)
write(jsonData, "./data/swr.json")