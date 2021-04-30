library(shiny)
library(DT)

tableTemplate <- "<table class='display'> <thead> <tr>"

tableTemplate <- paste(tableTemplate, paste("<td> No </td>"))
tableTemplate <- paste(tableTemplate, paste("<td> Consumer </td>"))

for( i in 1:maxRange) {
  tableTemplate <- paste(tableTemplate, paste("<td colspan='5'> Per ", i , "</td>"))
}

tableTemplate <- paste(tableTemplate, "</tr></tr>")

tableTemplate <- paste(tableTemplate, paste("<td></td>"))

for( i in 1:maxRange) {
  print(i)
  tableTemplate <- paste(tableTemplate, paste("<td>STS</td>"))
  tableTemplate <- paste(tableTemplate, paste("<td>TS</td>"))
  tableTemplate <- paste(tableTemplate, paste("<td>KS</td>"))
  tableTemplate <- paste(tableTemplate, paste("<td>S</td>"))
  tableTemplate <- paste(tableTemplate, paste("<td>SS</td>"))
}

tableTemplate <- paste(tableTemplate, "</tr></thead></table>")






datatable(iris[1:20, c(5, 1:4)], container = tableTemplate)