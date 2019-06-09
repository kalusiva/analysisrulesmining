convert_to_basket<- function(olddata) {
  newdata <- ""
  for (row in 1:nrow(olddata)) {
    if (row !=1) {
      newdata <- paste0(newdata, "\n")}
    newdata <- paste0(newdata, row,",")
    for (col in 2:ncol(olddata)) {
     
      if(olddata[row,col] == 0 || olddata[row,col] == 1) {
        if (olddata[row,col] == 1) {
          if (col !=2) {
            newdata <- paste0(newdata, ",")
          }
          newdata <- paste0(newdata, colnames(olddata)[col])
        }
      }else {
        if (col !=2) {
          newdata <- paste0(newdata, ",")
        }
        newdata <- paste0(newdata, olddata[row,col])
      }
    }
  }
  write(newdata,"newdata.csv")
  return (newdata)
}

data = read.csv(file="inputdata.csv", sep="," , header = TRUE)
mybasketdata = convert_to_basket(data)
basketdata = read.transactions("newdata.csv",header = FALSE, format="basket",cols=1, sep=",")
rules = apriori(basketdata,parameter = list(supp=.3, conf = 0.6, target="rules"))
inspect(rules[1:20])
itemset = eclat(basketdata, parameter = list(supp = 0.3))
inspect(itemset[1:10])
