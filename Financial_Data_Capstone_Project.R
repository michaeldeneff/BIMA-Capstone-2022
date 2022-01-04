###### Financial Data #########

####### http://stock.tradingninja.com/how-to-manipulate-daily-stock-market-data-with-quantmod-r-package/
#merge(MSFT,PLAY,join = "inner") merges xts objects by date

library(stringr)
library(rvest)
library(quantmod)
library(dplyr)

SP500 = "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies" %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[1]') %>%
  html_table()

SP500 = SP500[[1]]
SP500 = SP500[,c(1,4)]

View(SP500)
# get all S&P500 index in a list
Stocks = vector("list", nrow(SP500)) 
for (i in 1:nrow(SP500)) {
  
  try({  Stocks[[i]] = getSymbols(SP500$Symbol[i],from = "2020-1-1", auto.assign = FALSE)
  })
  Sys.sleep(1)
} # takes ~20 min to run
#Stocks  = lapply(Stocks, tail)
databackup = Stocks
# delete NULL rows in SP500 and in Stocks list
yes_info = lapply(Stocks,is.null) == F
Stocks = Stocks[yes_info]
SP500 = SP500[yes_info,]

# delete Stocks that have smaller time frame
x = lapply(Stocks, nrow) 
x = which(lapply(Stocks,nrow) != max(range(x)))
x = as.numeric(x)
Stocks = Stocks[-x]
SP500 = SP500[-x,]


# assign names to list items
names(Stocks) = SP500$Symbol

# create a Day.Change column
for (i in 1:length(Stocks)) {
  Day.Change = ((Stocks[[i]][,4] - Stocks[[i]][,1])/Stocks[[i]][,1])*100
  colnames(Day.Change) = "Day.Change"
  Stocks[[i]] = cbind(Stocks[[i]],Day.Change)
}

# create a Week.Change column (stock is open ~5 days a week)
for (i in 1:length(Stocks)) {
  Stocks[[i]]$Week.Change = ((Stocks[[i]][,6]-lag(as.numeric(Stocks[[i]][,6]),5))/Stocks[[i]][,6])*100
  Stocks[[i]] = Stocks[[i]][6:nrow(Stocks[[i]]),]
}

#get all industries (GSCI) 
GICS = unique(SP500$`GICS Sector`)
x = vector("list",length(GICS))
names(x) = GICS
GICS = x
rm(x)
for (i in 1:length(GICS)) {
  
  GICS[[i]] =  SP500$`GICS Sector` == names(GICS[i])
  
}  #insert ticker to their respective industries in the list



# Industry average day change
for (i in 1:length(GICS)) {
  industry_columns = Stocks[GICS[[i]]]
  industry_columns = as.data.frame(industry_columns)
  
  target_columns = data.frame(industry_columns[7])
  
  for (d in 2:(ncol(industry_columns)/8)) {
    target_columns = cbind(target_columns,industry_columns[(8*d)-1])
  }
  
  Industry.Average = as.numeric(rowMeans(target_columns))
  pos_pos = which(GICS[[i]] %in% TRUE)
  
  for (s in pos_pos){
    Stocks[[s]] = cbind(Stocks[[s]],Industry.Average)
  }
  
}

# Industry Average week change
for (i in 1:length(GICS)) {
  industry_columns = Stocks[GICS[[i]]]
  industry_columns = as.data.frame(industry_columns)
  
  target_columns = data.frame(industry_columns[8])
  
  for (d in 2:(ncol(industry_columns)/9)) {
    target_columns = cbind(target_columns,industry_columns[(9*d)-1])
  }
  
  Industry.Week.Average = as.numeric(rowMeans(target_columns))
  pos_pos = which(GICS[[i]] %in% TRUE)
  
  for (s in pos_pos){
    Stocks[[s]] = cbind(Stocks[[s]],Industry.Week.Average)
  }
  
}

### capstone

### extract Excel with dates
x = data.frame(1)
names(x) = "day"
x = as.data.frame(Stocks[[1]][,0])
write.csv(x,"C:\\Users\\david\\OneDrive\\Desktop\\data.csv")
day = read.csv("C:\\Users\\david\\OneDrive\\Desktop\\data.csv",header=FALSE)
day = day[,1]
day = substring(day,1,10)
day = as.data.frame(day)



### find and extract excel with top 10 companies

x = numeric(0)
for (i in 1:length(Stocks)) {
  y = tail(Stocks[[i]][,7],5)
  y = mean(y$Day.Change)
  x[[i]] = y
  
}

names(x) = SP500$Symbol
x = tail(sort(x),10)
tail(Stocks[["HPE"]])

for (i in 1:length(x)) {
  mydata = names(Stocks) == names(x)[i]
  mydata = Stocks[mydata]
  write.csv(mydata,paste("C:\\Users\\david\\OneDrive\\Desktop\\",names(x)[i],".csv",sep=""))
  
}

newstocks = vector("list", length(x))

for (i in 1:length(x)) {
  gainer =read.csv(paste("C:\\Users\\david\\OneDrive\\Desktop\\",names(x)[i],".csv",sep=""))
  gainer = tail(gainer,5)
  gainer = gainer[,c(1,8)]
  
  newstocks[[i]] = gainer
}
names(newstocks) = names(x)
newstocks

View(Stocks[["ABMD"]])
write.csv(newstocks,"C:\\Users\\david\\OneDrive\\Desktop\\gainers.csv",sep="")