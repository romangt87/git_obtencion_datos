
###############
# question 2
###############

library(xlsx)
library(sqldf)
library(data.table)


if(!file.exists("data")){
  dir.create("data")
}

fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06pid.csv"
#download.file(fileUrl, destfile="./data/datos.csv", method="curl")
download.file(fileUrl, destfile="./data/datos.csv")
dateDownloaded = date()

datos = read.csv("./data/datos.csv")

#convertimos datos en un objeto R:
acs = fread("./data/datos.csv")
