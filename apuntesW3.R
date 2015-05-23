
###############
# question 1
###############
library(xlsx)
library(sqldf)
library(data.table)


if(!file.exists("data")){
  dir.create("data")
}

fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
#download.file(fileUrl, destfile="./data/datos.csv", method="curl")
download.file(fileUrl, destfile="./data/datos.csv")
dateDownloaded = date()

datos = read.csv("./data/datos.csv")

datos[datos$ACR == 3]

acres <- datos$ACR == 3
products <- datos$AGS == 6

agricultureLogical = acres & products

which(agricultureLogical)

datos2 = datos[agricultureLogical,]

#evitamos valores vacios:
datos3 = datos[which(agricultureLogical),]

#ordenamos variables
acres2 = sort(datos$ACR)
acres3 = sort(datos$ACR, decreasing=TRUE)

#ordenamos el data frame entero en función de la variable ACR:
datos4 = datos[order(datos$ACR), ]

#calculamos valores distintos
unique(acres2)

#libreria para ordenar:
library(plyr)

datos5 = arrange(datos, ACR)

#añadimos nueva variable:
datos6 = cbind(datos, rnorm(dim(datos)[1]))


###############
# question 2
###############
library(jpeg)

if(!file.exists("data")){
  dir.create("data")
}

fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fjeff.jpg"
#download.file(fileUrl, destfile="./data/datos.csv", method="curl")
download.file(fileUrl, destfile="./data/foto.jpg", mode = "wb") #lo descargamos como binario
dateDownloaded = date()

foto = readJPEG("./data/foto.jpg", native = TRUE)

probs = c(0.3, 0.8)
quantile(foto, probs)



###############
# question 3
###############

if(!file.exists("data")){
  dir.create("data")
}

fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
download.file(fileUrl, destfile="./data/GDP.csv")
dateDownloaded = date()

fileUrl = "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
download.file(fileUrl, destfile="./data/edu_data.csv")
dateDownloaded = date()

datos_gdp = read.csv("./data/GDP.csv", skip=4, nrows=215)
datos_edu = read.csv("./data/edu_data.csv")

setnames(datos_gdp, c("X", "X.1", "X.3", "X.4"), 
         c("CountryCode", "rankingGDP", "Long.Name", "gdp"))

union = merge(datos_gdp, datos_edu, all=TRUE, by = c("CountryCode"))
enlaces = unique(union$rankingGDP)
enlaces2 = sum(!is.na(enlaces))

union[order(union$rankingGDP, decreasing = TRUE), list(CountryCode, Long.Name.x, Long.Name.y, rankingGDP, gdp)][13]

union_desc = union[order(union$rankingGDP, decreasing = TRUE), ]
union_desc[13,]


###############
# question 4
###############
#union[, mean(union$rankingGDP, na.rm = TRUE, by = Income.Group)]
union[, mean(union$rankingGDP, na.rm = TRUE)]

alto_income = union[union$Income.Group == "High income: nonOECD",]
mean(alto_income$rankingGDP, na.rm=TRUE)

bajo_income = union[union$Income.Group == "High income: OECD",]
mean(bajo_income$rankingGDP, na.rm=TRUE)


###############
# question 5
###############
prob = c(0.2, 0.4, 0.6, 0.8, 1)

quantile(union$rankingGDP, prob, na.rm=TRUE)

mayor_ranking = union[union$rankingGDP<=38.8 & union$Income.Group=="Lower middle income", ]

breaks <- quantile(union$rankingGDP, probs = seq(0, 1, 0.2), na.rm = TRUE)
union$quantileGDP <- cut(union$rankingGDP, breaks = breaks)
lower_income = union[union$Income.Group == "Lower middle income", ]

lower_income2 = lower_income[order(lower_income$quantileGDP), ]



###############
# SUMMARIZING DATA...
###############
if(!file.exists("data")){
  dir.create("data")
}

fileUrl = "https://data.baltimorecity.gov/api/views/k5ry-ef3g/rows.csv?accessType=DOWNLOAD"
#download.file(fileUrl, destfile="./data/restaurantes.csv", method="curl")
download.file(fileUrl, destfile="./data/restaurantes.csv")

datos = read.csv("./data/restaurantes.csv")

#el principio y el final del data.frame
head(datos, n=7)
tail(datos, n=7)

#resumen del data.frame:
summary(datos)

#mas información variables:
str(datos)

#make table:
table(datos$zipCode, useNA="ifany")
table(datos$councilDistrict, datos$zipCode)

#check missing values:
sum(is.na(datos$councilDistrict))
any(is.na(datos$councilDistrict))
all(datos$councilDistrict > 0)
all(datos$zipCode > 0)

#row and columns sums:
colSums(is.na(datos))
all(colSums(is.na(datos)) == 0)

#seleccionar valores específicos:
table(datos$zipCode %in% c("21212"))

sub_datos = datos[datos$zipCode %in% c("21212"), ]

#cross tabs:
data(UCBAdmissions)
DF = as.data.frame(UCBAdmissions)
summary(DF)

xt = xtabs(Freq ~ Gender + Admit, data=DF)
xt

#flat tables
ftable(xt)

#espacio que ocupa un data set:
object.size(datos)
print(object.size(datos), units="Mb")



###############
# CREATING NEW VARIABLES
###############

#creando secuencias:
s1 = seq(1, 10, by=2)
s2 = seq(1, 10, length=3)

#nueva variable:
datos$nearMe = datos$neighborhood %in% c("Homeland")

datos$errorZIP = ifelse(datos$zipCode < 0, TRUE, FALSE)
table(datos$errorZIP)

#creando grupos:
datos$gruposZIP = cut(datos$zipCode, breaks=quantile(datos$zipCode))
table(datos$gruposZIP)
table(datos$zipCode, datos$gruposZIP)

#libreria que facilita la creación de grupos:
library(Hmisc)
datos$gruposZIP2 = cut2(datos$zipCode, g=6)
table(datos$gruposZIP2)

#creating factor variables:
datos$factorzip = factor(datos$zipCode)
class(datos$factorzip)


###############
# RESHAPING DATA
###############
library(reshape2)
head(mtcars)

#casting
dcast()

library(dplyr)



