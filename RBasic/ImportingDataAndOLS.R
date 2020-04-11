##### Importing data and OLS estimation #####

# change directory and import data
getwd()
setwd("C:/Users/Heejoon/Desktop/R_Quant")
dir()


## Load data

# Importing a csv file


# �پ��� ����� ���� 
data2 <- read.csv("DataHousingPrice.csv")
Y <- cbind(PRICE)
X <- cbind(LOTSIZE, SQRFT, BDRMS)

attach(data2)   # R�� ����(?)
Y <- cbind(PRICE)
X <- cbind(LOTSIZE, SQRFT, BDRMS)


# Rbasic2������ ���� �Ʒ� ����� ����
data1 = fread("DataHousingPrice.csv")
Y <- cbind(PRICE)
X <- cbind(LOTSIZE, SQRFT, BDRMS)


# Orternatively, matrix�� ���� 
data = as.matrix(read.csv("DataHousingPrice.csv",header=T))

y = cbind(data[,1])   # or y = matrix(data[,1])
x = cbind(data[,2:4])
head(x)

fit0 = lm(y~x)
summary(fit0)


# import txt file
data22 = as.matrix(read.table("DataHousingPrice.txt",header=T))


# import excel data
# Install package : readxl
install.packages("readxl")
# Use installed package
library("readxl")

# import data from a xls file
dataxls = as.matrix(read_excel("DataHousingPrice.xls"))

# import data in the first sheet in a xlsx file
dataxlsx = as.matrix(read_excel("DataHousingPrice.xlsx",sheet=1)) 
# import data in the second sheet in a xlsx file
dataxlsx2 = as.matrix(read_excel("DataHousingPrice.xlsx",sheet=2))
?read_excel


## create new dataset without missing data if there are missing data

#Case1: no missing data

# NA�� �ִ����� Ȯ��
complete.cases(data)
# na.omit�� NA(missing data)�� ����
data = na.omit(data)  
n    = nrow(data)


# Case2: there are missing data
dataMissing = as.matrix(read.csv("DataHousingPriceMISSING.csv",header=T))

complete.cases(dataMissing)

# na.omit�� NA(missing data)�� ���� 
dataCleaned = na.omit(dataMissing)
nCleaned    = nrow(dataCleaned)

####################################
# OLS estimation using matrix
####################################

n    = nrow(data)
y = matrix(data[,1])
x = cbind(1,data[,2:4])
head(x)

invx = solve(t(x)%*%x)
olsb = invx%*%t(x)%*%y
olsb

#fit2 = lm(y~x-1) # regression without intercept
#summary(fit2)

## You may want to try log(y) instead of y
y=log(y)
