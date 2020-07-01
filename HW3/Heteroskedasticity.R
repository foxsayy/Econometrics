# Heteroskedasticity, chapter 13

getwd()
setwd("E:/0퀀트입문/R_Quant")
dir()

Housing <- read.csv("DataHousingPrice.csv")

install.packages("car")
install.packages("sandwich")
install.packages("lmtest")
library(car)
library(sandwich)
library(lmtest)


ols1 = lm(PRICE~LOTSIZE+SQRFT+BDRMS,data=Housing)
summary(ols1)


#1. Breusch-Pagan test for heteroskedasticity
?bptest
bptest(ols1,~LOTSIZE+SQRFT+BDRMS, data = Housing)


uhat = resid(ols1)
#or uhat = ols1$residuals
aux = lm(I(uhat^2)~LOTSIZE+SQRFT+BDRMS,data=Housing) 
n = nrow(Housing)
n*summary(aux)$r.squared

# Note that you need I() for a squared explanatory variable 
lm(PRICE~I(LOTSIZE^2), data = Housing)
lm(PRICE~LOTSIZE^2, data = Housing)
lm(PRICE~LOTSIZE, data = Housing)


# Taking logs often helps to secure homoscedasticity

ols2 = lm(log(PRICE)~log(LOTSIZE)+log(SQRFT)+BDRMS,data=Housing)

# Conduct the Breusch-Pagan test for heteroskedasticity







bptest(ols2,~log(LOTSIZE)+log(SQRFT)+BDRMS,data=Housing)
summary(ols2)


#2. Heteroskedasticity robust standard error

?vcovHC
vcovHC(ols1)  
#or vcovHC(ols1,type="HC3")

?coeftest
coeftest(ols1,vcov=vcovHC)
#or coeftest(ols1,vcov=vcovHC,type="HC3")

# compare this with coeftest(ols1)


#3. Weighted Least Squares EStimation

set.seed(101)
n = 100
x1 = rnorm(n)
x2 = rnorm(n)
u = x1*rnorm(n)
plot(u~x1)

y = 1 + x1 - x2 + u
ols3=lm(y~x1+x2)
summary(ols3)

#R parameterizes the weights as inversely proportional to the variances.
#u = x1*rnorm(n)  var(u)=x1^2

wls = lm(y~x1+x2,weights=1/(x1^2))
summary(wls)


#Feasible GLS for unknown form of heteroskedasticity

ols1 = lm(PRICE~LOTSIZE+SQRFT+BDRMS,data=Housing)

uhat = resid(ols1)
olsuhat = lm(I(log(uhat^2))~LOTSIZE+SQRFT+BDRMS,data=Housing)

h = exp(fitted(olsuhat))

fgls = lm(PRICE~LOTSIZE+SQRFT+BDRMS,weights=1/h,data=Housing)
coeftest(fgls)



#####
# 예제 13.5
Death <- read.csv("deathrate.csv")
ols4 = lm(deathrate~drink+smoke+aged+vehipc+factor(year),data=Death)
summary(ols4)



# Test for heteroskedasticity

bptest(ols4,~drink+smoke+aged+vehipc+factor(year),data=Death)


# heteroskedasticity robust standard error 
coeftest(ols4,vcov=vcovHC)



# FGLS for unknonwn
uhat2 = resid(ols4)
olsuhat2 = lm(I(log(uhat2^2))~drink+smoke+aged+vehipc+factor(year),data=Death)

h2 = exp(fitted(olsuhat2))

fgls2 = lm(deathrate~drink+smoke+aged+vehipc+factor(year),weights=1/h2,data=Death)
coeftest(fgls2)

