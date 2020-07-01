#Serial Correlation

dir()

install.packages("car")
install.packages("sandwich")
install.packages("lmtest")
library(car)
library(sandwich)
library(lmtest)


minwage <- read.csv("prminwge.csv")

n    = nrow(minwage)

prepop = minwage[,1]
mincov = minwage[,2]
usgnp = minwage[,3]

ols1 = lm(log(prepop)~log(mincov)+log(usgnp),data=minwage)
summary(ols1)


#Durbin-Watson test
library("lmtest")
dwtest(ols1)

uhat = resid(ols1)
sum(diff(uhat)^2/sum(uhat^2))

#Breusch-Godfrey test
bgtest(ols1)





bgtest(ols1,order=3)



install.packages("sandwich")
library(sandwich)

#Newey-West HAC, robust standard error

coeftest(ols1,vcov=vcovHAC)
coeftest(ols1)
 


#Cochrane-Orcutt estimation


uhat[2:n]
head(uhat)
head(uhat[2:n])
tail(uhat)
tail(uhat[2:n])

ols2 = lm(uhat[2:n]~uhat[1:(n-1)])
summary(ols2)
rho = coefficients(ols2,2)
rho = rho[2]

ystar = log(prepop[2:n])-rho*log(prepop[1:(n-1)])
x1star = log(mincov[2:n])-rho*log(mincov[1:(n-1)])
x2star = log(usgnp[2:n])-rho*log(usgnp[1:(n-1)])

cochraneorcutt = lm(ystar~x1star+x2star)  
summary(cochraneorcutt)



# iterated Cochrane-Orcutt: iterating until convergence

install.packages("orcutt")
library("orcutt")

co=cochrane.orcutt(ols1, convergence = 8, max.iter=100)
summary(co)

