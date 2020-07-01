# Homework 4

dir()

#################
fertil2 <- read.csv("fertil2.csv")
                    
n    = nrow(fertil2)

children=fertil2[,1]
age=fertil2[,2]
educ=fertil2[,3]
frsthalf=fertil2[,4]
tv=fertil2[,5]

ols1 = lm(children~educ+age+I(age^2),data=fertil2)
coeftest(ols1)

ols2 = lm(educ~age+I(age^2)+frsthalf,data=fertil2)
coeftest(ols2)

install.packages("AER")
library(AER)

#2SLS

tsls = ivreg(children~educ+age+I(age^2)|age+I(age^2)+frsthalf,data=fertil2)
coeftest(tsls)


ols3 = lm(children~educ+age+I(age^2)+tv,data=fertil2)
coeftest(ols3)
tsls2 = ivreg(children~educ+age+I(age^2)+tv|age+I(age^2)+frsthalf+tv,data=fertil2)
coeftest(tsls2)

