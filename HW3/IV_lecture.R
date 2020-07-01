# IV Estimation

dir()

#install.packages("car")
#install.packages("sandwich")
#install.packages("lmtest")

library(car)
library(sandwich)
library(lmtest)


WAGEIV = read.csv("mrozdata.csv")


# Run the regression of log(wage) on educ, exper, exper^2
ols1 = 
summary(ols1)


install.packages("AER")
library(AER)


# We think that educ is endogenous. 
# exper, motheduc, and fatheduc are assumed to be exogenous.

# Is 'motheduc' reasonable IV candidate for educ?
ols_m=

# Is 'fatheduc' reasonable IV candidate for educ?
ols_f=

  
  
    

#2SLS estimation using motheduc and fatheduc as IVs

tsls = 
summary(tsls)
coeftest(tsls)



#2 stage regressions

#1st stage
firststage = lm()
fittededuc = fitted(firststage)
#2nd stage
secondstage=lm()
coeftest(secondstage)
# Note that 1) the estimates are same as 'tsls' but 2) standard errors are different. standard errors should be those in 'tsls'.





#Test whether educ is exogenous or endogenous
ols2 = lm(educ~exper+I(exper^2)+motheduc+fatheduc,data=WAGEIV)
vhat = resid(ols2)

ols3 = lm(log(wage)~educ+exper+I(exper^2)+vhat,data=WAGEIV)
summary(ols3)


########

phillips <- read.csv("phillips.csv")

n    = nrow(phillips)

unem=phillips[,1]
inf=phillips[,2]

dinf = diff(inf)
unem = as.matrix(unem)
head(unem)

# Run the regression of dinf_t on unem_t
ols4 = 
summary(ols4)



# Is unem_(t-1) a reasonable IV candidate for unem_t
ols5 = 
coeftest(ols5)


#library(AER)

#2SLS using unem_(t-1) as an IV

tsls2 = 
coeftest(tsls2)




