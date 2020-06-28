#####################
# Chapter 8 examples
# ??�� 8.1, ??�� 8.2

datadir = "http://econ.korea.ac.kr/~chirokhan/book/data"

Death <- read.csv(file.path(datadir,"deathrate.csv"))
summary(Death)

# ?츮???? ???? ???��? ?ڷ?
# deathrate: ???��? ?α? 1õ???? ???? ?????? ??
# drink: ��??��, ????��

# Run the regression of deathrate on drink, 2010?? ?ڷḸ ?̿? (year==2010)





ols1 = lm(deathrate~drink,data=Death,subset=year==2010)
summary(ols1)






# aged: ?ش? ???? ?????? ??��
plot(drink ~ aged,data=Death)

# Run the regression of deathrate on drink and aged, year 2010






ols2 = lm(deathrate~drink+aged,data=Death,subset=year==2010)
summary(ols2)



## ????��?? page 218
## partial out


install.packages("Ecdat")
library("Ecdat")

data(Housing,package="Ecdat")
summary(Housing)

ols3 = lm(log(price)~log(lotsize)+bedrooms,data=Housing)
summary(ols3)

# 1) run the regression of drink on aged and get the residual

ols4 = lm(log(lotsize)~bedrooms,data=Housing)
summary(ols4)

uhat_x1 = resid(ols4)

# 2) run the regression of deathrate on uhat_x1
# check out the coefficient estimate of uhat_x1

ols5 = lm(log(price)~uhat_x1,data=Housing)
summary(ols5)

ols6 = lm(log(price)~uhat_x1+bedrooms,data=Housing)
summary(ols6)


# checking, run the regression of uhat_y on uhat_x1
ols7 = lm(log(price)~bedrooms,data=Housing)
uhat_y = resid(ols7)

ols8=lm(uhat_y~uhat_x1+bedrooms,data=Housing)
summary(ols8)

ols9=lm(uhat_y~uhat_x1,data=Housing)
summary(ols9)
######
# ??�� 8.3

data(Wages1,package="Ecdat")
summary(Wages1)
head(Wages1)
# We make a dummy variable 'male' and 'female'
Wages1$male <- as.numeric(Wages1$sex=="male")
Wages1$female <- as.numeric(Wages1$sex=="female")

head(Wages1)
sum(Wages1$male)
sum(Wages1$female)


lm(wage~female,data=Wages1)

lm(wage~female,data=Wages1,subset=female==1)


lm(wage~female+male,data=Wages1)


lm(wage~female+male-1,data=Wages1)
lm(wage~female,data=Wages1)




ols9 = lm(log(wage)~female+school+exper,data=Wages1)
summary(ols9)



# random numbers
Wages1$rnd = rnorm(nrow(Wages1))
head(Wages1)

summary(lm(log(wage)~female+school+exper,data=Wages1))$r.sq

# R ��??�� ? ?????? ???ԵǾ ?????ϴ? ???? 
summary(lm(log(wage)~female+school+exper+rnd,data=Wages1))$r.sq

summary(lm(log(wage)~female+school+exper+rnd, data=Wages1))

