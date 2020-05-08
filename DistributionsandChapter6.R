# Distributions and chapter 6

# distribution, cdf, quantile function, etc.

# normal distribution
?dnorm
curve(dnorm(x,mean=0,sd=1),-6,6)
qnorm(0.95,mean=0,sd=1)

qnorm(0.975,mean=0,sd=1)
qnorm(0.025,mean=0,sd=1)

2*(1-pnorm(1.96,mean=0,sd=1))
1-pnorm(1.96,mean=0,sd=1)

# chi-square distribution
?dchisq
curve(dchisq(x,df=5),0,12)
qchisq(0.95,2)
# If the statistic=9 and dof=3, p-value?
1-pchisq(9,3)

# student's t distribution
?dt

curve(dt(x,df=1),-6,6)
curve(dt(x,df=5),-6,6)
curve(dt(x,df=10),-6,6)
curve(dt(x,df=50),-6,6)

qt(0.95,20)

# F distribution
?df

curve(df(x,df1=2,df2=20),0,12)
# If F-statistic=7, p-value?
1-pf(7, 2, 20)

#####################
# Chapter 6 examples

install.packages("Ecdat")
library("Ecdat")

data(Housing,package="Ecdat")

ols = lm(log(price)~log(lotsize),data=Housing)
summary(ols)

# 1% critical value for two-sided test
qt(0.995,(546-2))
# p-value for two-sided test
2*(1-pt(16.61,546-2))

# confidence interval
confint(ols)
confint(ols, 'log(lotsize)',0.95)
confint(ols, 'log(lotsize)',0.99)
confint(ols, 'log(lotsize)',0.90)

# H0: beta_1 = 1
# 1) 추정치와 s.e.로 계산
(0.54218-1)/0.03265
# 2) 다소 복잡한 방법, theta = beta_1 - 1
summary(lm(log(price/lotsize)~log(lotsize),data=Housing))


#######
datadir = "http://econ.korea.ac.kr/~chirokhan/book/data"
HH <- read.csv(file.path(datadir,"hhsvy14.csv"))
summary(HH)
# age: 가구주 나이, comm: 총소비지출 중 통신비 비중, rec: 오락/문화비 지출 비중 

# Run the regression of comm on age
# significant?
# coefficient interpretation
# 95% confidence interval





ols1 = lm((comm~age), data=HH)
summary(ols1)

confint(ols1)

######
# 예제 6.2
klosa <- readRDS(file.path(datadir), "klosasubset.rds")

klosa <- readRDS("klosasubset.rds")

#satisfy5: 삶의 만족도 (100점 만점)
#married: 배우자가 있으면 1 
#working: 취업해 있으면 1 
#hlth3: 건강 정도 평균 0, 좋으면 1, 나쁘면 -1
#age: 나이

# Run the regression of satisfy5 on married, for 비취업자이면서 65세 이상
ols2 = lm(satisfy5~married, data=klosa, subset=working==0 & age>=65)
summary(ols2)

#비취업자, 65세 이상, 건강 상태가 평균 이상
ols3 = lm(satisfy5~married, data=klosa, subset=working==0 & age>=65 & hlth3>=0)
summary(ols3)

#비취업자, 65세 이상, 건강 상태가 나쁜 
ols4 = lm(satisfy5~married, data=klosa, subset=working==0 & age>=65 & hlth3<0)
summary(ols4)

#설명변수 hlth3 추가
ols5 = lm(satisfy5~married+hlth3, data=klosa, subset=working==0 & age>=65)
summary(ols5)


# 예제 6.3
Salary <- read.csv(file.path(datadir,"avgsal12.csv"))
summary(Salary)
# avgsal: 평균 연간 급여
# sales: 매출액
# emp: 종업원수

# Run the regression of log(avgsal) on log(종업원 1인당 매출액), subset: kospi 상장, sector는 ElecElectron)

ols6 = lm(log(avgsal)~log(sales/emp),data=Salary, subset=kospi==1 & sector=='ElecElectron')
summary(ols6)

confint(ols6)

### 예제6.5
install.packages("AER")
library("AER")
data(CigarettesB)
ols7 = lm(packs~price,data=CigarettesB)
summary(ols7)

# test wheter price elasticity is -1
# theta = beta_1 +1 (so that theta=0 under Ho)

tsta = (-1.1983+1)/0.2818
tsta

# p-value for two-sided test
2*(pt(tsta,46-2))




ols8 = lm(I(packs+price)~price,data=CigarettesB)
summary(ols8)








