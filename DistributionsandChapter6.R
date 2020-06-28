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
# 1) ����ġ�� s.e.�� ���
(0.54218-1)/0.03265
# 2) �ټ� ������ ���, theta = beta_1 - 1
summary(lm(log(price/lotsize)~log(lotsize),data=Housing))


#######
datadir = "http://econ.korea.ac.kr/~chirokhan/book/data"
HH <- read.csv(file.path(datadir,"hhsvy14.csv"))
summary(HH)
# age: ������ ����, comm: �ѼҺ����� �� ��ź� ����, rec: ����/��ȭ�� ���� ���� 

# Run the regression of comm on age
# significant?
# coefficient interpretation
# 95% confidence interval





ols1 = lm((comm~age), data=HH)
summary(ols1)

confint(ols1)

######
# ���� 6.2
klosa <- readRDS(file.path(datadir), "klosasubset.rds")

klosa <- readRDS("klosasubset.rds")

#satisfy5: ���� ������ (100�� ����)
#married: ����ڰ� ������ 1 
#working: ����� ������ 1 
#hlth3: �ǰ� ���� ��� 0, ������ 1, ���ڸ� -1
#age: ����

# Run the regression of satisfy5 on married, for ��������̸鼭 65�� �̻�
ols2 = lm(satisfy5~married, data=klosa, subset=working==0 & age>=65)
summary(ols2)

#�������, 65�� �̻�, �ǰ� ���°� ��� �̻�
ols3 = lm(satisfy5~married, data=klosa, subset=working==0 & age>=65 & hlth3>=0)
summary(ols3)

#�������, 65�� �̻�, �ǰ� ���°� ���� 
ols4 = lm(satisfy5~married, data=klosa, subset=working==0 & age>=65 & hlth3<0)
summary(ols4)

#�������� hlth3 �߰�
ols5 = lm(satisfy5~married+hlth3, data=klosa, subset=working==0 & age>=65)
summary(ols5)


# ���� 6.3
Salary <- read.csv(file.path(datadir,"avgsal12.csv"))
summary(Salary)
# avgsal: ��� ���� �޿�
# sales: �����
# emp: ��������

# Run the regression of log(avgsal) on log(������ 1�δ� �����), subset: kospi ����, sector�� ElecElectron)

ols6 = lm(log(avgsal)~log(sales/emp),data=Salary, subset=kospi==1 & sector=='ElecElectron')
summary(ols6)

confint(ols6)

### ����6.5
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







