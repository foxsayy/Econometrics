#-- 1. CEOSAL2 ------------

#-- 데이터 불러오기 -------
setwd(getwd())
datadir = getwd()
library(readxl)
ceosal2 = read_excel(file.path(datadir, "ceosal2.xls"), col_names=FALSE)
nrow(ceosal2)
tail(ceosal2)
ceosal2 = na.omit(ceosal2)
nrow(ceosal2)

colnames(ceosal2) = c("salary", "age", "college", "grad", "comten", "ceoten", "sales", "profits", "mktval", "lsalary", "lsales", "lmktval", "comtensq", "ceotensq", "profmarg")
tail(ceosal2)

#-- 1.1 -------------------
summary(ceosal2[c("salary", "ceoten")])

#-- 1.2 -------------------
nrow(subset(ceosal2, ceoten==0))
summary(ceosal2$ceoten)

#-- 1.3 -------------------
reg1 = lm(log(salary)~ceoten, data=ceosal2)
summary(reg1)
plot(log(salary)~ceoten, data=ceosal2)
abline(reg1)

#-- 2. 401k ---------------

#-- 데이터 불러오기 -------
datadir = getwd()
library(readxl)
data_401k = read_excel(file.path(datadir, "401k.xls"), col_names=FALSE)
tail(data_401k)
colnames(data_401k) = c("prate", "mrate", "totpart", "totelg", "age", "totemp", "sole", "ltotemp")
tail(data_401k)
nrow(data_401k)
data_401k = na.omit(data_401k)
nrow(data_401k)

#-- 2.1 -------------------
summary(data_401k[c("prate", "mrate")])

#-- 2.2 -------------------
reg2 = lm(prate~mrate, data=data_401k)
summary(reg2)

#-- 2.3 -------------------
plot(prate~mrate, data=data_401k)
abline(reg2)

#-- 2.4 -------------------
prate_3.5 = 83.0755 + 5.8611 * 3.5
prate_3.5

data_401k_sub = subset(data_401k, mrate>3 & mrate<4)
reg3 = lm(prate~mrate, data=data_401k_sub)
summary(reg3)
plot(prate~mrate, data=data_401k_sub)
abline(reg3)
prate_3.5 = 103.310 -1.601 * 3.5
prate_3.5

#-- 2.5 -------------------
summary(reg2)
names(data_401k)

reg_totpart = lm(prate~totpart, data=data_401k)
summary(reg_totpart)

reg_totelg = lm(prate~totelg, data=data_401k)
summary(reg_totelg)

reg_age = lm(prate~age, data=data_401k)
summary(reg_age)

reg_totemp = lm(prate~totemp, data=data_401k)
summary(reg_totemp)

reg_sole = lm(prate~sole, data=data_401k)
summary(reg_sole)

reg_ltotemp = lm(prate~ltotemp, data=data_401k)
summary(reg_ltotemp)