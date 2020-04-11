##### R basic 3 #####
getwd()
setwd("/Users/rohsnghwan/Documents/R_skku")
dir()


## ----fread_select, ?????ؼ? ?ҷ????̱?-------------------------------------

install.packages("data.table")
library(data.table)
survey = fread("old_survey.csv",
               select = c("handedness", "height", "handspan"))

attach(survey)   # R?? ????(?)

## ----cor: correlation, use = "complete.obs"�� ???? missing observations ��?? -----------
cor(handspan, height, use = "complete.obs")

## ----cor_dt--------------------------------------------------------------
cor(survey, use = "complete.obs")
#alternatively, there's the na.omit function. na.omit�� NA?? ��?? 
cor(na.omit(survey))

# same correlation as in line 10?

## ----find_missing--------------------------------------------------------
survey[is.na(handedness) & !is.na(height) & !is.na(handspan)]

is.na(handedness) & !is.na(height) & !is.na(handspan)

## ----cov: covariance ------------------------------------------------------
cov(survey, use = "complete.obs")
cov(na.omit(survey))

## ----lm: linear model ??��, OLS ??��---------------------------------------
lm(handspan ~ height)

## ----plot_formula, results = 'hide'--------------------------------------
plot(handspan ~ height)

## ----abline_lm, regression line: intercept and slope of the linear model -
abline(lm(handspan ~ height))


### Alternatively
fit = lm(handspan ~ height)
summary(fit)
plot(handspan ~ height)
abline(fit)


## ----abline?? sample mean ?߰? ---------------------------------
plot(handspan ~ height)
abline(lm(handspan ~ height))
abline(v = mean(height, na.rm = TRUE),
       h = mean(handspan, na.rm = TRUE),
       col = 'red', lty = 2)


## ----abline_ab, -----------------------------------------
x = seq(from = 0, to = 1, by = 0.1)
y = x^2
plot(y ~ x)
abline(a = 0, b = 1)   #intercept?? 0��??, slope�� 1?? ??��

## ----exercise_1, ----------------------------------------
reg = lm(height ~ handedness)
plot(height ~ handedness)
abline(reg)
abline(v = mean(handedness, na.rm = TRUE),
       h = mean(height, na.rm = TRUE),
       col = 'red', lty = 2)


#####################
## function 
#####################

## ----function_write------------------------------------------------------
z.score = function(x){
  z = (x - mean(x, na.rm = TRUE))/sd(x, na.rm = TRUE)
  return(z)
}

## ----mymean--------------------------------------------------------------
mymean = function(x){
  x = x[!is.na(x)]    # x is overwritten
  x.bar = sum(x)/length(x)
  return(x.bar)
}

## ----mean: R?? ?????? ?Լ?----------------------------------
mean(survey$height, na.rm = TRUE)

## ----mymean: ��???? ��?ǵ? ?Լ?-----------------------------------------
mymean(survey$height)

## ----mymean2-------------------------------------------------------------
mymean2 = function(x){
  x.bar = sum(x, na.rm = TRUE)/length(x)
  return(x.bar)
}
mymean2(survey$height)
# mymean?? mymean2?? ???? 
# mymean2 ?Լ? ?ȿ??? sum ignores missing observations but length does not.
 

## ----myvar---------------------------------------------------------------
myvar = function(x){
  x = x[!is.na(x)]
  s.squared = sum((x-mymean(x))^2)/(length(x) - 1)
  return(s.squared)
}

## ----myvar_test----------------------------------------------------------
var(survey$handspan, na.rm = TRUE)
myvar(survey$handspan)

## ----exercise_2----------------------------------------------------------
#Exercise #2 - Write a Function to Calculate Skewness
skew = function(x){
  x = x[!is.na(x)]
  numerator = sum((x - mean(x))^3)/length(x)
  denominator = sd(x)^3
  return(numerator/denominator)  
}
skew(survey$handedness)

## ----function_return_data.table------------------------------------------
summary.stats = function(x){
  x = x[!is.na(x)]
  sample.mean = mean(x)
  std.dev  = sd(x)
  out = data.table(sample.mean, std.dev)
  return(out)
}
results = summary.stats(survey$handedness)
results
results$sample.mean
results$std.dev

## ----mycov---------------------------------------------------------------
mycov = function(x, y){
  
  keep = !is.na(x) & !is.na(y)
  x = x[keep]
  y = y[keep]
  
  n = length(x)
  
  s.xy = sum( (x - mean(x)) * (y - mean(y)) ) / (n-1)
  return(s.xy)
}

## ----mycov ?Լ??? R?? ?????? ?Լ? ????-----------------------------------------
cov(handspan, handedness, use = "complete.obs")
mycov(handspan, handedness)

