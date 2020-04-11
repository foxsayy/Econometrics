##### R basic 3 #####

setwd("C:/Users/Heejoon/Desktop/R_Quant")
dir()


## ----fread_select, 선택해서 불러들이기-------------------------------------
library(data.table)
survey = fread("old_survey.csv",
               select = c("handedness", "height", "handspan"))


## ----cor: correlation, use = "complete.obs"은 모든 missing observations 제거 -----------
survey[ , cor(handspan, height, use = "complete.obs")]

## ----cor_isna------------------------------------------------------------
survey[!is.na(handspan) & !is.na(height), cor(handspan, height)]

## ----cor_dt--------------------------------------------------------------
cor(survey, use = "complete.obs")
#alternatively, there's the na.omit function. na.omit은 NA를 제거 
cor(na.omit(survey))

# same correlation as in line 10?

## ----find_missing--------------------------------------------------------
survey[is.na(handedness) & !is.na(height) & !is.na(handspan)]

## ----cov: covariance ------------------------------------------------------
survey[ , cov(handspan, height, use = "complete.obs")]
cov(survey, use = "complete.obs")
cov(na.omit(survey))

## ----lm: linear model 추정, OLS 추정---------------------------------------
survey[ , lm(height ~ handspan)]

## ----lm_rev--------------------------------------------------------------
survey[ , lm(handspan ~ height)]

## ----plot_formula, results = 'hide'--------------------------------------
survey[ , plot(handspan ~ height)]

## ----abline_lm, regression line: intercept and slope of the linear model -
survey[ , plot(handspan ~ height)]
survey[ , abline(lm(handspan ~ height))]

## ----abline과 sample mean 추가 ---------------------------------
survey[ , {
  plot(handspan ~ height)
  abline(lm(handspan ~ height))
  abline(v = mean(height, na.rm = TRUE),
         h = mean(handspan, na.rm = TRUE),
         col = 'red', lty = 2)
}]

## ----abline_ab, -----------------------------------------
x = seq(from = 0, to = 1, by = 0.1)
y = x^2
plot(y ~ x)
abline(a = 0, b = 1)   #intercept를 0으로, slope을 1로 설정

## ----exercise_1, ----------------------------------------
survey[ , {
  reg = lm(height ~ handedness)
  plot(height ~ handedness)
  abline(reg = reg)
  abline(v = mean(handedness, na.rm = TRUE),
         h = mean(height, na.rm = TRUE),
         col = 'red', lty = 2)
}]


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

## ----mean: R에 내장된 함수----------------------------------
mean(survey$height, na.rm = TRUE)

## ----mymean: 위에서 만든 함수-----------------------------------------
mymean(survey$height)

## ----mymean2-------------------------------------------------------------
mymean2 = function(x){
  x.bar = sum(x, na.rm = TRUE)/length(x)
  return(x.bar)
}
mymean2(survey$height)
# mymean과 mymean2의 비교 
# mymean2 함수 안에서 sum ignores missing observations but length does not.
yvar---------------------------------------------------------------
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

## ----mycov_test----------------------------------------------------------
survey[ , cov(handspan, handedness, use = "complete.obs")]
survey[ , mycov(handspan, handedness)]

