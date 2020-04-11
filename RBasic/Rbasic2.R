##### R basic 2 #####

#####################
## 데이터 형태 
#####################

# Numeric (수치형), Logical (논리형), Character (문자형), Comlex (복소수형)

# mode() 또는 class() 데이터 유형 출력
mode(3)
mode(3>4)
mode(TRUE)
mode("퀀트")
mode(3+2i)

# 데이터 유형 검증
# is.numeric(x), is.double(x), is.interger(x), is.logical(x), is.character(x), is.complex(x), is.na(x), is.null(x), is.nan(x), is.finite(x), is.infinite(x), is.matarix(x)
# NA (결측치, Not Available or Missing Value)
# NULL 비어있는 값
# NaN 수학적으로 정의가 불가능한 수 Not a Number
# Inf 무한대 

is.double(2.2)  # 실수형 여부  
is.integer(2.2) # 정수형 여부
is.integer(1)
is.integer(1L)
is.integer(as.integer(1))

# 데이터 유형 변경
# as.numeric(x), as.double(x), as.integer(x), as.logical(x), as.character(x), as.complex(x), as.matrix(x)

is.numeric(1/3)
as.character(1/3)
a = "A"
as.numeric("A")

b = as.character(2)
b
b-5

c = "11"
c-5
as.numeric(c)-5



#############################

#  install.packages("data.table")
library(data.table)


setwd("/Users/rohsnghwan/Documents/R_skku/Rbasic")
dir()

survey = fread("old_survey.csv")


## ----add_column, ----------------------------------------
survey[ , height_handspan_ratio := height/handspan]

## ----remove_column, -------------------------------------
survey[ , 7:10 := NULL]

## ----head_tail-----------------------------------------------------------
head(survey)
tail(survey)

## ----hist_plain, 히스토그램 ----------------------------------------
survey[ , hist(handedness)]

## ----hist_dressed, --------------------------------------
survey[ , hist(handedness, xlab = 'Handedness Score',
               main = 'Histogram of Handedness Scores',
               ylab = '# of Students')]

## ----hist_breaks, 막대 20개로 조정  ---------------------------------------
survey[ , hist(handedness, breaks = 20, xlab = 'Handedness Score',
               main = 'Histogram of Handedness Scores')]

## ----hist_freq, y축 Density -----------------------------------------
survey[ , hist(handedness, breaks = 20, freq = FALSE,
               xlab = 'Handedness Score', main = 'Histogram of Handedness')]

## ----plot_basic, scatter plot ----------------------------------------
survey[ , plot(height, handspan)]

## ----plot_rev, ------------------------------------------
survey[ , plot(handspan, height)]

## ----plot_ornament, -------------------------------------
survey[ , plot(height, handspan, xlab = "height (in)", ylab = "handspan (cm)")]

## ----plot_col, 색깔 ------------------------------------------
survey[ , plot(height, handspan, xlab = "height (in)",
               ylab = "handspan (cm)", col = "red")]

## ----plot_pch, 형태 ------------------------------------------
survey[ , plot(height, handspan, xlab = "height (in)",
               ylab = "handspan (cm)", col = "red", pch = 3)]

## ----plot_type_l, connected line ---------------------------------------
survey[ , plot(height, handspan, xlab = "height (in)",
               ylab = "handspan (cm)", col = "red", pch = 3, type = 'l')]

## ----pairs---------------------------------------------------------------
pairs(survey[ , c("handedness", "handspan", "height")])

## ----boxplot, median, interquartile, min and max, outlier------------------
boxplot(survey$handspan, ylab = "Handspan(cm)")

## ----boxplot_comparison--------------------------------------------------
boxplot(handspan ~ sex, data = survey, 
        ylab= "Handspan (cm)", main = "Handspan by Sex")

## ----two_way_table-------------------------------------------------------
survey[ , table(eye.color, sex)]

## ----one_way_table-------------------------------------------------------
survey[ , table(eye.color)]

## ----addmargins, counts 더해짐 --------------------------------------------
my.table = survey[ , table(eye.color, sex)]
my.table
addmargins(my.table)  

## ----prop.table, percents로 표현 ------------------------------------------
my.table = survey[ , table(eye.color, sex)]
prop.table(my.table)

## ----prop.table_pct, percentage form으로 표현------------------------------
100 * prop.table(my.table)

## ----prob.table_round, 반올림 ---------------------------------------------
round(100 * prop.table(my.table), digits = 1)
round(100 * prop.table(my.table), digits = 0)

## ----summary-------------------------------------------------------------
summary(survey)

## ----sum_ missing observation이 있을 때------------------------------------
sum(survey$height)

## ----na.rm: ignore missing observations; NA를 제거하고 sum ----------------
sum(survey$height, na.rm = TRUE)

## ----mean_na.rm_1, mean 평균-----------------------------
mean(survey$height, na.rm = TRUE)

## ----mean_na.rm_2--------------------------------------------------------
#3 observations, 
mean(1:3)
#3 observations, but one missing observation --
#  missing observation을 무시하기 때문에, 평균은 (1+2)/2
mean(c(1, 2, NA), na.rm = TRUE)

## ----var_na.rm, variance 분산---------------------------------------------
var(survey$height, na.rm = TRUE)

## ----sd_na.rm, standard deviation 표준편차 -------------------------------
sd(survey$height, na.rm = TRUE)

## ----표준편차는 분산의 제곱근----------------------------------------------
sqrt(var(survey$height, na.rm = TRUE))

## ----median_na.rm, median 중위값 또는 중앙값-------------------------------
median(survey$height, na.rm = TRUE)

## ----quantile_5no, quantile 분위수 --------------------------------------
quantile(survey$height, na.rm = TRUE)

## ----quantile_probs, 분위수------------------------------------------------
quantile(survey$height, na.rm = TRUE, probs = 0.3)

## ----quantile_many_probs-------------------------------------------------
quantile(survey$height, na.rm = TRUE, probs = c(0.1, 0.3, 0.7, 0.9))

## ----iqr, Inter-Quartile Range---------------------------------------------
IQR(survey$height, na.rm = TRUE)

## ----iqr_w_quantile, ------------------------------------------------------
x = quantile(survey$height, na.rm = TRUE, probs = c(.25, .75))
x[2] - x[1]

## ----max_min, 최대 최소----------------------------------------------------
max(survey$height, na.rm = TRUE)
min(survey$height, na.rm = TRUE)

## ----range_by_hand-------------------------------------------------------
max(survey$height, na.rm = TRUE) - min(survey$height, na.rm = TRUE)

## ----range---------------------------------------------------------------
range(survey$height, na.rm = TRUE)

## ----which.max_min-------------------------------------------------------
which.max(survey$height)     # 몇번째 observation? 
survey[which.max(height)]
survey[which.min(height)]


## ----by_mean-------------------------------------------------------------
survey[ , mean(height, na.rm = TRUE), by = sex]

## ----by_var--------------------------------------------------------------
survey[ , var(height, na.rm = TRUE), by = sex]

## ----by_var_named--------------------------------------------------------
survey[ , .(variance = var(height, na.rm = TRUE)), by = sex]

## ----by_eye.color--------------------------------------------------------
survey[ , mean(height, na.rm = TRUE), by = eye.color]

## ----by_multiple---------------------------------------------------------
survey[ , .(avg_height = mean(height, na.rm = TRUE)), by = .(sex, credits)]

## ----is_na  NA인지 여부 ---------------------------------------------------
x = c(1, 2, NA, 3, NA, 4)
is.na(x) 

## ----not-----------------------------------------------------------------
!is.na(x)

## ----and-----------------------------------------------------------------
y = c(NA, 1, NA, 2, 3, NA)
is.na(y)
!is.na(y)
!is.na(x) & !is.na(y)   #둘다 NA가 아님 

## ----not_is_na-----------------------------------------------------------
survey[!is.na(sex) & !is.na(credits),
       .(avg_height = mean(height, na.rm = TRUE)),
       by = .(sex, credits)]

## ----keyby 정렬 -----------------------------------------------------------
survey[!is.na(sex) & !is.na(credits),
       .(avg_height = mean(height, na.rm = TRUE)),
       keyby = .(sex, credits)]

