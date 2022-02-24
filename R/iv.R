library(stargazer)
library(estimatr)
library(AER)
library(ggplot2)
library(cowplot)
library(sandwich)
library(lmtest)
library(dplyr)
library(lfe)
library(here)


# IMPORT CSV DATA
FULTON <- read.csv(here("data/FULTON.csv")) %>%
  mutate(log_tots = log(tots),
         log_price = log(pricelevel))


# SUMMARY STATISTICS
# day1 -day4 price = 0.85cents per lb 
# cold =   
# rainy = candidate instrument, 
stargazer(FULTON, type="text", digits=2)


# BASIC OLS REGRESSION - slide 13-14
# elasticity of demand, 1% change in price in fish would result in a reduction in demand of 0.56

ols <- lm(formula = log_tots ~ log_price, data=FULTON)
summary(ols)


# FIRST_STAGE REGRESSION - JUST-IDENTIFIED MODEL
# Lecture 8, slide 12
# use this to make sure senses
# log B1 windspd increases by 1% then the price of fish increases by 
# F statistic - testing all parameters of regression, including constant term, test another way than this
# 
fs1 <- lm(formula = log_price ~ windspd, data=FULTON)
summary(fs1)

# F-test for non-weak and relevant instruments (Lecture 9, slides 13-14)
# 12.7 is bigger than 10, so relavent and not weak, just look at F-stat value
linearHypothesis(fs1, c("windspd=0"), white.adjust = "hc2")

# TSLS - JUST-IDENTIFIED MODEL
# Lecture 8, slide 13
# B1 parameter - -1.3744, when slope of demand curve, only variation of differences in windspd across days & 
# windspeed has an effect on supplies not demand

tsls1 <- ivreg(log_tots ~ log_price | windspd, data = FULTON)
summary(tsls1)


# Calculate robust standard errors for OLS and FS1 using starprep()
se_ols_fs1 <- starprep(ols,fs1, stat = c("std.error"), se_type = "HC2", alpha = 0.05) 

# Calculate robust standard errors for TSLS1 using sandwich and lmtest packages (starprep() does not like ivreg() objects)
se_tsls1 <- coeftest(tsls1, vcov = vcovHC(tsls1, type = "HC2"))[, "Std. Error"]

# Combine standard errors and output results with stargazer()
se_models <- append(se_ols_fs1,list(se_tsls1))
stargazer(ols, fs1, tsls1, se = se_models, type="text")


# FIRST_STAGE REGRESSION - OVER-IDENTIFIED MODEL
# Lecture 9, slide 6
# added days to 1st stage regression, windspd, cold wx data
# effect of windspd 0.8

fs2 <- lm(formula = log_price ~ day1 + day2 + day3 + day4 + windspd + cold, data=FULTON)
summary(fs2)

# F-test for non-weak and relevant instruments (Lecture 9, slides 13-14)
# Fstat is less than 10, 7.6, so instruments are weak
linearHypothesis(fs2, c("windspd=0", "cold=0"), white.adjust = "hc2")

# TSLS - OVER-IDENTIFIED MODEL
# Lecture 9, Slide 7
# log quantity on log price, days are exogenous, right of the bar are instruments
# instruments belong in 1st stage regression, right of | tells model to include
# if not included, leads to bias in TSLS
# predicting log price - elasticity is effect of windspd = 

tsls2 <- ivreg(log_tots ~ log_price + day1 + day2 + day3 + day4 | day1 + day2 + day3 + day4 + windspd + cold, data = FULTON)
summary(tsls2)


fs3 <- lm(formula = log_price ~ day1 + day2 + day3 + day4 + windspd, data=FULTON)
summary(fs3)

linearHypothesis(fs3, c("windspd=0"), white.adjust = "hc2")


# FINAL TSLS - REMOVED WEAKED INSTRUMENT
# 
tsls3 <- ivreg(log_tots ~ log_price + day1 + day2 + day3 + day4 | day1 + day2 + day3 + day4 + windspd, data = FULTON)
summary(tsls3)


# Calculate robust standard errors for FS2 using starprep()
se_fs2 <- starprep(fs2, stat = c("std.error"), se_type = "HC2", alpha = 0.05) 

# Calculate robust standard errors for TSLS2 using sandwich and lmtest packages (starprep() does not like ivreg() objects)
se_tsls2 <- coeftest(tsls2, vcov = vcovHC(tsls2, type = "HC2"))[, "Std. Error"]

# Calculate robust standard errors for TSLS3 using sandwich and lmtest packages (starprep() does not like ivreg() objects)
se_tsls3 <- coeftest(tsls3, vcov = vcovHC(tsls3, type = "HC2"))[, "Std. Error"]

# Combine standard errors and output results with stargazer()
se_models <- append(se_fs2,list(se_tsls2))
stargazer(fs2, tsls2, tsls3, se = se_models, type="text")
