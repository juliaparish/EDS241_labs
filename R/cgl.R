library(MASS)
library(ggplot2)
library(vtable)
library(stargazer)
library(estimatr)
library(dplyr)
library(tidyr)
library(here)

# IMPORT CSV DATA
cgl <- read.csv(here("data/cgl_collapse_data_extract.csv"))

# SUMMARY STATISTICS
stargazer(cgl, type="text", digits=2)

# collapse should be 0 thru 1; 27%
# year only 2003
# DAPever - ITQ
# DAPimp - ??
# LME - type of large marine ecosystem (1-60) 
# genus/species - numerics don't make a lot of sense

# EXAMINE BALANCE IN COVARIATES
# COVARIATE MEAN DIFFERENCES by DAPever
m1 <- lm(formula = LME ~ DAPever, data= cgl)
m2 <- lm(formula = genus ~ DAPever, data=cgl)
m3 <- lm(formula = species ~ DAPever, data=cgl)

se_models = starprep(m1, m2, m3, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(m1, m2, m3, se = se_models, type="text")

# BOXPLOTS TO EXAMINE BALANCE IN COVARIATES
ggplot(cgl, aes(x=as.factor(DAPever), y=LME)) + 
  geom_boxplot(fill="cyan") + xlab("ITQ Yes/No")
# in fisheries w/o ITQ the LME the median is ~35, in tx (1) the median is ~39

# 2 other box plots look more similar, spread is equivalent
ggplot(cgl, aes(x=as.factor(DAPever), y=genus)) + 
  geom_boxplot(fill="cyan") + xlab("ITQ Yes/No")

ggplot(cgl, aes(x=as.factor(DAPever), y=species)) + 
  geom_boxplot(fill="cyan") + xlab("ITQ Yes/No")

# BASIC OLS by DAPever -- THEN ADD INDICATORS FOR OTHER COVARIATES 
# NOTE DO NOT INCLUDE SPECIES IN MODELS TO KEEP RUNNING TIME FAST
# collapse y on D (DAPever), apply vulnerability 
mA <- lm(formula = collapse ~ DAPever, data= cgl)
mB <- lm(formula = collapse ~ DAPever + as.factor(LME), data= cgl)
mC <- lm(formula = collapse ~ DAPever + as.factor(LME) + as.factor(genus), data= cgl)

se_models = starprep(mA, mB, mC, stat = c("std.error"), se_type = "HC2", alpha = 0.05)
stargazer(mA, mB, mC, se = se_models, type="text", omit = "(LME)|(genus)|(species)")

#table: DAPever est. the mean diff of prob of collapse is 14% lower than 

# BASIC PROPENSITY SCORE --- THIS IS A TOY MODEL
# ESTIMATE PROPENSITY SCORE MODEL AND PREDICT (EPS)
# prob D = 1, linear term for genus, LME; 
ps_model <- glm(DAPever ~ LME + genus, family = binomial(), data = cgl)

summary(ps_model)

EPS <- predict(ps_model, type = "response") # est. logistic equation to create new variable est. propensity score
PS_WGT <- (cgl$DAPever/EPS) + ((1-cgl$DAPever)/(1-EPS))


# COLLECT ALL RELEVANT VARIABLES IN DATAFRAME
DF <- data.frame(years = cgl$years, collapse = cgl$collapse, DAPever = cgl$DAPever, 
                 LME = cgl$LME, genus = cgl$genus, species = cgl$species, EPS, PS_WGT)

# BOXPLOTS TO EXAMINE OVERLAP IN P-SCORE DISTRIBUTIONS
ggplot(DF, aes(x=as.factor(DAPever), y=EPS)) + 
  geom_boxplot(fill="cyan") + xlab("Ever Collapsed")

# WLS USING EPS WEIGHTS
wls1 <- lm(formula = collapse ~ DAPever, data=DF, weights=PS_WGT)
wls2 <- lm(formula = collapse ~ DAPever + LME + genus, data=DF, weights=PS_WGT)

se_models = starprep(wls1, wls2, stat = c("std.error"), se_type = "HC2", alpha = 0.05)

stargazer(wls1, wls2, se = se_models, type="text", omit = "(LME)|(genus)|(species)")