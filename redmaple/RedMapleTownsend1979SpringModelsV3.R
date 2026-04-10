# Analysis of Spring phenology data from Townsend 1982 V3
# Author: Mary McCafferty

## DO NOT USE THIS VERSION


#load all necessary libraries
library(ggplot2)
library(ordinal)
library(MuMIn)
library(ggiraphExtra)
library(viridis)  
library(RColorBrewer)

# read in the data
rm.clim = read.csv("RedMaple_Townsend1982_withClimate.csv")
ts = read.csv("RedMaple_TestSites_ClimateNA_Normal_1941_1970SY_V2.csv")

#variable of interest is: Avg_Time_Flush (Degree of flushing in April 1979/80)

# remove na values for resp var of interest
rm.clim = rm.clim[!is.na(rm.clim$Avg_Time_Flush),]
#save test site variable as factor variable
rm.clim$test = as.factor(rm.clim$test)

# look at correlation matrix
cor_spring = rm.clim[names(rm.clim) %in% c("Avg_Time_Flush", "daylength_diff", 
                                           "DD_0_wt", "DD_0_sp", "DD5_sp", "DD5_wt", 
                                           "MAT", "MWMT", "MCMT", "TD", "MAP", "bFFP")]
pearson_cor_spring = cor(cor_spring, method = "pearson")
spearman_cor_spring = cor(cor_spring, method = "spearman")

#explore the data a bit
plot(rm.clim$DD_0_wt, rm.clim$Avg_Time_Flush)
ggplot(data=rm.clim, aes(TD, Avg_Time_Flush))+
  geom_point(aes(shape = test, color = test), size=6)

hist(rm.clim$Avg_Time_Flush) #this seems to be sufficiently normally distributed so we should use the probit link function
# using polr function and implementing test site as a fixed effect
#make response variable a factor
rm.clim$Avg_Time_Flush = as.factor(rm.clim$Avg_Time_Flush)
rm.clim$Avg_Time_Flush = ordered(rm.clim$Avg_Time_Flush)

set.seed(031026)

#run the models
#null model
#null model
model.null = clmm(Avg_Time_Flush ~ (1 | test), data = rm.clim, link = "logit", threshold = "flexible")
summary(model.null)
#emmeans::emmeans(model.null)
# AIC = 388.95
#ll.null = loocv.clmm.null(data = rm.clim, formula = Avg_Time_Flush ~ (1|test) + (1|seedSource), resp.var = Avg_Time_Flush)
#to assess model prediction accuracy, we need to identify which score is most likely at the seed source level
table(rm.clim[["Avg_Time_Flush"]])/nrow(rm.clim)
# 6 is the most commonly predicted score for all seed sources at .266
null.pred.accuracy = 0.267

#daylength
model.day = polr(Avg_Time_Flush ~ daylength_diff + test, data = rm.clim, Hess = TRUE)
summary(model.day)
# AIC = 378.26
# cross validate


#DD_0_wt
model.dd0wt = polr(Avg_Time_Flush ~ DD_0_wt + test, data = rm.clim, Hess = TRUE)
summary(model.dd0wt)
# AIC = 354.96

#DD_0_sp
model.dd0sp = polr(Avg_Time_Flush ~ DD_0_sp + test, data = rm.clim, Hess = TRUE)
summary(model.dd0sp)
# AIC = 356.42

#DD5_wt
model.dd5wt = polr(Avg_Time_Flush ~ DD5_wt + test, data = rm.clim, Hess = TRUE)
summary(model.dd5wt)
# AIC = 382.38.60

#DD5_sp
model.dd5sp = polr(Avg_Time_Flush ~ DD5_sp + test, data = rm.clim, Hess = TRUE)
summary(model.dd5sp)
# AIC = 356.58

#MAT
model.mat = polr(Avg_Time_Flush ~ MAT + test, data = rm.clim, Hess = TRUE)
summary(model.mat)
# AIC = 369.50

#MWMT
model.mwmt = polr(Avg_Time_Flush ~ MWMT + test, data = rm.clim, Hess = TRUE)
summary(model.mwmt)
# AIC = 381.45

#MCMT
model.mcmt = polr(Avg_Time_Flush ~ MCMT + test, data = rm.clim, Hess = TRUE)
summary(model.mcmt)
# AIC = 362.07

#MAP
model.map = polr(Avg_Time_Flush ~ MAP + test, data = rm.clim, Hess = TRUE)
summary(model.map)
# AIC = 374.85

#TD
model.td = polr(Avg_Time_Flush ~ TD + test, data = rm.clim, Hess = TRUE)
summary(model.td)
# AIC = 357.99

#bFFP
model.bffp = polr(Avg_Time_Flush ~ bFFP + test, data = rm.clim, Hess = TRUE)
summary(model.bffp)
#AIC = 376.23


# compare the best models
aics = AIC(model.null, model.day, model.dd0wt, model.dd0sp, model.dd5sp, model.dd5wt, model.mcmt, model.mwmt, model.mat, model.td, model.map, model.bffp)
anova(model.null, model.day, model.dd0wt, model.dd0sp, model.dd5sp, model.dd5wt, model.mcmt, model.mwmt, model.mat, model.td, model.map, model.bffp)
aics
results = cbind(aics, pred.accuracy)

results

library(pscl)
pR2(model.dd0wt) #this doesnt work with clmm models it seems
# compute pseudo r^2 for final model here
1 - logLik(model.dd0wt) / logLik(model.null)

# evaluate percent improvement in error rate
null.error = 1-null.pred.accuracy
pred.error = 1-pred.accuracy
pct = (null.error-pred.error) / pred.error


write.csv(results, "RedMapleSpringResults.csv")


