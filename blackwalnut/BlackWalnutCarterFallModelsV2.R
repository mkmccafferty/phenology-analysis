# Analysis of black walnut Fall data from Carter
# Author: Mary McCafferty
# Version 2

#load libraries
library(ggplot2)
library(boot)

# read in the files
bw.clim = read.csv("BlackWalnut_Carter_withClimate.csv")

# test site
tests = read.csv("BW_AllTestSites_Normal_1951_1980SY.csv")
#isolate the test climate data for VT
bw.me = tests[2,]

#variable of interest: BudSet_JD
#correlations
cor_fall = bw.clim[names(bw.clim) %in% c("BudSet_JD", "daylength_diff",
                             "Tmax_at", "DD_0_at", "DD_0_wt", 
                             "MAT", "MWMT", "MCMT", "TD", "MAP", "eFFP", "CMD")]


pearson_cor_fall = cor(cor_fall, method = "pearson")
spearman_cor_fall = cor(cor_fall, method = "spearman")

# run all models for leaf color
par(mfrow = c(2, 2))

# the null model
model.null = glm(BudSet_JD ~ 1, data = bw.clim)
summary(model.null)
plot(model.null)
tss = sum((bw.clim$BudSet_JD - mean(bw.clim$BudSet_JD))^2);n = nrow(bw.clim)

# daylength
model.day = glm(BudSet_JD ~ daylength_diff, data = bw.clim)
summary(model.day)
plot(model.day)
# okay, unequal var

# daylength^2
model.day2 = glm(BudSet_JD ~ daylength_diff + I(daylength_diff^2), data = bw.clim)
summary(model.day2)
plot(model.day2)
#quad term not significant
# evaluate r2 for linear model
rss = sum(model.day$residuals^2); r2 = 1-rss/tss
p = length(coef(model.day))-1
r2.day = 1 - (1 - r2) * ((n-1) / (n-p-1))

#tmax_at
model.tmaxat = glm(BudSet_JD ~ Tmax_at, data = bw.clim)
summary(model.tmaxat)
plot(model.tmaxat)
# this is strong but fit maybe quadratic
model.tmaxat2 = glm(BudSet_JD ~ Tmax_at + I(Tmax_at^2), data = bw.clim)
summary(model.tmaxat2)
plot(model.tmaxat2)
# not significant
# evaluate r2 for linear model
rss = sum(model.tmaxat$residuals^2); r2 = 1-rss/tss
p = length(coef(model.tmaxat))-1
r2.tmaxat = 1 - (1 - r2) * ((n-1) / (n-p-1))

#DD_0_at
model.dd0at = glm(BudSet_JD ~ DD_0_at, data = bw.clim)
summary(model.dd0at)
plot(model.dd0at)
# good but weaker than others
# check quad
model.dd0at2 = glm(BudSet_JD ~ DD_0_at + I(DD_0_at^2), data = bw.clim)
summary(model.dd0at2)
plot(model.dd0at2)
#not significant
# evaluate r2 for linear model
rss = sum(model.dd0at$residuals^2); r2 = 1-rss/tss
p = length(coef(model.dd0at))-1
r2.dd0at = 1 - (1 - r2) * ((n-1) / (n-p-1))

#DD_0_wt
model.dd0wt = glm(BudSet_JD ~ DD_0_wt, data = bw.clim)
summary(model.dd0wt)
plot(model.dd0wt)
# similarly weaker
ggplot(aes(DD_0_wt, BudSet_JD), data = bw.clim)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x+I(x^2))
# evaluate r2 for linear model
rss = sum(model.dd0wt$residuals^2); r2 = 1-rss/tss
p = length(coef(model.dd0wt))-1
r2.dd0wt = 1 - (1 - r2) * ((n-1) / (n-p-1))

# MAT
model.mat = glm(BudSet_JD ~ MAT, data = bw.clim)
summary(model.mat)
plot(model.mat)
# good but weaker than others
# evaluate r2 for linear model
rss = sum(model.mat$residuals^2); r2 = 1-rss/tss
p = length(coef(model.mat))-1
r2.mat = 1 - (1 - r2) * ((n-1) / (n-p-1))

# MCMT
model.mcmt = glm(BudSet_JD ~ MCMT, data = bw.clim)
summary(model.mcmt)
plot(model.mcmt)
# okay
# evaluate r2 for linear model
rss = sum(model.mcmt$residuals^2); r2 = 1-rss/tss
p = length(coef(model.mcmt))-1
r2.mcmt = 1 - (1 - r2) * ((n-1) / (n-p-1))

# MWMT
model.mwmt = glm(BudSet_JD ~ MWMT, data = bw.clim)
summary(model.mwmt)
plot(model.mwmt)
# very strong
ggplot(data = bw.clim, aes(x=MWMT, y=BudSet_JD))+
  geom_point()+ geom_smooth(method = 'lm')

# quadratic term not significant
model.mwmt2 = glm(BudSet_JD ~ MWMT + I(MWMT^2), data = bw.clim)
summary(model.mwmt2)
plot(model.mwmt2)
#not significant
# evaluate r2 for linear model
rss = sum(model.mwmt$residuals^2)
r2 = 1-rss/tss
p = length(coef(model.mwmt))-1
r2.mwmt = 1 - (1 - r2) * ((n-1) / (n-p-1))

# MAP
model.map = glm(BudSet_JD ~ MAP, data = bw.clim)
summary(model.map)
plot(model.map)
# not significant
# evaluate r2 for linear model
rss = sum(model.map$residuals^2); r2 = 1-rss/tss
p = length(coef(model.map))-1
r2.map = 1 - (1 - r2) * ((n-1) / (n-p-1))

# TD
model.td = glm(BudSet_JD ~ TD, data = bw.clim)
summary(model.td)
plot(model.td)
# not significant, maybe quadratic
# evaluate r2 for linear model
rss = sum(model.td$residuals^2); r2 = 1-rss/tss
p = length(coef(model.td))-1
r2.td = 1 - (1 - r2) * ((n-1) / (n-p-1))

# eFFP
model.effp = glm(BudSet_JD ~ eFFP, data = bw.clim)
summary(model.effp)
plot(model.effp)
# others better
# evaluate r2 for linear model
rss = sum(model.effp$residuals^2); r2 = 1-rss/tss
p = length(coef(model.effp))-1
r2.effp = 1 - (1 - r2) * ((n-1) / (n-p-1))

# CMD
model.cmd = glm(BudSet_JD ~ CMD, data = bw.clim)
summary(model.cmd)
plot(model.cmd)
# not strong
# evaluate r2 for linear model
rss = sum(model.cmd$residuals^2); r2 = 1-rss/tss
p = length(coef(model.cmd))-1
r2.cmd = 1 - (1 - r2) * ((n-1) / (n-p-1))


#best models are generally associated with summer heat
anova(model.null, model.day, model.dd0at, model.tmaxat, model.dd0wt, model.mwmt, model.mcmt, model.td, model.mat, model.map, model.cmd, model.effp)
aics = AIC(model.null, model.day, model.dd0at, model.tmaxat, model.dd0wt, model.mwmt, model.mcmt, model.td, model.mat, model.map, model.cmd, model.effp)
adj.r2s = c(0, r2.day,r2.dd0at, r2.tmaxat, r2.dd0wt, r2.mwmt, r2.mcmt, r2.td, r2.mat, r2.map, r2.cmd, r2.effp)


results = cbind(aics, adj.r2s)
results

#save the results
write.csv(results, "BlackWalnutCarterFallResults.csv")

#summary(model.mwmt)
#plot(model.mwmt)

# STOP HERE
#run cross validation to assess best model
models = list(model.null, model.day, model.dd0at, model.tmaxat, model.dd0wt, model.mwmt, model.mcmt, model.td, model.mat, model.map, model.cmd, model.effp)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(bw.clim, models[[i]])$delta[2]
}


