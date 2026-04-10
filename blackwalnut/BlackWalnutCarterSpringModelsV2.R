# Analysis of black walnut spring models from Carter
# VERSION 2
#author: Mary McCafferty

#load libraries
library(ggplot2)
library(boot)

# read in the files
bw.clim = read.csv("BlackWalnut_Carter_withClimate.csv")

#variable of interest: BudBurst_JD
bw.clim = subset(bw.clim, !is.na(BudBurst_JD))

# look at correlation matrix
cor_spring = bw.clim[names(bw.clim) %in% c("BudBurst_JD", "daylength_diff", 
                                           "DD_0_wt", "DD_0_sp", "DD5_sp", "DD5_wt", 
                                           "MAT", "MWMT", "MCMT", "TD", "MAP", "bFFP")]
pearson_cor_spring = cor(cor_spring, method = "pearson")
spearman_cor_spring = cor(cor_spring, method = "spearman")


par(mfrow= c(2,2))
# the null model
model.null = glm(BudBurst_JD ~ 1, data = bw.clim)
summary(model.null)
plot(model.null)
tss = sum((bw.clim$BudBurst_JD - mean(bw.clim$BudBurst_JD))^2);n = nrow(bw.clim)

# daylength
model.day = glm(BudBurst_JD ~ daylength_diff, data = bw.clim)
summary(model.day)
plot(model.day)
# daylength not strong
model.day2 = glm(BudBurst_JD ~ daylength_diff + I(daylength_diff^2), data = bw.clim)
summary(model.day2)
plot(model.day2)
#quadratic term not significant
rss = sum(model.day$residuals^2); r2 = 1-rss/tss
p = length(coef(model.day))-1
r2.day = 1 - (1 - r2) * ((n-1) / (n-p-1))

#dd_0_wt
model.dd0wt = glm(BudBurst_JD ~ DD_0_wt, data = bw.clim)
summary(model.dd0wt)
plot(model.dd0wt)
# not sig
rss = sum(model.dd0wt$residuals^2); r2 = 1-rss/tss
p = length(coef(model.dd0wt))-1
r2.dd0wt = 1 - (1 - r2) * ((n-1) / (n-p-1))

#dd_0_sp
model.dd0sp = glm(BudBurst_JD ~ DD_0_sp, data = bw.clim)
summary(model.dd0sp)
plot(model.dd0sp)
# not sig
rss = sum(model.dd0sp$residuals^2); r2 = 1-rss/tss
p = length(coef(model.dd0sp))-1
r2.dd0sp = 1 - (1 - r2) * ((n-1) / (n-p-1))


#dd5_wt
model.dd5wt = glm(BudBurst_JD ~ DD5_wt, data = bw.clim)
summary(model.dd5wt)
plot(model.dd5wt)
# not sig
rss = sum(model.dd5wt$residuals^2); r2 = 1-rss/tss
p = length(coef(model.dd5wt))-1
r2.dd5wt = 1 - (1 - r2) * ((n-1) / (n-p-1))

#dd5_sp
model.dd5sp = glm(BudBurst_JD ~ DD5_sp, data = bw.clim)
summary(model.dd5sp)
plot(model.dd5sp)
# not sig
rss = sum(model.dd5sp$residuals^2); r2 = 1-rss/tss
p = length(coef(model.dd5sp))-1
r2.dd5sp = 1 - (1 - r2) * ((n-1) / (n-p-1))

#MAT
model.mat = glm(BudBurst_JD ~ MAT, data = bw.clim)
summary(model.mat)
plot(model.mat)
#not significant
rss = sum(model.mat$residuals^2); r2 = 1-rss/tss
p = length(coef(model.mat))-1
r2.mat = 1 - (1 - r2) * ((n-1) / (n-p-1))

#MCMT
model.mcmt = glm(BudBurst_JD ~ MCMT, data = bw.clim)
summary(model.mcmt)
plot(model.mcmt)
# not significant

ggplot(data=bw.clim, aes(MCMT, BudBurst_JD))+
  geom_point()+geom_smooth(se=F, method='lm')

model.mcmt2 = glm(BudBurst_JD ~ MCMT + I(MCMT^2), data = bw.clim)
summary(model.mcmt2)
plot(model.mcmt2)
# quadratic not significant
rss = sum(model.mcmt$residuals^2); r2 = 1-rss/tss
p = length(coef(model.mcmt))-1
r2.mcmt = 1 - (1 - r2) * ((n-1) / (n-p-1))

#MWMT
model.mwmt = glm(BudBurst_JD ~ MWMT, data = bw.clim)
summary(model.mwmt)
plot(model.mwmt)
# not sig
rss = sum(model.mwmt$residuals^2); r2 = 1-rss/tss
p = length(coef(model.mwmt))-1
r2.mwmt = 1 - (1 - r2) * ((n-1) / (n-p-1))

#TD
model.td = glm(BudBurst_JD ~ TD, data = bw.clim)
summary(model.td)
plot(model.td)
# okay, not the best residuals

model.td2 = glm(BudBurst_JD ~ TD + I(TD^2), data = bw.clim)
summary(model.td2)
plot(model.td2)
#quadratic term not significant
ggplot(data=bw.clim, aes(TD, BudBurst_JD))+
  geom_point()+geom_smooth(se=F, method='lm')
rss = sum(model.td$residuals^2); r2 = 1-rss/tss
p = length(coef(model.td))-1
r2.td = 1 - (1 - r2) * ((n-1) / (n-p-1))

#MAP
model.map = glm(BudBurst_JD ~ MAP, data = bw.clim)
summary(model.map)
plot(model.map)
# not sig looking quadratic
model.map2 = glm(BudBurst_JD ~ MAP + I(MAT^2), data = bw.clim)
summary(model.map2)
plot(model.map2)
#not sig
rss = sum(model.map$residuals^2); r2 = 1-rss/tss
p = length(coef(model.map))-1
r2.map = 1 - (1 - r2) * ((n-1) / (n-p-1))


#bFFP
model.bffp = glm(BudBurst_JD ~ bFFP, data = bw.clim)
summary(model.bffp)
plot(model.bffp)
rss = sum(model.bffp$residuals^2); r2 = 1-rss/tss
p = length(coef(model.bffp))-1
r2.bffp = 1 - (1 - r2) * ((n-1) / (n-p-1))

# none of the models are significant

#compare models
anova(model.null, model.day, model.dd0wt, model.dd0sp, model.dd5wt, model.dd5sp, model.mat, model.mcmt, model.mwmt, model.map, model.td, model.bffp)
aics = AIC(model.null, model.day, model.dd0wt, model.dd0sp, model.dd5wt, model.dd5sp, model.mat, model.mcmt, model.mwmt, model.map, model.td, model.bffp)
adj.r2s = c(0, r2.day, r2.dd0wt, r2.dd0sp, r2.dd5wt, r2.dd5sp, r2.mat, r2.mcmt, r2.mwmt, r2.map, r2.td, r2.bffp)

results = cbind(aics, adj.r2s)
results
write.csv(results, "BlackWalnutCarterSpringResults.csv")


#run cross validation to assess best model
models = list(model.null, model.day, model.dd0wt, model.dd0sp, model.dd5wt, model.dd5sp, model.mat, model.mcmt, model.mwmt, model.map, model.td, model.bffp)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(bw.clim, models[[i]])$delta[2]
}
