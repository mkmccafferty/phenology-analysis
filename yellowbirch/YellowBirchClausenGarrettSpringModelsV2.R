# Spring models for yellow birch- Clausen & Garrett data
# VERSION 2
# author: Mary McCafferty

library(ggplot2)

# read in the data
yb.clim = read.csv("YellowBirch_ClausenGarrett_withClimate.csv")

#climate of the test site
yb.tests = read.csv("YB_AllTestSites_Normal_1941_1970SY_V2.csv")
# this test is in Wisconsin
yb.wi = yb.tests[which(yb.tests$ID2 == "WI"),]

# look at correlation matrix
cor_spring = yb.clim[names(yb.clim) %in% c("growthInitiation", "daylength_diff", 
                                           "DD_0_wt", "DD_0_sp", "DD5_sp", "DD5_wt", 
                                           "MAT", "MWMT", "MCMT", "TD", "MAP", "bFFP")]
pearson_cor_spring = cor(cor_spring, method = "pearson")
spearman_cor_spring = cor(cor_spring, method = "spearman")

#remove the outliar identified in fall analysis
yb.clim = yb.clim[which(yb.clim$SourceNumber!=2980),]

# run models
#implementing linear regressions because growth initiation scores were averaged

par(mfrow= c(2,2))
# the null model
model.null = lm(growthInitiation ~ 1, data = yb.clim)
summary(model.null)
plot(model.null)
r2.null = summary(model.null)$adj.r.squared

# daylength difference
model.day = lm(growthInitiation ~ daylength_diff, data = yb.clim)
summary(model.day)
plot(model.day)
# daylength strong potentially quadratic
model.day2 = lm(growthInitiation ~ daylength_diff + I(daylength_diff^2), data = yb.clim)
summary(model.day2)
plot(model.day2)
#quadratic term significant
r2.day = summary(model.day2)$adj.r.squared

#dd_0_wt
model.dd0wt = lm(growthInitiation ~ DD_0_wt, data = yb.clim)
summary(model.dd0wt)
plot(model.dd0wt)
# probably quadratic

model.dd0wt2 = lm(growthInitiation ~ DD_0_wt + I(DD_0_wt^2), data = yb.clim)
summary(model.dd0wt2)
plot(model.dd0wt2)
#significant, still weaker than latitude
r2.dd0wt = summary(model.dd0wt2)$adj.r.squared

#dd_0_sp
model.dd0sp = lm(growthInitiation ~ DD_0_sp, data = yb.clim)
summary(model.dd0sp)
plot(model.dd0sp)
# significant, quadratic
model.dd0sp2 = lm(growthInitiation ~ DD_0_sp + I(DD_0_sp^2), data = yb.clim)
summary(model.dd0sp2)
plot(model.dd0sp2)
# good but still weaker than latitude
r2.dd0sp = summary(model.dd0sp2)$adj.r.squared

#dd5_wt
model.dd5wt = lm(growthInitiation ~ DD5_wt, data = yb.clim)
summary(model.dd5wt)
plot(model.dd5wt)
# significant, many 0s
r2.dd5wt = summary(model.dd5wt)$adj.r.squared

#dd5_sp
model.dd5sp = lm(growthInitiation ~ DD5_sp, data = yb.clim)
summary(model.dd5sp)
plot(model.dd5sp)
# significant
r2.dd5sp = summary(model.dd5sp)$adj.r.squared

#MAT
model.mat = lm(growthInitiation ~ MAT, data = yb.clim)
summary(model.mat)
plot(model.mat)
# good, maybe quadratic, not strongest
model.mat2 = lm(growthInitiation ~ MAT + I(MAT^2), data = yb.clim)
summary(model.mat2)
plot(model.mat2)
#not significant
r2.mat = summary(model.mat)$adj.r.squared

#MCMT
model.mcmt = lm(growthInitiation ~ MCMT, data = yb.clim)
summary(model.mcmt)
plot(model.mcmt)
# significant
# maybe quadratic

model.mcmt2 = lm(growthInitiation ~ MCMT + I(MCMT^2), data = yb.clim)
summary(model.mcmt2)
plot(model.mcmt2)
#very strong, still weaker than latitude
r2.mcmt = summary(model.mcmt2)$adj.r.squared

#MWMT
model.mwmt = lm(growthInitiation ~ MWMT, data = yb.clim)
summary(model.mwmt)
plot(model.mwmt)
# not very strong
r2.mwmt = summary(model.mwmt)$adj.r.squared

#TD
model.td = lm(growthInitiation ~ TD, data = yb.clim)
summary(model.td)
plot(model.td)
# okay, potentially quadratic

model.td2 = lm(growthInitiation ~ TD + I(TD^2), data = yb.clim)
summary(model.td2)
plot(model.td2)
#quadratic term barely significant
r2.td = summary(model.td2)$adj.r.squared

#MAP
model.map = lm(growthInitiation ~ MAP, data = yb.clim)
summary(model.map)
plot(model.map)
# okay but not super strong, looking quadratic
r2.map = summary(model.map)$adj.r.squared

model.bffp = lm(growthInitiation ~ bFFP, data = yb.clim)
summary(model.bffp)
plot(model.bffp)
r2.bffp = summary(model.bffp)$adj.r.squared

#compare model results
anova(model.null, model.day2, model.dd0wt2, model.dd0sp2, model.dd5wt, model.dd5sp, model.mat, model.mcmt2, model.mwmt, model.td2, model.map, model.bffp)
aics = AIC(model.null, model.day2, model.dd0wt2, model.dd0sp2, model.dd5wt, model.dd5sp, model.mat, model.mcmt2, model.mwmt, model.td2, model.map, model.bffp)
# daylength wins by a landslide
r2s = c(r2.null, r2.day, r2.dd0wt, r2.dd0sp, r2.dd5wt, r2.dd5sp, r2.mat, r2.mcmt, r2.mwmt, r2.td, r2.map, r2.bffp)

#final model
#squared term marginally improves model, significantly greater F stat for linear model
summary(model.day)
plot(model.day)

results = cbind(aics, r2s)
results
write.csv(results, "YellowBirchClausenGarrettSpringResults.csv")


ggplot(data=yb.clim, aes(daylength_diff, growthInitiation))+
  geom_point()+geom_smooth(method='lm', se=F, formula = y~x+I(x^2))
#run cross validation to assess best model
models = list(model.null, model.day2, model.dd0wt2, model.dd0sp2, model.dd5wt, model.dd5sp, model.mat, model.mcmt2, model.mwmt, model.td2, model.map, model.bffp)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(yb.clim, models[[i]])$delta[2]
}

#calculate the r^2
rss = sum(model.day2$residuals^2)
tss = sum((yb.clim$growthInitiation - mean(yb.clim$growthInitiation))^2)
r2 = 1-rss/tss
n = nrow(yb.clim)
p = length(coef(model.day2))-1
adj.r2 = 1 - (1 - r2) * ((n-1) / (n-p-1))
adj.r2
