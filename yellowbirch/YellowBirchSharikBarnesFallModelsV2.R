# yellow birch Sharik and Barnes Fall models
# author: Mary McCafferty
# version 2

library(ggplot2)

#read in the data
yb.clim = read.csv("YellowBirch_SharikBarnes_withClimate.csv")

#climate of the test site
yb.tests = read.csv("YB_AllTestSites_Normal_1941_1970SY_V2.csv")
# this test is in Michigan
yb.mi = yb.tests[which(yb.tests$ID2 == "MI"),]

# response variable: GrowthCessation_MeanDays
yb.clim$GrowthCessation_MeanDays
yb.clim = na.omit(yb.clim)

cor_fall = yb.clim[names(yb.clim) %in% c("GrowthCessation_MeanDays", "daylength_diff",
                             "Tmax_at", "DD_0_at", "DD_0_wt", 
                             "MAT", "MWMT", "MCMT", "TD", "MAP", "eFFP", "CMD")]

pearson_cor_fall = cor(cor_fall, method = "pearson")
spearman_cor_fall = cor(cor_fall, method = "spearman")

# run all models for leaf color
par(mfrow = c(2, 2))

# the null model
model.null = lm(GrowthCessation_MeanDays ~ 1, data = yb.clim)
summary(model.null)
plot(model.null)
r2.null = summary(model.null)$adj.r.squared

# daylength
model.day = lm(GrowthCessation_MeanDays ~ daylength_diff, data = yb.clim)
summary(model.day)
plot(model.day)
# significant but a lot of repeat values

# daylength^2
model.day2 = lm(GrowthCessation_MeanDays ~ daylength_diff + I(daylength_diff^2), data = yb.clim)
summary(model.day2)
plot(model.day2)
#quad term not significant
r2.day = summary(model.day)$adj.r.squared

#tmax_at
model.tmaxat = lm(GrowthCessation_MeanDays ~ Tmax_at, data = yb.clim)
summary(model.tmaxat)
plot(model.tmaxat)
# this is strong but fit maybe quadratic
model.tmaxat2 = lm(GrowthCessation_MeanDays ~ Tmax_at + I(Tmax_at^2), data = yb.clim)
summary(model.tmaxat2)
plot(model.tmaxat2)
# not significant
r2.tmaxat = summary(model.tmaxat)$adj.r.squared

#DD_0_at
model.dd0at = lm(GrowthCessation_MeanDays ~ DD_0_at, data = yb.clim)
summary(model.dd0at)
plot(model.dd0at)
# good 
r2.dd0at = summary(model.dd0at)$adj.r.squared

#DD_0_wt
model.dd0wt = lm(GrowthCessation_MeanDays ~ DD_0_wt, data = yb.clim)
summary(model.dd0wt)
plot(model.dd0wt)
# similarly weaker
ggplot(aes(DD_0_wt, GrowthCessation_MeanDays), data = yb.clim)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x+I(x^2))
# non linear relationship, need to think more and come back

model.dd0wt2 = lm(GrowthCessation_MeanDays ~ DD_0_wt + I(DD_0_wt^2), data = yb.clim)
summary(model.dd0wt2)
plot(model.dd0wt2)
# quad term not sig, lower r2 than other models
r2.dd0wt = summary(model.dd0wt)$adj.r.squared

# MAT
model.mat = lm(GrowthCessation_MeanDays ~ MAT, data = yb.clim)
summary(model.mat)
plot(model.mat)
# very strong
r2.mat = summary(model.mat)$adj.r.squared

# MCMT
model.mcmt = lm(GrowthCessation_MeanDays ~ MCMT, data = yb.clim)
summary(model.mcmt)
plot(model.mcmt)
# okay
r2.mcmt = summary(model.mcmt)$adj.r.squared

# MWMT
model.mwmt = lm(GrowthCessation_MeanDays ~ MWMT, data = yb.clim)
summary(model.mwmt)
plot(model.mwmt)
# not super strong
ggplot(data = yb.clim, aes(x=MWMT, y=GrowthCessation_MeanDays))+
  geom_point()+ geom_smooth(method = 'lm')

model.mwmt2 = lm(GrowthCessation_MeanDays ~ MWMT + I(MWMT^2), data = yb.clim)
summary(model.mwmt2)
plot(model.mwmt2)
#not significant
r2.mwmt = summary(model.mwmt)$adj.r.squared

# MAP
model.map = lm(GrowthCessation_MeanDays ~ MAP, data = yb.clim)
summary(model.map)
plot(model.map)
# not significant
r2.map = summary(model.map)$adj.r.squared

# TD
model.td = lm(GrowthCessation_MeanDays ~ TD, data = yb.clim)
summary(model.td)
plot(model.td)
# weaker than others
r2.td = summary(model.td)$adj.r.squared

# eFFP
model.effp = lm(GrowthCessation_MeanDays ~ eFFP, data = yb.clim)
summary(model.effp)
plot(model.effp)
# others better
r2.effp = summary(model.effp)$adj.r.squared

# CMD
model.cmd = lm(GrowthCessation_MeanDays~ CMD, data = yb.clim)
summary(model.cmd)
plot(model.cmd)
# not strong
r2.cmd = summary(model.cmd)$adj.r.squared

#generally strong correlation with fall temperature variables
anova(model.null, model.day, model.tmaxat, model.dd0at, model.mwmt, model.mcmt, model.mat, model.map, model.effp, model.dd0wt, model.cmd, model.td)
aics = AIC(model.null, model.day, model.tmaxat, model.dd0at, model.mwmt, model.mcmt, model.mat, model.map, model.effp, model.dd0wt, model.cmd, model.td)
r2s = c(r2.null, r2.day, r2.tmaxat, r2.dd0at, r2.mwmt, r2.mcmt, r2.mat, r2.map, r2.effp, r2.dd0wt, r2.cmd, r2.td)


#save the results
results = cbind(aics, r2s)
results
write.csv(results, "YellowBirchSharikBarnesFallResults.csv")
#################################################

#tmaxat is best model
summary(model.tmaxat)
#run cross validation to assess best model
models = list(model.null, model.day, model.tmaxat, model.dd0at, model.mwmt, model.mcmt, model.mat, model.map, model.effp, model.dd0wt, model.cmd, model.td)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(yb.clim, models[[i]])$delta[2]
}

#calculate the r^2
rss = sum(model.tmaxat$residuals^2)
tss = sum((yb.clim$GrowthCessation_MeanDays - mean(yb.clim$GrowthCessation_MeanDays))^2)
r2 = 1-rss/tss
n = nrow(yb.clim)
p = length(coef(model.tmaxat))-1
adj.r2 = 1 - (1 - r2) * ((n-1) / (n-p-1))
adj.r2

