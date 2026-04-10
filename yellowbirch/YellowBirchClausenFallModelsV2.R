# fall models for Clausen data
# version 2
# author: Mary McCafferty

library(ggplot2)

# read in the data
yb.clim = read.csv("YellowBirch_Clausen2yr_withClimate.csv")

#climate of the test site
yb.tests = read.csv("YB_AllTestSites_Normal_1941_1970SY_V2.csv")
# this test is in Wisconsin
yb.wi = yb.tests[which(yb.tests$ID2 == "WI"),]


# response variable: growthCessation_weeksAfterJuly6_1966

cor_fall = yb.clim[names(yb.clim) %in% c("growthCessation_weeksAfterJuly6_1966", "daylength_diff",
                             "Tmax_at", "DD_0_at", "DD_0_wt", 
                             "MAT", "MWMT", "MCMT", "TD", "MAP", "eFFP", "CMD")]


pearson_cor_fall = cor(cor_fall, method = "pearson")
spearman_cor_fall = cor(cor_fall, method = "spearman")

hist(yb.clim$growthCessation_weeksAfterJuly6_1966)
#remove the outlier
yb.clim = yb.clim[which(yb.clim$StandNumber!=2980),]

# run all models for leaf color
par(mfrow = c(2, 2))

# the null model
model.null = lm(growthCessation_weeksAfterJuly6_1966 ~ 1, data = yb.clim)
summary(model.null)
plot(model.null)
r2.null = summary(model.null)$adj.r.squared

# daylength difference
model.day = lm(growthCessation_weeksAfterJuly6_1966 ~ daylength_diff, data = yb.clim)
summary(model.day)
plot(model.day)
# significant but needs nonlinear fit potentially
# pretty strong

# daylength difference
model.day2 = lm(growthCessation_weeksAfterJuly6_1966 ~ daylength_diff + I(daylength_diff^2), data = yb.clim)
summary(model.day2)
plot(model.day2)
#quad term significant but residuals look okay
r2.day = summary(model.day2)$adj.r.squared

#tmax_at
model.tmaxat = lm(growthCessation_weeksAfterJuly6_1966 ~ Tmax_at, data = yb.clim)
summary(model.tmaxat)
plot(model.tmaxat)
# this is strong but fit is not super linear
model.tmaxat2 = lm(growthCessation_weeksAfterJuly6_1966 ~ Tmax_at + I(Tmax_at^2), data = yb.clim)
summary(model.tmaxat2)
plot(model.tmaxat2)
# 24 seems to be an outlier
r2.tmaxat = summary(model.tmaxat2)$adj.r.squared

#dd0_at
model.dd0at = lm(growthCessation_weeksAfterJuly6_1966 ~ DD_0_at, data = yb.clim)
summary(model.dd0at)
plot(model.dd0at)
# better linear fit than other models but less strong r2
r2.dd0at = summary(model.dd0at)$adj.r.squared

#DD_0_wt
model.dd0wt = lm(growthCessation_weeksAfterJuly6_1966 ~ DD_0_wt, data = yb.clim)
summary(model.dd0wt)
plot(model.dd0wt)
# similarly weaker
ggplot(aes(DD_0_wt, growthCessation_weeksAfterJuly6_1966), data = yb.clim)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x+I(x^2))
# non linear relationship, need to think more and come back

model.dd0wt2 = lm(growthCessation_weeksAfterJuly6_1966 ~ DD_0_wt + I(DD_0_wt^2), data = yb.clim)
summary(model.dd0wt2)
plot(model.dd0wt2)
# quad term not significant
r2.dd0wt = summary(model.dd0wt)$adj.r.squared

# MAT
model.mat = lm(growthCessation_weeksAfterJuly6_1966 ~ MAT, data = yb.clim)
summary(model.mat)
plot(model.mat)
# pretty good but weaker thn latitude, 24 again an outlier
r2.mat = summary(model.mat)$adj.r.squared

# MCMT
model.mcmt = lm(growthCessation_weeksAfterJuly6_1966 ~ MCMT, data = yb.clim)
summary(model.mcmt)
plot(model.mcmt)
# okay
r2.mcmt = summary(model.mcmt)$adj.r.squared

# MWMT
model.mwmt = lm(growthCessation_weeksAfterJuly6_1966 ~ MWMT, data = yb.clim)
summary(model.mwmt)
plot(model.mwmt)
# not super strong
ggplot(data = yb.clim, aes(x=MWMT, y=growthCessation_weeksAfterJuly6_1966))+
  geom_point()+ geom_smooth(method = 'lm')

# quadratic term is better
model.mwmt2 = lm(growthCessation_weeksAfterJuly6_1966 ~ MWMT + I(MWMT^2), data = yb.clim)
summary(model.mwmt2)
plot(model.mwmt2)
r2.mwmt2 = summary(model.mwmt2)$adj.r.squared


# MAP
model.map = lm(growthCessation_weeksAfterJuly6_1966 ~ MAP, data = yb.clim)
summary(model.map)
plot(model.map)
# not as strong
r2.map = summary(model.map)$adj.r.squared

# TD
model.td = lm(growthCessation_weeksAfterJuly6_1966 ~ TD, data = yb.clim)
summary(model.td)
plot(model.td)
# weaker than others
r2.td = summary(model.td)$adj.r.squared

# eFFP
model.effp = lm(growthCessation_weeksAfterJuly6_1966 ~ eFFP, data = yb.clim)
summary(model.effp)
plot(model.effp)
# others better
r2.effp = summary(model.effp)$adj.r.squared

# CMD
model.cmd = lm(growthCessation_weeksAfterJuly6_1966 ~ CMD, data = yb.clim)
summary(model.cmd)
plot(model.cmd)
# not strong
r2.cmd = summary(model.cmd)$adj.r.squared

anova(model.null, model.day2, model.tmaxat2, model.dd0at, model.dd0wt, model.mwmt2, model.mcmt, model.mat, model.map, model.effp, model.cmd, model.td)
aics = AIC(model.null, model.day2, model.tmaxat2, model.dd0at, model.dd0wt, model.mwmt2, model.mcmt, model.mat, model.map, model.effp, model.cmd, model.td)
r2s = c(r2.null, r2.day, r2.tmaxat, r2.dd0at, r2.dd0wt, r2.mwmt2, r2.mcmt, r2.mat, r2.map, r2.effp, r2.cmd, r2.td)


#save the results
results = cbind(aics, r2s)
results
write.csv(results, "YellowBirchClausenFallResults.csv")

#run cross validation to assess best model
models = list(model.null, model.day2, model.tmaxat2, model.dd0at, model.dd0wt, model.mwmt2, model.mcmt, model.mat, model.map, model.effp, model.cmd, model.td)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(yb.clim, models[[i]])$delta[2]
}
#calculate the r^2
rss = sum(model.day2$residuals^2)
tss = sum((yb.clim$growthCessation_weeksAfterJuly6_1966 - mean(yb.clim$growthCessation_weeksAfterJuly6_1966))^2)
r2 = 1-rss/tss
n = nrow(yb.clim)
p = length(coef(model.day2))-1
adj.r2 = 1 - (1 - r2) * ((n-1) / (n-p-1))
adj.r2

summary(model.day2)
summary(model.tmaxat2)
# daylength model is strongest
plot(model.lat2)
ggplot(data = yb.clim, aes(x=daylength_diff, y=growthCessation_weeksAfterJuly6_1966, color = MAT))+
  geom_point()+ geom_smooth(method = 'lm', formula = y~x +I(x^2), se=F)

#examine outlier
#upon inspecting the data files, the latitude is incorrect for 2980 so it should be removed
View(yb.clim)
yb.clim = yb.clim[which(yb.clim$StandNumber!=2980),]
# latitude^2
model.day2.narm = lm(growthCessation_weeksAfterJuly6_1966 ~ daylength_diff + I(daylength_diff^2), data = yb.clim.narm)
summary(model.day2.narm)
plot(model.day2.narm)
ggplot(data = yb.clim.narm, aes(x=daylength_diff, y=growthCessation_weeksAfterJuly6_1966, color=MAP))+
  geom_point(size=4)+ geom_smooth(method = 'lm', formula = y~x+I(x^2), se=F)+
  geom_vline(xintercept = yb.wi$daylength_diff)
# heteroskedastic

#also check 48 (3294)
# coordinate is correct, perhaps there is just more variation in more southerly populations?


#daylength is best model
# populations from lower latitudes were less well predicted by latitude than populations from higher latitudes
# what does that mean

