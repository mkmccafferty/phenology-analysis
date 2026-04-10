# analysis of Sharik Barnes Spring data
# VERSION 2
# author: Mary McCafferty

library(ggplot2)
#read in the data
yb.clim = read.csv("YellowBirch_SharikBarnes_withClimate.csv")

#response variable: LeafFlushing_MeanStage
# look at correlation matrix
cor_spring = yb.clim[names(yb.clim) %in% c("LeafFlushing_MeanStage", "daylength_diff", 
                                           "DD_0_wt", "DD_0_sp", "DD5_sp", "DD5_wt", 
                                           "MAT", "MWMT", "MCMT", "TD", "MAP", "bFFP")]
pearson_cor_spring = cor(cor_spring, method = "pearson")
spearman_cor_spring = cor(cor_spring, method = "spearman")


# run models
#implementing linear regressions because leaf flushing scores were averaged

par(mfrow= c(2,2))
# the null model
model.null = lm(LeafFlushing_MeanStage ~ 1, data = yb.clim)
summary(model.null)
plot(model.null)
r2.null = summary(model.null)$adj.r.squared

# daylength difference
model.day = lm(LeafFlushing_MeanStage ~ daylength_diff, data = yb.clim)
summary(model.day)
plot(model.day)
# latitude strong potentially quadratic, residuals not great
model.day2 = lm(LeafFlushing_MeanStage ~ daylength_diff + I(daylength_diff^2), data = yb.clim)
summary(model.day2)
plot(model.day2)
#quadratic term not significant
r2.day = summary(model.day)$adj.r.squared

#dd_0_wt
model.dd0wt = lm(LeafFlushing_MeanStage ~ DD_0_wt, data = yb.clim)
summary(model.dd0wt)
plot(model.dd0wt)
# VERY STRONG

model.dd0wt2 = lm(LeafFlushing_MeanStage ~ DD_0_wt + I(DD_0_wt^2), data = yb.clim)
summary(model.dd0wt2)
plot(model.dd0wt2)
#quad not significant
r2.dd0wt = summary(model.dd0wt)$adj.r.squared

#dd_0_sp
model.dd0sp = lm(LeafFlushing_MeanStage ~ DD_0_sp, data = yb.clim)
summary(model.dd0sp)
plot(model.dd0sp)
# significant, quadratic
model.dd0sp2 = lm(LeafFlushing_MeanStage ~ DD_0_sp + I(DD_0_sp^2), data = yb.clim)
summary(model.dd0sp2)
plot(model.dd0sp2)
# good but still weaker than others
r2.dd0sp = summary(model.dd0sp)$adj.r.squared

#dd5_wt
model.dd5wt = lm(LeafFlushing_MeanStage ~ DD5_wt, data = yb.clim)
summary(model.dd5wt)
plot(model.dd5wt)
# significant, many 0s, maybe quadratic
r2.dd5wt = summary(model.dd5wt)$adj.r.squared

#dd5_sp
model.dd5sp = lm(LeafFlushing_MeanStage~ DD5_sp, data = yb.clim)
summary(model.dd5sp)
plot(model.dd5sp)
# significant, weaker than others
r2.dd5sp = summary(model.dd5sp)$adj.r.squared

#MAT
model.mat = lm(LeafFlushing_MeanStage ~ MAT, data = yb.clim)
summary(model.mat)
plot(model.mat)
# good, maybe quadratic, not strongest
model.mat2 = lm(LeafFlushing_MeanStage ~ MAT + I(MAT^2), data = yb.clim)
summary(model.mat2)
plot(model.mat2)
#not significant
r2.mat = summary(model.mat)$adj.r.squared

#MCMT
model.mcmt = lm(LeafFlushing_MeanStage ~ MCMT, data = yb.clim)
summary(model.mcmt)
plot(model.mcmt)
# VERY STRONG

model.mcmt2 = lm(LeafFlushing_MeanStage ~ MCMT + I(MCMT^2), data = yb.clim)
summary(model.mcmt2)
plot(model.mcmt2)
# quadratic not significant
r2.mcmt = summary(model.mcmt)$adj.r.squared

#MWMT
model.mwmt = lm(LeafFlushing_MeanStage ~ MWMT, data = yb.clim)
summary(model.mwmt)
plot(model.mwmt)
# not very strong
r2.mwmt = summary(model.mwmt)$adj.r.squared

#TD
model.td = lm(LeafFlushing_MeanStage ~ TD, data = yb.clim)
summary(model.td)
plot(model.td)
# okay, not the best residuals

model.td2 = lm(LeafFlushing_MeanStage ~ TD + I(TD^2), data = yb.clim)
summary(model.td2)
plot(model.td2)
#quadratic term not significant
r2.td = summary(model.td)$adj.r.squared

#MAP
model.map = lm(LeafFlushing_MeanStage ~ MAP, data = yb.clim)
summary(model.map)
plot(model.map)
# okay but not super strong, looking quadratic
r2.map = summary(model.map)$adj.r.squared

#bFFP
model.bffp = lm(LeafFlushing_MeanStage ~ bFFP, data = yb.clim)
summary(model.bffp)
plot(model.bffp)
r2.bffp = summary(model.bffp)$adj.r.squared

anova(model.null, model.day, model.dd0wt, model.dd0sp, model.dd5wt, model.dd5sp, model.mat, model.mcmt, model.mwmt, model.td, model.map, model.bffp)
aics = AIC(model.null, model.day, model.dd0wt, model.dd0sp, model.dd5wt, model.dd5sp, model.mat, model.mcmt, model.mwmt, model.td, model.map, model.bffp)
r2s = c(r2.null, r2.day, r2.dd0wt, r2.dd0sp, r2.dd5wt, r2.dd5sp, r2.mat, r2.mcmt, r2.mwmt, r2.td, r2.map, r2.bffp)
# latitude and mcmt are strongest models

#save the results
results = cbind(aics, r2s)
results
write.csv(results, "YellowBirchSharikBarnesSpringResults.csv")






########


#run cross validation to assess best model
models = list(model.null, model.day, model.dd0wt, model.dd0sp, model.dd5wt, model.dd5sp, model.mat, model.mcmt, model.mwmt, model.td, model.map, model.bffp)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(yb.clim, models[[i]])$delta[2]
}

#calculate the r^2
rss = sum(model.mcmt$residuals^2)
tss = sum((yb.clim$LeafFlushing_MeanStage - mean(yb.clim$LeafFlushing_MeanStage))^2)
r2 = 1-rss/tss
n = nrow(yb.clim)
p = length(coef(model.mcmt))-1
adj.r2 = 1 - (1 - r2) * ((n-1) / (n-p-1))
adj.r2
plot(model.day)
plot(model.mcmt)
summary(model.mcmt)
summary(model.day)
#daylength wins on F stat, mcmt wins on everything else
# multiple repeat values for daylength which i am not a fan of
#go with mcmt

View(yb.clim)

ggplot(data=yb.clim, aes(daylength_diff, LeafFlushing_MeanStage))+
  geom_point()+geom_smooth(method='lm', se=F)

ggplot(data=yb.clim, aes(MCMT, LeafFlushing_MeanStage))+
  geom_point()+geom_smooth(method='lm', se=F)

ggplot(data=yb.clim, aes(MCMT, daylength_diff))+
  geom_point()+geom_smooth(method='lm', se=F)
