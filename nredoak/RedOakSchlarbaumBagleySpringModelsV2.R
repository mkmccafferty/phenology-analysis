# spring models for schlarbaum bagley data
# VERSION 2
# author: Mary McCafferty


library(ggplot2)
library(boot)

#read in the data
ro.ne = read.csv("RedOak_SchlarbaumBagley_WithClimate.csv")
testsites = read.csv("RedOakTestSites_ClimateNA_Normal_1931_1960SY.csv")
ts = testsites[which(testsites$ID2 == "NE"),]
testsites1975 = read.csv("RedOakTestSites_ClimateNA_Year_1975SY.csv")
ts1975 = testsites1975[which(testsites$ID2 == "NE"),]
ts_normalv1975 = rbind(ts, ts1975)

# Analysis of spring phenology
# response variable: LeafBudsOpen50Percent_Days


ggplot(data = ro.ne, aes(x=TD, y=LeafBudsOpen50Percent_Days_JD_adj))+
  geom_point()+
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), se=F)



cor_spring = ro.ne[which(names(ro.ne) %in% c("LeafBudsOpen50Percent_Days_JD", "LeafBudsOpen50Percent_Days_JD_adj", 
                                             "Lat.Deg", "DD_0_wt", "DD_0_sp", "DD5_sp", "DD5_wt", 
                                             "MAT", "MWMT", "MCMT", "TD", "MAP"))]
pearson_cor_spring = cor(cor_spring, method = "pearson")
spearman_cor_spring = cor(cor_spring, method = "spearman")

# removing obs number 27 based on outlier analysis from "RedOak_SchlarbaumBagley_EDA
ro.ne = ro.ne[1:26,]


par(mfrow= c(2,2))
# the null model
model.null = lm(LeafBudsOpen50Percent_Days_JD_adj ~ 1, data = ro.ne)
summary(model.null)
plot(model.null)
r2.null = summary(model.null)$adj.r.squared

# daylength_difference
model.day = lm(LeafBudsOpen50Percent_Days_JD_adj ~ daylength_diff, data = ro.ne)
summary(model.day)
plot(model.day)
# daylength not strong potentially quadratic
model.day2 = lm(LeafBudsOpen50Percent_Days_JD_adj ~ daylength_diff + I(daylength_diff^2), data = ro.ne)
summary(model.day2)
plot(model.day2)
#quadratic term significant
#significant with 27 removed
r2.day = summary(model.day2)$adj.r.squared

#dd_0_wt
model.dd0wt = lm(LeafBudsOpen50Percent_Days_JD_adj ~ DD_0_wt, data = ro.ne)
summary(model.dd0wt)
plot(model.dd0wt)
# not significant, poor residuals, maybe lightly quadratic

model.dd0wt2 = lm(LeafBudsOpen50Percent_Days_JD_adj ~ DD_0_wt + I(DD_0_wt^2), data = ro.ne)
summary(model.dd0wt2)
plot(model.dd0wt2)
#significant with 27 removed, residuals poor
r2.dd0wt = summary(model.dd0wt2)$adj.r.squared

#dd_0_sp
model.dd0sp = lm(LeafBudsOpen50Percent_Days_JD_adj ~ DD_0_sp, data = ro.ne)
summary(model.dd0sp)
plot(model.dd0sp)
# not significant,
# significant without 27
r2.dd0sp = summary(model.dd0sp)$adj.r.squared

#dd5_wt
model.dd5wt = lm(LeafBudsOpen50Percent_Days_JD_adj ~ DD5_wt, data = ro.ne)
summary(model.dd5wt)
plot(model.dd5wt)
# not significant
r2.dd5wt = summary(model.dd5wt)$adj.r.squared

#dd5_sp
model.dd5sp = lm(LeafBudsOpen50Percent_Days_JD_adj ~ DD5_sp, data = ro.ne)
summary(model.dd5sp)
plot(model.dd5sp)
# not significant
r2.dd5sp = summary(model.dd5sp)$adj.r.squared

#MAT
model.mat = lm(LeafBudsOpen50Percent_Days_JD_adj ~ MAT, data = ro.ne)
summary(model.mat)
plot(model.mat)
# not significant, maybe quadratic
model.mat2 = lm(LeafBudsOpen50Percent_Days_JD_adj ~ MAT + I(MAT^2), data = ro.ne)
summary(model.mat2)
plot(model.mat2)
#low r2
r2.mat = summary(model.mat2)$adj.r.squared

#MCMT
model.mcmt = lm(LeafBudsOpen50Percent_Days_JD_adj ~ MCMT, data = ro.ne)
summary(model.mcmt)
plot(model.mcmt)
# not significant
# maybe quadratic with 27 removed

model.mcmt2 = lm(LeafBudsOpen50Percent_Days_JD_adj ~ MCMT + I(MCMT^2), data = ro.ne)
summary(model.mcmt2)
plot(model.mcmt2)
r2.mcmt = summary(model.mcmt2)$adj.r.squared

#MWMT
model.mwmt = lm(LeafBudsOpen50Percent_Days_JD_adj ~ MWMT, data = ro.ne)
summary(model.mwmt)
plot(model.mwmt)
# not significant
r2.mwmt = summary(model.mwmt)$adj.r.squared

#TD
model.td = lm(LeafBudsOpen50Percent_Days_JD_adj ~ TD, data = ro.ne)
summary(model.td)
plot(model.td)
# stronger, potentially quadratic

model.td2 = lm(LeafBudsOpen50Percent_Days_JD_adj ~ TD + I(TD^2), data = ro.ne)
summary(model.td2)
plot(model.td2)
#quadratic term not significant
# significant with 27 removed, residuals better than most others
r2.td = summary(model.td2)$adj.r.squared

#MAP
model.map = lm(LeafBudsOpen50Percent_Days_JD_adj ~ MAP, data = ro.ne)
summary(model.map)
plot(model.map)
# okay but not super strong
r2.map = summary(model.map)$adj.r.squared

#bFFP
model.bffp = lm(LeafBudsOpen50Percent_Days_JD_adj ~ bFFP, data = ro.ne)
summary(model.bffp)
plot(model.bffp)
# okay but not super strong
r2.bffp = summary(model.bffp)$adj.r.squared

# compare models
anova(model.null, model.day2, model.dd0wt, model.dd0sp, model.dd5wt, model.dd5sp, model.mwmt, model.mcmt2, model.td2, model.mat2, model.map, model.bffp)
aics = AIC(model.null, model.day2, model.dd0wt, model.dd0sp, model.dd5wt, model.dd5sp, model.mwmt, model.mcmt2, model.td2, model.mat2, model.map, model.bffp)
# td strongest
r2s = c(r2.null, r2.day, r2.dd0wt, r2.dd0sp, r2.dd5wt, r2.dd5sp, r2.mwmt, r2.mcmt, r2.td, r2.mat, r2.map, r2.bffp)

results = cbind(aics, r2s)
results
write.csv(results, "RedOakSchlarbaumBagleySpringResults.csv")



#calculate the r^2
rss = sum(model.td2$residuals^2)
tss = sum((ro.ne$LeafBudsOpen50Percent_Days_JD_adj - mean(ro.ne$LeafBudsOpen50Percent_Days_JD_adj))^2)
r2 = 1-rss/tss
n = nrow(ro.ne)
p = length(coef(model.td2))-1
adj.r2 = 1 - (1 - r2) * ((n-1) / (n-p-1))
adj.r2

#run cross validation to assess best model
models = list(model.null, model.day2, model.dd0wt, model.dd0sp, model.dd5wt, model.dd5sp, model.mwmt, model.mcmt2, model.td2, model.mat2, model.map, model.bffp)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(ro.ne, models[[i]])$delta[2]
}
