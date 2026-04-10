# Black cherry carter fall models
# version 2
# author: Mary McCafferty

#load packages
library(ggplot2) #for visualization

#read in data
bc.clim = read.csv("BlackCherryCarter_withClimate.csv")

#make the date variables in date format
bc.clim$BudSet_1978_Mean = as.Date(bc.clim$BudSet_1978_Mean, "%Y-%m-%d")
bc.clim$LeafFall_1978_Mean = as.Date(bc.clim$LeafFall_1978_Mean, "%Y-%m-%d")

hist(bc.clim$BudSet_1978_Mean, breaks = 10)
hist(bc.clim$LeafFall_1978_Mean, breaks = 10)
# not normally distributed
# more variation in budset than leaf fall at same test site

bc.clim$budset_days = as.numeric(bc.clim$BudSet_1978_Mean- as.Date("12/31/1977", "%m/%d/%Y"))
bc.clim$leaffall_days = as.numeric(bc.clim$LeafFall_1978_Mean- as.Date("12/31/1977", "%m/%d/%Y"))

bc.fall.cor = bc.clim[names(bc.clim) %in% c("budset_days", "leaffall_days", "daylength_diff",
                                              "Tmax_at", "DD_0_at", "DD_0_wt", 
                                              "MAT", "MWMT", "MCMT", "TD", "MAP", "eFFP", "CMD")]

bc.fall.cor = na.omit(bc.fall.cor)

bc.fall.pearson = cor(bc.fall.cor, method = "pearson") 
bc.fall.spearman = cor(bc.fall.cor, method = "spearman")

# budset highly correlated with summer temperature variables (particularly ones of max/ high temperatures)
# leaf fall also highly correlated with summer heat variables
# not as strong with drought/ moisture index


# filter to only Reedsville observations
bc.rd = na.omit(bc.clim)

# run all models for budset
par(mfrow = c(2, 2))

# the null model
set.null = lm(budset_days ~ 1, data = bc.rd)
summary(set.null)
plot(set.null)
r2.null = summary(set.null)$adj.r.squared

# daylength
set.day = lm(budset_days ~ daylength_diff, data = bc.rd)
summary(set.day)
plot(set.day)
# significant but needs nonlinear fit potentially

set.day2 = lm(budset_days ~ daylength_diff + I(daylength_diff^2), data = bc.rd)
summary(set.day2)
plot(set.day2)
#significant
r2.day = summary(set.day2)$adj.r.squared

#tmax_at
set.tmaxat = lm(budset_days ~ Tmax_at, data = bc.rd)
summary(set.tmaxat)
plot(set.tmaxat)
plot(bc.rd$Tmax_at, bc.rd$budset_days)
# this is strong but fit is not super linear
r2.tmaxat = summary(set.tmaxat)$adj.r.squared

#DD_0_at
set.dd0at = lm(budset_days ~ DD_0_at, data = bc.rd)
summary(set.dd0at)
plot(set.dd0at)
r2.dd0at = summary(set.dd0at)$adj.r.squared

#DD_0_wt
set.dd0wt = lm(budset_days ~ DD_0_wt, data = bc.rd)
summary(set.dd0wt)
plot(set.dd0wt)
r2.dd0wt = summary(set.dd0wt)$adj.r.squared

# MAT
set.mat = lm(budset_days ~ MAT, data = bc.rd)
summary(set.mat)
plot(set.mat)
# qqplot not great
r2.mat = summary(set.mat)$adj.r.squared

# MCMT
set.mcmt = lm(budset_days ~ MCMT, data = bc.rd)
summary(set.mcmt)
plot(set.mcmt)
# not super strong
r2.mcmt = summary(set.mcmt)$adj.r.squared

# MWMT
set.mwmt = lm(budset_days ~ MWMT, data = bc.rd)
summary(set.mwmt)
plot(set.mwmt)
# not great resid
ggplot(data = bc.rd, aes(x=MWMT, y=budset_days))+
  geom_point()+ geom_smooth(method = 'lm')

# quadratic term is better
set.mwmt2 = lm(budset_days ~ MWMT + I(MWMT^2), data = bc.rd)
summary(set.mwmt2)
plot(set.mwmt2)
ggplot(data = bc.rd, aes(x=MWMT, y=budset_days))+
  geom_point()+ geom_smooth(method = 'lm', formula = y~x+I(x^2), se=F)
r2.mwmt = summary(set.mwmt2)$adj.r.squared

# MAP
set.map = lm(budset_days ~ MAP, data = bc.rd)
summary(set.map)
plot(set.map)
# not significant
r2.map = summary(set.map)$adj.r.squared

# TD
set.td = lm(budset_days ~ TD, data = bc.rd)
summary(set.td)
plot(set.td)
# weaker than others
r2.td = summary(set.td)$adj.r.squared

# eFFP
set.effp = lm(budset_days ~ eFFP, data = bc.rd)
summary(set.effp)
plot(set.effp)
# pretty good residuals
r2.effp = summary(set.effp)$adj.r.squared

# CMD
set.cmd = lm(budset_days ~ CMD, data = bc.rd)
summary(set.cmd)
plot(set.cmd)
# better residuals, weaker model
r2.cmd = summary(set.cmd)$adj.r.squared

#compare the models
anova(set.null, set.day2, set.tmaxat, set.dd0at, set.dd0wt, set.mat, set.mcmt, set.mwmt2, set.map, set.td, set.effp, set.cmd)
aics = AIC(set.null, set.day2, set.tmaxat, set.dd0at, set.dd0wt, set.mat, set.mcmt, set.mwmt2, set.map, set.td, set.effp, set.cmd)
r2s = c(r2.null, r2.day, r2.tmaxat, r2.dd0at, r2.dd0wt, r2.mat, r2.mcmt, r2.mwmt, r2.map, r2.td, r2.effp, r2.cmd)

results = cbind(aics, r2s)
results
write.csv(results, "BlackCherryBudsetResults.csv")

ggplot(aes(MWMT, budset_days), data = bc.rd)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x +I(x^2))

#MWMT is best model

par(mfrow = c(2,2))
### Repeat with models for leaf fall
# the null model
fall.null = lm(leaffall_days ~ 1, data = bc.rd)
summary(fall.null)
plot(fall.null)
r2.null = summary(fall.null)$adj.r.squared

# daylength
fall.day = lm(leaffall_days ~ daylength_diff, data = bc.rd)
summary(fall.day)
plot(fall.day)
# significant but others are more

fall.day2 = lm(leaffall_days ~ daylength_diff + I(daylength_diff^2), data = bc.rd)
summary(fall.day2)
plot(fall.day2)
#quadratic term not significant
r2.day = summary(fall.day)$adj.r.squared

#tmax_at
fall.tmaxat = lm(leaffall_days ~ Tmax_at, data = bc.rd)
summary(fall.tmaxat)
plot(fall.tmaxat)
plot(bc.rd$Tmax_at, bc.rd$leaffall_days)
# this is strong but tmaxsm better
r2.tmaxat = summary(fall.tmaxat)$adj.r.squared

#DD_0_at
fall.dd0at = lm(leaffall_days ~ DD_0_at, data = bc.rd)
summary(fall.dd0at)
plot(fall.dd0at)
# not great
#check quadratic
fall.dd0at2 = lm(leaffall_days ~ DD_0_at + I(DD_0_at^2), data = bc.rd)
summary(fall.dd0at2)
plot(fall.dd0at2)
#significant
r2.dd0at = summary(fall.dd0at2)$adj.r.squared

#DD_0_wt
fall.dd0wt = lm(leaffall_days ~ DD_0_wt, data = bc.rd)
summary(fall.dd0wt)
plot(fall.dd0wt)
# weaker
#check quadratic
fall.dd0wt2 = lm(leaffall_days ~ DD_0_wt + I(DD_0_wt^2), data = bc.rd)
summary(fall.dd0wt2)
plot(fall.dd0wt2)
#better but not significant enough
r2.dd0wt = summary(fall.dd0wt)$adj.r.squared

# MAT
fall.mat = lm(leaffall_days ~ MAT, data = bc.rd)
summary(fall.mat)
plot(fall.mat)
# qqplot not great
r2.mat = summary(fall.mat)$adj.r.squared

# MCMT
fall.mcmt = lm(leaffall_days ~ MCMT, data = bc.rd)
summary(fall.mcmt)
plot(fall.mcmt)
# not super strong
r2.mcmt = summary(fall.mcmt)$adj.r.squared

# MWMT
fall.mwmt = lm(leaffall_days ~ MWMT, data = bc.rd)
summary(fall.mwmt)
plot(fall.mwmt)
# one of strongest
ggplot(data = bc.rd, aes(x=MWMT, y=leaffall_days))+
  geom_point()+ geom_smooth(method = 'lm')

# quadratic term not significant
fall.mwmt2 = lm(leaffall_days ~ MWMT + I(MWMT^2), data = bc.rd)
summary(fall.mwmt2)
plot(fall.mwmt2)
r2.mwmt = summary(fall.mwmt)$adj.r.squared

# MAP
fall.map = lm(leaffall_days ~ MAP, data = bc.rd)
summary(fall.map)
plot(fall.map)
# not significant

fall.map2 = lm(leaffall_days ~ MAP + I(MAP^2), data = bc.rd)
summary(fall.map2)
plot(fall.map2)
#not significant enough
r2.map = summary(fall.map)$adj.r.squared

# TD
fall.td = lm(leaffall_days ~ TD, data = bc.rd)
summary(fall.td)
plot(fall.td)
# weaker than others
r2.td = summary(fall.td)$adj.r.squared

# eFFP
fall.effp = lm(leaffall_days ~ eFFP, data = bc.rd)
summary(fall.effp)
plot(fall.effp)
# pretty good residuals, slight curve
r2.effp = summary(fall.effp)$adj.r.squared

# CMD
fall.cmd = lm(leaffall_days ~ CMD, data = bc.rd)
summary(fall.cmd)
plot(fall.cmd)
# better residuals, weaker model
r2.cmd = summary(fall.cmd)$adj.r.squared

#compare the models
anova(fall.null, fall.day, fall.tmaxat, fall.dd0at2, fall.dd0wt, fall.mat, fall.mcmt, fall.mwmt, fall.map, fall.td, fall.effp, fall.cmd)
aics = AIC(fall.null, fall.day, fall.tmaxat, fall.dd0at2, fall.dd0wt, fall.mat, fall.mcmt, fall.mwmt, fall.map, fall.td, fall.effp, fall.cmd)
r2s = c(r2.null, r2.day, r2.tmaxat, r2.dd0at, r2.dd0wt, r2.mat, r2.mcmt, r2.mwmt, r2.map, r2.td, r2.effp, r2.cmd)


#save the results
results = cbind(aics, r2s)
results
write.csv(results, "BlackCherryLeafFallResults.csv")

