# Black cherry carter spring models
# VERSION 2
# author: Mary McCafferty

#load packages
library(ggplot2) # for graphics

#read in data
bc.clim = read.csv("BlackCherryCarter_withClimate.csv")

# explore the data
hist(bc.clim$budbreak_mean_days)

#change response variables to date format
bc.clim$budbreak_JD = as.Date(bc.clim$budbreak_JD, "%m/%d/%Y")
bc.clim$flowering_JD = as.Date(bc.clim$flowering_JD, "%m/%d/%Y")

# not normally distributed
# more variation in budset than leaf fall at same test site

# assess correlation matrices for spring phenology with winter, spring, and annual climate variables
colnames(bc.clim)
bc.cor = bc.clim[names(bc.clim) %in% c("budbreak_mean_days", "flowering_mean_days", "daylength_diff", 
                                       "DD_0_wt", "DD_0_sp", "DD5_sp", "DD5_wt", "MAT", "MWMT", "MCMT", "TD", "MAP")]

bc.spring.pearson = cor(bc.cor, method = "pearson")
bc.spring.spearman = cor(bc.cor, method = "spearman")

length(unique(bc.clim$Provenance))
bc.clim$NumberOfProvenances
bc.clim = bc.clim[!is.na(bc.clim$budbreak_mean_days),] # remove na values (provenances not planted at test site)

# run all models including test site as a fixed effect

par(mfrow= c(2,2))
# the null model
budbreak.null = lm(budbreak_mean_days ~ 1, data = bc.clim)
summary(budbreak.null)
plot(budbreak.null)
r2.null = summary(budbreak.null)$adj.r.squared

# test site
budbreak.test = lm(budbreak_mean_days ~ TestSite, data = bc.clim)
summary(budbreak.test)
plot(budbreak.test)
r2.test = summary(budbreak.test)$adj.r.squared
n = nrow(bc.clim); fss = sum(budbreak.test$residuals^2)


# daylength difference
budbreak.day = lm(budbreak_mean_days ~ daylength_diff + TestSite, data = bc.clim)
summary(budbreak.day)
plot(budbreak.day)
# latitude not significant

#test quad term
budbreak.day2 = lm(budbreak_mean_days ~ daylength_diff +I(daylength_diff^2) + TestSite, data = bc.clim)
summary(budbreak.day2)
plot(budbreak.day2)
#not significantly improved
r2.day = summary(budbreak.day)$adj.r.squared
rss = sum(budbreak.day$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.day))-1
test.r2.day = 1 - (1 - r2) * ((n-1) / (n-p-1))

#dd_0_wt
budbreak.dd0wt = lm(budbreak_mean_days ~ DD_0_wt + TestSite, data = bc.clim)
summary(budbreak.dd0wt)
plot(budbreak.dd0wt)
# not significant

#check quad term
budbreak.dd0wt2 = lm(budbreak_mean_days ~ DD_0_wt + I(DD_0_wt^2) + TestSite, data = bc.clim)
summary(budbreak.dd0wt2)
plot(budbreak.dd0wt2)
# strong
#quad term much stronger
#too many values concentrated near 0
r2.dd0wt = summary(budbreak.dd0wt2)$adj.r.squared
rss = sum(budbreak.dd0wt2$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.dd0wt2))-1
test.r2.dd0wt = 1 - (1 - r2) * ((n-1) / (n-p-1))

ggplot(aes(x=DD_0_wt, y=budbreak_mean_days, color = TestSite), data = bc.clim)+
  geom_point()+
  geom_smooth(method='lm', se=F, formula = y~x+I(x^2))

#dd_0_sp
budbreak.dd0sp = lm(budbreak_mean_days ~ DD_0_sp + TestSite, data = bc.clim)
summary(budbreak.dd0sp)
plot(budbreak.dd0sp)
# a little significant
#test quadratic term
budbreak.dd0sp2 = lm(budbreak_mean_days ~ DD_0_sp + I(DD_0_sp^2) + TestSite, data = bc.clim)
summary(budbreak.dd0sp2)
plot(budbreak.dd0sp2)
#quad term significant
r2.dd0sp = summary(budbreak.dd0sp2)$adj.r.squared
rss = sum(budbreak.dd0sp2$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.dd0sp2))-1
test.r2.dd0sp = 1 - (1 - r2) * ((n-1) / (n-p-1))

#dd5_wt
budbreak.dd5wt = lm(budbreak_mean_days ~ DD5_wt + TestSite, data = bc.clim)
summary(budbreak.dd5wt)
plot(budbreak.dd5wt)
# not significant

budbreak.dd5wt2 = lm(budbreak_mean_days ~ DD5_wt + I(DD5_wt^2) + TestSite, data = bc.clim)
summary(budbreak.dd5wt2)
plot(budbreak.dd5wt2)
#marginally significant, not better than linear model
r2.dd5wt = summary(budbreak.dd5wt)$adj.r.squared
rss = sum(budbreak.dd5wt$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.dd5wt))-1
test.r2.dd5wt = 1 - (1 - r2) * ((n-1) / (n-p-1))

#dd5_sp
budbreak.dd5sp = lm(budbreak_mean_days ~ DD5_sp + TestSite, data = bc.clim)
summary(budbreak.dd5sp)
plot(budbreak.dd5sp)
# not significant

budbreak.dd5sp2 = lm(budbreak_mean_days ~ DD5_sp + I(DD5_sp^2) + TestSite, data = bc.clim)
summary(budbreak.dd5sp2)
plot(budbreak.dd5sp2)
#not significant
r2.dd5sp = summary(budbreak.dd5sp)$adj.r.squared
rss = sum(budbreak.dd5sp$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.dd5sp))-1
test.r2.dd5sp = 1 - (1 - r2) * ((n-1) / (n-p-1))

#MAT
#visual relationship is a mess
budbreak.mat = lm(budbreak_mean_days ~ MAT + TestSite, data = bc.clim)
summary(budbreak.mat)
plot(budbreak.mat)
# not significant

#check quadratic model
budbreak.mat2 = lm(budbreak_mean_days ~ MAT + I(MAT^2) + TestSite, data = bc.clim)
summary(budbreak.mat2)
plot(budbreak.mat2)
# significant
r2.mat = summary(budbreak.mat2)$adj.r.squared
rss = sum(budbreak.mat2$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.mat2))-1
test.r2.mat = 1 - (1 - r2) * ((n-1) / (n-p-1))

ggplot(aes(x=MAT, y=budbreak_mean_days, color = TestSite), data = bc.clim)+
  geom_point()+
  geom_smooth(method='lm', se=F, formula = y~x+I(x^2))

#MCMT
budbreak.mcmt = lm(budbreak_mean_days ~ MCMT + TestSite, data = bc.clim)
summary(budbreak.mcmt)
plot(budbreak.mcmt)
# not significant

budbreak.mcmt2 = lm(budbreak_mean_days ~ MCMT + I(MCMT^2) + TestSite, data = bc.clim)
summary(budbreak.mcmt2)
plot(budbreak.mcmt2)
# not significant but better than linear model
r2.mcmt = summary(budbreak.mcmt)$adj.r.squared
rss = sum(budbreak.mcmt$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.mcmt))-1
test.r2.mcmt = 1 - (1 - r2) * ((n-1) / (n-p-1))

#MWMT
budbreak.mwmt = lm(budbreak_mean_days ~ MWMT + TestSite, data = bc.clim)
summary(budbreak.mwmt)
plot(budbreak.mwmt)
# not significant

budbreak.mwmt2 = lm(budbreak_mean_days ~ MWMT + I(MWMT^2) + TestSite, data = bc.clim)
summary(budbreak.mwmt2)
plot(budbreak.mwmt2)
#significant
r2.mwmt = summary(budbreak.mwmt2)$adj.r.squared
rss = sum(budbreak.mwmt2$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.mwmt2))-1
test.r2.mwmt = 1 - (1 - r2) * ((n-1) / (n-p-1))

#TD
budbreak.td = lm(budbreak_mean_days ~ TD + TestSite, data = bc.clim)
summary(budbreak.td)
plot(budbreak.td)
# SIGNIFICANT

budbreak.td2 = lm(budbreak_mean_days ~ TD + I(TD^2) + TestSite, data = bc.clim)
summary(budbreak.td2)
plot(budbreak.td2)
#not significant
r2.td = summary(budbreak.td)$adj.r.squared
rss = sum(budbreak.td$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.td))-1
test.r2.td = 1 - (1 - r2) * ((n-1) / (n-p-1))

#MAP
budbreak.map = lm(budbreak_mean_days ~ MAP + TestSite, data = bc.clim)
summary(budbreak.map)
plot(budbreak.map)
# not significant

# test quadratic term
budbreak.map2 = lm(budbreak_mean_days ~ MAP + I(MAP^2) + TestSite, data = bc.clim)
summary(budbreak.map2)
plot(budbreak.map2)
#not significant
r2.map = summary(budbreak.map)$adj.r.squared
rss = sum(budbreak.map$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.map))-1
test.r2.map = 1 - (1 - r2) * ((n-1) / (n-p-1))

#bFFP
budbreak.bffp = lm(budbreak_mean_days ~ bFFP + TestSite, data = bc.clim)
summary(budbreak.bffp)
plot(budbreak.bffp)
# not significant
r2.bffp = summary(budbreak.bffp)$adj.r.squared
rss = sum(budbreak.bffp$residuals^2)
r2 = 1 - rss/fss
p = length(coef(budbreak.bffp))-1
test.r2.bffp = 1 - (1 - r2) * ((n-1) / (n-p-1))

#The slopes at the test sites seem to be similar and the test sites have similar climates so we will not test for interaction

# compare models
anova(budbreak.null, budbreak.test, budbreak.day, budbreak.dd5sp, budbreak.dd5wt, budbreak.dd0sp2, budbreak.dd0wt2, budbreak.td, budbreak.mat, budbreak.mwmt2, budbreak.mcmt2, budbreak.map, budbreak.bffp)
aics = AIC(budbreak.null, budbreak.test, budbreak.day, budbreak.dd5sp, budbreak.dd5wt, budbreak.dd0sp2, budbreak.dd0wt2, budbreak.td, budbreak.mat, budbreak.mwmt2, budbreak.mcmt2, budbreak.map, budbreak.bffp)
r2s = c(r2.null, r2.test, r2.day, r2.dd5sp, r2.dd5wt, r2.dd0sp, r2.dd0wt, r2.td, r2.mat, r2.mwmt, r2.mcmt, r2.map, r2.bffp)
test.r2s = c(r2.null, r2.test, test.r2.day, test.r2.dd5sp, test.r2.dd5wt, test.r2.dd0sp, test.r2.dd0wt, test.r2.td, test.r2.mat, test.r2.mwmt, test.r2.mcmt, test.r2.map, test.r2.bffp)



results = cbind(aics, r2s, test.r2s)
results
# Export to CSV
write.csv(results, "BlackCherryCarterSpringResultsV2.csv")


