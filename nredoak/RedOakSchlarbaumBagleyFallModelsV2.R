# N. red oak Fall models (Schlarbaum & Bagley)
# VERSION 2
# author: Mary McCafferty

library(ggplot2)

#read in the data
ro.ne = read.csv("RedOak_SchlarbaumBagley_WithClimate.csv")
testsites = read.csv("RedOakTestSites_ClimateNA_Normal_1931_1960SY.csv")
ts = testsites[which(testsites$ID2 == "NE"),]

par(mfrow = c(2,2))
plot(ro.ne$MAT, ro.ne$Leaf_Color_Days)
plot(ro.ne$MAT, ro.ne$Leaf_Death_days)
plot(ro.ne$MAT, ro.ne$Leaf_Drop_days)

ggplot(data = ro.ne, aes(x=MAT))+
  geom_point(aes(y=Leaf_Color_Days_Date, color = "Leaf Color"))+
  geom_point(aes(y=Leaf_Death_Days_Date, color = "Leaf Death"))+
  geom_point(aes(y=Leaf_Drop_Days_Date, color = "Leaf Drop"))+
  geom_vline(xintercept = ts$MAT[1])

par(mfrow = c(2,2))
plot(ro.ne$MAP, ro.ne$Leaf_Color_Days)
plot(ro.ne$MAP, ro.ne$Leaf_Death_days)
plot(ro.ne$MAP, ro.ne$Leaf_Drop_days)


# plot again with year adjustment
ggplot(data = ro.ne, aes(x=MAT))+
  geom_point(aes(y=Leaf_Color_Days_JD_adj, color = "Leaf Color"))+
  geom_point(aes(y=Leaf_Death_Days_JD_adj, color = "Leaf Death"))+
  geom_point(aes(y=Leaf_Drop_Days_JD_adj, color = "Leaf Drop"))+
  geom_smooth(aes(y=Leaf_Color_Days_JD_adj), formula = y~x, method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Death_Days_JD_adj), formula = y~x, method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Drop_Days_JD_adj), formula = y~x, method = 'lm', se = F)
#geom_vline(xintercept = ts$MAT[1])+
#geom_vline(xintercept = -0.8)+
#geom_vline(xintercept = 19.4)


ggplot(data = ro.ne, aes(x=MCMT))+
  geom_point(aes(y=Leaf_Color_Days_JD_adj, color = "Leaf Color"))+
  geom_point(aes(y=Leaf_Death_Days_JD_adj, color = "Leaf Death"))+
  geom_point(aes(y=Leaf_Drop_Days_JD_adj, color = "Leaf Drop"))+
  geom_smooth(aes(y=Leaf_Color_Days_JD_adj), formula = y~x, method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Death_Days_JD_adj), formula = y~x, method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Drop_Days_JD_adj), formula = y~x, method = 'lm', se = F)

ggplot(data = ro.ne, aes(x=daylength_diff))+
  geom_point(aes(y=Leaf_Color_Days_JD_adj, color = "Leaf Color"))+
  geom_point(aes(y=Leaf_Death_Days_JD_adj, color = "Leaf Death"))+
  geom_point(aes(y=Leaf_Drop_Days_JD_adj, color = "Leaf Drop"))+
  geom_smooth(aes(y=Leaf_Color_Days_JD_adj), formula = y~x + I(x^2), method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Death_Days_JD_adj), formula = y~x + I(x^2), method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Drop_Days_JD_adj), formula = y~x + I(x^2), method = 'lm', se = F)

ggplot(data = ro.ne, aes(x=DD_0_at))+
  geom_point(aes(y=Leaf_Color_Days_JD_adj, color = "Leaf Color"))+
  geom_point(aes(y=Leaf_Death_Days_JD_adj, color = "Leaf Death"))+
  geom_point(aes(y=Leaf_Drop_Days_JD_adj, color = "Leaf Drop"))+
  geom_smooth(aes(y=Leaf_Color_Days_JD_adj), formula = y~x, method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Death_Days_JD_adj), formula = y~x, method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Drop_Days_JD_adj), formula = y~x, method = 'lm', se = F)

hist(ro.ne$Leaf_Color_Days_JD_adj)
hist(ro.ne$Leaf_Death_Days_JD_adj)
hist(ro.ne$Leaf_Drop_Days_JD_adj)

# evaluate correlations over select variables
cor_fall = ro.ne[names(ro.ne) %in% c("Leaf_Color_Days_JD_adj", "Leaf_Death_Days_JD_adj", "Leaf_Drop_Days_JD_adj", 
                                     "daylength_diff", "Tmax_at", "DD_0_at", "DD_0_wt",
                                     "MAT", "MWMT", "MCMT", "TD", "MAP", "eFFP", "CMD")]

pearson_cor_fall = cor(cor_fall, method = "pearson")
spearman_cor_fall = cor(cor_fall, method = "spearman")


# run all models for leaf color
par(mfrow = c(2, 2))

# the null model
col.null = lm(Leaf_Color_Days_JD_adj ~ 1, data = ro.ne)
summary(col.null)
plot(col.null)
r2.null = summary(col.null)$adj.r.squared

# daylength
col.day = lm(Leaf_Color_Days_JD_adj ~ daylength_diff, data = ro.ne)
summary(col.day)
plot(col.day)
# significant but needs nonlinear fit potentially
# pretty strong

# daylength^2
col.day2 = lm(Leaf_Color_Days_JD_adj ~ daylength_diff + I(daylength_diff^2), data = ro.ne)
summary(col.day2)
plot(col.day2)
#quad term marginally significant but residuals are still curved and heteroskedastic
ggplot(aes(daylength_diff, Leaf_Color_Days_JD_adj), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x + I(x^2))
#not significant enough
r2.day = summary(col.day)$adj.r.squared

#dd_0_at
col.dd0at = lm(Leaf_Color_Days_JD_adj ~ DD_0_at, data = ro.ne)
summary(col.dd0at)
plot(col.dd0at)
ggplot(aes(DD_0_at, Leaf_Color_Days_JD_adj), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x)
# pretty strong
r2.dd0at = summary(col.dd0at)$adj.r.squared


#DD_0_wt
col.dd0wt = lm(Leaf_Color_Days_JD_adj ~ DD_0_wt, data = ro.ne)
summary(col.dd0wt)
plot(col.dd0wt)
# similarly nonlinear,weaker
ggplot(aes(DD_0_wt, Leaf_Color_Days_JD_adj), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x)
# non linear relationship, need to think more and come back

col.dd0wt2 = lm(Leaf_Color_Days_JD_adj ~ DD_0_wt + I(DD_0_wt^2), data = ro.ne)
summary(col.dd0wt2)
plot(col.dd0wt2)
# quad term not significant
r2.dd0wt = summary(col.dd0wt)$adj.r.squared

#Tmax_at
col.tmaxat = lm(Leaf_Color_Days_JD_adj ~ Tmax_at, data = ro.ne)
summary(col.tmaxat)
plot(col.tmaxat)
# pretty good but weaker than others

col.tmaxat2 = lm(Leaf_Color_Days_JD_adj ~ Tmax_at + I(Tmax_at^2), data = ro.ne)
summary(col.tmaxat2)
plot(col.tmaxat2)
#not significant
r2.tmaxat = summary(col.tmaxat)$adj.r.squared

# MAT
col.mat = lm(Leaf_Color_Days_JD_adj ~ MAT, data = ro.ne)
summary(col.mat)
plot(col.mat)
# not strongest
r2.mat = summary(col.mat)$adj.r.squared

# MCMT
col.mcmt = lm(Leaf_Color_Days_JD_adj ~ MCMT, data = ro.ne)
summary(col.mcmt)
plot(col.mcmt)
# pretty strong
ggplot(aes(MCMT, Leaf_Color_Days_JD_adj), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F)
r2.mcmt = summary(col.mcmt)$adj.r.squared

# MWMT
col.mwmt = lm(Leaf_Color_Days_JD_adj ~ MWMT, data = ro.ne)
summary(col.mwmt)
plot(col.mwmt)
# not super strong
ggplot(data = ro.ne, aes(x=MWMT, y=Leaf_Color_Days_JD_adj))+
  geom_point()+ geom_smooth(method = 'lm', se=F)
r2.mwmt = summary(col.mwmt)$adj.r.squared

# MAP
col.map = lm(Leaf_Color_Days_JD_adj ~ MAP, data = ro.ne)
summary(col.map)
plot(col.map)
# not as strong
r2.map = summary(col.map)$adj.r.squared

# TD
col.td = lm(Leaf_Color_Days_JD_adj ~ TD, data = ro.ne)
summary(col.td)
plot(col.td)
# pretty strong but a bit weaker than others
r2.td = summary(col.td)$adj.r.squared

# CMD
col.cmd = lm(Leaf_Color_Days_JD_adj ~ CMD, data = ro.ne)
summary(col.cmd)
plot(col.cmd)
# pretty strong but a bit weaker than others
r2.cmd = summary(col.cmd)$adj.r.squared

# eFFP
col.effp = lm(Leaf_Color_Days_JD_adj ~ eFFP, data = ro.ne)
summary(col.effp)
plot(col.effp)
# pretty strong but a bit weaker than others
r2.effp = summary(col.effp)$adj.r.squared

anova(col.null, col.dd0wt, col.dd0at, col.day, col.tmaxat, col.mat, col.mwmt, col.mcmt, col.map, col.td, col.cmd, col.effp)
aics = AIC(col.null, col.dd0wt, col.dd0at, col.day, col.tmaxat, col.mat, col.mwmt, col.mcmt, col.map, col.td, col.cmd, col.effp)
r2s = c(r2.null, r2.dd0wt, r2.dd0at, r2.day, r2.tmaxat, r2.mat, r2.mwmt, r2.mcmt, r2.map, r2.td, r2.cmd, r2.effp)


#save the results
results = cbind(aics, r2s)
results
write.csv(results, "RedOakFallColorResults.csv")


# repeat for leaf death

#leaf death
# the null model
death.null = lm(Leaf_Death_Days_JD_adj ~ 1, data = ro.ne)
summary(death.null)
plot(death.null)
r2.null = summary(death.null)$adj.r.squared

# daylength
death.day = lm(Leaf_Death_Days_JD_adj ~ daylength_diff, data = ro.ne)
summary(death.day)
plot(death.day)
# significant but needs nonlinear fit potentially
# pretty strong

# daylength^2
death.day2 = lm(Leaf_Death_Days_JD_adj ~ daylength_diff + I(daylength_diff^2), data = ro.ne)
summary(death.day2)
plot(death.day2)
ggplot(aes(daylength_diff, Leaf_Death_Days_JD_adj), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x + I(x^2))
# looks better than leaf color but still heteroskedastic
r2.day = summary(death.day)$adj.r.squared

#Tmin_at
death.dd0at = lm(Leaf_Death_Days_JD_adj ~ DD_0_at, data = ro.ne)
summary(death.dd0at)
plot(death.dd0at)
r2.dd0at = summary(death.dd0at)$adj.r.squared

#DD_0_wt
death.dd0wt = lm(Leaf_Death_Days_JD_adj ~ DD_0_wt, data = ro.ne)
summary(death.dd0wt)
plot(death.dd0wt)
# similarly nonlinear,okay model
ggplot(aes(DD_0_wt, Leaf_Death_Days_JD_adj), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x+I(x^2))
r2.dd0wt = summary(death.dd0wt)$adj.r.squared

#Tmax_at
death.tmaxat = lm(Leaf_Death_Days_JD_adj ~ Tmax_at, data = ro.ne)
summary(death.tmaxat)
plot(death.tmaxat)
# pretty good but 
r2.tmaxat = summary(death.tmaxat)$adj.r.squared

# MAT
death.mat = lm(Leaf_Death_Days_JD_adj ~ MAT, data = ro.ne)
summary(death.mat)
plot(death.mat)
# not strongest
r2.mat = summary(death.mat)$adj.r.squared

# MCMT
death.mcmt = lm(Leaf_Death_Days_JD_adj ~ MCMT, data = ro.ne)
summary(death.mcmt)
plot(death.mcmt)
# pretty strong
ggplot(aes(MCMT, Leaf_Death_Days_JD_adj), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x)
r2.mcmt = summary(death.mcmt)$adj.r.squared

# MWMT
death.mwmt = lm(Leaf_Death_Days_JD_adj ~ MWMT, data = ro.ne)
summary(death.mwmt)
plot(death.mwmt)
# not super strong, poor residuals
ggplot(data = ro.ne, aes(x=MWMT, y=Leaf_Death_Days_JD_adj))+
  geom_point()+ geom_smooth(method = 'lm', se=F)
r2.mwmt = summary(death.mwmt)$adj.r.squared

# MAP
death.map = lm(Leaf_Death_Days_JD_adj ~ MAP, data = ro.ne)
summary(death.map)
plot(death.map)
# not as strong
r2.map = summary(death.map)$adj.r.squared

# TD
death.td = lm(Leaf_Death_Days_JD_adj ~ TD, data = ro.ne)
summary(death.td)
plot(death.td)
# pretty strong but a bit weaker than others
r2.td = summary(death.td)$adj.r.squared

#CMD
death.cmd = lm(Leaf_Death_Days_JD_adj ~ CMD, data = ro.ne)
summary(death.cmd)
plot(death.cmd)
r2.cmd = summary(death.cmd)$adj.r.squared

#eFFP
death.effp = lm(Leaf_Death_Days_JD_adj ~ eFFP, data = ro.ne)
summary(death.effp)
plot(death.effp)
r2.effp = summary(death.effp)$adj.r.squared


anova(death.null, death.dd0wt, death.dd0at, death.day, death.tmaxat, death.mat, death.mcmt, death.mwmt, death.td, death.map, death.cmd, death.effp)
aics = AIC(death.null, death.dd0wt, death.dd0at, death.day, death.tmaxat, death.mat, death.mcmt, death.mwmt, death.td, death.map, death.cmd, death.effp)
# dd0at or daylength best model here
r2s = c(r2.null, r2.dd0wt, r2.dd0at, r2.day, r2.tmaxat, r2.mat, r2.mwmt, r2.mcmt, r2.map, r2.td, r2.cmd, r2.effp)


#save the results
results = cbind(aics, r2s)
results
write.csv(results, "RedOakFallDeathResults.csv")


#leaf drop
# the null model
drop.null = lm(Leaf_Drop_Days_JD_adj ~ 1, data = ro.ne)
summary(drop.null)
plot(drop.null)
r2.null = summary(death.null)$adj.r.squared

# daylength
drop.day = lm(Leaf_Drop_Days_JD_adj ~ daylength_diff, data = ro.ne)
summary(drop.day)
plot(drop.day)
# significant but needs nonlinear fit potentially
# pretty strong

# daylength^2
drop.day2 = lm(Leaf_Drop_Days_JD_adj ~ daylength_diff + I(daylength_diff^2), data = ro.ne)
summary(drop.day2)
plot(drop.day2)
# not good residuals, heteroskedastic
r2.day = summary(death.day)$adj.r.squared

#DD_0_at
drop.dd0at = lm(Leaf_Drop_Days_JD_adj ~ DD_0_at, data = ro.ne)
summary(drop.dd0at)
plot(drop.dd0at)
r2.dd0at = summary(death.dd0at)$adj.r.squared

#DD_0_wt
drop.dd0wt = lm(Leaf_Drop_Days_JD_adj ~ DD_0_wt, data = ro.ne)
summary(drop.dd0wt)
plot(drop.dd0wt)
# similarly nonlinear,weaker
ggplot(aes(DD_0_wt, Leaf_Drop_Days_JD_adj), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x+I(x^2))
#DD_0_wt^2
drop.dd0wt2 = lm(Leaf_Drop_Days_JD_adj ~ DD_0_wt +I(DD_0_wt^2), data = ro.ne)
summary(drop.dd0wt2)
plot(drop.dd0wt2)
#not significant
r2.dd0wt = summary(death.dd0wt)$adj.r.squared

#Tmax_at
drop.tmaxat = lm(Leaf_Drop_Days_JD_adj ~ Tmax_at, data = ro.ne)
summary(drop.tmaxat)
plot(drop.tmaxat)
# pretty good 
r2.tmaxat = summary(death.tmaxat)$adj.r.squared

# MAT
drop.mat = lm(Leaf_Drop_Days_JD_adj ~ MAT, data = ro.ne)
summary(drop.mat)
plot(drop.mat)
# not strongest
r2.mat = summary(death.mat)$adj.r.squared

# MCMT
drop.mcmt = lm(Leaf_Drop_Days_JD_adj ~ MCMT, data = ro.ne)
summary(drop.mcmt)
plot(drop.mcmt)
# pretty strong
ggplot(aes(MCMT, Leaf_Drop_Days_JD_adj), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x)
r2.mcmt = summary(death.mcmt)$adj.r.squared

# MWMT
drop.mwmt = lm(Leaf_Drop_Days_JD_adj ~ MWMT, data = ro.ne)
summary(drop.mwmt)
plot(drop.mwmt)
# not super strong
ggplot(data = ro.ne, aes(x=MWMT, y=Leaf_Drop_Days_JD_adj))+
  geom_point()+ geom_smooth(method = 'lm', se=F)

# MWMT^2
drop.mwmt2 = lm(Leaf_Drop_Days_JD_adj ~ MWMT + (MWMT^2), data = ro.ne)
summary(drop.mwmt2)
plot(drop.mwmt2)
r2.mwmt = summary(death.mwmt)$adj.r.squared

# MAP
drop.map = lm(Leaf_Drop_Days_JD_adj ~ MAP, data = ro.ne)
summary(drop.map)
plot(drop.map)
# not as strong
r2.td = summary(death.td)$adj.r.squared

# TD
drop.td = lm(Leaf_Drop_Days_JD_adj ~ TD, data = ro.ne)
summary(drop.td)
plot(drop.td)
# pretty strong but a bit weaker than others
r2.td = summary(death.td)$adj.r.squared

#CMD
drop.cmd = lm(Leaf_Drop_Days_JD_adj ~ CMD, data = ro.ne)
summary(drop.cmd)
plot(drop.cmd)
r2.cmd = summary(death.cmd)$adj.r.squared

#eFFP
drop.effp = lm(Leaf_Drop_Days_JD_adj ~ eFFP, data = ro.ne)
summary(drop.effp)
plot(drop.effp)
r2.effp = summary(death.effp)$adj.r.squared

anova(drop.null, drop.dd0wt, drop.dd0at, drop.day2, drop.tmaxat, drop.mat, drop.mcmt, drop.mwmt, drop.td, drop.map, drop.cmd, drop.effp)
aics = AIC(drop.null, drop.dd0wt, drop.dd0at, drop.day2, drop.tmaxat, drop.mat, drop.mcmt, drop.mwmt, drop.td, drop.map, drop.cmd, drop.effp)
r2s = c(r2.null, r2.dd0wt, r2.dd0at, r2.day, r2.tmaxat, r2.mat, r2.mwmt, r2.mcmt, r2.map, r2.td, r2.cmd, r2.effp)

#save the results
results = cbind(aics, r2s)
results
write.csv(results, "RedOakFallDropResults.csv")

#MCMT and daylength important in all three models
ggplot(data = ro.ne, aes(x=DD_0_at))+
  geom_point(aes(y=Leaf_Color_Days_JD_adj, color = "Leaf Color"))+
  geom_point(aes(y=Leaf_Death_Days_JD_adj, color = "Leaf Death"))+
  geom_point(aes(y=Leaf_Drop_Days_JD_adj, color = "Leaf Drop"))+
  geom_smooth(aes(y=Leaf_Color_Days_JD_adj), formula = y~x, method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Death_Days_JD_adj), formula = y~x, method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Drop_Days_JD_adj), formula = y~x, method = 'lm', se = F)

ggplot(data = ro.ne, aes(x=daylength_diff))+
  geom_point(aes(y=Leaf_Color_Days_JD_adj, color = "Leaf Color"))+
  geom_point(aes(y=Leaf_Death_Days_JD_adj, color = "Leaf Death"))+
  geom_point(aes(y=Leaf_Drop_Days_JD_adj, color = "Leaf Drop"))+
  geom_smooth(aes(y=Leaf_Color_Days_JD_adj), formula = y~x+I(x^2), method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Death_Days_JD_adj), formula = y~x+I(x^2), method = 'lm', se = F)+
  geom_smooth(aes(y=Leaf_Drop_Days_JD_adj), formula = y~x+I(x^2), method = 'lm', se = F)


#there appears to be a difference in the slopes of these models so make "color to death" variable to evaluate
ro.ne$ColorToDrop_Days = ro.ne$Leaf_Death_Days_JD - ro.ne$Leaf_Color_Days_JD

#leaf drop
# the null model
cd.null = lm(ColorToDrop_Days ~ 1, data = ro.ne)
summary(cd.null)
plot(cd.null)
r2.null = summary(cd.null)$adj.r.squared

# daylength
cd.day = lm(ColorToDrop_Days ~ daylength_diff, data = ro.ne)
summary(cd.day)
plot(cd.day)
# significant but needs nonlinear fit potentially
# pretty strong

# daylength^2
cd.day2 = lm(ColorToDrop_Days ~ daylength_diff + I(daylength_diff^2), data = ro.ne)
summary(cd.day2)
plot(cd.day2)
# not good residuals, heteroskedastic
r2.day = summary(cd.day)$adj.r.squared

#DD_0_at
cd.dd0at = lm(ColorToDrop_Days ~ DD_0_at, data = ro.ne)
summary(cd.dd0at)
plot(cd.dd0at)
r2.dd0at = summary(cd.dd0at)$adj.r.squared

#DD_0_wt
cd.dd0wt = lm(ColorToDrop_Days ~ DD_0_wt, data = ro.ne)
summary(cd.dd0wt)
plot(cd.dd0wt)
# weaker
ggplot(aes(DD_0_wt, ColorToDrop_Days), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x+I(x^2))
r2.dd0wt = summary(cd.dd0wt)$adj.r.squared

#Tmax_at
cd.tmaxat = lm(ColorToDrop_Days ~ Tmax_at, data = ro.ne)
summary(cd.tmaxat)
plot(cd.tmaxat)
# pretty good 
#check quadratic
cd.tmaxat2 = lm(ColorToDrop_Days ~ Tmax_at + I(Tmax_at^2), data = ro.ne)
summary(cd.tmaxat2)
plot(cd.tmaxat2)
#quad term is significant
r2.tmaxat = summary(cd.tmaxat2)$adj.r.squared

# MAT
cd.mat = lm(ColorToDrop_Days ~ MAT, data = ro.ne)
summary(cd.mat)
plot(cd.mat)
# not strongest
#check quadratic
cd.mat2 = lm(ColorToDrop_Days ~ MAT + I(MAT^2), data = ro.ne)
summary(cd.mat2)
plot(cd.mat2)
#quad term is significant
r2.mat = summary(cd.mat2)$adj.r.squared

# MCMT
cd.mcmt = lm(ColorToDrop_Days ~ MCMT, data = ro.ne)
summary(cd.mcmt)
plot(cd.mcmt)
# pretty strong
ggplot(aes(MCMT, ColorToDrop_Days), data = ro.ne)+
  geom_point()+geom_smooth(method = 'lm', se=F, formula = y~x)
#test quadratic term
cd.mcmt2 = lm(ColorToDrop_Days ~ MCMT + I(MCMT^2), data = ro.ne)
summary(cd.mcmt2)
plot(cd.mcmt2)
#quad term is significant
r2.mcmt = summary(cd.mcmt2)$adj.r.squared

# MWMT
cd.mwmt = lm(ColorToDrop_Days ~ MWMT, data = ro.ne)
summary(cd.mwmt)
plot(cd.mwmt)
# not super strong
ggplot(data = ro.ne, aes(x=MWMT, y=ColorToDrop_Days))+
  geom_point()+ geom_smooth(method = 'lm', se=F)
r2.mwmt = summary(cd.mwmt)$adj.r.squared

# MAP
cd.map = lm(ColorToDrop_Days ~ MAP, data = ro.ne)
summary(cd.map)
plot(cd.map)
# not as strong
#quadratic?
cd.map2 = lm(ColorToDrop_Days ~ MAP + I(MAP^2), data = ro.ne)
summary(cd.map2)
plot(cd.map2)
#significant
r2.map = summary(cd.map2)$adj.r.squared

# TD
cd.td = lm(ColorToDrop_Days ~ TD, data = ro.ne)
summary(cd.td)
plot(cd.td)
# pretty strong but a bit weaker than others
r2.td = summary(cd.td)$adj.r.squared

#CMD
cd.cmd = lm(ColorToDrop_Days ~ CMD, data = ro.ne)
summary(cd.cmd)
plot(cd.cmd)
r2.cmd = summary(cd.cmd)$adj.r.squared

#eFFP
cd.effp = lm(ColorToDrop_Days ~ eFFP, data = ro.ne)
summary(cd.effp)
plot(cd.effp)
r2.effp = summary(cd.effp)$adj.r.squared

#compare the models 
anova(cd.null, cd.day2, cd.dd0at, cd.dd0wt, cd.tmaxat2, cd.mat2, cd.mcmt2, cd.mwmt, cd.map2, cd.td, cd.cmd, cd.effp)
aics = AIC(cd.null, cd.day2, cd.dd0at, cd.dd0wt, cd.tmaxat2, cd.mat2, cd.mcmt2, cd.mwmt, cd.map2, cd.td, cd.cmd, cd.effp)
r2s = c(r2.null, r2.dd0wt, r2.dd0at, r2.day, r2.tmaxat, r2.mat, r2.mwmt, r2.mcmt, r2.map, r2.td, r2.cmd, r2.effp)

#save the results
results = cbind(aics, r2s)
results
write.csv(results, "RedOakFallColorToDeathResults.csv")


ggplot(data=ro.ne, aes(x=DD_0_at, y=ColorToDrop_Days))+
  geom_point()+
  geom_smooth(method='lm', se=F, formula = y~x)

cor(ro.ne$DD_0_at, ro.ne$daylength_diff)
cor(ro.ne$MCMT, ro.ne$daylength_diff)
#more variance at lower values of daylength_diff



# cross validation being left ouy
#run cross validation to assess best model
models = list(col.null, col.dd0wt, col.dd0at, col.day2, col.tmaxat, col.mat, col.mwmt, col.mcmt, col.map, col.td, col.cmd, col.effp)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(ro.ne, models[[i]])$delta[2]
}

#calculate the r^2
rss = sum(col.dd0at$residuals^2)
tss = sum((ro.ne$Leaf_Color_Days_JD_adj - mean(ro.ne$Leaf_Color_Days_JD_adj))^2)
r2 = 1-rss/tss
n = nrow(ro.ne)
p = length(coef(col.dd0at))-1
adj.r2 = 1 - (1 - r2) * ((n-1) / (n-p-1))
adj.r2


#run cross validation to assess best model
models = list(death.null, death.dd0wt, death.dd0at, death.day2, death.tmaxat, death.mat, death.mcmt, death.mwmt, death.td, death.map, death.cmd, death.effp)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(ro.ne, models[[i]])$delta[2]
}

#calculate the r^2
rss = sum(death.dd0at$residuals^2)
tss = sum((ro.ne$Leaf_Death_Days_JD_adj - mean(ro.ne$Leaf_Death_Days_JD_adj))^2)
r2 = 1-rss/tss
n = nrow(ro.ne)
p = length(coef(death.dd0at))-1
adj.r2 = 1 - (1 - r2) * ((n-1) / (n-p-1))
adj.r2

#run cross validation to assess best model
models = list(drop.null, drop.dd0wt, drop.dd0at, drop.day2, drop.tmaxat, drop.mat, drop.mcmt, drop.mwmt, drop.td, drop.map, drop.cmd, drop.effp)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(ro.ne, models[[i]])$delta[2]
}

#calculate the r^2
rss = sum(drop.dd0at$residuals^2)
tss = sum((ro.ne$Leaf_Drop_Days_JD_adj - mean(ro.ne$Leaf_Drop_Days_JD_adj))^2)
r2 = 1-rss/tss
n = nrow(ro.ne)
p = length(coef(drop.dd0at))-1
adj.r2 = 1 - (1 - r2) * ((n-1) / (n-p-1))
adj.r2


#run cross validation to assess best model
models = list(cd.null, cd.day2, cd.dd0at, cd.dd0wt, cd.tmaxat2, cd.mat2, cd.mcmt2, cd.mwmt, cd.map2, cd.td, cd.cmd, cd.effp)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(ro.ne, models[[i]])$delta[2]
}