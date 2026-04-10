# analysis of black walnut fall phenology data from VT
# author: Mary McCafferty
# version 2

library(ggplot2)
library(boot)

# read in the files
bw.clim = read.csv("BlackWalnut_VTNurseryAggregated_withClimate.csv")

#climate data for test sites
tests = read.csv("BW_AllTestSites_Normal_1951_1980SY.csv")
#isolate the test climate data for VT
bw.vt = tests[3,]

# adjust so it is not overinflated with 0s and 1s
for (i in 1:length(bw.clim$Leaf_Ret_10_79)){
  if (bw.clim$Leaf_Ret_10_79[i]== 0) {
    bw.clim$Leaf_Ret_10_79_adj[i] =  0.000000001
  }else if (bw.clim$Leaf_Ret_10_79[i]== 1){
    bw.clim$Leaf_Ret_10_79_adj[i] =  0.99999
  }else{
    bw.clim$Leaf_Ret_10_79_adj[i] =  bw.clim$Leaf_Ret_10_79[i]
  }
}
bw.clim$Leaf_Ret_10_79_adj
# visualize the data

#assess grouping variables
ggplot(aes(Prov, Leaf_Ret_10_79), data = bw.clim)+
  geom_point()

#ggplot(aes(Rep, Leaf_Ret_10_79), data = bw.clim)+
#  geom_point()

#ggplot(aes(Latitude, Leaf_Ret_10_79), data = bw.clim)+
#  geom_point()
# definite trend here
hist(bw.clim$Leaf_Ret_10_79)

# correlation matrix
#correlation matrix
cor_fall = bw.clim[names(bw.clim) %in% c("Leaf_Ret_10_79", "daylength_diff",
                             "Tmax_at", "DD_0_at", "DD_0_wt", 
                             "MAT", "MWMT", "MCMT", "TD", "MAP", "eFFP", "CMD")]


pearson_cor_fall = cor(cor_fall, method = "pearson")
spearman_cor_fall = cor(cor_fall, method = "spearman")

# make the response variable integers
bw.clim$Leaf_Ret_10_79_percentage = as.integer(bw.clim$Leaf_Ret_10_79_percentage)
bw.clim$NotDef_percentage = as.integer(bw.clim$NotDef_percentage)
#make bed and provenance factors
#bw.clim$Bed = as.factor(bw.clim$Bed)
#bw.clim$Prov = as.factor(bw.clim$Prov)

# run the models
#null
glm.null <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ 1,
                  data=bw.clim, family = binomial(link = 'logit'))
summary(glm.null)

#daylength
glm.day <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ daylength_diff,
                 data=bw.clim, family = binomial(link = 'logit'))
summary(glm.day)
#sjPlot::plot_model(glm.day, type = "pred", terms = "daylength_diff[all]" , show.data = T, se=F)
r2.day = 1 - logLik(glm.day) / logLik(glm.null)

#Tmax_at
glm.tmaxat <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ Tmax_at,
                    data=bw.clim, family = binomial(link = 'logit'))
summary(glm.tmaxat)
r2.tmaxat = 1 - logLik(glm.tmaxat) / logLik(glm.null)

#DD_0_at
glm.dd0at <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ DD_0_at,
                    data=bw.clim, family = binomial(link = 'logit'))
summary(glm.dd0at)
r2.dd0at = 1 - logLik(glm.dd0at) / logLik(glm.null)

#DD_0_wt
glm.dd0wt <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ DD_0_wt,
                    data=bw.clim, family = binomial(link = 'logit'))
summary(glm.dd0wt)
r2.dd0wt = 1 - logLik(glm.dd0wt) / logLik(glm.null)

#MAT
glm.mat <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ MAT,
                 data=bw.clim, family = binomial(link = 'logit'))
summary(glm.mat)
#r.squaredGLMM(glm.mat)
sjPlot::plot_model(glm.mat, type = "pred", terms = "MAT[all]" , show.data = T, se=F)
#not sure what else to use to visualize
r2.mat = 1 - logLik(glm.mat) / logLik(glm.null)

#MCMT
glm.mcmt <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ MCMT,
                  data=bw.clim, family = binomial(link = 'logit'))
summary(glm.mcmt)
# AIC = 8476.5
sjPlot::plot_model(glm.mcmt, type = "pred", terms = "MCMT[all]" , show.data = T, se=F)
r2.mcmt = 1 - logLik(glm.mcmt) / logLik(glm.null)

#MWMT
glm.mwmt <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ MWMT,
                  data=bw.clim, family = binomial(link = 'logit'))
summary(glm.mwmt)
r2.mwmt = 1 - logLik(glm.mwmt) / logLik(glm.null)

#TD
glm.td <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ TD,
                data=bw.clim, family = binomial(link = 'logit'))
summary(glm.td)
r2.td = 1 - logLik(glm.td) / logLik(glm.null)


#MAP
glm.map <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ I(MAP/100),
                 data=bw.clim, family = binomial(link = 'logit'))
summary(glm.map)
r2.map = 1 - logLik(glm.map) / logLik(glm.null)

#eFFP
glm.effp <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ I(eFFP/10),
                  data=bw.clim, family = binomial(link = 'logit'))
summary(glm.effp)
r2.effp = 1 - logLik(glm.effp) / logLik(glm.null)

#CMD
glm.cmd <- glm(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ I(CMD/10),
                 data=bw.clim, family = binomial(link = 'logit'))
summary(glm.cmd)
r2.cmd = 1 - logLik(glm.cmd) / logLik(glm.null)

anova(glm.null, glm.day, glm.tmaxat, glm.dd0at, glm.dd0wt, glm.mat, glm.mcmt, glm.mwmt, glm.td, glm.map, glm.effp, glm.cmd)
aics = AIC(glm.null, glm.day, glm.tmaxat, glm.dd0at, glm.dd0wt, glm.mat, glm.mcmt, glm.mwmt, glm.td, glm.map, glm.effp, glm.cmd)
r2s = c(0, r2.day, r2.tmaxat, r2.dd0at, r2.dd0wt, r2.mat, r2.mcmt, r2.mwmt, r2.td, r2.map, r2.effp, r2.cmd)
# daylength and mcmt are the best models

results = cbind(aics, r2s)
results
write.csv(results, "BlackWalnutLeafRetResults.csv")

#STOP HERE

cv(glm.cmd, k="loo")
# issue with data having two response variables?
#run cross validation to assess best model
# delta is the average mean square error from the cross validation
models = list(glm.null, glm.day, glm.tmaxat, glm.dd0at, glm.dd0wt, glm.mat, glm.mcmt, glm.mwmt, glm.td, glm.map, glm.effp, glm.cmd)
set.seed(12022025)
cv.error = rep(0, 12)
for (i in 1:length(models)){
  cv.error[i] = cv.glm(bw.clim, models[[i]])$delta[2]
}


ggplot(data = bw.clim, aes(daylength_diff, MCMT))+
  geom_point()+
  geom_smooth(se=F, method='lm')
cor(bw.clim$daylength_diff, bw.clim$Leaf_Ret_10_79)
# these two are so highly correlated, they basically are the same. Which one to drop and which to keep?




