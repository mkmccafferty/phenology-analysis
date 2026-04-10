# visualize red maple spring model
# author: Mary McCafferty

library(ggplot2)
library(ordinal)
library(MASS)

setwd("C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Data/RedMaple/ReadyFiles")

# read in the data
rm.clim = read.csv("RedMaple_Townsend1982_withClimate.csv")
ts = read.csv("RedMaple_TestSites_ClimateNA_Normal_1941_1970SY.csv")

#variable of interest is: Avg_Time_Flush (Degree of flushing in April 1979/80)

# remove na values for resp var of interest
rm.clim = rm.clim[!is.na(rm.clim$Avg_Time_Flush),]
rm.clim$Avg_Time_Flush = ordered(rm.clim$Avg_Time_Flush)

# the best model
#DD_0
model.dd0wt = clmm(Avg_Time_Flush ~ DD_0_wt + (1|test), data = rm.clim, link = "logit", threshold = "flexible")
summary(model.dd0wt)
# AIC = 352.58

model.dd0wt = polr(Avg_Time_Flush ~ DD_0_wt + test, data = rm.clim, Hess = TRUE)
summary(model.dd0wt)
# AIC = 354.96

#"#66C2A5" "#FC8D62" "#8DA0CB" "#E78AC3" "#A6D854" "#FFD92F" "#E5C494" "#B3B3B3"
library(ggiraphExtra)
library(viridis)  
library(RColorBrewer)
brewer.pal(n=8, name = "Set2")

safe_colorblind_palette <- c("#882255", "#888888", "#88CCEE", "#6699CC", "#44AA99", 
                             "#999933", "#117733", "#332288", "#CC6677", "#DDCC77", "#AA4499")



gpredictions = data.frame(ggeffects::predict_response(model.dd0wt,  terms = c("DD_0_wt [all]", "test"), by = "seedSource", type = "fixed", bias_correction = FALSE))

#round the DD_0 variable
gpredictions$DD_0_wt = factor(round(gpredictions$x, -2))

colnames(gpredictions)[6] = "Avg_Time_Flush"


#subset to only include avg time flush = 2, 4, 6, 8
gpredictions2 = gpredictions[which(gpredictions$Avg_Time_Flush == 2 | gpredictions$Avg_Time_Flush == 5 | gpredictions$Avg_Time_Flush == 8),]

ggplot(gpredictions2, aes(x = DD_0_wt, y = predicted, fill = Avg_Time_Flush)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#44AA99", "#DDCC77", "#AA4499")) + 
  scale_x_discrete(limits = rev(levels(gpredictions2$DD_0_wt)))+
  theme_bw()+ xlab("Population home climate (Winter degree days below 0 °C)") + ylab("Predicted probability of flush stage")+
  #ggtitle("Probabilities of bud flush stage across temperature gradient")+
  labs(fill = "Bud flush\nscore")+
  facet_wrap(vars(group))+
  theme(legend.text = element_text(size=12), legend.title = element_text(size=12),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

gpredictions2 = gpredictions[which(gpredictions$Avg_Time_Flush == 2 | gpredictions$Avg_Time_Flush == 8),]

ggplot(gpredictions2, aes(x = DD_0_wt, y = predicted, fill = Avg_Time_Flush)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#44AA99", "#AA4499")) + 
  scale_x_discrete(limits = rev(levels(gpredictions2$DD_0_wt)))+
  theme_bw()+ xlab("Population home climate (Winter degree days below 0 °C)") + ylab("Predicted probability of flush stage")+
  #ggtitle("Probabilities of bud flush stage across temperature gradient")+
  labs(fill = "Bud flush\nscore")+
  theme(legend.text = element_text(size=12), legend.title = element_text(size=12),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

#ggplot(gpredictions, aes(x = Avg_Time_Flush, y = predicted)) +
#  geom_point(aes(color = DD_0_wt), position =position_dodge(width = 0.5)) + 
#  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = DD_0_wt), 
#                position = position_dodge(width =0.5), width = 0.3) + 
#  scale_color_viridis(discrete = T) + theme_bw() 

#save the follwing figure
#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/RedMapleSpring1.png", height = 550, width = 750, unit='px')

ggplot(gpredictions2, aes(x = DD_0_wt, y = predicted, fill = Avg_Time_Flush)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#44AA99", "#AA4499")) + 
  scale_x_discrete(limits = rev(levels(gpredictions2$DD_0_wt)))+
  theme_bw()+ xlab("Population home climate (Winter degree days below 0 °C)") + ylab("Predicted probability of flush stage")+
  #ggtitle("Probabilities of bud flush stage across temperature gradient")+
  labs(fill = "Bud flush\nscore")+
  theme(legend.text = element_text(size=12), legend.title = element_text(size=12),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

dev.off()


# figure for the spring 1973 data
rm(list=ls())
# read in the data
rm.clim = read.csv("RedMaple_Townsend1977_withClimate.csv")
ts = read.csv("RedMaple_TestSites_ClimateNA_Normal_1941_1970SY.csv")

#variable of interest is: DegreeOfFlushing_April1973

#isolate 1973 observations
rm.clim.73 = rm.clim[which(is.na(rm.clim$DegreeOfFlushing_April1973)==FALSE),]

#make the response variable an ordered factor
rm.clim.73$DegreeFlushFactor = ordered(rm.clim.73$DegreeOfFlushing_April1973)

# the final model
# DD_0_wt
model.dd0wt = polr(DegreeFlushFactor ~ DD_0_wt, data = rm.clim.73, Hess=TRUE)
summary(model.dd0wt)

gpredictions = data.frame(ggeffects::ggpredict(model.dd0wt,  terms = c("DD_0_wt"), type = "fixed", bias_correction = TRUE))

#round the DD_0 variable
gpredictions$DD_0_wt = factor(round(gpredictions$x, -2))

colnames(gpredictions)[6] = "DegreeFlushFactor"

gpredictions2 = gpredictions[which(gpredictions$DegreeFlushFactor == 12 | gpredictions$DegreeFlushFactor == 35),]



ggplot(gpredictions, aes(x = DegreeFlushFactor, y = predicted)) +
  geom_point(aes(color = DD_0_wt), position =position_dodge(width = 0.5)) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = DD_0_wt), 
                position = position_dodge(width =0.5), width = 0.3) + 
  scale_color_viridis(discrete = T) + theme_bw() 


#save the following figure
#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/RedMapleSpringSupFig1.png", height = 550, width = 750, unit='px')


ggplot(gpredictions2, aes(x = DD_0_wt, y = predicted, fill = DegreeFlushFactor)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_manual(values = c("#44AA99", "#AA4499")) + 
  scale_x_discrete(limits = rev(levels(gpredictions2$DD_0_wt)))+
  theme_bw()+ xlab("Population home climate (Winter degree days below 0 °C)") + ylab("Predicted probability of flush stage")+
  #ggtitle("Probabilities of bud flush stage across temperature gradient")+
  labs(fill = "Bud flush\nscore")+
  theme(legend.text = element_text(size=12), legend.title = element_text(size=12),
        axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))


dev.off()




# visualize red maple fall phenology
# have both budset and leaf fall
rm(list=ls())
# read in the data
rm.clim = read.csv("RedMaple_Townsend1977_withClimate.csv")
ts = read.csv("RedMaple_TestSites_ClimateNA_Normal_1941_1970SY.csv")


#isolate 1972 observations
rm.clim.72 = rm.clim[which(is.na(rm.clim$TreesShowingDefoliation_Oct30_1972_percentage)==FALSE),]
#isolate 1973 observations
rm.clim.73 = rm.clim[which(is.na(rm.clim$TreesShowingBudset_Aug27_1973_percentage)==FALSE),]

rm.clim.72$NoDefoliation_percentage = 100 - rm.clim.72$TreesShowingDefoliation_Oct30_1972_percentage
rm.clim.73$NoBudset_percentage = 100 - rm.clim.73$TreesShowingBudset_Aug27_1973_percentage

# the final model
#effp
glm.effp <- glm(cbind(TreesShowingBudset_Aug27_1973_percentage, NoBudset_percentage) ~ eFFP,
                  data=rm.clim.73, family = binomial(link = 'logit'))
summary(glm.effp)

gpredictions = data.frame(ggeffects::ggpredict(glm.effp,  terms = c("eFFP [all]")))

colnames(gpredictions)[1] = "eFFP"
gpredictions$eFFP = factor(gpredictions$eFFP)

# save the figure
#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/RedMapleFall1.png", height = 550, width = 750, unit='px')

ggplot(data= gpredictions, aes(x=eFFP, y=predicted))+
  geom_bar(position = "dodge", stat = "identity", fill = "#CC6677")+
  scale_x_discrete(limits = levels(gpredictions$eFFP))+
  theme_bw()+ ylab("Predicted percent of trees showing \nbudset on August 27")+
  xlab("Population home climate (end of frost free period; DOY)")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

dev.off()

# repeat for defoliation
# the final model
#MCMT
glm.mcmt <- glm(cbind(TreesShowingDefoliation_Oct30_1972_percentage, NoDefoliation_percentage) ~ MCMT,
                  data=rm.clim.72, family = binomial(link = 'logit'))
summary(glm.mcmt)

gpredictions = data.frame(ggeffects::ggpredict(glm.mcmt,  terms = c("MCMT")))

colnames(gpredictions)[1] = "MCMT"
gpredictions$MCMT = factor(gpredictions$MCMT)

# save the figure
#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/RedMapleFall2.png", height = 550, width = 750, unit='px')

ggplot(data= gpredictions, aes(x=MCMT, y=predicted))+
  geom_bar(position = "dodge", stat = "identity", fill = "#CC6677")+
  scale_x_discrete(limits = levels(gpredictions$MCMT))+
  theme_bw()+ ylab("Predicted percent of trees showing \ndefoliation on October 30")+
  xlab("Population home climate (mean coldest month temperature in °C)")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

dev.off()





