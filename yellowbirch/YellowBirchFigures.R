# yellow birch figures




setwd("C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Data/YellowBirch/ReadyFiles")

library(ggplot2)
# read in the data
safe_colorblind_palette <- c("#882255", "#888888", "#88CCEE", "#6699CC", "#44AA99", 
                             "#999933", "#117733", "#332288", "#CC6677", "#DDCC77", "#AA4499")


#clausen data for fall
yb.clim = read.csv("YellowBirch_Clausen2yr_withClimate.csv")

yb.clim$growthCessation_weeksAfterJuly6_1966*7

#remove outlier

yb.clim = yb.clim[which(yb.clim$StandNumber!=2980),]
# daylength^2
model.day2 = lm(growthCessation_weeksAfterJuly6_1966 ~ daylength_diff + I(daylength_diff^2), data = yb.clim)
summary(model.day2)
plot(model.day2)

yb.clim$growthCessation_date = as.Date(yb.clim$growthCessation_weeksAfterJuly6_1966*7 + as.Date("1966-07-06"))

#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/YellowBirchFall1.png", height = 550, width = 750, unit='px')

ggplot(data = yb.clim, aes(x=daylength_diff, y=growthCessation_date))+
  geom_point(color = "#CC6677", size = 6, shape = 16)+ geom_smooth(method = 'lm', formula = y~x+I(x^2), se=F, color = "black")+
  theme_bw()+xlab("Population home daylength difference")+ylab("Average growth cessation date")+ 
  guides(size = "none", color = guide_legend(override.aes = list(size=4)))+
  scale_x_reverse()+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

#close the device
dev.off()

# Clausen spring data
rm(list=ls())
# clausen figure for spring
# climate data
yb.clim = read.csv("YellowBirch_ClausenGarrett_withClimate.csv")

#remove outlier
yb.clim = yb.clim[which(yb.clim$SourceNumber!=2980),]

# the final model
model.day = lm(growthInitiation ~ daylength_diff, data = yb.clim)
summary(model.day)
plot(model.day)

#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/YellowBirchSpring1.png", height = 550, width = 750, unit='px')

# plot
ggplot(data = yb.clim, aes(x=daylength_diff, y=growthInitiation))+
  geom_point(color = "#44AA99", size = 6, shape = 16)+
  geom_smooth(se=F, method='lm', color = "black")+
  xlab("Difference between longest and shortest daylength (hrs)")+ ylab("Average growth initiation score on May 5")+
  theme_bw() + guides(size="none")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))
# close device
dev.off()


# sharik data for spring
rm(list=ls())
yb.clim = read.csv("YellowBirch_SharikBarnes_withClimate.csv")

#MCMT
model.mcmt = lm(LeafFlushing_MeanStage ~ MCMT, data = yb.clim)
summary(model.mcmt)
plot(model.mcmt)

#do not use this method because scores were averaged to decimal
gpredictions = data.frame(ggeffects::ggpredict(model.mcmt,  terms = c("MCMT"), type = "fixed"))

#make MCMT a factor
gpredictions$MCMT = factor(gpredictions$x)

colnames(gpredictions)[6] = "LeafFlushing_MeanStage"


ggplot(gpredictions, aes(x = MCMT, y = predicted, fill = LeafFlushing_MeanStage)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_brewer(palette = "Set2") + #scale_x_discrete(limits = rev(levels(gpredictions2$DD_0)))+
  theme_bw()+ xlab("Mean Coldest Month Temperagure (°C)") + ylab("Predicted probability of flush stage")+
  ggtitle("Probabilities of bud flush stage across temperature gradient")+
  labs(fill = "Bud flush score")
# does not really work because they averaged to continuous value

#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/YellowBirchSpring2.png", height = 550, width = 750, unit='px')


ggplot(data = yb.clim, aes(x=MCMT, y=LeafFlushing_MeanStage))+
  geom_point(color = "#44AA99", size = 6, shape = 16)+
  geom_smooth(se=F, method='lm', color = "black")+
  xlab("Population home climate (mean coldest month temperature; °C)")+ ylab("Average leaf flushing stage on May 2")+
  theme_bw() + guides(size="none")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))
  #geom_point(aes(y=LeafFlushing_Range_min), color = "blue")+
  #geom_point(aes(y=LeafFlushing_Range_max), color = "red")


dev.off()


# yellow birch fall models from sharik and barnes
rm(list=ls())
#read in the data
yb.clim = read.csv("YellowBirch_SharikBarnes_withClimate.csv")

#final model
model.tmaxat = lm(GrowthCessation_MeanDays ~ Tmax_at, data = yb.clim)
summary(model.tmaxat)
plot(model.tmaxat)

yb.clim$growthCessation_date = as.Date(yb.clim$GrowthCessation_MeanDays + as.Date("1969-07-24"))

#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/YellowBirchFall2.png", height = 550, width = 750, unit='px')

ggplot(data = yb.clim, aes(x=Tmax_at, y=growthCessation_date))+
  geom_point(color = "#CC6677", size = 6, shape=16)+ geom_smooth(method = 'lm', formula = y~x, se=F, color = "black")+
  theme_bw()+xlab("Population home climate (Average maximum autumn temperature; °C)")+
  ylab("Average growth cessation date")+ 
  guides(size = "none", color = guide_legend(override.aes = list(size=4)))+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

#close the device
dev.off()


rm(list=ls())
# compare the two different studies growth cessation
yb.clim1 = read.csv("YellowBirch_SharikBarnes_withClimate.csv")
yb.clim1$growthCessation_date = as.Date(yb.clim1$GrowthCessation_MeanDays + as.Date("1969-07-24"))
yb.clim1$growthCessation_doy = as.Date(yb.clim1$GrowthCessation_MeanDays + as.Date("1969-07-24")) - as.Date("1968-12-31")


yb.clim2 = read.csv("YellowBirch_Clausen2yr_withClimate.csv")
#remove outlier
yb.clim2 = yb.clim2[which(yb.clim2$StandNumber!=2980),]
yb.clim2$growthCessation_date = as.Date(yb.clim2$growthCessation_weeksAfterJuly6_1966*7 + as.Date("1966-07-06"))
yb.clim2$growthCessation_doy = as.Date(yb.clim2$growthCessation_weeksAfterJuly6_1966*7 + as.Date("1966-07-06")) - as.Date("1965-12-31")


yb.tests = read.csv("YB_AllTestSites_Normal_1941_1970SY_V2.csv")


growthCessation_doy = c(yb.clim1$growthCessation_doy, yb.clim2$growthCessation_doy)
test = c(rep("MI", nrow(yb.clim1)), rep("WI", nrow(yb.clim2)))
MCMT = c(yb.clim1$MCMT, yb.clim2$MCMT)
Tmax_at = c(yb.clim1$Tmax_at, yb.clim2$Tmax_at)
daylength_diff = c(yb.clim1$daylength_diff, yb.clim2$daylength_diff)
yb = data.frame(growthCessation_doy, test, MCMT, Tmax_at, daylength_diff)



#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/YellowBirchFallSupFig1.png", height = 550, width = 750, unit='px')

ggplot(data = yb, aes(x=Tmax_at, y=growthCessation_doy, color = test, group = test))+
  geom_point(aes(size = 2))+ geom_smooth(method = 'lm', formula = y~x, se=F, color = "black")+
  theme_bw()+xlab("Population home climate (Average maximum autumn temperature; °C)")+
  ylab("Average growth cessation date")+ 
  guides(size = "none", color = guide_legend(override.aes = list(size=4)))+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

dev.off()

#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/YellowBirchFallSupFig2.png", height = 550, width = 750, unit='px')

ggplot(data = yb, aes(x=daylength_diff, y=growthCessation_doy, color = test, group = test))+
  geom_point(aes(size = 2))+ geom_smooth(method = 'lm', formula = y~x, se=F, color = "black")+
  theme_bw()+xlab("Population home daylength difference")+
  ylab("Average growth cessation date")+ 
  guides(size = "none", color = guide_legend(override.aes = list(size=4)))+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))
dev.off()



