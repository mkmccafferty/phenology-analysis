# Black walnut figure making
# author: Mary McCafferty


#load libraries
library(ggplot2)

# read in the files
bw = read.csv("BlackWalnutCarterSixYearResultsInMaine_ready.csv")
clim = read.csv("BlackWalnutCarterSixYearResultsInMaine_ready_ClimateNA_Normal_1951_1980SY.csv")

safe_colorblind_palette <- c("#882255", "#888888", "#88CCEE", "#6699CC", "#44AA99", 
                             "#999933", "#117733", "#332288", "#CC6677", "#DDCC77", "#AA4499")

# test site
test = bw[30,]

# merge the data
bw.clim = merge(bw, clim, by.x = "Provenance.Number", by.y = "ID1")

# fall phenology from Carter
# MWMT
model.mwmt = lm(BudSet_JD ~ MWMT, data = bw.clim)
summary(model.mwmt)
plot(model.mwmt)

bw.clim$BudSet_date = as.Date(bw.clim$BudSet_JD + as.Date("1984-12-31"))

#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/BlackWalnutFall1.png", height = 550, width = 750, unit='px')

ggplot(data = bw.clim, aes(x=MWMT, y=BudSet_date))+
  geom_point(size = 6, color = "#CC6677", shape = 17)+ 
  geom_smooth(method = 'lm', se = F, color = "black")+
  #scale_x_reverse()+
  guides(size = "none",  color = guide_legend(override.aes = list(size=4)))+theme_bw()+
  xlab("Population home climate (mean warmest month temperature; °C)")+ylab("Average date of budset")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

dev.off()




# the spring data


rm(list=ls())
#read in the data
bw.clim = read.csv("BlackWalnutWright_withClimate.csv")


bw.clim$BudBurst_JD = bw.clim$leafOutDate_May + 30+31+29+31

#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/BlackWalnutSpring1.png", height = 550, width = 750, unit='px')

ggplot(data = bw.clim, aes(MCMT, BudBurst_JD))+
  geom_point(color = "#44AA99", size = 6, shape = 17)+
  theme_bw() + guides(size="none")+
  xlab("Population home climate (MCMT; °C)")+ ylab("Average leaf out date")+
  scale_y_continuous(limits = c(132, 140), breaks = c(133, 136, 139), labels = c("May-13", "May-15", "May-17"))+
  geom_smooth(color = "black", method = 'lm', se=F, formula = y~x+I(x^2))+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

dev.off()


library(lme4)
#leaf retention model
#MCMT
# read in the files
bw.clim = read.csv("BlackWalnut_VTNursery_withClimate.csv")

glm.dd <- glmer(cbind(Leaf_Ret_10_79_percentage, NotDef_percentage) ~ (1 | Bed) + (1 | Prov) + daylength_diff,
                  data=bw.clim, family = binomial(link = 'logit'))
summary(glm.dd)


gpredictions = data.frame(ggeffects::ggpredict(glm.dd,  terms = c("daylength_diff")))

colnames(gpredictions)[1] = "DD"
gpredictions$DD = factor(gpredictions$DD)

#save the figure
#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/BlackWalnutFall2.png", height = 550, width = 750, unit='px')

ggplot(data= gpredictions, aes(x=DD, y=predicted))+
  geom_bar(position = "dodge", stat = "identity", fill = "#6699CC")+
  scale_x_discrete(limits = levels(gpredictions$DD))+
  theme_bw()+ ylab("Predicted percent of trees retaining leaves in October")+
  xlab("Population home difference betwen longest and shortest days (hours)")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

dev.off()





