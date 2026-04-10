# black cherry figures
#author: Mary McCafferty


#load packages
library(ggplot2)

safe_colorblind_palette <- c("#882255", "#888888", "#88CCEE", "#6699CC", "#44AA99", 
                             "#999933", "#117733", "#332288", "#CC6677", "#DDCC77", "#AA4499")

# exploration of the Carter data set

#read in the data
bc.clim = read.csv("BlackCherryCarter_withClimate.csv")

# explore the data
hist(bc.clim$budbreak_mean_days)

bc.clim$BudSet_1978_Mean = as.Date(bc.clim$BudSet_1978_Mean, "%Y-%m-%d")
bc.clim$LeafFall_1978_Mean = as.Date(bc.clim$LeafFall_1978_Mean, "%Y-%m-%d")
bc.clim$budset_days = as.numeric(bc.clim$BudSet_1978_Mean- as.Date("1977-12-31", "%Y-%m-%d"))
bc.clim$leaffall_days = as.numeric(bc.clim$LeafFall_1978_Mean- as.Date("1977-12-31", "%Y-%m-%d"))


# filter to only Reedsville observations
bc.rd = na.omit(bc.clim)


set.mwmt = lm(budset_days ~ MWMT, data = bc.rd)
summary(set.mwmt)
plot(set.mwmt)
# very strong model

fall.mwmt = lm(leaffall_days ~ MWMT, data = bc.rd)
summary(fall.mwmt)
plot(fall.mwmt)
# very strong model


# save the figure
#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/BlackCherryFall1.png", height = 550, width = 750, unit='px')

ggplot(data= bc.rd, aes(x=MWMT))+
  geom_point(aes(y=budset_days, color = "Budset"), size = 6, shape = 18)+
  geom_point(aes(y=leaffall_days, color = "Leaf fall"), size = 6, shape = 18)+
  geom_smooth(aes(y=budset_days), se=F, method='lm', formula = y~ x + I(x^2), color = "black")+
  geom_smooth(aes(y=leaffall_days), se=F, method='lm', color = "black")+
  guides(size="none", color = guide_legend(override.aes = list(size=4)))+theme_bw()+
  scale_color_manual(name = "", breaks = c("Leaf fall", "Budset"), values = c("Leaf fall" = "#6699CC", "Budset" = "#CC6677"))+
  scale_y_continuous(limits = c(225, 325), breaks = c(225, 240, 255, 270, 285, 300, 315), labels = c("Aug-13","Aug-28", "Sept-12", "Sept-27", "Oct-12", "Oct-27", "Nov-11"))+
  ylab("Average date of phenology event")+xlab("Population home climate (Mean warmest month temperature °C)")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16),
        legend.text = element_text(size=12), legend.position = "inside", 
        legend.justification = c("left", "top"), legend.background = element_blank(), 
        legend.box.background = element_rect(color = "black"))

dev.off()

ggplot(data= bc.rd, aes(x=DD18_sm, y=leaffall_days))+
  geom_point(aes(size=2), color = "#FC8D62")+
  geom_smooth(se=F, method='lm', color = "black")+
  guides(size="none")+theme_bw()

bc.clim = bc.clim[!is.na(bc.clim$budbreak_mean_days),]

#best model is DD_0_wt with quadratic term
# now for the spring models
budbreak.dd0wt = lm(budbreak_mean_days ~ DD_0_wt + I(DD_0_wt^2) + TestSite, data = bc.clim)
summary(budbreak.dd0wt)
plot(budbreak.dd0wt)

budbreak.td = lm(budbreak_mean_days ~ TD + TestSite, data = bc.clim)
summary(budbreak.td)

bc.clim = cbind(bc.clim, pred = predict(budbreak.td))

# adjust for test site to make simpler figure
budbreak.dd0wt$coefficients[3]
for (i in 1:nrow(bc.clim)) {
  if (bc.clim$TestSite[i] == "RD"){
    bc.clim$budbreak.test.adj[i] = bc.clim$budbreak_mean_days[i] - budbreak.dd0wt$coefficients[4]
  } else {
    bc.clim$budbreak.test.adj[i] = bc.clim$budbreak_mean_days[i]
  }
 
}


# save the figure
#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/BlackCherrySpring1V2.png", height = 550, width = 750, unit='px')

ggplot(data = bc.clim, aes(x=DD_0_wt, y=budbreak_mean_days, color = TestSite, group = TestSite))+
  geom_point(size=6, shape = 18)+
  scale_color_manual(labels = c("Cooper's rock, WV", "Reedsville, WV"), values = c("#44AA99", "#AA4499"))+
  geom_line(mapping = aes(y=pred), linewidth=1)+
  scale_y_continuous(limits = c(88, 117), breaks = c(90, 95, 100, 105, 110, 115), labels = c("Mar-31","Apr-05", "Apr-10", "Apr-15", "Apr-20", "Apr-25"))+
  theme_bw()+ guides(size="none",  color = guide_legend(override.aes = list(size=4)))+labs(color = "Test site", shape = "Test site")+
  xlab("Population home climate (Degree days below 0 °C in Winter)")+
  ylab("Average budbreak date")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))

dev.off()

#png device

ggplot(data = bc.clim, aes(x=TD, y=budbreak.test.adj))+
  geom_point(size=6, color = "#44AA99", shape = 18)+
  stat_smooth(method='lm', se=F, color = "black", formula = y~x)+
  scale_y_continuous(limits = c(93, 117), breaks = c(95, 100, 105, 110, 115), labels = c("Apr-05", "Apr-10", "Apr-15", "Apr-20", "Apr-25"))+
  theme_bw()+ guides(size="none",  color = guide_legend(override.aes = list(size=4)))+labs(color = "Test site", shape = "Test site")+
  xlab("Population home climate (Continentality; MWMT - MCMT °C)")+
  ylab("Average budbreak date")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))


png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/BlackCherrySpring2.png", height = 550, width = 750, unit='px')

ggplot(data = bc.clim, aes(x=TD, y=budbreak_mean_days, shape = TestSite, color = TestSite))+
  geom_point(size=6)+
  geom_line(mapping = aes(y=pred), color = "black", linewidth = 1)+
  scale_color_manual(labels = c("Cooper's rock, WV", "Reedsville, WV"), values = c(CR = "#66C2A5", RD = "#E78AC3"))+
  scale_shape_manual(labels = c("Cooper's rock, WV", "Reedsville, WV"), values = c(CR = 20, RD = 18))+
  scale_y_continuous(limits = c(88, 112), breaks = c(90, 95, 100, 105, 110), labels = c("March-31", "Apr-5", "Apr-10", "Apr-15", "Apr-20"))+
  theme_bw()+ guides(size="none",  color = guide_legend(override.aes = list(size=4)))+labs(color = "Test site", shape = "Test site")+
  xlab("Continentality (MWMT - MCMT) of Seed Source (°C)")+
  ylab("Mean budbreak date at test site")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), axis.title.x = element_text(size=14), axis.title.y = element_text(size=14))

dev.off()




