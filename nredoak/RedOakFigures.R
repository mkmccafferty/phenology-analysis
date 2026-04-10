# red oak figures


library(ggplot2)
library(ggpmisc)

#read in the data
setwd("C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Data/RedOak/ReadyFiles")
testsites = read.csv("RedOakTestSites_ClimateNA_Normal_1931_1960SY.csv")
ts_climate1968 = read.csv("RedOakTestSites_ClimateNA_Year_1968SY.csv")
ro.oh = read.csv("RedOak_Kriebel1976_WithClimate.csv")

ts = testsites[which(testsites$ID2 == "OH"),]
ts1968 = ts_climate1968[which(ts_climate1968$ID2 == "OH"),]

safe_colorblind_palette <- c("#882255", "#888888", "#88CCEE", "#6699CC", "#44AA99", 
                             "#999933", "#117733", "#332288", "#CC6677", "#DDCC77", "#AA4499")


hist(ro.oh$AverageNumberOfGrowthFlushes)

plot(ro.oh$Yr.Planted, ro.oh$AverageNumberOfGrowthFlushes)
par(mfrow = c(2,2))
model_year = lm(AverageNumberOfGrowthFlushes ~ as.factor(Yr.Planted), data =ro.oh)
summary(model_year)
plot(model_year)
# year effect significant, remove

for (i in 1:nrow(ro.oh)){
  if(ro.oh$Yr.Planted[i] == 1963) {
    ro.oh$AverageNumberOfGrowthFlushes_adj[i] = ro.oh$AverageNumberOfGrowthFlushes[i] - model_year$coefficient[2]
  } else if (ro.oh$Yr.Planted[i] == 1964) {
    ro.oh$AverageNumberOfGrowthFlushes_adj[i] = ro.oh$AverageNumberOfGrowthFlushes[i] - model_year$coefficient[3]
  } else {
    ro.oh$AverageNumberOfGrowthFlushes_adj[i] = ro.oh$AverageNumberOfGrowthFlushes[i]
  }
}

#run the final model
model.dd5sp = lm(AverageNumberOfGrowthFlushes_adj ~ DD5_sp, data = ro.oh)
summary(model.dd5sp)
plot(model.dd5sp)

#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/RedOakSpringSupFig1.png", height = 550, width = 750, unit='px')

#the figure
ggplot(aes(x=DD5_sp, y=AverageNumberOfGrowthFlushes_adj), data = ro.oh)+
  geom_point(size=6, color = "#44AA99", shape = 43)+
  geom_smooth(method = 'lm', se=F, color = "black")+
  theme_bw()+ guides(size="none",  color = guide_legend(override.aes = list(size=4)))+labs(color = "Test site", shape = "Test site")+
  xlab("Degree days above 5°C in Spring")+
  ylab("Average number of growth flushes")+
  theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))
#close device
dev.off()



rm(list=ls())
# read in the other spring data
ro.ne = read.csv("RedOak_SchlarbaumBagley_WithClimate.csv")

#remove the outlier that was identified during analysis
ro.ne.noout = ro.ne[-27,]
model_td_noout = lm(LeafBudsOpen50Percent_Days_JD_adj ~ TD, data = ro.ne.noout)
summary(model_td_noout)
plot(model_td_noout)

#run the final model
model_td_noout2 = lm(LeafBudsOpen50Percent_Days_JD_adj ~ TD + I(TD^2), data = ro.ne.noout)
summary(model_td_noout2)
plot(model_td_noout2)

#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/RedOakSpring2.png", height = 550, width = 750, unit='px')

ggplot(data = ro.ne.noout, aes(TD, LeafBudsOpen50Percent_Days_JD_adj))+
  geom_point(size = 6, color = "#44AA99", shape = 43)+
  geom_smooth(method = 'lm', formula = y ~ x + I(x^2), se=F, color = "black")+
  scale_y_continuous(limits = c(116, 126), breaks = c(118, 120, 122, 124, 126), labels = c("Apr-28", "Apr-30", "May-02", "May-04", "May-06"))+
  xlab("Population home climate (continentality (MWMT - MCMT; °C))")+
  ylab("Date at which 50% of the leaf buds were open")+
  #annotate("text", x=22, y=126, label="y = 101.436 + 1.718x -0.036x^2, adj.R^2 = 0.476")+
  #ggtitle("Spring bud burst vs. continentality at Nebraska test site in 1975")+
  theme(legend.position = "none")+ scale_color_brewer(palette = "Set2")+ theme_bw()+
  guides(size = "none")+theme(legend.key.size = unit(1, "cm"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size=12), axis.title.x = element_text(size=16), axis.title.y = element_text(size=16))


dev.off()



# the fall data
par(mfrow = c(2,2))
model_color_day = lm(Leaf_Color_Days_JD_adj ~ daylength_diff+I(daylength_diff^2), data = ro.ne)
summary(model_color_day)
plot(model_color_day)
#model_color_dd0at$coefficients

model_death_day = lm(Leaf_Death_Days_JD_adj ~ daylength_diff+I(daylength_diff^2), data = ro.ne)
summary(model_death_day)
plot(model_death_day)
#model_death_dd0at$coefficients

model_drop_day = lm(Leaf_Drop_Days_JD_adj ~ daylength_diff+I(daylength_diff^2), data = ro.ne)
summary(model_drop_day)
plot(model_drop_day)


#png device
png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/RedOakFall1.png", height = 550, width = 750, unit='px')

# the figure with all three response variables
ggplot(data = ro.ne, aes(x=DD_0_at))+
  geom_point(aes(y=Leaf_Color_Days_JD_adj, color = "Leaf color"), size = 6,shape = 43)+
  geom_point(aes(y=Leaf_Death_Days_JD_adj, color = "Leaf death"), size = 6,shape = 43)+
  geom_point(aes(y=Leaf_Drop_Days_JD_adj, color = "Leaf drop"), size = 6,shape = 43)+
  stat_poly_line(aes(y=Leaf_Death_Days_JD_adj), formula = y~x, method = 'lm', se = F, color = "black")+
  stat_poly_line(aes(y=Leaf_Color_Days_JD_adj), formula = y~x, method = 'lm', se = F, color = "black")+
  stat_poly_line(aes(y=Leaf_Drop_Days_JD_adj), formula = y~x, method = 'lm', se = F, color = "black")+
  scale_y_continuous(limits = c(275, 320), breaks = c(280, 290, 300, 310, 320), 
                     labels = c("Oct-07", "Oct-17", "Oct-27", "Nov-06", "Nov-16"))+
  xlab("Population home climate (Degree days below 0°C in Autumn)")+ 
  ylab("Date of observed stage in senescence process")+
  guides(size = "none", color = guide_legend(override.aes = list(size = 6)))+ 
  labs(color = NULL)+ 
  #ggtitle("Stages of fall leaf senescence at Nebraska test site in 1975 vs.seed source degree days below 0°C in fall")+
  theme_bw()+
  scale_color_manual(breaks = c("Leaf color", "Leaf death", "Leaf drop"), 
                     values = c("Leaf color" = "#CC6677", "Leaf death" = "#DDCC77", "Leaf drop" = "#6699CC"))+
  theme(legend.key.size = unit(1, "cm"), legend.text = element_text(size = 12), 
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size=12), 
        axis.title.x = element_text(size=16), 
        axis.title.y = element_text(size=16))

dev.off()

rm(list=ls())
#read in the deneke data
ro.ks = read.csv("RedOak_Deneke_WithClimate.csv")
#the final model
col.mcmt = lm(Color_adj ~ MCMT, data = ro.ks)
summary(col.mcmt)
plot(col.mcmt)

png(filename = "C:/Users/mkmcc/OneDrive - The Pennsylvania State University/MaryMcCafferty/Phenology/Figures/RedOakSupFig2.png", height = 550, width = 750, unit='px')

ggplot(aes(MCMT, Color_adj), data = ro.ks)+
  geom_point(size = 6, color = "#CC6677", shape = 43)+
  geom_smooth(method = 'lm', se = F, color = "black")+
  theme_light()+ guides(size = "none")+
  xlab("Population home climate (mean coldest month temperature; °C)")+
  ylab("Average Fall color score")+
  theme(axis.title.x = element_text(size=14), axis.title.y=element_text(size=14))
#ggtitle("Average fall color score at Kansas test site in 1968 vs. seed source mean annual precipitation")

dev.off()








