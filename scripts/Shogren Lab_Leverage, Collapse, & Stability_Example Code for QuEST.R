# Title: Subcatchment Leverage, Variance Collapse, and Stability Examples
# Author: Code written by AJS
# Purpose: QuEST 
# Last Updated Date: 02-27-2024 

# ---- Load Required Packages ---- 
### LOAD REQUIRED PACKAGES
library(dplyr)
require(reshape2)
require(ggplot2)
require(RColorBrewer)
require(gridExtra)
require(sciplot)
install.packages("ggsn")
require(ggthemes)
require(ggExtra)
require(grid)
require(cowplot)
library(rgdal)
library(ggsn)
library(wesanderson)
library(sp)
library(broom)
library(scales)
library(readxl)
tshogren = theme_bw() %+replace% theme(strip.background = element_blank(), panel.grid.minor = element_blank(), panel.border = element_rect(colour = "black", fill=NA), axis.line.x = element_line(color="black"), axis.line.y = element_line(color="black"), panel.background = element_blank(), legend.background = element_blank(), axis.line = element_line(color = 'black'),  legend.key = element_rect(colour = NA), legend.position = "bottom", legend.direction = "horizontal")

# ---- Load Data - Example data from Toolik Field Station ----
# You can find this dataset in the GDrive
db17 = read_excel("data/Toolik Field Station_Stream Chemistry_v3.xlsx", 
                  sheet = "TFS2017",  na = c("bd", "-bd","-0.0001", ""))



# Chemistry data from Toolik 2017
season.17 = db17$season
watershed.17 = db17$watershed
year.17 = db17$Year
site.17 = db17$site
area.17 = as.numeric(db17$Area_km2)
lat.17 = as.numeric(db17$x)
long.17 = as.numeric(db17$y)
doc.17 = as.numeric(db17$doc)
no3.17 = as.numeric(db17$no3)
nh4.17 = as.numeric(db17$nh4)
srp.17 = as.numeric(db17$srp)


# Now create a dataframe of just selected variables - make sure your headers are correct
df17 = data.frame(season.17, watershed.17, year.17, site.17, area.17, lat.17, long.17, doc.17, no3.17, nh4.17, srp.17)
names(df17) = c("season", "watershed", "year", "site", "area", "lat", "long", "doc", "no3", "nh4", "srp")
head(df17)

# ---- 1. Catchment Chemical Structure  ----
# You will want to melt your dataframe from wide to long here!

melt17=reshape2::melt(df17, id.vars=c("watershed", "site", "season", "area", "lat", "long"), measure.vars = c("doc", "no3", "nh4",  "srp"), na.rm=TRUE)

# We want data only below aufeis field - so remove larger watersheds here!
melt17 = melt17[melt17$area < 200,] 
str(melt17)

# Make sure that you are defining your categorical variables as factors 
melt17$watershed = as.factor(melt17$watershed)
melt17$season = as.factor(melt17$season)
melt17$variable = as.factor(melt17$variable)

# First, try plotting concentration over area by season...
# Note: early = June, late = August
# This shows you how the variance structure declines as you move towards the outlet
conc.area.scatplot = ggplot()+
  geom_point(data = melt17, aes(x = area, y = value, fill = watershed), pch = 21, size = 3, alpha = 0.75)+
  scale_fill_manual(values = c("royalblue3", "orange3", "seagreen3"))+
  facet_grid(rows = vars(melt17$variable), cols = vars(melt17$season), scales = "free")+
  labs(x = "Subcatchment Area (km^2)", y = "Subcatchment Concentration (uM)")+
  tshogren+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())
conc.area.scatplot

# Let's just pull out Kuparuk data...
melt17.kup=subset(melt17, watershed == "KUP")
kup.conc.area.scatplot = ggplot()+
  geom_point(data = melt17.kup, aes(x = area, y = value, fill = watershed), pch = 21, size = 3, alpha = 0.75)+
  scale_fill_manual(values = c("royalblue3"))+
  facet_grid(rows = vars(melt17.kup$variable), cols = vars(melt17.kup$season), scales = "free")+
  labs(x = "Subcatchment Area (km^2)", y = "Subcatchment Concentration (uM)")+
  tshogren+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())
kup.conc.area.scatplot
# ---- 2. Variance Collapse (spatial scale of concentration ariance reduction) ----
# Example from the Kuparuk River 2017 dataset
# NOTE: Changepoint has been decomissioned, so we will need to rewrite this code! 
require(changepoint)
head(df17)

# Quick subset of Kup dataset
kup17=subset(df17, watershed == "KUP")
kup17E=subset(kup17, season == "Early")
kup17L=subset(kup17, season == "Late")

# The changepoint function cannot handle rows with NAs. Should delete/omit. 
# Here I just changed the NA values to 0 to keep the points in the analysis for simplicity.
kup17E[is.na(kup17E)] = 0
kup17L[is.na(kup17L)] = 0

kupbp17E = apply(kup17E[, 8:11], 2, function(x) cpts(cpt.var(x, method="SIC")))
View(kupbp17E) # - this gives you the value of the significant breakpoints for early season

kupbp17L = apply(kup17L[, 8:11], 2, function(x) cpts(cpt.var(x, method="PELT")))
View(kupbp17L) # - this gives you the value of the significant breakpoint for late season

# These can be compiled and plotted on the "kup.conc.area.scatplot" figures!

# ---- 3. Subcatchment Leverage (a spatially-distributed mass balance) ----
# Calculates subcatchment leverage - see code for when you have Q vs. not
lev.17 = melt17 %>% 
  group_by(watershed, season, variable) %>%
  #mutate(leverage = ((value - value[which.max(area)])*100/ value[which.max(area)]) * discharge/discharge[which.max(area)]) %>% # use if you have estimates of instantaneous Q at nested site, in L/km2/s
  mutate(leverage = ((value - value[which.max(area)])*100/ value[which.max(area)]) * area/max(area)) # use if you don’t have specific Q values
head(lev.17)
#View(lev.17)

# You can then make data table of the watershed means
DT17 = data.table(lev.17, na.rm = TRUE)
leverage2017 = DT17[ , .(Meanval = mean(value), ValSD = sd(value), MeanLev = mean(leverage), MeanSD = sd(leverage)), by = .(watershed,variable,season)]
head(leverage2017)

# Here, you can subset your variables... we'll just use Kup as an example
kup.leverage = subset(lev.17, watershed %in% c("KUP"))
str(kup.leverage)

# Let's look at the concentration structure again...
kup.conc.area.scatplot = ggplot()+
  geom_point(data = kup.leverage, aes(x = area, y = value, fill = watershed), pch = 21, size = 3, alpha = 0.75)+
  scale_fill_manual(values = c("royalblue3"))+
  facet_grid(rows = vars(kup.leverage$variable), cols = vars(kup.leverage$season), scales = "free")+
  labs(x = "Subcatchment Area (km^2)", y = "Subcatchment Concentration (uM)")+
  tshogren+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())
kup.conc.area.scatplot

# Try plotting leverage over area by season...
# This tells you each site's leverage values
# Note reversed axes! Above = Production, Below = Removal
kup.leverage.area.scatplot = ggplot()+
  geom_point(data = kup.leverage, aes(x = area, y = leverage, fill = watershed), pch = 21, size = 3, alpha = 0.75)+
  scale_fill_manual(values = c("royalblue3"))+
  facet_grid(rows = vars(kup.leverage$variable), cols = vars(kup.leverage$season), scales = "free")+
  labs(x = "Subcatchment Area (km^2)", y = "Subcatchment Leverage (%)")+
  tshogren+
  scale_y_reverse()+
  #ylim(c(250,-250))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())
kup.leverage.area.scatplot

# Finally, you can plot mean watershed leverage over time by solute...
kup.leverage.area.boxplot = ggplot(data = kup.leverage, aes(x = variable, y = leverage, fill = variable))+
  geom_boxplot(alpha = 0.25, position = position_dodge(width=0.75))+
  # scale_fill_manual(values = c("royalblue3"))+
  stat_summary(fun=mean, geom="point", shape=21, size=3, position = position_dodge(width=0.75))+
  facet_grid(cols = vars(kup.leverage$season), scales = "free")+
  labs(x = "Solute", y = "Watershed Leverage (%)")+
  tshogren+
  scale_y_reverse()+
  ylim(c(50,-50))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,hjust = 1),
        legend.title = element_blank())
kup.leverage.area.boxplot




# ---- 4. Spatial stability / persistence (correlation between seasons)----
## THIS CODE IS UGLY AF AND I'M SO SORRY

# We're going back to the original melted df here. We want to start with this!
# I'm subsetting Kup here for ease fo interpretation
melt17.kup = melt17[melt17$watershed == "KUP",]
melt17.kupE = melt17.kup[melt17.kup$season == "Early",]
melt17.kupL = melt17.kup[melt17.kup$season == "Late",]

# Then you then want to merge your "early" and "late" seasonal data
# Again, this is fudging ugly, but this was last minute so.... 

# Unmelt Early Season
kup.17E = dcast(melt17.kupE, site ~ variable, sum)
head(kup.17E)

# Unmelt Late Season
kup.17L = dcast(melt17.kupL, site ~ variable, sum)
head(kup.17L)
#names(um17.kupE.DOC) = c("site", "doc.early")

# Line up data by site ID
kup.17 = merge(kup.17E, kup.17L, by = "site")
head(kup.17)

# Correlation Test between Early & Late Season sampling events
# DOC
rho.doc.kup = cor.test(kup.17$doc.x, kup.17$doc.y, method = "spearman")
rho.doc.kup

# NO3
rho.no3.kup = cor.test(kup.17$no3.x, kup.17$no3.y, method = "spearman")
rho.no3.kup

# SRP
rho.no3.kup = cor.test(kup.17$srp.x, kup.17$srp.y, method = "spearman")
rho.no3.kup


kup.doc.stability.scatplot = ggplot()+
  geom_point(data = kup.17, aes(x = doc.x, y = doc.y), fill = "royalblue3", pch = 21, size = 3, alpha = 0.75)+
  geom_line(linetype = "solid")+
  scale_fill_manual(values = c())+
  labs(x = "Early Season DOC (uM)", y = "Late Season DOC (uM)")+
  ylim(c(0,800))+
  xlim(c(0,800))+
  tshogren+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())
kup.doc.stability.scatplot

kup.no3.stability.scatplot = ggplot()+
  geom_point(data = kup.17, aes(x = no3.x, y = no3.y), fill = "royalblue3", pch = 21, size = 3, alpha = 0.75)+
  geom_line(linetype = "solid")+
  scale_fill_manual(values = c())+
  labs(x = "Early Season NO3 (uM)", y = "Late Season NO3 (uM)")+
  ylim(c(0,20))+
  xlim(c(0,20))+
  tshogren+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())
kup.no3.stability.scatplot

kup.srp.stability.scatplot = ggplot()+
  geom_point(data = kup.17, aes(x = srp.x, y = srp.y), fill = "royalblue3", pch = 21, size = 3, alpha = 0.75)+
  geom_line(linetype = "solid")+
  scale_fill_manual(values = c())+
  labs(x = "Early Season SRP (uM)", y = "Late Season SRP (uM)")+
  ylim(c(0,0.5))+
  xlim(c(0,0.5))+
  tshogren+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank())
kup.srp.stability.scatplot
