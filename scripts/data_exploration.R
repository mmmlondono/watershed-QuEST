#### read me ####

# The purpose of this script is to explore my data, including
# - describing dataset size (# variables & # observations)
# - describing data types
# - checking data distributions
# - checking for spatial autocorrelation
# - checking for temporal autocorrelation
# - checking for correlation between variables


#### libraries ####

library(tidyverse)
library(lubridate)
library(psych) # to plot pair-wise correlations
library(car) # I like their qq plot fxn
library(tsibble) # useful for creating time series objects
library(forecast) # I like their Acf fxn
library(ape) # for spatial autocorrelation
library(ade4)# for spatial autocorrelation
library(rgdal) # for mapping

#### load and tidy data ####

dat = read.csv("coal_WQ.csv")

# reduce to 3 sites for the purposes of most of this tutorial
dat = dat[dat$Site_Name=="VR-3"| dat$Site_Name=="VR-2"| dat$Site_Name=="VR-1",]

# check data classes 
str(dat)

# format date/time
dat$datetime_NM = as.POSIXct(dat$Sample_DateTime, format="%m/%d/%y %H:%M", tz="MST")

# convert characters that should be factors (categories) to factors
dat$Parameter = as.factor(dat$Parameter)
dat$Site_Name = as.factor(dat$Site_Name)
dat$Is_Nondetect = as.factor(dat$Is_Nondetect)

# convert water quality data to numeric
dat$Value = as.numeric(dat$Value)

#### describe dataset size and structure ####

head(dat)
str(dat)

with(dat, table(Parameter, Site_Name))

### check timesteps by looking and time series of most frequently collected parameters
# make dataset of one of the most frequently collected parameters
dat_lts = 
  dat %>% 
  group_by(Parameter) %>% 
  filter(n() > 500) %>% 
  arrange(datetime_NM)
dat_lts_alk = 
  dat %>% 
  filter(Parameter=="Alkalinity") %>% 
  arrange(datetime_NM)
# add year and day of year for plotting
dat_lts_alk$year = lubridate::year(dat_lts_alk$datetime_NM)
dat_lts_alk$doy = lubridate::yday(dat_lts_alk$datetime_NM)
# plot
ggplot(data=dat_lts_alk, aes(x=doy, y=Value, color=Site_Name))+
  geom_point() + geom_path()+
  facet_wrap(~year, scales="free_y")+
  theme(legend.title = element_blank()) +
  theme_bw()
# can also look at single years in detail
ggplot(data=dat_lts_alk, aes(x=datetime_NM, y=Value, color=Site_Name))+
  geom_point() + geom_path()+
  xlim(c(as.POSIXct("1995-01-01"), as.POSIXct("1996-01-01")))+
  theme(legend.title = element_blank()) +
  theme_bw()

# timesteps are all over the place from year to year and site to site, what we would call "irregular"
# timesteps are not sub-daily, at most frequent are approximately monthly

# ........ etc. for each parameter I'm interested in using in analysis .........


### How many variables are in your dataset?
str(dat)
# 65 parameters

### How many observations are in your dataset?
nrow(dat)
# 12541 total
with(dat, table(Parameter, Site_Name))
range(with(dat, table(Parameter, Site_Name)))
# there are a variable # of observations for each water quality parameter in each site, from 0 to 273 total

### Are the data nested in time or space?
# Yes in time - observations were collected repeatedly on an irregular schedule
# Yes in space - observations were collected in three different sites, need more research/exploration to find out if sites are connected in any way

#### describe data types ####

str(dat)
summary(dat$Parameter)
# most water quality parameters are numerical continous ratios
# temp, pH are numerical continous interval
# I would need to research on how some of these other parameters were measured and what units they're in to decide whether they're ratio or interval - take time to do this for your dataset!

#### check distributions ####

# I'm only going to check the distributions of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
dat_r = 
  dat %>% 
  group_by(Parameter, Site_Name) %>% 
  filter(n() > 100) %>% 
  arrange(datetime_NM)
summary(dat_r$Parameter)

temp = dat_r[dat_r$Parameter == "Alkalinity",]
qqPlot(temp$Value); shapiro.test(temp$Value) # normal
qqPlot(temp$Value[temp$Site_Name=='VR-1']); shapiro.test(temp$Value[temp$Site_Name=='VR-1']) # no data
qqPlot(temp$Value[temp$Site_Name=='VR-2']); shapiro.test(temp$Value[temp$Site_Name=='VR-2']) # normal
qqPlot(temp$Value[temp$Site_Name=='VR-3']); shapiro.test(temp$Value[temp$Site_Name=='VR-3']) # normal

temp = dat_r[dat_r$Parameter == "Ammonia",]
qqPlot(temp$Value); shapiro.test(temp$Value) # NOT normal
qqPlot(temp$Value[temp$Site_Name=='VR-1']); shapiro.test(temp$Value[temp$Site_Name=='VR-1']) # no data
qqPlot(temp$Value[temp$Site_Name=='VR-2']); shapiro.test(temp$Value[temp$Site_Name=='VR-2']) # NOT normal
qqPlot(temp$Value[temp$Site_Name=='VR-3']); shapiro.test(temp$Value[temp$Site_Name=='VR-3']) # NOT normal

temp = dat_r[dat_r$Parameter == "Bicarbonate",]
qqPlot(temp$Value); shapiro.test(temp$Value) # normal
qqPlot(temp$Value[temp$Site_Name=='VR-1']); shapiro.test(temp$Value[temp$Site_Name=='VR-1']) # no data
qqPlot(temp$Value[temp$Site_Name=='VR-2']); shapiro.test(temp$Value[temp$Site_Name=='VR-2']) # normal
qqPlot(temp$Value[temp$Site_Name=='VR-3']); shapiro.test(temp$Value[temp$Site_Name=='VR-2']) # normal

# etc........ for the rest of the parameters that I think I'll use in this analysis

### Examine non-normal data closely ###
# ask:
# are outliers making it non-normal?
# can I justify removing outliers based on my knowledge of the data?
# if data is still non-normal, what distribution is it?

temp = dat_r[dat_r$Parameter == "Ammonia",]
summary(temp$Value)
hist(temp$Value)
plot(density(temp$Value))
# this data has 1 an extreme negative outlier. Ammonia values cannot be negative, so this is an error. I will remove it in the main datasets an re-check the data's normality
dat$Value[dat$Parameter=="Ammonia" & dat$Value<0] = NA # rplace it in main dataset
dat_r$Value[dat_r$Parameter=="Ammonia" & dat_r$Value<0] = NA # replace it in reduced dataset
temp = dat_r[dat_r$Parameter == "Ammonia",]
qqPlot(temp$Value); shapiro.test(temp$Value)
# there is now a high outlier to examine
temp = dat_r[dat_r$Parameter == "Ammonia",]
summary(temp$Value)
hist(temp$Value)
plot(density(temp$Value, na.rm = T))
# this data has 1 an extreme positive outlier. Ammonia values do not get this high in natural conditions. This is coal data so maybe it isn't natural, but even still, we'd expect to see more than one point if this were not an error. I will remove it in the main datasets an re-check the data's normality
dat$Value[dat$Parameter=="Ammonia" & dat$Value>11] = NA # rplace it in main dataset
dat_r$Value[dat_r$Parameter=="Ammonia" & dat_r$Value>11] = NA # replace it in reduced dataset
temp = dat_r[dat_r$Parameter == "Ammonia",]
qqPlot(temp$Value); shapiro.test(temp$Value)
hist(temp$Value)
plot(density(temp$Value, na.rm = T))
range(temp$Value, na.rm = T)
# still not normal!
# this looks like a lognormal, Gamma, or Weibull distribution
# it is bounded above zero and is right-skewed
# what happens if I log-transform it?
temp = dat_r[dat_r$Parameter == "Ammonia",]
qqPlot(log10(temp$Value)); shapiro.test(log10(temp$Value))

# a log10 transformation did the trick! That tells me that it is lognormal. I will note in my report that a log10 transformation is a possible option if my models don't meet assumptions.
# Also note the stair-steps in the data at lower values. This could result from detection limits where the low value was replaced with a standard value. It shouldn't be a huge problem, but it is worth noting as a thing to investigate if the analyses don't turn out well. 


#### check for temporal autocorrelation ####

# I'm going to check these one site at a time and only of data with at least 100 obs in each site, as I am unlikely to analyze less frequently gathered data
dat_r = 
  dat %>% 
  group_by(Parameter, Site_Name) %>% 
  filter(n() > 100) %>% 
  arrange(datetime_NM)
summary(dat_r$Parameter) # note which parameters are left after filtering
summary(dat_r$Site_Name) # note that VR-1 no longer has any observations, so I will focus on the other two sites

# checking for temporal autocorrelation requires the data to be a time series object (read ?ts for details on this)
# To achieve this, I need regularly spaced data. This data is irregularly spaced, approximately monthly, but sometimes there are more than one observations per month or fewer
# I will start by averaging observations within the same month:
dat_monthly = 
  dat_r %>%
  mutate(yr = lubridate::year(datetime_NM)) %>%
  mutate(mo = lubridate::month(datetime_NM)) %>%
  dplyr::select(Site_Name, Parameter, yr, mo, Value) %>%
  group_by(Site_Name, Parameter, yr, mo) %>%
  summarise(Value.mn = mean(Value, na.rm = T)) %>%
  mutate(date = paste(yr, mo, "15", sep="-")) %>%
  mutate(date = as.Date(date))


#### Alkalinity in VR-2
### subset data to be one site and one parameter
temp = dat_monthly[dat_monthly$Parameter == "Alkalinity" & dat_monthly$Site_Name=="VR-2" ,]
### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = date)
## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=12, start=c(1979, 10)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details). 
forecast::Acf(temp_ts, na.action = na.pass) 
forecast::Acf(temp_ts, na.action = na.contiguous) 
forecast::Acf(temp_ts, na.action = na.interp)

forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

# acf tells me that there is temporal autocorrelation. The sin-wave-like pattern is typical of a ts impacted by seasonality
# pcaf tells me that strongest source of autocorrelation is at lag 1, which indicates a random walk/AR1 process. There is possibly ac at other lags, depending on how NAs are handled. 


#### Alkalinity in VR-3
### subset data to be one site and one parameter
temp = dat_monthly[dat_monthly$Parameter == "Alkalinity" & dat_monthly$Site_Name=="VR-3" ,]
### make this a time series object
## first, make doubly sure that the data is arranged by time before converting to ts object!
temp = temp %>% arrange(date) 
## second, make the spacing of dates consistent and fill in missing obs with NA. This is a handy fxn. You can also create a df of evenly spaced dates and left_join the data to this.
temp_ts =
  temp %>% 
  complete(date = seq(min(date), max(date), by = "1 month"), 
           fill = list(value = NA)) %>%
  as_tsibble(index = date)
## finally, convert to a ts object
# a ts object is a vector of data taken sequentially through time. Required arguments are:
# - the data vector
# - the frequency, which is the number of observations per unit of time. Lots of ways to specify this. For monthly data, you can put in 12 and it will assume that's 12 obs in a year. Google for help for other frequencies.
# - the start, which specifies when the first obs occured. Lots of ways to specify this. For monthly data, you can put in c(year, month) and it will know what you mean. 
head (temp_ts)
temp_ts = ts(temp_ts$Value.mn, frequency=12, start=c(1983, 1)) 
# check that you specified the ts correctly
print(temp_ts, calendar = T) 
### now we're ready to check for temporal autocorrelation in this ts!
# I prefer the forecast pkg's Acf fxn over base R acf() because Acf() doesn't include 0 (which is always 1) and shows month #s by default instead of decimal years. Note the different options for dealing with NAs and how this changes the results (see ?na.fail and ?Acf for details).
forecast::Acf(temp_ts, na.action = na.pass) 
forecast::Acf(temp_ts, na.action = na.contiguous) 
forecast::Acf(temp_ts, na.action = na.interp) 
forecast::Pacf(temp_ts, na.action = na.pass)
forecast::Pacf(temp_ts, na.action = na.contiguous)
forecast::Pacf(temp_ts, na.action = na.interp)

# acf tells me that there is temporal autocorrelation. The sin-wave-like pattern is typical of a ts impacted by seasonality
# pcaf tells me that strongest source of autocorrelation is at lag 1, which indicates a random walk/AR1 process. There is possibly ac at other lags, depending on how NAs are handled. 


# ....... ect. for each parameter and site combination I might include in the analysis .......



#### check for spatial autocorrelation ####

# I'm interested in spatial and not temporal autocorrelation, so I am going to look at just a few observations across all sites

# reload and format data with all sites
dat_all = read.csv("Week 5/coal_WQ.csv")
dat_all$datetime_NM = as.POSIXct(dat_all$Sample_DateTime, format="%m/%d/%y %H:%M", tz="MST")
dat_all$Parameter = as.factor(dat_all$Parameter)
dat_all$Site_Name = as.factor(dat_all$Site_Name)
dat_all$Is_Nondetect = as.factor(dat_all$Is_Nondetect)
dat_all$Value = as.numeric(dat_all$Value)
# how many sites are there?
length(unique(dat_all$Site_Name))
# 1288

# what parameters were collected across all sites in June 1995?
dat_june1995 = dat_all[dat_all$datetime_NM >= as.POSIXct("1995-06-01") &
                         dat_all$datetime_NM < as.POSIXct("1995-07-01"),]
tb = as.data.frame( with(dat_june1995, table(Site_Name, Parameter)) )
tb = tb[tb$Freq>0,]
tb2 = tb %>% group_by(Parameter) %>% summarise(n = n()) %>% arrange(desc(n))
head(tb2, 15)
#   parameter          # of sites it was collected at in June 1995
# 1 Bicarbonate        82
# 2 Calcium            82
# 3 Chloride           82
# 4 LabpH              82
# 5 LabTDS             82
# 6 Magnesium          82
# 7 Potassium          82
# 8 Sodium             82
# 9 Sulfate            82
# ^ these are good options for testing for spatial autocorrelation

### Bicarbonate in June 1995
dat_june1995 = dat_all[dat_all$datetime_NM >= as.POSIXct("1995-06-01") &
                         dat_all$datetime_NM < as.POSIXct("1995-07-01"),]
temp = dat_june1995 %>%  filter(Parameter=="Bicarbonate")
# randomly generate lat/lon for demo
set.seed(42)
temp$lat = runif(nrow(temp),35.090956,35.634117)
temp$lon = runif(nrow(temp),-107.65829,-106.65829)
## Moran.I
# generate an inverse distance matrix 
dists = as.matrix(dist(cbind(temp$lon, temp$lat)))
dists.inv = 1/dists
diag(dists.inv) = 0
# calculate Moran.I
Moran.I(temp$Value, dists.inv)
# we can NOT reject the null hypothesis that there is zero spatial autocorrelation present. In other words, there doesn't seem to be a lot of spatial autocorrelation. 
## Mantel test
# generate spatial distance matrix
site_dists = dist(cbind(temp$lon, temp$lat))
# generate response distance matrix 
resp_dists = dist(temp$Value)
# run Mantel test
mantel.rtest(site_dists, resp_dists, nrepet = 9999)
# 'observation' is the correlation between the distance matrices
# p value suggests that they are NOT correlated
# So, based on this test, there is no detectable correlation
## Map
proj = CRS("+proj=longlat +datum=WGS84")
temp_spatial  <- SpatialPointsDataFrame(coords= cbind(temp$lon, temp$lat),
                                    data = as.data.frame(cbind(temp$Site_Name, temp$Value)),
                                    proj4string = proj)
plot(temp_spatial)

# ect.......... for other parameters of interest and for a few other time points, depending on how your data is structured ...........

#
#### check correlation between variables ####

# first, returning to the dataset of just 3 sites and more than 100 obs per parameter (dat_r), reformat data to make it wider, such that parameters get their own columns. 

dat_r_long = dat_r %>% 
  select(c(Site_Name, datetime_NM, Parameter, Value))%>%
  group_by(Site_Name, datetime_NM, Parameter) %>%
  summarise(Value = mean(Value, na.rm = T)) %>%
  pivot_wider(names_from = Parameter, 
              values_from = Value,
              values_fill = list(Value = NA))

# reduce data down to one site - VR-2
temp = dat_r_long %>% filter(Site_Name=="VR-2") 
# plot correlations (of data columns only)
pairs.panels(temp[,3:24], scale=T)
pairs.panels(temp[,3:24], scale=F)
# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab = round(as.data.frame(cor(cov(temp[,3:24], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] = "no_corr"

# reduce data down to one site - VR-3
temp = dat_r_long %>% filter(Site_Name=="VR-3")
# plot correlations (of data columns only)
pairs.panels(temp[,3:24], scale=T)
pairs.panels(temp[,3:24], scale=F)
# make table of correlations (I am rounding and replacing low values with text so that it is easier to see results)
tab = round(as.data.frame(cor(cov(temp[,3:24], use="na.or.complete"))), 2)
tab[abs(tab)<0.4] = "no_corr"



