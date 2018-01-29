library(lubridate)
library(ggplot2)
library(dplyr)
library(geoR)
library(ggmap)
library(proj4)
library(fields)

DATADIR = "~/dev/rohan/rashi/thesis/final data/"

get_dir <- function(subdir) {
  paste(DATADIR, subdir, sep='')
}

read_climate_data <- function(filename) {
  df <- read.csv(filename)
  names(df)[names(df) == 'date'] <- 'Date'
  df$Date <- as.Date(df$Date, "%Y-%m-%d")
  df
}

read_data <- function(filename, address, city, state) {
  read_PM2.5<-read.csv(paste(get_dir("PM2.5 CSV/"),filename, sep = ''))
  dfPM2.5<-read_PM2.5[,c("Date","Concentration")]
  dfPM2.5$Date <- as.Date(dfPM2.5$Date, "%d/%m/%Y")
  
  read_PM10<-read.csv(paste(get_dir("pm10.csv/"),filename, sep = ''))
  dfPM10<-read_PM10[,c("Date","Concentration")]
  dfPM10$Date <- as.Date(dfPM10$Date, "%d/%m/%Y")
  
  read_climate_2016 <- read_climate_data(paste(get_dir("climate_data_agg/2016/"), mapping[[address]], sep = ''))
  read_climate_2017 <- read_climate_data(paste(get_dir("climate_data_agg/2017/"), mapping[[address]], sep = ''))
  dfclimate <- rbind(read_climate_2016, read_climate_2017)
  
  # Merging 3 data frames using package reshape
  
  mtemp1<-merge(dfPM2.5, dfPM10, by="Date")
  total <- merge(mtemp1, dfclimate, by="Date", all.x=TRUE)
  
  names(total)[names(total) == 'Concentration.y'] <- 'PM10'
  names(total)[names(total)=='Concentration.x'] <- 'PM2.5'
  
  total$PM2.5 <- as.numeric(total$PM2.5)
  total$PM10<- as.numeric(total$PM10)
  total$Monitoring_Station <- rep(address, nrow(total))
  total$City<-rep(city, nrow(total))
  total$State <- rep(state, nrow(total))
  total$month<-as.factor(month(total$Date))
  total$week_of_year <- as.factor(week(total$Date))
  total$day_of_week <- as.factor(weekdays(total$Date))
  total
}

# loop over all files
# extract -> address, city, state from name
mapping <- list(
  'Anand Vihar' = 'new delhi X safdarjung.csv',
  'Aya Nagar' = 'new delhi X safdarjung.csv',
  'Burari Crossing' = 'new delhi X safdarjung.csv',
  'CRRI Mathura Road' = 'new delhi X safdarjung.csv',
  'IGI Airport Terminal-3' = 'new delhi X safdarjung.csv',
  'ITO'='new delhi X safdarjung.csv',
  'Lodhi Road' = 'new delhi X safdarjung.csv',
  'Mandir Marg' = 'new delhi X safdarjung.csv',
  'North Campus' = 'new delhi X safdarjung.csv',
  'Punjabi Bagh' = 'new delhi X safdarjung.csv',
  'Pusa' = 'new delhi X safdarjung.csv',
  'R K Puram' = 'new delhi X safdarjung.csv',
  'HSPCBGurgaon' = 'new delhi X palam.csv',
  'Golden Temple' = 'amritsar.csv',
  'Punjab Agricultural University' = 'patiala.csv',
  'RIMT University' = 'patiala.csv',
  'Jaipur' = 'jaipur X sanganer.csv',
  'Police Commissionerate' = 'jaipur X sanganer.csv',
  'Jodhpur' = 'jodhpur.csv',
  'Vasundhara' = 'new delhi X safdarjung.csv',
  'Sector-62' = 'new delhi X safdarjung.csv',
  'Sector-125' = 'new delhi X safdarjung.csv',
  'Lajpat Nagar' = 'bareilly.csv',
  'Rohtak' = 'hissar.csv',
  'Ardhali Bazar' = 'varanasi X babatpur.csv'
)

list_of_data <- list()
i=1
for(fname in list.files(path = get_dir("/pm10.csv/"))) {
  parts <-strsplit(fname,"\\.")
  locations_part<-unlist(parts)[1]
  locations<-unlist(strsplit(locations_part,'_')[[1]])
  state = locations[1]
  city = locations[2]
  address = locations[3]
  cat(fname, "-->", state, city, address, "\n")
  df <- read_data(fname, address, city, state)
  
  list_of_data[[i]] <- df
  i = i + 1
}

all_df <- do.call(rbind, list_of_data)


plot(all_df$Date, all_df$T)
plot(all_df$Date, all_df$PM2.5)
plot(all_df$Date, all_df$PM10)


cols<-list("PM10", "H", "SLP", "T", "TM", "V", "VG", "VM", "VV", "Tm")#, "PM2.5")
for(col in cols) {
  all_df[[col]] <- as.numeric(all_df[[col]])
  mu =  mean(all_df[[col]], na.rm = TRUE)
  all_df[is.na(all_df[[col]]),col] <- mu

  new_col_name = paste(col,"_cent", sep="")

  all_df[[new_col_name]] <- all_df[[col]]-mu
}

par(mfrow=c(2,2))
all_df$logPM2.5 <- log(all_df$PM2.5)
model <- lm(logPM2.5~PM10+T+H+week_of_year, data = all_df)
summary(model)
plot(model)
par(mfrow=c(1,1))
# View(all_df)

#Understanding pairwise coorelation prior to model slection
coorelation <- cor(data.frame(all_df$PM10_cent, all_df$H_cent, all_df$SLP_cent, all_df$T_cent, 
               all_df$TM_cent, all_df$V_cent, all_df$VG_cent, all_df$VM_cent,
               all_df$VV_cent, all_df$Tm_cent))

library(leaps)
library(car)
library(MASS)

regsubsets.out <- regsubsets(logPM2.5 ~ PM10_cent + month + H_cent + SLP_cent + T_cent
                             + TM_cent + V_cent + VG_cent + VM_cent + VV_cent
                             + Tm_cent + T_cent:H_cent, data=all_df, nbest=1,# 1 best model for each predictor 
                             nvmax = NULL, #null for no limit on number of variables 
                             force.in = NULL, force.out=NULL, method="exhaustive")
                             
regsubsets.out
summary(regsubsets.out)
summary.out <- summary(regsubsets.out)
as.data.frame(summary.out$outmat)
plot(regsubsets.out, scale = "adjr2" , main = "Adjusted R^2")

layout(matrix(1:2, ncol=2))
##Adjusted R^2
res.legend <- subsets(regsubsets.out, 
                      statistic = "adjr2", 
                      legend = FALSE, 
                      min.size = 5, main = "Adjusted R^2")


##Mallow Cp
res.legend <- subsets(regsubsets.out, statistic = "cp", 
                      legend= FALSE, min.size = 5, main= "Mallow Cp")
abline(a=5.5, b=1, lty=2)

which.max(summary.out$adjr2)
summary.out$which[17,]

# Regression with the best model

# getSeason <- function(date) {
#   if(month(date) %in% c(12, 1, 2))
#     return("winter")
#   else if(month(date) %in% c(3,4,5))
#       return("spring")
#   else if(month(date) %in% c(6,7,8))
#     return("summer")
#   else if(month(date) %in% c(9,10,11))
#     return("autum")
# }
# 
# all_df$season <- as.factor(sapply(all_df$Date, getSeason))

best.model <- lm(logPM2.5 ~ PM10_cent 
                  + week_of_year
                 #+ season
                 + H_cent +  T_cent
                 + TM_cent + VM_cent + VV_cent + VG_cent
                 +  T_cent:H_cent, data=all_df)
summary(best.model)

# Stepwise selection
fit <- lm(logPM2.5 ~ PM10_cent + month + H_cent + SLP_cent + T_cent
          + TM_cent + V_cent + VM_cent + VV_cent
          + Tm_cent + T_cent:H_cent + VG_cent, data=all_df)
step<-stepAIC(fit, direction="both")
summary(step)
step$anova # display results

fit <- lm(logPM2.5 ~ PM10_cent, data=all_df)
fit
summary(fit)


list_of_data_2015 = list()
i=1
for(f in list.files(get_dir("pm10_2015/"))) {
  cat(f,"\n")
  df <- read.csv(paste(get_dir("pm10_2015/"), f, sep = ''))
  # fix date
  df$Sampling.Date <- as.Date(df$Sampling.Date, "%d-%m-%y")
  names(df)[names(df) == 'RSPM.PM10'] <- 'PM10'
  list_of_data_2015[[i]] <- df
  i = i + 1
}
all_data_2015 <- do.call(rbind, list_of_data_2015)

mapping_2015 = list(
  'Chandigarh' = 'patiala.csv',
  'Delhi'='new delhi X safdarjung.csv',
  'Faridabad' = 'new delhi X safdarjung.csv',
  'Shimla' = 'dehradun.csv',
  'Paonta Sahib' = 'dehradun.csv',
  'Parwanoo'= 'dehradun.csv',
  'Damtal'='amritsar.csv',
  'Baddi' = 'patiala.csv',
  'Kala Amb' = 'patiala.csv',
  'Nalagarh' = 'patiala.csv',
  'Sunder Nagar' = 'patiala.csv',
  'Una' = 'patiala.csv',
  'Manali'='dehradun.csv',
  'Dharamshala' = 'amritsar.csv',
  'Ludhiana' = 'patiala.csv',
  'Jalandhar' = 'amritsar.csv',
  'Gobindgarh' = 'patiala.csv',
  'Naya Nangal' = 'shimla.csv',
  'Khanna' = 'patiala.csv',
  'Amritsar' = 'amritsar.csv',
  'Dera Bassi'= 'patiala.csv',
  'Bathinda' = 'ganganagar.csv',
  'Dera Baba' = 'amritsar.csv',
  'Patiala' = 'patiala.csv',
  'Sangrur' = 'patiala.csv',
  'Faridkot' = 'amritsar.csv',
  'Hoshiarpur' = 'amritsar.csv',
  'Kota' = 'kota aerodrome.csv',
  'Alwar' = 'jaipur X sanganer.csv',
  'Jodhpur' = 'jodhpur.csv',
  'Udaipur' = 'udaipur dabok.csv',
  'Jaipur' = 'jaipur X sanganer.csv',
  'Agra' = 'agra.csv',
  'Anpara'= 'daltonganj.csv',
  'Renusagar' = 'daltonganj.csv',
  'Kanpur' = 'lucknow X amausi.csv',
  'Lucknow'= 'lucknow X amausi.csv',
  'Gajraula' = 'new delhi X safdarjung.csv',
  'Ghaziabad' = 'new delhi X safdarjung.csv',
  'Varanasi' = 'varanasi X babatpur.csv',
  'Noida'= 'new delhi X safdarjung.csv',
  'Firozabad' = 'agra.csv',
  'Jhansi'='gwalior.csv',
  'Khurja' = 'new delhi X safdarjung.csv',
  'Allahabad' = 'varanasi X babatpur.csv',
  'Moradabad' = 'bareilly.csv',
  'Bareilly' = 'bareilly.csv',
  'Rai Bareilly' = 'lucknow X amausi.csv',
  'Unnao' = 'lucknow X amausi.csv',
  'Gorakhpur' = 'gorakhpur.csv',
  'Dehradun'= 'dehradun.csv',
  'Haldwani' = 'bareilly.csv',
  'Rudrapur' = 'bareilly.csv',
  'Kashipur' = 'bareilly.csv',
  'Haridwar' = 'dehradun.csv',
  'Rishikesh' = 'dehradun.csv'
)

unique(all_data_2015$City.Town.Village.Area)

#Merging climate and 2015 data
list_temp_2015 = list()
i = 1
cols<-list("PM10", "H", "SLP", "T", "TM", "V", "VG", "VM", "VV", "Tm")
for(city in unique(all_data_2015$City.Town.Village.Area)) {
  cat(city, mapping_2015[[city]], "\n")
  climate_data <- read_climate_data(paste(get_dir("climate_data_agg/2015/"), mapping_2015[[city]], sep = ''))
  cat(city," ", mapping_2015[[city]]," ", nrow(all_data_2015[all_data_2015$City.Town.Village.Area==city,]) , "->", nrow(climate_data) , "\n")
  temp_df <- merge(all_data_2015[all_data_2015$City.Town.Village.Area==city,], climate_data, by.x = 'Sampling.Date', by.y='Date', all.x = TRUE)
  
  for(col in cols) {
    temp_df[[col]] <- as.numeric(temp_df[[col]])
    mu =  mean(temp_df[[col]], na.rm = TRUE)
    temp_df[is.na(temp_df[[col]]),col] <- mu
    new_col_name = paste(col,"_cent", sep="")
    temp_df[[new_col_name]] <- temp_df[[col]]-mu
  }
  list_temp_2015[[i]] <- temp_df
  i=i+1
  
}
all_data_with_climate_2015 <- do.call(rbind, list_temp_2015)
all_data_with_climate_2015$month <- as.factor(month(all_data_with_climate_2015$Sampling.Date))
all_data_with_climate_2015$week_of_year <- as.factor(week(all_data_with_climate_2015$Sampling.Date))
# prediction
all_data_with_climate_2015$logPM2.5<-predict(best.model, all_data_with_climate_2015)
all_data_with_climate_2015$PM2.5 <- exp(all_data_with_climate_2015$logPM2.5)

amritsar <- all_data_with_climate_2015[all_data_with_climate_2015$City.Town.Village.Area=='Amritsar',]

library(ggplot2)
ggplot(amritsar, aes(x = Sampling.Date)) + 
  geom_line(aes(y=PM10), colour="red") + 
  geom_line(aes(y=PM2.5), colour="green") 

amritsar_486 <- all_data_with_climate_2015[all_data_with_climate_2015$Stn.Code=='486',]

ggplot(amritsar_486, aes(x = Sampling.Date)) + 
  geom_line(aes(y=PM10), colour="red") + 
  geom_line(aes(y=PM2.5), colour="green") 

dehradun <- all_data_with_climate_2015[all_data_with_climate_2015$City.Town.Village.Area=='Dehradun',]
ggplot(dehradun, aes(x = Sampling.Date)) + 
  geom_line(aes(y=PM10), colour="red") + 
  geom_line(aes(y=PM2.5), colour="green") 

jaipur <- all_df[all_df$city=='jaipur X sanganer',]
ggplot(jaipur, aes(x = Date)) + 
  geom_line(aes(y=PM10), colour="red") + 
  geom_line(aes(y=PM2.5), colour="green") 
  
  
ggplot(all_data_with_climate_2015, aes(x = Sampling.Date)) + 
  geom_line(aes(y=PM10), colour="red") + 
  geom_line(aes(y=PM2.5), colour="green")   
  
# add lat/lons of stations to data

stn_data <- read.csv(get_dir("station_data/stn_data.csv"))
stn_data_delhi <- read.csv(get_dir("station_data/delhi_stn_data.csv"))
stn_data <- rbind(stn_data, stn_data_delhi)
for(stn in unique(all_data_with_climate_2015$Stn.Code)) {
  if(nrow(stn_data[stn_data$stn_code==stn,]) ==0)
    cat(stn , ",")
}

final_data <- merge(all_data_with_climate_2015, stn_data, by.x = "Stn.Code", by.y="stn_code", all.x = TRUE)
# save(final_data, file="combined_data_all_vars.Rda")


library(dplyr)
# Spatial Analysis
#Subsetting data Frame 

df_summary<-final_data %>% group_by(City.Town.Village.Area) %>% summarise(PM2.5=mean(PM2.5), PM10=mean(PM10), N=length(Stn.Code), NumMon=length(unique(Stn.Code)))

#dataframe<-write.csv(df_summary, file = "SummaryStats.csv", row.names=FALSE, na="")

df_summary2016 <- all_df %>% group_by(City) %>% summarise(PM2.5=mean(PM2.5, na.rm = TRUE), 
                                                          PM10=mean(PM10), 
                                                          N=length(Monitoring_Station), 
                                                          NumMon=length(unique(Monitoring_Station))
                                                          )

#write.csv(df_summary2016, file = "SummaryStats2016.csv", row.names=FALSE, na="")

df_2015_agg <- final_data %>% group_by(Stn.Code, Latitude, Longitude) %>% summarise(PM2.5=mean(PM2.5), PM10=mean(PM10))
# write.csv(df_2015_agg, file = "df_2015_agg.csv", row.names=FALSE, na="")
df_2015_agg$logPM10 <- log(df_2015_agg$PM10)

df_2015_agg$logPM2.5 <- log(df_2015_agg$PM2.5)
View(df_2015_agg)

library(geoR)
library(ggmap)
# Create geodata objects for original and logged PM2.5, PM10 concentrations (geoR library) 
df_2015_agg.geo.PM2.5<-as.geodata(df_2015_agg, coords.col=c(3,2), data.col=4)
df_2015_agg.geo.logPM2.5<-as.geodata(df_2015_agg,coords.col=c(3,2),data.col=7)
df_2015_agg.geo.PM10 <- as.geodata(df_2015_agg,coords.col=c(3,2), data.col=5)
df_2015_agg.geo.PM10log <- as.geodata(df_2015_agg,coords.col=c(3,2), data.col=6)

# Plotting the geodata object is useful for exploratory analysis
plot(df_2015_agg.geo.PM2.5)
plot(df_2015_agg.geo.logPM2.5)
plot(df_2015_agg.geo.PM10)
plot(df_2015_agg.geo.PM10log)

#Creating map using ggplot
# First get a basemap (google) **zoom=1 endsget up with globe
basemap <- get_map(location='New Delhi', zoom = 5, maptype='roadmap', source='google')

# Convert basemap to ggplot object so we can add data
g<-ggmap(basemap) + geom_point(aes(x=Longitude,y=Latitude,color=PM10), alpha=0.5,size=4, data=df_2015_agg)+
  scale_color_gradient(low="blue", high="red")
plot(g)

### PROJECTIONS 
# projecting lat and lon to utm plane
# project() is from proj4 library
# Mapping from projection code
library(proj4)
proj.utm43<-"+proj=utm +zone=43 +north +datum=WGS84 +units=km"

newcoords<-project(as.matrix(cbind(df_2015_agg$Longitude, df_2015_agg$Latitude)), proj=proj.utm43)
df_2015_agg$x<-newcoords[,1]
df_2015_agg$y<-newcoords[,2]
# View(df_2015_agg)
# Create new geodata objects with projected coordinates
# Warning for 1 replicated data location
df_2015_agg.geo.utm<-as.geodata(df_2015_agg,coords.col=c(8,9), data.col=5)
plot(df_2015_agg.geo.utm)
points(df_2015_agg.geo.utm,cex.max=0.8,col= rev(heat.colors(20)), pt.divide="equal", main="PM10 concentrations (ug/m3)", xlab="Easting",ylab="Northing")


# Empirical Semivariogram Analysis:
# Using UTM projection
vario1<-variog(df_2015_agg.geo.utm,option="cloud")
vario1$max.dist
plot(vario1,xlab="Distance (h), km")

vario2<-variog(df_2015_agg.geo.utm,uvec=seq(0,1158,l=20),option="bin")
plot(vario2,xlab="Distance (h), km")

par(mfrow=c(2,2))
# Robust estimator "modulus" is preferred
vario2m<-variog(df_2015_agg.geo.utm,uvec=seq(0,1158,l=15),option="bin",estimator.type="modulus")
plot(vario2m,xlab="Distance (h), km")

vario3m<-variog(df_2015_agg.geo.utm,uvec=seq(0,1158,l=15),bin.cloud=T,estimator.type="modulus")
plot(vario3m,bin.cloud=T,xlab="Distance (h), km")


vario.dir<-variog4(df_2015_agg.geo.utm,uvec=seq(0,1158,l=15),option="bin",estimator.type="modulus")
plot(vario.dir)

# Nugget=1000, sill=2500, range=400
# Using weighted least square method with robust estimator
par(mfrow=c(2,2))
vfit.wls=variofit(vario2m,ini.cov.pars=c(2500,400),nugget=1000,cov.model='gaussian',weights='cressie')
plot(vario2m,xlab="Distance (h), km")
lines(vfit.wls)
title("Gaussian Semivariogram by WLS")
summary(vfit.wls)

vfit.wls.exp=variofit(vario2m,ini.cov.pars=c(2500,400),nugget=1000,cov.model='exponential',weights='cressie')
plot(vario2m,xlab="Distance (h), km")
lines(vfit.wls.exp)
title("Exponential Semivariogram by WLS")
summary(vfit.wls.exp)

vfit.wls.sph=variofit(vario2m,ini.cov.pars=c(2500,400),nugget=1000,cov.model='spherical',weights='cressie')
plot(vario2m,xlab="Distance (h), km")
lines(vfit.wls.sph)
title("Spherical Semivariogram by WLS")
summary(vfit.wls.sph)

vfit.wls.mat=variofit(vario2m,ini.cov.pars=c(2500,400),nugget=1000,cov.model='matern',kappa=0.5, weights='cressie')
plot(vario2m,xlab="Distance (h), km")
lines(vfit.wls.mat)
title("Matern Semivariogram by WLS")
summary(vfit.wls.mat)

# # Fitting covariance function by ML
# # ROHAN
# mlfit.gau=likfit(df_2015_agg.geo.utm,
#                  ini.cov.pars=c(2500,100),
#                  nugget=1000, 
#                  fix.nugget=FALSE, 
#                  cov.model='gaussian',
#                  lik.method='ML')
#                 
# summary(mlfit.gau)
# 
# dst <- dist(df_2015_agg[,c(8,9)])
# dst <- data.matrix(dst)
# dim <- ncol(dst)
# image(1:dim, 1:dim, dst, axes = FALSE, xlab="", ylab="")
# 
# axis(1, 1:dim, nba[1:20,1], cex.axis = 0.5, las=3)
# axis(2, 1:dim, nba[1:20,1], cex.axis = 0.5, las=1)
# text(expand.grid(1:dim, 1:dim), sprintf("%0.1f", dst), cex=0.6)
# 
# #plot
# plot(vario2m,xlab="Distance (h), km")
# lines(mlfit.gau)
# lines(mlfit.gau, col="red")

# WLS gives better fit
#create grid for kriging/interpolation why y and x
res=200
xs=seq(min(df_2015_agg$x-500),max(df_2015_agg$x+500),len=res)
ys=seq(min(df_2015_agg$y-500),max(df_2015_agg$y+500),len=res)
myGrid=expand.grid(xs,ys)
names(myGrid)=c('y','x')

# Base map for india
library(rgdal)
library(rgeos)
india <- raster::getData("GADM", country = "India", level = 2)
states <- subset(india, NAME_1 %in% c("Punjab", "Haryana", "Himachal Pradesh", "Chandigarh",
                                      "Uttar Pradesh", "Rajasthan", "NCT of Delhi", "Uttarakhand", "Jammu and Kashmir"))

states <- spTransform(states, CRS("+proj=utm +zone=43 +north +datum=WGS84 +units=km"))
plot(states)

par(mfrow=c(2,1))
#Kriging by WLS estimator
#universal kriging with trend
vario2m_1<-variog(df_2015_agg.geo.utm,uvec=seq(0,1158,l=15),option="bin",estimator.type="modulus", trend = "1st")
plot(vario2m_1,xlab="Distance (h), km")
vfit.wls_1=variofit(vario2m_1,ini.cov.pars=c(2500,400),nugget=1000,cov.model='gaussian',weights='cressie')
plot(vario2m_1,xlab="Distance (h), km")

KCtrend<-krige.control(obj.m=vfit.wls_1, trend.d='1st',trend.l='1st')
universal.krige<-krige.conv(df_2015_agg.geo.utm,locations=myGrid,krige=KCtrend,output=output.control(signal=TRUE))
#plot on grid
dev.off()
image.plot(xs,ys,matrix(universal.krige$predict,res,res,byrow=FALSE),col=tim.colors(256))
plot(states, add=TRUE)
text(coordinates(states)[,1], coordinates(states)[,2], states$NAME_2, cex=0.5)

#variance plot
image.plot(xs,ys,matrix(sqrt(universal.krige$krige.var),res,res,byrow=FALSE),col=tim.colors(256))
plot(states, add=TRUE)
text(coordinates(states)[,1], coordinates(states)[,2], states$NAME_2, cex=0.5)







