df_pred <- read.csv("~/dev/rohan/rashi/thesis/final data/final_data_e_c_o.csv")

newcoords<-project(as.matrix(cbind(df_pred$Longitude, df_pred$Latitude)), proj=proj.utm43)
df_pred$x<-newcoords[,1]
df_pred$y<-newcoords[,2]




uk1 <-krige.conv(df_2015_agg.geo.utm, locations = newcoords, krige=KCtrend,output=output.control(signal=TRUE))

df_pred$pred_pm10 <- uk1$predict

df_pred.geo.utm<-as.geodata(df_pred,coords.col=c('x','y'), data.col = 'pred_pm10')
plot(df_pred.geo.utm)
points(df_pred.geo.utm,
       cex.max=0.8,
       col= rev(heat.colors(256)),
       pt.divide="equal",
       main="PM10 concentrations (ug/m3)", xlab="Easting",ylab="Northing")

write.csv(df_pred, "finaldata2.csv")
