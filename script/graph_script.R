#Construct column for difference of temperature, delta E, U*difference of temperature, rh in fraction

#cal difference of temperature
pt <- NA
pt = data$TS - data$TA
data<- cbind(data,pt)

#cal in-situ wind speed * difference of T
upt <- NA
upt = data$pt * data$wind_speed
data<- cbind(data,upt)

#Make the rh in fraction
rh_fraction <-NA
rh_fraction= data$RH /100
data<-cbind(data,rh_fraction)

source('tools/tool_vapor_cal.R')
e_s1 <- vap_pres_Buck(data$TS, 1.00)  # RH = 1.00 because saturated
e_a <- vap_pres_Buck(data$TA, data$rh_fraction) 
# deltaE must be in kPa for bulk aerodynamic transfer equation below
deltaE <- (e_s1 - e_a) * 0.1 # because to convert to kPa
data<-cbind(data,deltaE)

#Make udeltaE
ude <- NA
ude = data$deltaE * data$wind_speed
data<-cbind(data,ude)


#Regression lines
lm1 <- lm(data$LE ~ data$deltaE)
plot(data$deltaE,data$LE,pch = 17, col='red')
abline(lm1,col='blue', lwd=3)

lm2 <- lm(data$LE ~ data$ude)
plot(data$ude,data$LE,pch = 17,col='red')
abline(lm2,col='blue', lwd=3)

lm3 <- lm(data$H ~ data$upt)
plot(data$upt, data$H, pch= 17,col='red')
abline(lm3, col='blue', lwd =3)



#plot the monthly scale
d16 <- read.csv('csv_file/monthly_2016.csv')
d17 <- read.csv('csv_file/monthly_2017.csv')

x1 <- d16$WS_satellite
x2 <- d17$WS_satellite

y1 <- d16$WS_site
y2 <- d17$WS_site

path_fig <- file.path('figs/mCO2_flux_V4.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='serif', mar = c(4.1, 4.1, 0.1, 0.1))
plot(x1,y1,type='l', col='red', xlab= 'Month', ylab='CO'['2'],' flux')
lines(x2,y2, col='blue')



# mtext(side = 1, 'Month', line = 2.5, cex = 1.5)
# mtext(side = 2, expression(paste('CO'['2'],' flux')),
#       line = 2.1, cex = 1.5)
dev.off()

path_fig <- file.path('figs/ws_satellite_ws_insitu_V2.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='serif', mar = c(4.1, 4.1, 0.1, 0.1))
plot(x1,y1,pch=16, col='red', xlab= 'ws_satellite', ylab='WS_insitu')
points(x2,y2, col='blue')
dev.off()



path_fig <- file.path('figs/LE_ude_half_hourly.jpg')
jpeg(file=path_fig,width=16,height=8,res=400, units = 'cm')
par(family='serif', mar = c(4.1, 4.1, 0.1, 0.1))
plot(data$ude,data$LE,pch=16, col='red', xlab= 'ude', ylab='LE')
dev.off()


#Regression line and plots between Swh and CO2 flux, LE, H, WS_insitu, WS_sat
swh <- df_group_month$swh
co2_flux <- df_group_month$co2_flux
LE <- df_group_month$LE
H <- df_group_month$H
ws <- df_group_month$WS_site
ws_sat <- df_group_month$WS_satellite

path_fig <- file.path('figs/relationship_v2.jpg')
jpeg(file=path_fig,width=20,height=18,res=400, units = 'cm')
par(family='serif', mfrow = c(3,2),
    mar = c(3.9, 3.9, 1, 1))
#co2
plot(swh,co2_flux, pch=16, col='blue',xlab= '', ylab='CO2_flux')
lm1 <- lm(co2_flux ~ swh)
abline(lm1,col='red', lwd=3)

#LE
plot(swh,LE, pch=16, col='blue',xlab= '', ylab='LE')
lm2 <- lm(LE ~ swh)
abline(lm2,col='red', lwd=3)

#H
plot(swh,H, pch=16, col='blue',xlab= '', ylab='H')
lm3 <- lm(H ~ swh)
abline(lm3,col='red', lwd=3)

#ws
plot(swh,ws, pch=16, col='blue',xlab= '', ylab='Wind_Speed_Insitu')
lm4 <- lm(ws ~ swh)
abline(lm4,col='red', lwd=3)

#ws_sat
plot(swh,ws_sat, pch=16, col='blue',xlab= '', ylab='Wind_Speed_Satellite')
lm5 <- lm(ws_sat ~ swh)
abline(lm5,col='red', lwd=3)

mtext(side = 1, 'SWH [m]', line = 2.5, cex = 1.0)
dev.off()


#plot H and LE

path_fig <- file.path('figs/H_LE_V1.jpg')
jpeg(file=path_fig,width=16,height=14,res=400, units = 'cm')
par(family='serif', mfrow = c(2,2),
    mar = c(3.9, 3.9, 1, 1))

#LE vs ude
#half-hourly time scale
plot(data$ude, data$LE, pch=16, col='darkblue',xlab= 'ude', ylab='LE')
lm6 <- lm(data$LE ~ data$ude)
abline(lm6,col='red', lwd=3)

#monthly time scale
plot(df_group_month$ude, df_group_month$LE, pch=16, col='darkblue',xlab= 'ude', ylab='LE')
lm7 <- lm(df_group_month$LE ~ df_group_month$ude)
abline(lm7,col='red', lwd=3)

#H vs upt
#half-hourly
plot(data$upt, data$H, pch=16, col='darkblue',xlab= 'upt', ylab='H')
lm8 <- lm(data$H ~ data$upt)
abline(lm8,col='red', lwd=3)

#monthly
plot(df_group_month$upt, df_group_month$H, pch=16, col='darkblue',xlab= 'upt', ylab='H')
lm9 <- lm(df_group_month$H ~ df_group_month$upt)
abline(lm9,col='red', lwd=3)
dev.off()


#CO2 flux,H,LE vs Wind speed
path_fig <- file.path('figs/R_WS_V1.jpg')
jpeg(file=path_fig,width=16,height=14,res=400, units = 'cm')
par(family='serif', mfrow = c(3,2),
    mar = c(3.9, 3.9, 1, 1))
#CO2 flux vs ws_insitu
plot(ws,co2_flux, pch=16, col='blue',xlab= 'Wind_Speed_Insitu', ylab='CO2_flux')
la1 <- lm(co2_flux ~ ws)
abline(la1,col='red', lwd=3)

#CO2 flux vs ws_sat
plot(ws_sat,co2_flux, pch=16, col='blue',xlab= 'Wind_Speed_Satellite', ylab='CO2_flux')
la2 <- lm(co2_flux ~ ws_sat)
abline(la2,col='red', lwd=3)

#LE vs ws_insitu
plot(ws,LE, pch=16, col='blue',xlab= 'Wind_Speed_Insitu', ylab='LE')
la3 <- lm(LE ~ ws)
abline(la3,col='red', lwd=3)

#LE vs ws_sat
plot(ws_sat,LE, pch=16, col='blue',xlab= 'Wind_Speed_Sat', ylab='LE')
la4 <- lm(LE ~ ws_sat)
abline(la4,col='red', lwd=3)

#H vs ws_insitu
plot(ws,H, pch=16, col='blue',xlab= 'Wind_Speed_Insitu', ylab='H')
la5 <- lm(H ~ ws)
abline(la5,col='red', lwd=3)

#H vs ws_sat
plot(ws_sat,H, pch=16, col='blue',xlab= 'Wind_Speed_Sat', ylab='H')
la6 <- lm(H ~ ws_sat)
abline(la6,col='red', lwd=3)

dev.off()

summary(lm1)
summary(lm2)
summary(lm3)
summary(lm4)
summary(lm5)
summary(lm6)
summary(lm7)
summary(lm8)
summary(lm9)
summary(la1)
summary(la2)
summary(la3)
summary(la4)
summary(la5)
summary(la6)