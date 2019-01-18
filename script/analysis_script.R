#open Muka Head and Wave data

muka<-read.csv('csv_file/CMukaHead.csv')
wave <-read.csv('csv_file/DWave.csv')

#change the time to be same
#Muka Head Data (muka)
date <- as.POSIXct(muka$daytime, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
muka <- cbind(date,muka)
muka <- muka[,-2]

#Wave Data(wave)
date <- as.POSIXct(wave$time1, format = "%d/%m/%Y %H:%M", tz = "GMT")
date <- as.POSIXct(wave$time1, format = "%d/%m/%Y %H:%M", tz = "Asia/Kuala_Lumpur")
wave <- cbind(date,wave)
wave <- wave[,-5]



#Filter Data
#Remove lat1, lon1, index1 in wave data
wave <- wave[,-c(2,3,4)]

#Remove problematic value in wind speed
plot(muka$wind_speed)
muka$wind_speed[muka$wind_speed >5]<-NA

which.max(muka$wind_speed)
which.min(muka$wind_speed)

#Merge 2 data
library(openair)
names(wave)[1] <- "date"
temp <- timeAverage(wave, avg.time = "30 min", start.date = "2015-11-03 11:09:00", interval = "30 min")
merged_df <- merge(muka, temp, by = c("date","date"))

#Write in csv
write.csv(merged_df,file="merged_data_wt.csv")

#Filter data
#Create temp data frame
merged_df2 <- merged_df
#based on wind direction
merged_df2$co2_flux[merged_df2$dir > 90 & merged_df2$qc_co2_flux == 2] <- NA
merged_df2$LE[merged_df2$dir > 90 & merged_df2$qc_LE == 2] <- NA

#write merged_df2 into csv
write.csv(merged_df2, file="done_merged_df2.csv")

#Data analysis
#Time Series
library(ggplot2)
ggplot(merged_df2, aes(x=date,y=swh1))+ geom_smooth()
ggplot(merged_df2, aes(x=date,y=LE))+ geom_smooth()
ggplot(merged_df2, aes(x=date,y=co2_flux))+ geom_smooth()
ggplot(merged_df2, aes(x=date,y=wind_speed))+ geom_smooth()
ggplot(merged_df2, aes(x=date,y=wind_speed_alt1))+ geom_smooth()

plot(merged_df2$LE~merged_df2$date, type = 'l',ylim=c(-200,200))
plot(merged_df2$co2_flux~merged_df2$date, type = 'l',ylim=c(-20,20))
plot(merged_df2$wind_speed~merged_df2$date, pch =19)
plot(merged_df2$wind_speed_alt1~merged_df2$date, pch=19)
plot(merged_df2$swh1~merged_df2$date, pch=19)
plot(merged_df2$swh1~date,type='l',col='red',main="Time Series Plot of Significant Wave Height",ylab='swh',xlab='Time(Month)')

#Relationship between the variables
plot(merged_df2$swh1,merged_df2$LE, pch=19, col = "green", xlim=c(0,2),ylim=c(-5,25))
abline(lm(merged_df2$LE~merged_df2$swh1),col="red")
plot(merged_df2$swh1,merged_df2$co2_flux, pch=19, col = "blue", xlim=c(0,2), ylim=c(-1,1))
abline(lm(merged_df2$co2_flux~merged_df2$swh1),col="red")







