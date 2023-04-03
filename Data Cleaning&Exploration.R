library(tidyverse)
library(data.table)
library(forecast)
#read data
flu<-fread("Data/french_flu.csv")
head(flu)
#find out all missing value in all columns
map(flu,~sum(is.na(.)))
glimpse(flu)

#create new columns as numeric columns
flu[, flu.rate := as.numeric(TauxGrippe)]
# some columns filled with NA's by coercion 
# We also do some basic quality checks,
# such as looking for NA in our variables of interest.
nrow(flu[is.na(flu.rate)]) / nrow(flu)
unique(flu[is.na(flu.rate)]$region_name)
# The overall rate of NA data points is not very high. Additionally, 
# Île-de-France, is not included in the list of regions with NA values

#create year and week columns from week columns
flu[, year := as.numeric(substr(week, 1, 4))]
flu[, wk := as.numeric(substr(week, 5, 6))]

# the purpose of paste0(),
# which marks each date as the first day of the week, pasting a "1" onto a string
# note %U for week of the year and %u day of the week

flu[, date:= as.Date(paste0(as.character(flu$week), "1"), "%Y%U%u")]
#note week column before paste0 function is ex "201307" and after paste0 "2013521"
warnings()
head(flu,20)
#filter region name by ILE-DE-FRANCE & order it by increasing order
paris.flu = flu[region_name == "ILE-DE-FRANCE"]
paris.flu = paris.flu[order(date, decreasing = FALSE)]

#show some columns
paris.flu[, .(week, date, flu.rate)]
# the result contain 522 rows because we encabsulte year into number of weeks
#we also have two rows have NA values because some years contain 53 week rather than 52 weeks 
#we can check that by the following code 

paris.flu[,.N,year]
#note N here represent number of rows in each year  

#for weeks we will see also two week contain 53  
paris.flu[,.N,wk]

#find out trend in the data for flu.rate column
paris.flu[, plot(date, flu.rate,
                 type = "l", xlab = "Date",
                 ylab = "Flu rate",col = "red")]

#for more sophisticated figure we use ggplot

  ggplot(paris.flu,aes(date,y=flu.rate))+
  geom_line(na.rm=TRUE,color="blue")+
  theme_dark()+
  scale_y_continuous(breaks = c(0,100,200,300,400,500,600))+
  labs(title = "flu rate for paris")  

# fitting sarima model (because we find trend in the data so we use sesonal
# autoregressive moving average process)
 #first we explore autocorrelation function between samples with and without difference  
  acf(paris.flu$flu.rate )
  acf(diff(paris.flu$flu.rate, 52))  
#to show most of lags we use lag.max parameter   
  acf(paris.flu$flu.rate, lag.max = 104)
  acf(diff(paris.flu$flu.rate, 52), lag.max = 104)
#note living in a four-season climate. Flu rates will have a strong
#correlation with neighboring weeks—that is, close to their time of measurement.   
  
#we know that in half a year the flu value will likely have changed quite a bit. 
#If it was high earlier, it should be low now and vice versa.  
  
#SARIMA model can't fit real world perfectly so we need more seasonally model by increase stationary more and more
par(mfrow = c(2,1))
plot(diff(diff(paris.flu$flu.rate, 52), 52),type = "o",col = "blue")
plot(diff(diff(paris.flu$flu.rate, 52), 1),type = "o")  

#find model parameter using acf and pacf functions
par(mfrow = c(2, 1))
acf (diff(diff(paris.flu$flu.rate, 52), 1), lag.max = 104,main = "ACF for paris flu")
pacf(diff(diff(paris.flu$flu.rate, 52), 1), lag.max = 104,main = "PACF for paris flu")

#save the data for now
write.csv(paris.flu,"Data/paris.flu.csv")
