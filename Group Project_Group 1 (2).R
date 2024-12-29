setwd("/Users/jiyun/Desktop/GWU/DNSC 6319_time series/project")

bc=read.csv("bikecounts.csv",header=TRUE)
attach(bc)
head(bc)

# Defining ts (time series) objects
cnt=ts(data=cnt, start=c(2011, 1), frequency=365) 
ts.plot(cnt,col="blue", lwd="2",ylab="count",main="Bike Share Rentals")

# Define parameters
n_CNT=cnt[1:625]
n_MONTH=mnth[1:625]
time<-seq(1, length(n_CNT))
ctime<-seq(626, 731)
n=length(n_CNT)

# 2.1
# Seasonal dummies & Trend
sfit<-lm(n_CNT~time+as.factor(n_MONTH))
summary(sfit)

# Cyclical
# Periodogram
library(TSA)

#removing trend
detrend<-lm(n_CNT~time)
prdgrm=periodogram(detrend$residuals)

period=1/prdgrm$freq

# Listing periodogram values
frequency=prdgrm$freq
amplitude=prdgrm$spec

periodogram(detrend$residuals,col="blue")
plot(period,prdgrm$spec, type="h",col="blue",ylab="Peridogram",lwd=2)

all=cbind(period,frequency,amplitude)
all

# Find Top 6 amplitude to create sine and cos pairs
all6 <- all[order(all[,3], decreasing = TRUE),]
top6 <- all6[1:6,]
top6

# Creating the sine and cosine terms 
cos1=cos(2*pi*(2/n)*time)
sin1=sin(2*pi*(2/n)*time)

cos2=cos(2*pi*(1/n)*time)
sin2=sin(2*pi*(1/n)*time)

cos3=cos(2*pi*(4/n)*time)
sin3=sin(2*pi*(4/n)*time)

cos4=cos(2*pi*(8/n)*time)
sin4=sin(2*pi*(8/n)*time)

cos5=cos(2*pi*(11/n)*time)
sin5=sin(2*pi*(11/n)*time)

cos6=cos(2*pi*(93/n)*time)
sin6=sin(2*pi*(93/n)*time)

cfit<-lm(n_CNT~time+cos1+sin1+cos2+sin2+cos3+sin3+cos4+sin4+cos5+sin5+cos6+sin6)
summary(cfit)

plot.ts(n_CNT, main="Actual versus Predicted Count", ylab="Count",lwd=2)
lines(predict(sfit),col="red", lwd=2)

plot.ts(n_CNT, main="Actual versus Predicted Count", ylab="Count",lwd=2)
lines(predict(cfit),col="red", lwd=2)

# 2.2
# mean absolute percentile error
spred <- predict(sfit, newdata = data.frame(n_MONTH=mnth[626:731], time = c(626:731)), interval="prediction")
smape <- mean(abs(cnt[626:731]-spred[,1])/cnt[626:731])

# Create sine and cosine terms for the new data
cos1_new <- cos(2 * pi * (2 / n) * ctime)
sin1_new <- sin(2 * pi * (2 / n) * ctime)
cos2_new <- cos(2 * pi * (1 / n) * ctime)
sin2_new <- sin(2 * pi * (1 / n) * ctime)
cos3_new <- cos(2 * pi * (4 / n) * ctime)
sin3_new <- sin(2 * pi * (4 / n) * ctime)
cos4_new <- cos(2 * pi * (8 / n) * ctime)
sin4_new <- sin(2 * pi * (8 / n) * ctime)
cos5_new <- cos(2 * pi * (11 / n) * ctime)
sin5_new <- sin(2 * pi * (11 / n) * ctime)
cos6_new <- cos(2 * pi * (93 / n) * ctime)
sin6_new <- sin(2 * pi * (93 / n) * ctime)

# Predict using the new data
cpred <- predict(cfit, newdata = data.frame(time = ctime, cos1 = cos1_new, sin1 = sin1_new,
                                            cos2 = cos2_new, sin2 = sin2_new, cos3 = cos3_new,
                                            sin3 = sin3_new, cos4 = cos4_new, sin4 = sin4_new,
                                            cos5 = cos5_new, sin5 = sin5_new, cos6 = cos6_new,
                                            sin6 = sin6_new))

predictions <- cpred
cmape <- mean(abs(cnt[626:731] - predictions) / cnt[626:731])

# Correlations with trend
cor(n_CNT,predict(sfit))
# Correlations of Cyclical
cor(n_CNT,predict(cfit))

# 2.3
acf(sfit$resid, main="ACF of the residuals of the seasonal model",col="blue")
Box.test(sfit$resid,lag=20)

acf(cfit$resid, main="ACF of the residuals of the cyclical model",col="blue")
Box.test(cfit$resid,lag=20)

#Project
getwd()
proj<-read.csv("bikecounts.csv", header=TRUE, sep=",")
attach(proj)
head(proj)
proj <- proj[, -which(names(proj) == "dteday")]
proj
# Assuming your data frame is called df and you want to drop the variable named "variable_to_drop"
proj <- proj[, -which(names(proj) == "mnth")]
proj
proj <- proj[, -which(names(proj) == "holiday")]
proj
proj <- proj[, -which(names(proj)== "weekday")]
proj
proj <- proj[, -which(names(proj)=="weathersit")]
proj

pairs(proj,lower.panel = NULL,col="blue")
cor(proj)


#1
#Plots and Correlation Estimates
bike_ts <- ts(data = cnt, start = c(2011, 1), frequency = 365)
ts.plot(bike_ts,col="red",lwd="2",main = "Count Vs Time", ylab="Count")
pairs(proj,lower.panel = NULL,col="blue")
cor(proj)

cor.test(cnt, windspeed)
cor.test(cnt, hum)
cor.test(cnt, temp)
cor.test(cnt, weathersit)
cor.test(cnt,weekday)
cor.test(cnt,holiday)
cor.test(cnt,mnth)

mlrfit<-lm(cnt~windspeed+hum+temp+weathersit+weekday+holiday+mnth)
summary(mlrfit)
confint(mlrfit)


library(ggplot2)
  
# Convert variables to factors
mnth <- as.factor(mnth)  
holiday <- as.factor(holiday)
workday <- as.factor(weekday)
weathersit <- as.factor(weathersit)
weekday <- as.factor(weekday)
  
ggplot(data = proj, aes(x = mnth, y = cnt, fill = mnth)) +
  geom_boxplot() + 
  labs(title = "Box Plot of Count by Month",
        x = "Month", 
        y = "Count") + 
  scale_fill_discrete(name = "Month")
  
ggplot(data = proj, aes(x = holiday, y = cnt, fill = holiday)) +
  geom_boxplot() +
  labs(title = "Box Plot of Count by Holiday",
       x = "Holiday",
        y = "Count") +
   scale_fill_discrete(name = "Holiday")
  
ggplot(data = proj, aes(x = workday, y = cnt, fill = workday)) +
  geom_boxplot() +
  labs(title = "Box Plot of Count by Weekday",
       x = "Weekday",
       y = "Count") +
   scale_fill_discrete(name = "Weekday")
  
ggplot(data = da, aes(x = weathersit, y = cnt, fill = weathersit)) +
  geom_boxplot() +
  labs(title = "Box Plot of Count by Weathersit",
        x = "Weathersit",
        y = "Count") +
  scale_fill_discrete(name = "Weathersit")
  
#Hold out sample prediction for Model 1 - Model 7
n=length(cnt)
  
n_temp=temp[1:625]
n_hum=hum[1:625]
n_windspeed=windspeed[1:625]
n_weathersit=weathersit[1:625]
n_cnt=cnt[1:625]
n_mnth=mnth[1:625]
n_holiday=holiday[1:625]
n_weekday=weekday[1:625]
  
#comparison of "candiate" models in terms of fit and hold-out sample
time <- seq(1, length(n_cnt))
  
# Make predictions
#model 1
time <- seq(1, length(n_cnt))
model1 <- lm(n_cnt ~ time+n_mnth, data = proj[1:625, ])
summary(model1)
pred <- predict(model1, 
                newdata = data.frame(n_mnth = mnth[626:731], 
                                       time = seq(626, 731)), 
                interval = "prediction")
  
mapemodel1=mean(abs(da$cnt[626:731]-pred[,1])/da$cnt[626:731])
pred
mapemodel1
  
#model 2
model2 <- lm(n_cnt ~ time + n_mnth +n_holiday+n_weekday+n_weathersit, data = proj[1:625, ])
summary(model2)
pred2 <- predict(model2, data.frame(n_mnth = mnth[626:731],n_holiday = holiday[626:731],n_weekday = weekday[626:731],n_weathersit = weathersit[626:731], time = seq(626, 731)), interval = "prediction")
mapemodel2=mean(abs(cnt[626:731]-pred2[,1])/da$cnt[626:731])
mapemodel2
  
#model 3
model3 <- lm(n_cnt ~ time+ n_temp + n_hum + n_windspeed , data = proj[1:625, ])
summary(model3)
pred3<- predict(model3, data.frame(n_temp = temp[626:731],n_hum = hum[626:731], n_windspeed = windspeed[626:731], time = seq(626, 731)), interval = "prediction")
  
mapemodel3=mean(abs(cnt[626:731]-pred3[,1])/cnt[626:731])
mapemodel3
  
#model 4
model4 <- lm(n_cnt ~ time + n_temp + n_hum + n_windspeed + n_mnth + n_holiday + n_weekday + n_weathersit, data = proj[1:625, ])
summary(model4)
pred4 <- predict(model4, 
                 data.frame(n_temp = temp[626:731],
                            n_hum = hum[626:731],
                            n_windspeed = windspeed[626:731],
                            n_mnth = mnth[626:731],
                            n_holiday = holiday[626:731],
                            n_weekday = weekday[626:731],
                            n_weathersit = weathersit[626:731],
                            time = seq(626, 731)), 
                  interval = "prediction")
  
mapemodel4=mean(abs(cnt[626:731]-pred4[,1])/cnt[626:731])
mapemodel4
  
model5 <- lm(n_cnt ~ time  + n_hum + n_windspeed + n_mnth + n_holiday + n_weekday + n_weathersit, data = proj[1:625, ])
summary(model5)
pred5 <- predict(model5, 
                 data.frame(
                    n_hum = hum[626:731],
                    n_windspeed = windspeed[626:731],
                    n_mnth = mnth[626:731],
                    n_holiday = holiday[626:731],
                    n_weekday = weekday[626:731],
                    n_weathersit = weathersit[626:731],
                    time = seq(626, 731)), 
                  interval = "prediction")
  
mapemodel5=mean(abs(cnt[626:731]-pred5[,1])/cnt[626:731])
mapemodel5
  
model6 <- lm(n_cnt ~ time  + n_windspeed + n_mnth + n_holiday + n_weekday + n_weathersit, data = proj[1:625, ])
summary(model6)
  
pred6 <- predict(model6, 
                 data.frame(
                   n_windspeed = windspeed[626:731],
                   n_mnth = mnth[626:731],
                   n_holiday = holiday[626:731],
                   n_weekday = weekday[626:731],
                   n_weathersit = weathersit[626:731],
                   time = seq(626, 731)), 
                 interval = "prediction")
  
mapemodel6=mean(abs(cnt[626:731]-pred6[,1])/cnt[626:731])
mapemodel6
  
model7 <- lm(n_cnt ~ time + + n_mnth + n_holiday + n_weekday + n_weathersit, data = proj[1:625, ])
summary(model7)
pred7 <- predict(model7, 
                  data.frame(
                    n_mnth = mnth[626:731],
                    n_holiday = holiday[626:731],
                    n_weekday = weekday[626:731],
                    n_weathersit = weathersit[626:731],
                    time = seq(626, 731)), 
                  interval = "prediction")
  
mapemodel7=mean(abs(cnt[626:731]-pred7[,1])/cnt[626:731])
mapemodel7
  
# plot for Actual vs Predicted for Model 4 and also Residuals for Model 4
plot.ts(n_cnt, main="Actual versus Predicted count", ylab="Count",col="blue")
lines((predict(model4)),col="red")
acf(model4$resid, main = "Residuals for Model 4", lag = 24, col = "red")
acf(model4$resid, main = " Model 4", lag = 24, col = "red")
pacf(residuals(model4), main = "Residuals for Model 4", lag = 24, col = "red")
  
# 4.1
#Residual Analysis
par(mfrow=c(1,2))

# Seasonal Dummies
sres=residuals(sfit)
acf(sres,main="ACF of Seasonal Residuals",col="blue")
pacf(sres,main="PACF of Seasonal Residuals",col="blue")

sr <- sfit$resid
ss<-Arima(sr, order = c(1, 0, 0))
ss

# Cyclical
cres=residuals(cfit)
acf(cres,main="ACF of Cyclical Residuals",col="blue")
pacf(cres,main="PACF of Cyclical Residuals",col="blue")

cr <- cfit$resid
cc<-Arima(cr, order = c(1, 0, 1))
cc

For 4.3
#Difference 
D_US=diff(da$cnt,lag=1)
par(mfrow=c(1,2))
plot.ts(D_US, col ="red")
acf(D_US, main= "first difference of series",col="red")
pacf(D_US, main= "first difference of series",col="red")

#CREATING DUMMY VARIABLES
n=length(da$cnt)
m1=rep(0,n)
m2=rep(0,n)
m3=rep(0,n)
m4=rep(0,n)
m5=rep(0,n)
m6=rep(0,n)
m7=rep(0,n)
m8=rep(0,n)
m9=rep(0,n)
m10=rep(0,n)
m11=rep(0,n)
m12=rep(0,n)

time=seq(1,n)
for (i in 1:n) {
  if (da$mnth[i]==1) m1[i]=1 else m1[i]=0
  if (da$mnth[i]==2) m2[i]=1 else m2[i]=0
  if (da$mnth[i]==3) m3[i]=1 else m3[i]=0
  if (da$mnth[i]==4) m4[i]=1 else m4[i]=0
  if (da$mnth[i]==5) m5[i]=1 else m5[i]=0
  if (da$mnth[i]==6) m6[i]=1 else m6[i]=0
  if (da$mnth[i]==7) m7[i]=1 else m7[i]=0
  if (da$mnth[i]==8) m8[i]=1 else m8[i]=0
  if (da$mnth[i]==9) m9[i]=1 else m9[i]=0
  if (da$mnth[i]==10) m10[i]=1 else m10[i]=0
  if (da$mnth[i]==11) m11[i]=1 else m11[i]=0
  if (da$mnth[i]==12) m12[i]=1 else m12[i]=0
}

library(forecast)
xnew=cbind(time[1:625],m2[1:625],m3[1:625],m4[1:625],m5[1:625],m6[1:625], m7[1:625],m8[1:625],m9[1:625],m10[1:625],m11[1:625],m12[1:625],da$temp[1:625],da$hum[1:625],da$holi[1:625],da$weekday[1:625],da$weathersit[1:625])

regarnew<-Arima(da$cnt[1:625], order = c(1, 1, 1), xreg=xnew)
summary(regarnew)
acf(regarnew$residuals,col="blue",lag.max=24)
accuracy(regarnew)

# for hold out sample
library(forecast)
xregarfit=cbind(da$temp[626:731],da$hum[626:731],da$holi[626:731],da$weekday[626:731],da$mnth[626:731],da$weathersit[626:731])

regarfith<-arima(da$cnt[626:731], order = c(1, 1, 1), xreg=xregarfit)
regarfith
accuracy(regarfith)
preds=fitted(regarfith)
plot.ts(da$cnt[626:731],col="blue",type="b",lwd=2,ylab="Bike Rental",main="Actual versus predicted")
lines(preds,col="red",lwd=2)
corr_coef <- cor(da$cnt[626:731], preds)^2
corr_coef 
# Test for white Noise
 acf(regar$residnew, main="ACF of the residuals from the  model",col="blue")
 Box.test(regarnew$resid,lag=20)

