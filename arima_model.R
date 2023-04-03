library(data.table)
library(forecast)
paris.flu <-fread("Data/paris.flu.csv")
# arima fit
# let's estimate 2 weeks ahead
first.fit.size <- 104
h <- 2
n <- nrow(paris.flu) - h - first.fit.size

# get standard dimensions for fits we'll produce
# and related info, such as coefs
first.fit <- arima(paris.flu$flu.rate[1:first.fit.size], order = c(2, 1, 0),
                   seasonal = list(order = c(0,1,0), period = 52))
first.order <- arimaorder(first.fit)

## pre-allocate space to store our predictions and coefficients
fit.preds <- array(0, dim = c(n, h))
fit.coefs <- array(0, dim = c(n, length(first.fit$coef)))

## after initial fit, we roll fit forward
## one week at a time, each time refitting the model
## and saving both the new coefs and the new forecast
## caution! this loop takes a while to run
for (i in (first.fit.size + 1):(nrow(paris.flu) - h)) {
  ## predict for an increasingly large window
  data.to.fit = paris.flu[1:i]
  fit = arima(data.to.fit$flu.rate, order = first.order[1:3],
              seasonal = first.order[4:6])
  fit.preds[i - first.fit.size, ] <- forecast(fit, h = 2)$mean
  fit.coefs[i - first.fit.size, ] <- fit$coef
}
#plot these rolling results(fitted model and it's prediction)
ylim <- range(paris.flu$flu.rate[300:400],
              fit.preds[, h][(300-h):(400-h)])
par(mfrow = c(1, 1))
plot(paris.flu$date[300:400], paris.flu$flu.rate[300:400],
     ylim = ylim, cex = 0.8,
     main = "Actual and predicted flu with SARIMA (2, 1, 0), (0, 1, 0)",
     xlab = "Date", ylab = "Flu rate")
lines(paris.flu$date[300:400], fit.preds[, h][(300-h):(400-h)],
      col = 2, type = "l",
      lty = 2, lwd = 2)

# alternative ARIMA model: Exogenous harmonic regressores instead of seasonality for higher accuracy 
#perform a dynamic harmonic regression

## preallocate vectors to hold coefs and fits
fit.preds <- array(0, dim = c(n, h))
fit.coefs <- array(0, dim = c(n, 100))
## exogenous regressors
## that is components of Fourier series fit to data
flu.ts <- ts(log(paris.flu$flu.rate + 1) + 0.0001,
             frequency = 52)
## add small offsets because small/0 vals
## cause numerical problems in fitting
exog.regressors <- fourier(flu.ts, K = 2)
exog.colnames <- colnames(exog.regressors)

## fit model anew each week with
## expanding window of training data
for (i in (first.fit.size + 1):(nrow(paris.flu) - h)) {
  data.to.fit <- ts(flu.ts[1:i], frequency = 52)
  exogs.for.fit <- exog.regressors[1:i,]
  exogs.for.predict <- exog.regressors[(i + 1):(i + h),]
  fit <- auto.arima(data.to.fit,
                    xreg = exogs.for.fit,
                    seasonal = FALSE)
  fit.preds[i - first.fit.size, ] <- forecast(fit, h = h,
                                              xreg = exogs.for.predict)$mean
  fit.coefs[i - first.fit.size, 1:length(fit$coef)] = fit$coef
}

# The hyperparameter K indicates how many separate sine/cosine pairs we will include
# in our fit, where each one represents a new frequency used to fit the sine/cosine. In
# general, K will be larger for larger seasonal period lengths and smaller for smaller one
#xreg parameter takes the fit Fourier series as additional regressors

#plot the performance of this model

ylim = range(paris.flu$flu.rate)
plot(paris.flu$date[300:400], paris.flu$flu.rate[300:400],
     ylim = ylim, cex = 0.8,
     main = "Actual and predicted flu with ARIMA +
harmonic regressors",
     xlab = "Date", ylab = "Flu rate")
lines(paris.flu$date[300:400], exp(fit.preds[, h][(300-h):(400-h)]),
      col = 2, type = 'l',
      lty = 2, lwd = 2)

#for more customized plot

flu.rate_date <-paris.flu[300:400]%>%
  select(date,flu.rate)
head(paris.flu)
head(flu.rate_date)

ggplot(flu.rate_date,aes(x=date,y=flu.rate))+
  geom_point()+
  geom_line(aes(date,exp(fit.preds[, h][(300-h):(400-h)])),color = "red")

#identify the peaks in the testing range of the data
test  = paris.flu[300:nrow(paris.flu)]
plot(test$flu.rate,ylab = "Flu Rate")
which(test$flu.rate > 400)
which(test$flu.rate > 400)
abline(v = 65)
which.max(test$flu.rate)
abline(v = 174)


##################################CONCLUSION###################
# Our dynamic harmonic regression
# would not take this into account, and it enforces a more rigid model of seasonality
# than does the SARIMA model, because the latter’s seasonal behavior can change
# over time This mismatch between assumptions and the data could go a long way
# toward explaining the poor performance of this model, such that using a bad model
# actually drew our attention to an important feature of our data we had not noticed
# earlier.
