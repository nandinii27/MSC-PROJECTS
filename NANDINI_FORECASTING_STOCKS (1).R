
library(rugarch)
library(quantmod)
library(ggplot2)
library(tseries)
library(fBasics)
library(zoo)
library(lmtest) 
library(forecast)
library(ggfortify)
library(prophet)
library(tsfknn)

# Download data for Apple
getSymbols("AAPL", src = "yahoo", from = "2000-01-01", to = Sys.Date())

chartSeries(AAPL, TA = NULL)

#ADF test
print(adf.test(AAPL$AAPL.Close))

#Plot ACF and PACF
par(mfrow = c(1, 2))
acf(AAPL$AAPL.Close)
pacf(AAPL$AAPL.Close)
par(mfrow = c(1, 1))

modelfit <-auto.arima(AAPL$AAPL.Close)

plot(resid(modelfit), ylab = "Residuals", main = "Residuals(Arima(5,1,2)) vs. Time")
plot(forecast(modelfit, h = 30))

tsdiag(modelfit)

x11()
price_forecast <- forecast(modelfit, h = 90)
plot(price_forecast)
head(price_forecast$mean)
head(price_forecast$upper)
head(price_forecast$lower)

N <- length(AAPL$AAPL.Close)
n <- 0.8 * N
train <- AAPL$AAPL.Close[1:n, ]
test <- AAPL$AAPL.Close[(n+1):N,]
trainarimafit <- auto.arima(train, lambda = "auto")
summary(trainarimafit)
predlen <- length(test)
trainarima_fit <- forecast(trainarimafit, h = predlen)

#Plotting mean predicted values vs real data
meanvalues <- as.vector(trainarima_fit$mean)
precios <- as.vector(test)
plot(meanvalues, type = "l", col = "red")
lines(precios, type = "l")

# Fitting ARFIMA model
fitarfima <- autoarfima(data = AAPL$AAPL.Adjusted, ar.max = 5, ma.max = 2, 
                        criterion = "AIC", method = "full")
fitarfima$fit

# Define the GARCH model
garch11closeprice <- ugarchspec(variance.model=list(garchOrder=c(1,1)),
                                mean.model=list(armaOrder=c(3,2)))

# Estimate the GARCH model
garch11closepricefit <- ugarchfit(spec=garch11closeprice, data=AAPL$AAPL.Close)

# Plot conditional volatility

x11()
plot.ts(sigma(garch11closepricefit), ylab="sigma(t)", col="blue")

# Model akike
infocriteria(garch11closepricefit)

# Normal residuals plot
garchres <- data.frame(residuals(garch11closepricefit))
plot(garchres$residuals.garch11closepricefit)

# Standardized residuals plot
garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE))
qqnorm(garchres$residuals.garch11closepricefit,standardize=TRUE)
qqline(garchres$residuals.garch11closepricefit,standardize=TRUE)

# Squared standardized residuals Ljung Box
garchres <- data.frame(residuals(garch11closepricefit, standardize=TRUE)^2)
Box.test(garchres$residuals.garch11closepricefit..standardize...TRUE..2, type="Ljung-Box")

# GARCH Forecasting
garchforecast <- ugarchforecast(garch11closepricefit, n.ahead = 30 )
plot(garchforecast)


















