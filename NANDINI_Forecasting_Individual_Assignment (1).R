#NANDINI GANTAYAT 
#MBD 2022-23

# ############# #
# Loading libraries #
# ############# #

library('fpp2')
library('readxl')
library('tseries')
library('lmtest')
library('forecast')
library('tidyverse')
library('ggfortify')

# ############# #
# Bankrupt data #
# ############# #
data <- read_excel("C:/Users/ngantayat1/Downloads/DataSets2023.xlsx", sheet="Bankrupt")
bankrupt_ts <- ts(data[,2], frequency = 12, start = c(2000, 1))

# Split the data in training and test set

bankrupt0<- window(bankrupt, end=c(2020,3))
bankrupt_train <- window(bankrupt, end=c(2017,3))
bankrupt_test <- window(bankrupt, start=c(2017,4), end=c(2020,3))
bankrupt_future <- window(bankrupt, start = c(2020, 4))

#length of the test set
h <- length(bankrupt_test)

# Plot the time series data
x11()
plot(bankrupt_ts)

#plot seasonality
seasonplot(bankrupt_ts, col=rainbow(20), year.labels = TRUE)

# Seasonal subseries plot
ggsubseriesplot(bankrupt_ts) + ggtitle("Seasonal Subseries Plot of Corporate Bankruptcies in Belgium")

# Plot the original and log-transformed data
autoplot(bankrupt_ts) + ggtitle("Corporate Bankruptcies in Belgium")
autoplot(bankrupt_log) + ggtitle("Log-transformed Corporate Bankruptcies in Belgium")
adf.test(bankrupt_ts, alternative = "stationary")

# Find optimal lambda value
lambda <- BoxCox.lambda(bankrupt_ts)

# Transform the time series using the optimal lambda value
bankrupt_ts_boxcox <- BoxCox(bankrupt_ts, lambda)

# Plot the transformed time series
plot(bankrupt_ts_boxcox, main = "Transformed Bankruptcy Time Series")

# Log transform the data
bankrupt_log <- log(bankrupt_ts)

# Auto-correlation & Partial Auto-correlation
x11()
#ACF
ggAcf(bankrupt_ts) 
#PACF
ggPacf(bankrupt_ts)  
#LAG Plot
lag.plot(bankrupt_ts)  

#Explore train data
plot(bankrupt_train)

#look at ts plot
tsdisplay(bankrupt_train)

#seasonal plot
seasonplot(bankrupt_train, col=rainbow(20), year.labels = TRUE)
fabletools::autoplot(bankrupt_train)

#Explore test data
plot(bankrupt_test)

#look at ts plot
tsdisplay(bankrupt_test)

#seasonal plot
seasonplot(bankrupt_test, col=rainbow(20), year.labels = TRUE)
fabletools::autoplot(bankrupt_test)

#Data transformation
bankrupt_ts_log <- log(bankrupt_ts)
plot(bankrupt_ts_log)

#Plotting the Seasonal and Monthly Trend Graphs
seasonplot(bankrupt_ts_log, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal Plot",
           ylab="Number of Bankruptcies in Belgium",
           col=rainbow(25),
           pch=20)

# Fit seasonal naive model and generate forecasts
snaive_fit <- snaive(bankrupt_train)
snaive_fcst <- forecast(snaive_fit, h = 24)

# Plot the forecasts and the actual values
plot(snaive_fcst, main = "Seasonal Naive Forecasts")
lines(bankrupt, col = "black")
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1))

# Check  residual diagnostics
checkresiduals(snaive_fcst)

# Calculate the accuracy measures of the forecasts
forecast::accuracy(snaive_fcst, bankrupt_test)

#Forecasting by decomposition
naive_fcst <- stlf(bankrupt_train, method="naive", h=h)
rwdrift_fcst <- stlf(bankrupt_train, method="rwdrift", h=h)
ets_fcst <- stlf(bankrupt_train, method="ets", h=h)
arima_fcst <- stlf(bankrupt_train, method="arima", h=h)

par(mfrow=c(1,1))
plot(naive_fcst); plot(rwdrift_fcst); plot(ets_fcst); plot(arima_fcst);

# Check residual diagnostics
checkresiduals(naive_fcst)
checkresiduals(rwdrift_fcst)
checkresiduals(ets_fcst)
checkresiduals(arima_fcst)

# Calculate forecast accuracy measures
forecast::accuracy(naive_fcst, bankrupt_test)
forecast::accuracy(rwdrift_fcst, bankrupt_test)
forecast::accuracy(ets_fcst, bankrupt_test)
forecast::accuracy(arima_fcst, bankrupt_test)

par(mfrow=c(1,1))
m2 <- mstl(bankrupt_train)
pl_adj <- seasadj(m2)
x11()
plot(rwf(pl_adj, h=h, drift = TRUE), ylim =c(0, 2200))
lines(bankrupt_test, col="red")
lines(rwdrift_fcst$mean, col="black")

#ETS MODELS

ets1 <- ets(bankrupt_train, model = "AAA", damped= FALSE)
ets2 <- ets(bankrupt_train, model = "AAA", damped= TRUE)
ets3 <- ets(bankrupt_train, model = "MAM", damped= FALSE)
ets4 <- ets(bankrupt_train, model = "MAM", damped= TRUE)
ets5 <- ets(bankrupt_train, model = "MMM", damped= FALSE)
ets6 <- ets(bankrupt_train, model = "MMM", damped= TRUE)


ets1_fcst <- forecast(ets1, h=h)
ets2_fcst <- forecast(ets2, h=h)
ets3_fcst <- forecast(ets3, h=h)
ets4_fcst <- forecast(ets4, h=h)
ets5_fcst <- forecast(ets5, h=h)
ets6_fcst <- forecast(ets6, h=h)


plot(ets1_fcst, main = "Forecasts using ETS: AAA")
lines(bankrupt, col = "red")
lines(ets1_fcst$mean, col= 'blue')
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1))

plot(ets2_fcst, main = "Forecasts using ETS: AAA with damped")
lines(bankrupt, col = "red")
lines(ets2_fcst$mean, col= 'blue')
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1))


plot(ets3_fcst, main = "Forecasts using ETS: MAM")
lines(bankrupt, col = "black")
lines(ets3_fcst$mean, col= 'blue')
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1))

plot(ets4_fcst, main = "Forecasts using ETS: MAM with damped")
lines(bankrupt, col = "black")
lines(ets4_fcst$mean, col= 'blue')
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1))

plot(ets5_fcst, main = "Forecasts using ETS: MMM")
lines(bankrupt, col = "black")
lines(ets5_fcst$mean, col= 'blue')
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1))

plot(ets6_fcst, main = "Forecasts using ETS: MMM with damped")
lines(bankrupt, col = "black")
lines(ets6_fcst$mean, col= 'blue')
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1))

checkresiduals(ets1_fcst)
checkresiduals(ets2_fcst)
checkresiduals(ets3_fcst)
checkresiduals(ets4_fcst)
checkresiduals(ets5_fcst)
checkresiduals(ets6_fcst)


# Check the accuracy
accuracy(ets1_fcst, bankrupt_test)
accuracy(ets2_fcst, bankrupt_test)
accuracy(ets3_fcst, bankrupt_test)
accuracy(ets4_fcst, bankrupt_test)
accuracy(ets5_fcst, bankrupt_test)
accuracy(ets6_fcst, bankrupt_test)



# Fit ETS models using automated procedure
auto_model <- ets(bankrupt_train)

#forecast
auto_model_fcst <- forecast(auto_model, h=h)


#plot
plot(auto_model_fcst, main = "Forecasts using Automatted ETS")
lines(bankrupt, col = "black")
lines(auto_model_fcst$mean, col= 'blue')
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1))


#residual of auto ets
checkresiduals(auto_model_fcst)

#accuracy on test set
forecast::accuracy(auto_model_fcst, bankrupt_test)

# Compare models using AIC and BIC
models <- list(ets1, ets2, ets3, ets4, ets5, ets6, auto_model)
AIC <- sapply(models, AIC)
BIC <- sapply(models, BIC)
results <- cbind(AIC, BIC)
rownames(results) <- c("AAA", "AAA with damped", 'MAM', 'MAM with damped', 'MMM', 'MMM with damped', 'auto ets')
results


tsdisplay(bankrupt)

# Apply auto arima to the train set
auto_arima <- auto.arima(bankrupt_train)

# Generate forecasts for the test set
auto_arima_fcst <- forecast(auto_arima, h = h)

checkresiduals(auto_arima_fcst)

forecast::accuracy(auto_arima_fcst, bankrupt_test)

tsdisplay(auto_arima_fcst$residuals)

coefficients(auto_arima)


# Try other model specifications

tsdisplay(bankrupt_train)
nsdiffs(bankrupt_train)           # 1 seasonal difference
ndiffs(diff(bankrupt_train,12))   # 0 non- seasonal difference

tsdisplay(diff(bankrupt_train,12), lag.max = 48)

arima1 <- arima(bankrupt_train, order = c(3,0,3), seasonal=c(2,1,1))
arima2 <- arima(bankrupt_train, order = c(3,0,3), seasonal=c(1,1,1))


tsdisplay(arima_fcst$residuals)
arima1_fcst <- forecast(arima1, h = h)
arima2_fcst <- forecast(arima2, h = h)


tsdisplay(arima1_fcst$residuals)
tsdisplay(arima2_fcst$residuals)


checkresiduals(arima1_fcst)

checkresiduals(arima2_fcst)

accuracy(arima1_fcst, bankrupt_test)

accuracy(arima2_fcst, bankrupt_test)



plot(arima1_fcst, main = "Forecasts using Arima")
lines(bankrypt_bc, col = "black")
lines(arima1_fcst$mean, col= 'blue')
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1))

plot(arima2_fcst, main = "Forecasts using Arima")
lines(bankrypt_bc, col = "black")
lines(arima2_fcst$mean, col= 'blue')
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = c(1, 1))


rsv_m1<-snaive_fcst 
rsv_m2<-naive_fcst
rsv_m3<-rwdrift_fcst
rsv_m4<-ets_fcst
rsv_m5<-arima_fcst
rsv_m6<-ets1_fcst
rsv_m7<-ets2_fcst
rsv_m8<-ets3_fcst
rsv_m9<-ets4_fcst
rsv_m10<-ets5_fcst
rsv_m11<-ets6_fcst
rsv_m12<-auto_model_fcst
rsv_m13<-auto_arima_fcst
rsv_m14<-arima1_fcst
rsv_m15<-arima2_fcst


#list of models
models <- c("Seasonal Naive method", 
            "STL naive", "STL rwdrift", "STL ets", "STL arima",
            "AAA", "AAA with damped", "MAM", "MAM with damped",
            "MMM", "MMM with damped", "Auto ETS", "Auto Arima",
            "Arima_303_211", "Arima_303_111")
nmodels = length(models)

#prepare the accuracy tables
a_train <- matrix(nrow = 0, ncol = 4)
a_test  <- matrix(nrow = 0, ncol = 4)

#prepare the residual diagnostic table
res_matrix <- matrix(nrow = nmodels, ncol = 3)
rownames(res_matrix) <- paste0(seq(1:nmodels), " ", models)
colnames(res_matrix) <- c("Q*", "df", "p-value")

#for loop to collect measures
for(i in 1:nmodels) 
{
  #accuracy measures
  assign(paste0("a_m", i), forecast::accuracy(get(paste0("rsv_m", i)), bankrupt_test)[,c(2,3,5,6)])
  a_train <- rbind(a_train, get(paste0("a_m", i))[1,])
  a_test <- rbind(a_test, get(paste0("a_m", i))[2,])
  
  #residual diagnostics  
  assign(paste0("res_m", i), checkresiduals(get(paste0("rsv_m", i)), plot = FALSE))
  res_matrix[i,] <- c(get(paste0("res_m", i))$statistic, 
                      get(paste0("res_m", i))$parameter, 
                      get(paste0("res_m", i))$p.value)
}

rownames(a_train) <- paste0(seq(1:nmodels), " ", models)
rownames(a_test) <- paste0(seq(1:nmodels), " ", models)

round(a_train, digits = 4)
round(a_test, digits = 4)
round(res_matrix, digits = 4)


# Final model (replace with your selected model)
model <- arima2

# Generate forecasts
fc <- forecast(model, h = 81)

# Plot forecasts from April 2020 to Dec 2023
plot(fc, xlim = c(2020, 2023.75), main = "Out-Of-Sample Forecasts")

# Training data
Bank_train_2 <- window(bankrupt, end=c(2020,3))

# Test data
Bank_test_2 <- window(bankrupt, start=c(2020,4), end=c(2023,2))
h_2 = length(Bank_test_2)

# Fit model
fit <- snaive(Bank_train_2, h = h_2)

# Make predictions for test data
pred <- forecast(fit)

# Plot predictions
plot(pred, main = "Final Forecasts of Monthly Bankruptcies in Belgium: 2020-2023", 
     xlab = "Year", ylab = "Number of Bankruptcies")

# Add actual values
lines(Bank_test_2, col = "red")
legend("topleft", legend = c("Actual", "Forecast"), 
       col = c("red", "blue"), lty = 1)




# Create seasonally adjusted time series
seasadj_br <- seasadj(mstl(bankrupt))

# Training data
train <- window(seasadj_br, end = c(2017, 3))

# Test data
test <- window(seasadj_br, start = c(2017, 4), end = c(2020, 3))

# ETS model
fit_ets <- ets(train)

# ARIMA model
fit_arima <- auto.arima(train)

# Generate forecasts
pred_ets <- forecast(fit_ets, h = length(test))
pred_arima <- forecast(fit_arima, h = length(test))

checkresiduals(pred_ets)
checkresiduals(pred_arima)

# Check the accuracy of the models
forecast::accuracy(pred_ets, test)
forecast::accuracy(pred_arima, test)

# Plot forecasts and actual values
plot(pred_ets, main = "Forecasts of Seasonally Adjusted Bankruptcies: ETS vs. ARIMA", 
     xlab = "Year", ylab = "Number of bankruptcies", test)
lines(pred_arima$mean, col = "blue", lty = 1)
lines(test, col = "red")
legend("topleft", legend = c("ETS", "ARIMA", "Actual"), 
       col = c("red", "blue", "black"), lty = 1)






