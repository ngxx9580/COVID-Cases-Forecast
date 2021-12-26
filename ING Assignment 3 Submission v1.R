library(forecast)
library(fpp)
library(dplyr)


#Load Covid19 Data
Covid19cases.data <- read.csv(file.choose())

#Filter for Canadian cases
CAN.cases.data <- Covid19cases.data %>% filter(Covid19cases.data$iso_code == "CAN")

#Basic Plot for New CAN Cases up to Sept 11, 2021
plot.ts(CAN.cases.data$new_cases)
plot.ts(CAN.cases.data$stringency_index)

#Set different data frame for assignment
CANNewcase<- CAN.cases.data$new_cases

#-------------Question 1 - Auto.arima on original time series, no pre-processing
model.auto <- auto.arima( CANNewcase, stepwise=FALSE, seasonal= FALSE)
model.auto

checkresiduals(model.auto)

#------------Question 2 - Redo foresting by first preprocessing, then forecasting. Compare results to Step 
CANNewcase<-tail(CANNewcase,-100)
CANNewcase <- ts(CANNewcase, frequency=365, start=c(2020, 122))

plot.ts(CANNewcase, xlab="Date", ylab="CAN New cases")

#Stabilize variance
CANNewcase.lambda <- BoxCox.lambda(CANNewcase)
CANNewcase.BoxCox<-BoxCox(CANNewcase, CANNewcase.lambda)

# Check the transformed data and compare it with log transform
par(mfrow=c(1,2))
plot.ts(CANNewcase.BoxCox, xlab="Date", ylab="CAN New cases")
plot.ts(log(CANNewcase), xlab="Date", ylab="CAN New cases")

logCANNewcase<-log(CANNewcase) # We use log transform for simplicity 
plot.ts(logCANNewcase, xlab="Date", ylab="log CAN New cases")

#Remove Seasonality through Seasonal Differencing
Acf(diff(logCANNewcase,1),lag.max =25)

# We now remove the seasonality using seasonal differencing
logCANNewcase.deSeasonality <- diff(logCANNewcase,7) # period is 7 because of the weekly pattern 
plot.ts(logCANNewcase.deSeasonality, xlab="Date", ylab="log CAN New Case after removing trend and seasonality")
Acf(logCANNewcase.deSeasonality,lag.max =25) 

#Check Stationarity
# Perform the augmented Dickey-Fuller (ADF) test to check stationarity. The null hypothesis assumes that the series is non-stationary.
adf.test(CANNewcase,alternative = "stationary") #Large p value means fails to reject the null hypothesis.  Original data is not stationary
adf.test(logCANNewcase.deSeasonality,alternative = "stationary")

#Auto Arima modelling after Preprocessing
model.auto <- auto.arima( logCANNewcase.deSeasonality, stepwise=FALSE, seasonal= FALSE) #Fit using the Hyndman-Khandakar algorithm (Hyndman & Khandakar, 2008)
model.auto
#Suggest Arima (0,0,5) with zero mean

checkresiduals(model.auto) #Check Quality of the fit. Residuals should: 
# (1) not have any significant autocorrelation
# (2) follow normal distribution
# (3) have stable variance over time

# We can use the auto selected model to make forecasting 
fit.yourself <- Arima(logCANNewcase, order=c(0,0,5), seasonal=list(order=c(0,1,0),period=7)) 
# The seasonal differencing with period=7 is equivalent to "seasonal=list(order=c(0,1,0),period=7)" 
#Basically putting everything we've done before in 1 line
fit.yourself
autoplot( forecast(fit.yourself,20) )

# Plot the forecasting in the original scale
fc<-forecast(fit.yourself,20)

fc$x <- exp(fc$x)
fc$mean <- exp(fc$mean)
fc$lower <- exp(fc$lower)
fc$upper <- exp(fc$upper)
autoplot(fc)
#AIC=374.35   AICc=374.53   BIC=399.49

#--------------Question 3 - Improve model by exploring alternative orders & describe how you came to final model
#---------------------------Comment on the quality of your forecasting.

#Alternative 1 - Increase p value
#(1,0,4) = AIC=414.84   AICc=415.01   BIC=439.98
#(0,0,4) = AIC=431.44   AICc=431.56   BIC=452.39
# Increasing and lowering q by 1-> higher AIC
# Increasing p up to 25 -> higher AIC
fit.alternative1 <- Arima(logCANNewcase, order=c(25,0,5), seasonal=list(order=c(0,1,0),period=7)) 
fit.alternative1
checkresiduals(fit.alternative1)
fc1<-forecast(fit.alternative1,20)

fc1$x <- exp(fc1$x)
fc1$mean <- exp(fc1$mean)
fc1$lower <- exp(fc1$lower)
fc1$upper <- exp(fc1$upper)
autoplot(fc1)
#Best Model AIC=238.85   AICc=243.2   BIC=368.75

#Alternative 2 - increase/decrease q value
fit.alternative2 <- Arima(logCANNewcase, order=c(0,0,4), seasonal=list(order=c(0,1,0),period=7)) 
fit.alternative2
#AIC=431.44   AICc=431.56   BIC=452.39

fit.alternative2.1 <- Arima(logCANNewcase, order=c(0,0,6), seasonal=list(order=c(0,1,0),period=7)) 
fit.alternative2.1
#AIC=376.28   AICc=376.51   BIC=405.61

#Alternative 3 - increase d value
fit.alternative3 <- Arima(logCANNewcase, order=c(0,1,5), seasonal=list(order=c(0,1,0),period=7)) 
fit.alternative3
#AIC=405.8   AICc=405.98   BIC=430.93

#--------------Question 4 - Find another time series that can possibly serves as a predictor (X) and incorporate it into
#your time series model through dynamic regression. Consider at least two different future paths of X
#between September 14 and 28, and explore how they could affect the future evolution of the
#number of new cases (Y) in the same forecasting window.
plot.ts(CAN.cases.data$icu_patients)

#Set different data frame
CANicu_patients<- CAN.cases.data$icu_patients

# Lagged predictors. Test 0, 1, 2 or 3 lags.
icu <- cbind(CANicu_patients[,3],
                c(NA,CANicu_patients[1:495,3]),
                c(NA,NA,CANicu_patients[1:495,3]),
                c(NA,NA,NA,CANicu_patients[1:495,3]))
colnames(icu) <- paste("ICULag",0:3,sep="")
icu

# Choose optimal lag length for advertising based on AIC
# Restrict data so models use same fitting period
fit1 <- auto.arima(CANNewcase[1:495,1], xreg=CANicu_patients[1:495,1:1], d=0)
fit2 <- auto.arima(CANNewcase[1:495,1], xreg=CANicu_patients[1:495,1:2], d=0)
fit3 <- auto.arima(CANNewcase[1:495,1], xreg=CANicu_patients[1:495,1:3], d=0)
fit4 <- auto.arima(CANNewcase[1:495,1], xreg=CANicu_patients[1:495,1:4], d=0)

# Compute Akaike Information Criteria (Lower AIC is better)
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)


# Compute Bayesian Information Criteria (Lower BIC is better)  The same fit should be the best for both criteria. 
BIC(fit1)
BIC(fit2)
BIC(fit3)
BIC(fit4)

#Depending on which version how the lowest AIC, BIC, that version would be the ideal model.  