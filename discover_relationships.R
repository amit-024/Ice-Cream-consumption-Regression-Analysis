
#==============================================================
# Step 1: Load required libraries
#==============================================================
library(ggplot2) # data visualization
library(GGally) # pairwise plots
library(lmtest) # heteroscedasticity tests
library(car) # Durbin-Watson & VIF
library(olsrr) # model selection & diagnostics
library(dplyr) # data handling
library(MASS)

#==============================================================
# Step 2: Import and prepare data
#==============================================================

x <- read.table("icecream.txt", header = TRUE, skip = 14)
write.csv(x, file = "icecream_dataset.csv")  
ice1 <- read.csv("icecream_dataset.csv")[-1]
head(ice1)
ice1 = ice1[c(1,5,3,4,2)]


#==============================================================
# Step 3: discover relationships
#==============================================================
dev.new()
ggpairs(ice1, switch = "both")
## period seems to have same relationship with both Temp and Consumption

ice1$Consumption_z <- scale(ice1$Consumption)
ice1$Temp_z <- scale(ice1$Temp)

## plot period on x axis and standardized Consumption and Temp on y-axis
ggplot(ice1, aes(x = Period)) +
  geom_line(aes(y = Consumption_z, color = "Consumption")) +
  geom_line(aes(y = Temp_z, color = "Temperature")) +
  labs(title = "Standardized Consumption & Temperature vs Period",
       x = "Period", y = "Standardized Value") +
  theme_minimal()
## Temperature and Consumption go hand in hand.

# Plot Temp vs Consumption
ggplot(ice1, aes(x = Temp, y = Consumption)) +
  geom_point(color = 'blue') +
  geom_line(color = 'darkgray') +
  labs(title = "Period vs Temperature (Fitted Seasonal Line)",
       x = "period", y = "Temperature")
## exponential relationship spotted. need to apply log transform?

dev.new()
# Plot Temp vs poweronsumption
ggplot(ice1, aes(x = Income, y = powerconsumption)) +
  geom_point(color = 'blue') +
  geom_line(color = 'darkgray') +
  labs(title =,
       x = "Income", y = "powerconsumption")
## looks linear!!

#==============================================================
# Step 4: Fix functional form of Temp
#==============================================================

fit1 = lm(Consumption~Temp, data=ice1)
summary(fit1)
## R2 is 0.6016, both intercept and slope ***, sigma^=0.04226

# Plot residuals vs fitted
ggplot(ice1, aes(x = predict(fit1), y = residuals(fit1))) +
  geom_point(color = 'blue') +
  #geom_line(color = 'darkgray') +
  #geom_line(aes(y = fit_values), color = "red", size = 1) +
  labs(title = "residuals vs predicted for fit1",
       x = "residuals", y = "predicted")


# Plot temperature vs consumption with fitted line
ggplot(ice1, aes(x = Temp, y = Consumption)) +
  geom_point(color = 'blue') +
  geom_line(color = 'darkgray') +
  geom_line(aes(y = predict(fit1)), color = "red", size = 1) +
  labs(title = "Consumption vs Temperature",
       x = "Temperature", y = "Consumption")

# Plot temperature vs log of consumption
ggplot(ice1, aes(x = Temp, y = log(Consumption))) +
  geom_point(color = 'blue') +
  geom_line(color = 'darkgray') +
  labs(title = "log of Consumption vs Temperature",
       x = "Temperature", y = "log(Consumption)")
## much better. linear relationship spotted. fit a line now

## box cox analysis
bc = boxcox(fit1, lambda = seq(-2, 2, 0.1))  
# Best lambda
best_lambda <- bc$x[which.max(bc$y)]
best_lambda


fit2 = lm(I(Consumption^(-0.7))~Temp, data=ice1)
summary(fit2)
## R2 is 0.6294, adjR2 is 0.6162, both intercept and slope ***, 
## sigma^=0.1508

AICc(fit2)
#######################################

# (a) Normality of residuals
shapiro.test(residuals(fit2))
# Result: W = 0.9339, p = 0.0623 → residuals approximately normal.

ggplot(ice, aes(sample = Consumption)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot for Residuals")
# Q-Q plot: Points near line → supports normality assumption.

# (b) Homoscedasticity (constant variance)
gqtest(fit2)   # Goldfeld-Quandt
bptest(fit2)   # Breusch-Pagan
# Result: p = 0.5939 (GQ), p = 0.4012 (BP) → constant variance holds.

# Residuals vs Fitted plot
ggplot(fit2, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "blue", linetype = "dashed") +
  labs(title = "Residuals vs Fitted Values", x = "Fitted", y = "Residuals")
# Result: Random scatter around zero → homoscedasticity confirmed.

# (c) Independence of errors
durbinWatsonTest(fit2)
# Result: DW = 1.88, p = 0.148 → no autocorrelation detected.

# (d) Multicollinearity
Initial_fit <- lm(Consumption ~ Price + Income + Temp, data = ice)
vif(fit2)
# Result: All VIF < 2 → no multicollinearity problem.

############################################33


fit3 = lm(I(1/Consumption)~Temp, data=ice1)
summary(fit3)
## R2 is 0.629, adjR2 is 0.6157, both intercept and slope ***, 
## sigma^=0.3094

fit4 = lm(log(Consumption)~Temp, data=ice1)
summary(fit4)
## R2 is 0.6245, adjR2 is 0.6111, both intercept and slope ***, 
## sigma^=0.1108

# Plot residuals vs fitted
ggplot(ice1, aes(x = predict(fit2), y = residuals(fit2))) +
  geom_point(color = 'blue') +
  #geom_line(color = 'darkgray') +
  #geom_line(aes(y = fit_values), color = "red", size = 1) +
  labs(title = "residuals vs predicted for fit1",
       x = "residuals", y = "predicted")


# Plot temperature vs log of consumption
ggplot(ice1, aes(x = Temp, y = Consumption)) +
  geom_point(color = 'blue') +
  geom_line(color = 'darkgray') +
  geom_line(aes(y = exp(predict(fit2))), color = "red", size = 1) +
  labs(title = "log of Consumption vs Temperature",
       x = "Temperature", y = "log(Consumption)")
## much better. linear relationship spotted. fit a line now

# Predictions back on original scale
ice1$pred1 <- predict(fit1)   # linear model
ice1$pred2 <- exp(predict(fit2))  # back-transformed

# RMSE comparison
rmse1 <- sqrt(mean((ice1$Consumption - ice1$pred1)^2))
rmse2 <- sqrt(mean((ice1$Consumption - ice1$pred2)^2))

rmse1; AIC(fit1) ; rmse2 ; AIC(fit2)
## rmse1 = 0.0408, aic1=-100.766, rmse2 = 0.0403, aic2=-42.91

## AIC not comparable. response is on different scales. need to add adjustment

#==============================================================
# Step 5: Fix functional form of Period
#==============================================================



# Consumption vs Period
ggplot(ice1, aes(x = Temp, y = Consumption)) +
  geom_point(color = 'blue') +
  # geom_line(color = 'darkgray') +
  labs(title = "Consumption vs Period", x = "Period", y = "Consumption")
# Result: Consumption follows a clear seasonal pattern and mild upward trend.
# Suggests Temperature, Season, and Income affect demand.


season <- c("spring", "summer", "fall", "winter")
season <- rep(rep(season, each = 3), 3)#[1:30]

year <- c(rep(1951, 10), rep(1952, 13), rep(1953, 7))

ice1$Year <- factor(year)
ice1$Season <- factor(season)
#ice <- ice[-1]  # remove Period column

fit = lm(Consumption~Period, data=ice1)
summary(fit)

# Create sine wave data
sine_df <- data.frame(
  Period = ice1$Period,
  SinWave = mean(ice1$Consumption) +
    (max(ice1$Consumption) - min(ice1$Consumption)) / 2 *
    sin(2 * pi * ice1$Period / 12) # 12 = seasonal cycle
)

ggplot(ice1, aes(x = Period, y = Temp)) +
  geom_point(color = 'blue') +
  geom_line(color = 'darkgray') +
  geom_line(data = sine_df, aes(x = Period, y = SinWave), 
            color = "red", size = 1) +
  labs(
    title = "Consumption vs Period (with Seasonal Sine Curve)",
    x = "Period", 
    y = "Consumption"
  )

ice1$sin_term <- sin(2 * pi * ice1$Period / 12)  # seasonal component
ice1$cos_term <- cos(2 * pi * ice1$Period / 12)

# Fit regression: Consumption ~ seasonality
fit = lm(Consumption ~ Period + Income + cos_term+ sin_term, data = ice1)

# Plot with fitted temperature line
ggplot(ice1, aes(x = Temp, y = Consumption)) +
  geom_point(color = 'blue') +
  geom_line(color = 'darkgray') +
  geom_line(aes(y = predict(fit)), color = "red", size = 1) +
  labs(title = "Consumption vs Temperature (Fitted Seasonal Line)",
       x = "Temperature", y = "Consumption")

# Plot with residuals against period
ggplot(ice1, aes(x = Period, y = residuals(fit))) +
  geom_point(color = 'blue') +
  #geom_line(color = 'darkgray') +
  geom_line(aes(y = predict(fit2)), color = "red", size = 1) +
  labs(title = "residuals vs Period (Fitted Seasonal Line)",
       x = "Period", y = "residuals from temp fit")

fit <- lm(Consumption ~ sin_term + cos_term , data = ice1)

# Add fitted values to dataset
ice1$fit_values <- predict(fit)

# Plot with fitted seasonal line
ggplot(ice1, aes(x = Period, y = residuals(fit))) +
  geom_point(color = 'blue') +
  geom_line(color = 'darkgray') +
  geom_line(aes(y = fit_values), color = "red", size = 1) +
  labs(title = "Consumption vs Period (Fitted Seasonal Line)",
       x = "Period", y = "Consumption")

summary(fit)
