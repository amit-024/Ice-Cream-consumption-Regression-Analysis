
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
library(lattice)
library(AICcmodavg)
library(plotly)
library(caret)
library(glmnet)

#==============================================================
# Step 2: Import and prepare data
#==============================================================

x <- read.table("icecream.txt", header = TRUE, skip = 14)
write.csv(x, file = "icecream_dataset.csv")  
ice <- read.csv("icecream_dataset.csv")[-1]
head(ice)
ice = ice[c(1,5,3,4,2)]
ice$powerconsumption = (ice$Consumption)^(-.7)

#==============================================================
# Step 3: Fix functional form of Temp
#==============================================================

fit1 = lm(Consumption~Temp, data=ice)
summary(fit1)
## adj R2 is 0.5874, sigma^=0.04226

xyplot(residuals(fit1) ~ Temp, data =ice, smooth=TRUE, col.line='black')
## mostly linear trend

## box cox analysis
bc = boxcox(fit1, lambda = seq(-2, 2, 0.1))  
# Best lambda
best_lambda <- bc$x[which.max(bc$y)]
best_lambda
## best_lambda = -0.7

fit2 = lm(I(Consumption^(-0.7))~Temp, data=ice)
summary(fit2)
AICc(fit2)  
##  adjR2 is 0.6162, sigma^=0.1508, AICc= -20

# Plot residuals vs fitted
ggplot(ice, aes(x = fitted(fit2), y = residuals(fit2))) +
  geom_point(color = 'blue') +
  labs(x = "fitted", y = "residuals")
## no clear patterns identified.

#==============================================================
# Step 4: Fix functional form of Period
#==============================================================

# Plot period vs consumption
ggplot(ice, aes(x = Period, y = Consumption)) +
  geom_point(color = 'blue') +
  labs(x = "Period", y = "Consumption")
## shows a seasonal pattern

# seasonal component can be modelled from period as follows:
ice$sin_term <- sin(2 * pi * ice$Period / 12) 
ice$cos_term <- cos(2 * pi * ice$Period / 12)

fit3 = lm(I(Consumption^(-0.7))~Temp + cos_term+ sin_term , data=ice)
summary(fit3)
AICc(fit3)  
## adjR2 is 0.7705, sigma^=0.1223, AICc= -32

p=ggplot(ice, aes(x = Period)) +
  geom_point(aes(y = Consumption, color = "Actual")) +
  geom_line(aes(y = fitted(fit3)^(-1/0.7), color = "Fitted")) +
  labs(x = "Period", y = "Consumption", color = "Legend") +
  scale_color_manual(values = c("Actual" = "blue", "Fitted" = "red"))
ggsave("fit3.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)

# Plot residuals vs fitted
ggplot(ice, aes(x = fitted(fit3), y = residuals(fit3))) +
  geom_point(color = 'blue') +
  labs(x = "fitted", y = "residuals")
## no clear patterns identified.

#==============================================================
# Step 5: Fix functional form of Price
#==============================================================

xyplot(residuals(fit3) ~ Price, data =ice, smooth=TRUE, col.line='black')
## quadratic trend detected

fit4 = lm(I(Consumption^(-0.7))~Temp + cos_term+ sin_term +I(Price^2) + I(Price) , data=ice)
summary(fit4)
AICc(fit4)
## adjR2 is 0.8007, sigma^=0.114, AICc= -32.77


# Plot residuals vs fitted
ggplot(ice, aes(x = fitted(fit4), y = residuals(fit4))) +
  geom_point(color = 'blue') +
  labs(x = "fitted", y = "residuals")
## no clear patterns identified.

xyplot(residuals(fit4) ~ Price, data =ice, smooth=TRUE, col.line='black')
## quadratic trend somewhat gone.



#==============================================================
# Step 6: Fix functional form of Income
#==============================================================

xyplot(residuals(fit4) ~ Income, data =ice, smooth=TRUE, col.line='black')
## downward trend detected when income is high

ice$high_income <- ifelse(ice$Income > 83, 1, 0)
fit5 = lm(I(Consumption^(-0.7))~Temp + cos_term+ sin_term +I(Price^2) + I(Price) + Income*high_income , data=ice)
summary(fit5)
AICc(fit5)
## adjR2 is 0.8432, sigma^=0.1011, AICc= -31.47

# Plot residuals vs fitted
ggplot(ice, aes(x = fitted(fit4), y = residuals(fit4))) +
  geom_point(color = 'blue') +
  labs(x = "fitted", y = "residuals")
## heterosckedasticity identified.

xyplot(residuals(fit5) ~ Income, data =ice, smooth=TRUE, col.line='black')
## downward trend gone.


#==============================================================
# Step 6: Consider Transformation of variables we may have missed.
#         Also consider interaction terms and build a model 
#         with all possible predictors
#==============================================================

ice$powerconsumption = (ice$Consumption)^(-.7)

ice$sin_term <- sin(2 * pi * ice$Period / 12)  # seasonal component
ice$cos_term <- cos(2 * pi * ice$Period / 12)
season <- c("spring", "summer", "fall", "winter")
season <- rep(rep(season, each = 3), 3)[1:30]

year <- c(rep(1951, 10), rep(1952, 13), rep(1953, 7))

ice$Year <- factor(year)
ice$Season <- factor(season)

ice$high_income <- ifelse(ice$Income > 83, 1, 0)
ice$high_temp <- ifelse(ice$Temp > 50, 1, 0)
ice$high_period <- ifelse(ice$Period > 20, 1, 0)
ice$high_price <- ifelse(ice$Period > 0.28, 1, 0)


# Define the null and full model
null_model <- lm(Consumption ~ 1, data = ice)  # only intercept
full_model <- lm(Consumption ~ Temp*high_temp + Temp*high_income + Temp*high_price  
                 + I(Temp^2)*high_temp + I(Temp^2)*high_income + I(Temp^2)*high_price 
                 + I(Period^2) + Period + sin_term + cos_term + Year + Season
                 + Price*high_temp + Price*high_income + Price*high_price 
                 + Income*high_temp + Income*high_price + Income*high_income, 
                 data = ice)

## box cox analysis
bc = boxcox(full_model, lambda = seq(-2, 2, 0.1))  
# Best lambda
best_lambda <- bc$x[which.max(bc$y)]
best_lambda
## best_lambda = 0.78, so no need to do box cox transformation

# Forward stepwise selection using AIC
forward_aic <- stepAIC(null_model, 
                       scope = list(lower = formula(null_model),
                                    upper = formula(full_model)),
                       direction = "forward")

summary(forward_aic)
AICc(forward_aic)
## adjR2 is 0.9184, sigma^=0.0188, AICc= -128.81

car::vif(forward_aic) 
## vif is too high. number of predictors is too high. 
## number of data points is too low. need to remove remove some 
## predictors to ensure we are not overfitting. 
## will target variables that have high vif and pvalues.

##################################################################
##  Step7: Prune the model: removed Period and Year
##################################################################

## remove year -- highest GVI &  highest p value
reduced_model <- update(forward_aic, . ~ . - Year)
# Compare the two models
anova(forward_aic, reduced_model)   # Likelihood ratio test - high p value. accept null - reduced model true
AICc(reduced_model)     # AIC improved by -7 points to -135.25
summary(reduced_model)$adj.r.squared ## adj r^2 = 0.9174
forward_aic = reduced_model
summary(forward_aic)
car::vif(forward_aic) 


## remove period -- highest GVIF & highest p value
reduced_model <- update(forward_aic, . ~ . - Period)
anova(forward_aic, reduced_model)   # Likelihood ratio test - high p value. accept null - reduced model true
AICc( reduced_model)     # AIC improved by -3 points to -138.799
summary(reduced_model)$adj.r.squared  ## adj r^2 = 0.9193
forward_aic = reduced_model
summary(forward_aic)
car::vif(forward_aic)

## remove Income -- high GVIF and high pvalue
reduced_model <- update(forward_aic, . ~ . - Income)
anova(forward_aic, reduced_model)   # Likelihood ratio test - very small p value.  full model true
AICc( reduced_model)     # AIC detoriated by +10 points to -128.93
summary(reduced_model)$adj.r.squared  ## adj r^2 = 0.8783
car::vif(reduced_model)
# Plot residuals vs fitted
forward_aic = reduced_model
summary(forward_aic)
car::vif(forward_aic)



## remove cos_term - highest GVIF
reduced_model <- update(forward_aic, . ~ . - cos_term)
anova(forward_aic, reduced_model)   # Likelihood ratio test - moderate p value.  reduced model true
AICc( reduced_model)     # AIC detoriated by +7 points to -121.8541
summary(reduced_model)$adj.r.squared  ## adj r^2 = 0.834
car::vif(reduced_model)
forward_aic = reduced_model
summary(forward_aic)
car::vif(forward_aic)
AICc(forward_aic)


## Price is no longer significant. should we remove it?
reduced_model <- update(forward_aic, . ~ . - Price)
anova(forward_aic, reduced_model)   # Likelihood ratio test - high p value.  reduced model true
AICc( reduced_model)     # AIC improved by -2 points to -123.2951
summary(reduced_model)$adj.r.squared  ## adj r^2 = 0.8311
car::vif(reduced_model)
forward_aic = reduced_model
summary(forward_aic)
car::vif(forward_aic)
AICc(forward_aic)

ggplot(ice, aes(x = fitted(reduced_model), y = residuals(reduced_model))) +
  geom_point(color = 'blue') +
  labs(x = "fitted", y = "residuals") ## outliers in y identified.



############################################
## adhoc analysis:
###########################################


## box cox analysis
bc = boxcox(forward_aic, lambda = seq(-2, 2, 0.1))  
# Best lambda
best_lambda <- bc$x[which.max(bc$y)]
best_lambda
## best_lambda = -1, so apply box cox transformation

fit6 = lm(I(Consumption^(-1))~I(Temp^2) + sin_term + I(Period^2), data=ice)
summary(fit6)
car::vif(fit6)
AICc(fit6) + 2* sum(2 * log((ice$Consumption))) 
## AICc after scaling with jacobian= -137
## cannot just compute AICc, because we just applied box cox transformation
## hence the response variables are not on the same scale.
## since `boxcox(forward_aic, lambda = seq(-2, 2, 0.1))`  compares the 
## scaled likelihoods, and the models before and after transformation
## have the same variables, scaled likelihood of transformed model 
## being higher automatically translates to it having a lower AICc. 


###############################################################
## step8: verify linearity, homoscedasticity, autocorrelation, 
##        normality
#################################################################

## final model
fit = fit6
X =  as.data.frame(model.matrix(fit))

# (b) Homoscedasticity (constant variance)
gqtest(fit)   # Goldfeld-Quandt
bptest(fit)   # Breusch-Pagan
# Result: p = 0.5997 (GQ), p = 0.6434 (BP) - constant variance holds.

# (c) Independence of errors
durbinWatsonTest(fit)
# Result: DW = 1.678, p = 0.134 -  residuals are reasonably independent.

# (a) Normality of residuals
shapiro.test(residuals(fit2))
# Result: W = 0.97528, p = 0.6908 - residuals approximately normal.

p = ggplot(ice, aes(sample = Consumption)) +
  stat_qq(color = "blue") +
  stat_qq_line(color = "red") +
  labs(title = "Normal Q-Q Plot for Residuals")
# Q-Q plot: Points near line → supports normality assumption.
ggsave("qq_plot.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)

#########################################################
## step 9: outlier detection and influence diagnostics

##########################################################

## visualizing residuals
# Plot residuals vs fitted
p <- ggplot(ice, aes(x = fitted(fit), y = residuals(fit), text = rownames(ice))) +
  geom_point(color = "blue") +
  labs(x = "Fitted", y = "Residuals") +
  scale_y_continuous(limits = c(-1, 1)) ## observation 1 is an outlier

ggplotly(p, tooltip = "text") ## outlier in y spotted.

## visualizing standardized residuals
p <- ggplot(ice, aes(x = fitted(fit), y = rstandard(fit), text = rownames(ice))) +
  geom_point(color = "blue") +
  labs(x = "Fitted", y = "std. Residuals") +
  scale_y_continuous(limits = c(-1, 1))  ## observation 1 is an outlier

ggplotly(p, tooltip = "text") ## outlier in y spotted.

## visualizing studentized residuals
p <- ggplot(ice, aes(x = fitted(fit), y = rstudent(fit), text = rownames(ice))) +
  geom_point(color = "blue") +
  labs(x = "Fitted", y = "studentized Residuals") +
  scale_y_continuous(limits = c(-1, 1))  ## observation 1 is an outlier

ggplotly(p, tooltip = "text") ## outlier in y spotted.


# visualizing leverage. cutoff 2p/n = 3*4/30 approx 0.4 
p <- ggplot(ice, aes(x = 1:30, y = hatvalues(fit), text = rownames(ice))) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 1/2, color = "red", linetype = "dashed") +
  labs(x = "Index", y = "Leverage") ## no outlier

ggplotly(p, tooltip = "text") ## no outlier in y spotted.


## visualizing cooks distance . cutoff 2/n = 0.1333
p <- ggplot(ice, aes(x = 1:30, y = cooks.distance(fit), text = rownames(ice))) +
  geom_point(color = "blue") +
  geom_hline(yintercept = 0.134, color = "red", linetype = "dashed") +
  labs(x = "Index", y = "Cooks distance") ## 1 and 30 are outliers

ggplotly(p, tooltip = "text") ## outliers in y spotted.


## visualizing DFFITs
cutoff = 2* sqrt(4/30)
p <- ggplot(ice, aes(x = 1:30, y = dffits(fit), text = rownames(ice))) +
  geom_point(color = "blue") +
  geom_hline(yintercept =  c(-cutoff, cutoff), color = "red", linetype = "dashed") +
  labs(title = "DFFITS",
       x = "Index",
       y = "DFFITS") +
  scale_y_continuous(limits = c(-1, 1)) ## 1 and 30 are outliers

ggplotly(p, tooltip = "text")## outliers in y spotted.


## effect of removing outliers:

fit_no_outliers <- lm(I(Consumption^(-1)) ~ I(Temp^2) + sin_term + I(Period^2), 
                      data = ice[-c(1,30), ])
summary(fit_no_outliers)
AICc(fit_no_outliers) + 2* sum(2 * log((ice$Consumption)))

coef_compare <- cbind(
  original = coef(fit),
  no_outliers = coef(fit_no_outliers)
)
print(coef_compare)

###############################################
# step 10: Alternate methods
###############################################



# Define predictors and response
X <- model.matrix(Consumption ~ Temp + Price + Income + Period + sin_term + cos_term, data=ice)[,-1]
y <- ice$Consumption
y_bc <- ice$Consumption^(-1) 

# Ridge regression (alpha = 0)
ridge_cv <- cv.glmnet(X, y_bc, alpha = 0)  
plot(ridge_cv)

best_lambda <- ridge_cv$lambda.min
ridge_model <- glmnet(X, y_bc, alpha = 0, lambda = best_lambda)

coef(ridge_model)   # Ridge coefficients

# Get fitted values from the ridge model
ridge_pred <- predict(ridge_model, newx = X)

# Add predictions to the dataset
ice$ridge_fit <- as.numeric(ridge_pred)

# Plot observed vs fitted over Period
library(ggplot2)

ggplot(ice, aes(x = Period)) +
  geom_point(aes(y = Consumption^(-1), color = "Observed")) +
  geom_line(aes(y = ridge_fit, color = "Ridge Fitted")) +
  labs(x = "Period", y = "Consumption", color = "Legend") +
  scale_color_manual(values = c("Observed" = "blue", "Ridge Fitted" = "red")) +
  theme_minimal()
ggsave("ridge.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)


#===============================================
# Step 10: Fitted vs Actual Consumption
#===============================================
ice$fit <- fitted.values(fit)^(-1)  ## reverse the box cox transformation
p=ggplot(ice, aes(x = 1:30)) +
  geom_line(aes(y = Consumption), color = "red", linetype = "dashed") +
  geom_line(aes(y = fit), color = "blue") +
  labs(title = "Fitted vs Actual Consumption",
       x = "Observation Index", y = "Consumption")
# Fitted (blue) and actual (red) lines overlap closely, indicating strong predictive accuracy and good model fit.
ggsave("compare.png", p, bg = "white",width = 10, height = 10, units = "in", dpi = 300)


##################################################
# step 11: final model performance
####################################################

summary(fit)
AICc(fit6) + 2* sum(2 * log((ice$Consumption))) 
