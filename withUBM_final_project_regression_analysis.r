data <- read.csv("D:/Documents/DLSU/3rd year/Term 3/STT141A/FinalProject/dropped_rdata_laptop_prices.csv")

# Correlation

# No ubm_effective_speed (kase may null values dito na kelangan natin i-drop)
a1 <- cor(data$price_euros, data$inches)
# a2 and a3 from resolution e.g., 1920x1080
a2 <- cor(data$price_euros, data$width)
a3 <- cor(data$price_euros, data$height)

a4 <- cor(data$price_euros, data$clock_speed)
a5 <- cor(data$price_euros, data$ram)
a6 <- cor(data$price_euros, data$disk_size)
a7 <- cor(data$price_euros, data$ubm_effective_speed)
a8 <- cor(data$price_euros, data$weight)

correlation_df <- data.frame(a1=a1, a2=a2, a3=a3, a4=a4, a5=a5, a6=a6, a7 = a7, a8=a8)
rownames(correlation_df) <- c("price_euros")
colnames(correlation_df) <- c("inches", "width", "height", "clock_speed", "ram", "disk_size", "ubm_effective_speed", "weight")

print(correlation_df)

# Fullmodel
fullmodel <- lm(price_euros ~ inches +width +height + clock_speed + ram + disk_size + ubm_effective_speed + weight, data = data)
summary(fullmodel)

# We try to make the model better with the use of stepwise
#intercept only
int_only <- lm(price_euros ~ 1, data = data)

#full model
modelforward = step(int_only, direction = "forward", scope = formula(fullmodel), trace = 0)
summary(modelforward)
modelbackward = step(fullmodel, direction = "backward", scope = formula(fullmodel), trace = 0)
summary(modelbackward)
modelboth = step(fullmodel, direction = "both", scope = formula(fullmodel), trace = 0)
summary(modelboth)

# These three methods arrive at the same result of having the same predictors

#VIF checking
library(car)
vif(modelforward)

# Assumption checks

mainmodel <- modelforward
summary(mainmodel)
par(mfrow = c(2,2))
plot(mainmodel)

res <- mainmodel[["residuals"]]

# Normality -----------------------------------------------------------------------
#Ho: residuals follow a normal distribution
#Ha: residuals do not follow a normal distribution

hist(res) #Not normally distributed
par(mfrow = c(2,2))
plot(mainmodel) #Check Q-Q Residuals

shapiro.test(res) #Not normally because reject Ho p-value < 0.05
ks.test(res, "pnorm", mean = mean(res), sd = sd(res)) #Not normally because reject Ho p-value < 0.05
# ---------------------------------------------------------------------------
#Homoscedasticity checker
#Ho: residuals are homoscedastic
#Ha: residuals are not homoscedastic
grp <- c(rep(1,619),rep(2,619))
plot(mainmodel)
bartlett.test(res, grp) #Not homoscedastic

#Independence checker
#Ho: residuals are independent
#Ha: residuals are not independent
install.packages("car")
library(car)
dwt(res)
durbinWatsonTest(mainmodel) # p-value > 0.05 (Fail to reject Ho) so independent sya

#Linearity checker
par(mfrow = c(2,2))
plot(mainmodel) #Not linear based on Residuals vs Fitted

# ---------------------------------------------------------------------------
#Transformation

# ln
data$lnprice_euros <- log(data$price_euros)
lnmodel <- lm(lnprice_euros ~ ram + height + ubm_effective_speed + disk_size + clock_speed + inches, data = data)
par(mfrow = c(2,2))
plot(lnmodel)

# Assumptions checking again
resln <- lnmodel[["residuals"]]

# Normality -----------------------------------------------------------------------
#Ho: residuals follow a normal distribution
#Ha: residuals do not follow a normal distribution
# Yes
hist(resln) #Not normally distributed
par(mfrow = c(2,2))
plot(lnmodel) #Check Q-Q Residuals

shapiro.test(resln) #Not normally because reject Ho p-value < 0.05
ks.test(resln, "pnorm", mean = mean(resln), sd = sd(resln)) #Normally because fail to reject Ho p-value > 0.05
# ---------------------------------------------------------------------------
#Homoscedasticity checker
#Ho: residuals are homoscedastic
#Ha: residuals are not homoscedastic
grp <- c(rep(1,619),rep(2,619))
grp1 <- rep(c(1,2),619)
plot(lnmodel)
bartlett.test(resln, grp) #Not homoscedastic
bartlett.test(resln, grp1) #Not homoscedastic

#Independence checker
#Ho: residuals are independent
#Ha: residuals are not independent
install.packages("car")
library(car)
dwt(resln)
durbinWatsonTest(lnmodel) # p-value > 0.05 (Fail to reject Ho) so independent sya

#Linearity checker
par(mfrow = c(2,2))
plot(lnmodel) #Not linear based on Residuals vs Fitted

# ---------------------------------------------------------------------------
#Transformations
# Exponential
data$expprice_euros <- exp(data$price_euros)
expmodel <- lm(expprice_euros ~ ram + height + ubm_effective_speed + disk_size + clock_speed + inches, data = data)
# The data points transformed exponentially produced exceed the limit in R that is why some are labeled Inf

# Square root
data$sqrtprice_euros <- sqrt(data$price_euros)
sqrtmodel <- lm(sqrtprice_euros ~ ram + height + ubm_effective_speed + disk_size + clock_speed + inches, data = data)
par(mfrow = c(2,2))
plot(sqrtmodel)
# Assumptions checking again
ressqrt <- sqrtmodel[["residuals"]]

# Normality -----------------------------------------------------------------------
#Ho: residuals follow a normal distribution
#Ha: residuals do not follow a normal distribution
# Yes
hist(ressqrt) #Not normally distributed
par(mfrow = c(2,2))
plot(lnmodel) #Check Q-Q Residuals

shapiro.test(ressqrt) #Not normally because reject Ho p-value < 0.05
ks.test(ressqrt, "pnorm", mean = mean(ressqrt), sd = sd(ressqrt)) #Normally because fail to reject Ho p-value > 0.05
# ---------------------------------------------------------------------------
#Homoscedasticity checker
#Ho: residuals are homoscedastic
#Ha: residuals are not homoscedastic
grp <- c(rep(1,619),rep(2,619))
grp1 <- rep(c(1,2),619)
plot(lnmodel)
bartlett.test(ressqrt, grp) #Not homoscedastic
bartlett.test(ressqrt, grp1) #Not homoscedastic

#Independence checker
#Ho: residuals are independent
#Ha: residuals are not independent
install.packages("car")
library(car)
dwt(ressqrt)
durbinWatsonTest(sqrtmodel) # p-value > 0.05 (Fail to reject Ho) so independent sya

#Linearity checker
par(mfrow = c(2,2))
plot(lnmodel) #Not linear based on Residuals vs Fitted

# ---------------------------------------------------------------------------
# Square
data$sqrprice_euros <- data$price_euros^2
sqrmodel <- lm(sqrprice_euros ~ ram + height + ubm_effective_speed + disk_size + clock_speed + inches, data = data)
par(mfrow = c(2,2))
plot(sqrmodel)
# Assumptions checking again
ressqr <- sqrtmodel[["residuals"]]

# Normality -----------------------------------------------------------------------
#Ho: residuals follow a normal distribution
#Ha: residuals do not follow a normal distribution
# Yes
hist(ressqr) #Not normally distributed
par(mfrow = c(2,2))
plot(sqrmodel) #Check Q-Q Residuals

shapiro.test(ressqrt) #Not normally because reject Ho p-value < 0.05
ks.test(ressqrt, "pnorm", mean = mean(ressqrt), sd = sd(ressqrt)) #Normally because fail to reject Ho p-value > 0.05
# ---------------------------------------------------------------------------
#Homoscedasticity checker
#Ho: residuals are homoscedastic
#Ha: residuals are not homoscedastic
grp <- c(rep(1,619),rep(2,619))
grp1 <- rep(c(1,2),619)
plot(sqrmodel)
bartlett.test(ressqr, grp) #Not homoscedastic
bartlett.test(ressqr, grp1) #Not homoscedastic

#Independence checker
#Ho: residuals are independent
#Ha: residuals are not independent
install.packages("car")
library(car)
dwt(ressqr)
durbinWatsonTest(sqrmodel) # p-value > 0.05 (Fail to reject Ho) so independent sya

#Linearity checker
par(mfrow = c(2,2))
plot(lnmodel) #Not linear based on Residuals vs Fitted

# -----------------------------------------------------------------------------
# Cooks distance
cooks <- cooks.distance(lnmodel)
t <- 4/length(data$company)
print(t)
data$cooks <- cooks
par(mfrow = c(2,2))

outlier <- ifelse(cooks >= t, 1, 0)
data$outlier = outlier
data_filtered <- data[data$outlier == 0,]
plot(lnmodel)
summary(data)

# Redoing the linear regression of the original model
mainmodel_wo <- lm(data_filtered$price_euros ~ data_filtered$ram + data_filtered$height + 
                     data_filtered$ubm_effective_speed + data_filtered$disk_size + 
                     data_filtered$clock_speed + data_filtered$inches)
# Assumptions checking again
res <- mainmodel_wo[["residuals"]]

# Normality -----------------------------------------------------------------------
#Ho: residuals follow a normal distribution
#Ha: residuals do not follow a normal distribution
# Yes
hist(res) #Not normally distributed
par(mfrow = c(2,2))
plot(mainmodel_wo) #Check Q-Q Residuals

shapiro.test(resln) #Not normally because reject Ho p-value < 0.05
ks.test(resln, "pnorm", mean = mean(resln), sd = sd(resln)) # Normally because fail to reject Ho p-value > 0.05
# ---------------------------------------------------------------------------
#Homoscedasticity checker
#Ho: residuals are homoscedastic
#Ha: residuals are not homoscedastic
grp <- c(rep(1,580),rep(2,580))
grp1 <- rep(c(1,2),580)
plot(lnmodel)
bartlett.test(resln, grp) #homoscedastic
bartlett.test(resln, grp1) #homoscedastic

#Independence checker
#Ho: residuals are independent
#Ha: residuals are not independent
install.packages("car")
library(car)
dwt(resln)
durbinWatsonTest(lnmodel) 

#Linearity checker
par(mfrow = c(2,2))
plot(lnmodel) #Not linear based on Residuals vs Fitted

# Redoing the linear regression ln

# ln
data_filtered$lnprice_euros <- log(data_filtered$price_euros)
lnmodel <- lm(data_filtered$lnprice_euros ~ data_filtered$ram + data_filtered$height + 
                data_filtered$ubm_effective_speed + data_filtered$disk_size + 
                data_filtered$clock_speed + data_filtered$inches)
par(mfrow = c(2,2))
plot(lnmodel)

# Assumptions checking again
resln <- lnmodel[["residuals"]]

# Normality -----------------------------------------------------------------------
#Ho: residuals follow a normal distribution
#Ha: residuals do not follow a normal distribution
# Yes
hist(resln) #Not normally distributed
par(mfrow = c(2,2))
plot(lnmodel) #Check Q-Q Residuals

shapiro.test(resln) #Not normally because reject Ho p-value < 0.05
ks.test(resln, "pnorm", mean = mean(resln), sd = sd(resln)) # Normally because fail to reject Ho p-value > 0.05
# ---------------------------------------------------------------------------
#Homoscedasticity checker
#Ho: residuals are homoscedastic
#Ha: residuals are not homoscedastic
grp <- c(rep(1,580),rep(2,580))
grp1 <- rep(c(1,2),580)
plot(lnmodel)
bartlett.test(resln, grp) #homoscedastic
bartlett.test(resln, grp1) #homoscedastic

#Independence checker
#Ho: residuals are independent
#Ha: residuals are not independent
install.packages("car")
library(car)
dwt(resln)
durbinWatsonTest(lnmodel) 

#Linearity checker
par(mfrow = c(2,2))
plot(lnmodel) #Not linear based on Residuals vs Fitted

# Summary
summary(lnmodel)

# Try adding dummy variables
# typenames
dummyvariable <- ifelse(data_filtered$typename == "Gaming", 5, 
                  ifelse(data_filtered$typename == "Ultrabook", 4, 
                  ifelse(data_filtered$typename == "Notebook", 2,
                  ifelse(data_filtered$typename == "2 in 1 Convertible", 3, 
                  ifelse(data_filtered$typename == "Workstation", 6, 1) ))))
data_filtered$typenamedummy <- dummyvariable

modelwithdummy <- lm(data_filtered$lnprice_euros ~ data_filtered$ram + data_filtered$height + 
                  data_filtered$ubm_effective_speed + data_filtered$disk_size + 
                  data_filtered$clock_speed + data_filtered$inches + data_filtered$typenamedummy)

summary(modelwithdummy)
summary(lnmodel)


#VIF checking
library(car)
vif(modelwithdummy)
vif(lnmodel)



