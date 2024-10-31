airquality
plot(airquality$Temp, airquality$Ozone)
plot(Ozone ~ Temp, data = airquality)
model <- lm(Ozone ~ Temp, data = airquality)
summary(model)
abline(model, col = "lightsalmon")

# Residuals

ozone_resid <- resid(model)
ozone_resid
length(ozone_resid)
nrow(airquality)
head(airquality)

# creating smaller version of the data set
# with NA values removed
airquality2 <- airquality[, c("Ozone", "Temp")]
head(airquality2)
airquality2 <- na.omit(airquality2)
nrow(airquality2)
# why don't do it with the full dataset?
nrow(na.omit(airquality)) # we lose 5 obs

plot(airquality2$Temp, ozone_resid)

# 1. making up an example with "bad" residuals
# -> trend in the residuals
x <- rnorm(100)
y <- 1.9 + 0.6 * x^2 + rnorm(100, sd = 0.3)
plot(x, y)
model2 <- lm(y ~ x)
summary(model2)

plot(x, resid(model2))

# 2. bad residuals: inhomogenous variance
# "heteroscadasticity"

y2 <- 1.9 + 0.6 * x
y2[x < 0] <- y2[x < 0] +
  rnorm(length(y2[x < 0]), sd = 0.3)
y2[x > 0] <- y2[x > 0] +
  rnorm(length(y2[x > 0]), sd = 1.8)
model3 <- lm(y2 ~ x)
plot(x, resid(model3))

# Predictions
# case 1: predicting the yhat

yhat_ozone <- predict(model)
yhat_ozone
plot(airquality2$Temp, yhat_ozone)

plot(Ozone ~ Temp, data = airquality2, cex = 1.5)
points(airquality2$Temp, yhat_ozone + ozone_resid,
       pch = 20, col = "red")
abline(model, col = "blue")

# case 2: outside of observations
airquality2$Temp == 95
which(airquality2$Temp == 95)
any(airquality2$Temp == 95)

# we need a new data frame with predictor values
# that were not observed previously
pred_data <- data.frame(Temp = 95)
predict(model, newdata = pred_data)

# uncertainty
summary(model)

predict(model, newdata = pred_data,
        interval = "prediction")

# Action 1
pred_data2 <- data.frame(Temp = 100)
predict(model, newdata = pred_data2,
        interval = "prediction")

pred_data3 <- data.frame(Temp = c(95, 100, 105))
predict(model, newdata = pred_data3,
        interval = "prediction")

# Multivariate regression

model2 <- lm(Ozone ~ Temp + Wind, data = airquality)
summary(model2)

# different kinds of R^2 
y <- rnorm(20)
x1 <- rnorm(20)
summary(lm(y ~ x1))

x2 <- rnorm(20)
x3 <- rnorm(20)
x4 <- rnorm(20)
summary(lm(y ~ x1 + x2 + x3 + x4))

# Scenarios with multivariate prediction
predict(model2, newdata = data.frame(Temp = 95, Wind = 7),
        interval = "prediction")

predict(model2, newdata = data.frame(Temp = 95, Wind = 14),
        interval = "prediction")
