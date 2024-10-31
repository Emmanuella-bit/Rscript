trees
?trees

# visual exploration
plot(trees$Girth, trees$Volume)
plot(trees$Height, trees$Volume)

# correlation formula applied
x <- trees$Girth
y <- trees$Volume
n <- length(x)

mean_x <- mean(x)
mean_y <- mean(y)

var_x <- sum((x - mean_x)^2)/(n - 1)
var_y <- sum((y - mean_y)^2)/(n - 1)
cov_xy <- sum((x - mean_x)*(y - mean_y))/(n - 1)

r <- cov_xy / (sqrt(var_x) * sqrt(var_y))
r
cor(x, y)

# for-loops

m <- 10
# "container"
cors <- numeric(m)

cors[1] <- cor(rnorm(31), rnorm(31))
cors[2] <- cor(rnorm(31), rnorm(31))
cors[3] <- cor(rnorm(31), rnorm(31))
cors[4] <- cor(rnorm(31), rnorm(31))

# applying a for loop

m <- 1000
cors <- numeric(m)

for (i in 1:m) {
  cors[i] <- cor(rnorm(31), rnorm(31))
}
cors

hist(cors)

# correlation testing

cor.test(x, y)
cor.test(trees$Girth, trees$Volume)

# Action 1

plot(trees$Height, trees$Volume)
cor(trees$Height, trees$Volume)
cor.test(trees$Height, trees$Volume)

# Linear regression

airquality
?airquality

model <- lm(Ozone ~ Temp, data = airquality)
model

plot(airquality$Temp, airquality$Ozone)
abline(model, col = "red")

summary(model)

# Action 1 (part 2)
?faithful

plot(faithful$waiting, faithful$eruptions)
plot(eruptions ~ waiting, data = faithful)
faith_model <- lm(eruptions ~ waiting, data = faithful)
faith_model
summary(faith_model)
abline(faith_model)
