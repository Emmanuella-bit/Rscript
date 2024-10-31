setwd("~/CodeWT2024")

set.seed(42)
all_fi <- rnorm(144, mean = 3, sd = 1)
n <- 20
samp20 <- sample(all_fi, n)

pop_sd <- sd(all_fi)
pop_mean <- mean(all_fi)
# SEM
sem <- pop_sd / sqrt(n)
# alternative way of writing the square root
sem <- pop_sd / n^(0.5)

samp_mean <- mean(samp20)
z <- (samp_mean - pop_mean) / sem

z
# it is between -1.96 and +1.96

# one sample t-test

beech <- read.csv2("rtsl_trw.csv")
beech$trw1975

t.test(beech$trw1975, mu = 1)
t.test(beech$trw1975, mu = 1, alternative = "greater")
t.test(beech$trw1975, mu = 1, alternative = "less")

thousand_samples <- sapply(
  1:1000,
  function(x) t.test(sample(all_fi, 20), mu = 1)
)

# Action 1

t.test(beech$trw1976, mu = 0.75,
       alternative = "greater")

t.test(beech$trw1976, mu = 0.71,
       alternative = "greater")

# Paired two-sided t-test
t.test(beech$trw1975, beech$trw1976,
       paired = TRUE)
# for paired data, two sample t.test
# is just one sample t.test on
# differences
t.test(beech$trw1975 - beech$trw1976,
       mu = 0)

# two sample t.test with independent samples
dbh <- read.csv2("dbh.csv")
boxplot(dbh)

t.test(dbh$site1, dbh$site2)






