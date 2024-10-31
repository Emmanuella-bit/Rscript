setwd("~/CodeWT2024")

body_heights <- c(171, 177, 178, 168, 179, 184, 165,
  179, 168, 181, 151, 158, 162, 155,
  164, 192)

mean(body_heights)
sd(body_heights)

# reading in the beech data from last time
beech <- read.csv2("rtsl_trw.csv")

