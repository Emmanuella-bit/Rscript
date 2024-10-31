library(dplR)

# Detrending ----

all_fs <- read.rwl("all.rwl")
stc <- c(1, 2, 1)
ids <- read.ids(all_fs, stc = stc)
all_tm <- treeMean(all_fs, ids)

plot(x = as.numeric(rownames(all_tm)),
     y = all_tm[, 11], type = "l",
     xlab = "Years AD", ylab = "Ringwidth (mm)",
     xlim = c(1960, 2023)) # xlim will set the limits
                           # for the x axis
                           # ylim would do this for the
                           # the y axis

# interactive detrending
i.detrend(all_tm)

# excursion: how to compute residuals
y <- na.omit(all_tm[, 11])
plot(ts(y))
ys <- smooth.spline(y)
plot(ts(ys$y))
lines(ts(y))
plot(ts(y - ys$y))

# batch detrending all trees with splines
# with the same properties (how flexible
# should it be)

all_tm_d <- detrend(all_tm, method = "Spline")
spag.plot(all_tm_d)

# Action 1 ----

conif <- read.rwl("data/conifers.rwl")
conif_ids <- read.ids(conif, stc)
conif_tm <- treeMean(conif, conif_ids)

# batch detrending with a spline which has
# a flexibility of 32 years
conif_d <- detrend(conif_tm, method = "Spline",
                  nyrs = 32)

spag.plot(conif_d)

# Averaging ----

# arithmetic mean
rowMeans(all_tm_d)
rowMeans(all_tm_d, na.rm = TRUE)

# robust mean; applied to detrended data
all_tm_c <- chron(all_tm_d)
all_tm_c

plot(all_tm_c)

# Pointer years ----

rtsl1 <- read.rwl("data/RTSL1.rwl")
rtsl1_ids <- read.ids(rtsl1, stc = c(5, 2, 1))
rtsl1_tm <- treeMean(rtsl1, rtsl1_ids)

spag.plot(rtsl1_tm)

# install.packages("pointRes")
library(pointRes)

rtsl1_d <- detrend(rtsl1_tm, method = "Spline",
                   nyrs = 32)
spag.plot(rtsl1_d)

p_norm <- pointer.norm(rtsl1_tm)
p_rgc <- pointer.rgc(rtsl1_tm)
p_zchron <- pointer.zchron(rtsl1_d)
