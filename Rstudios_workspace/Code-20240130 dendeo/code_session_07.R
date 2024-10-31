library(dplR)

# ingredient #1: tree-ring data
douglas <- read.rwl("data/douglas.rwl")
douglas_id <- read.ids(douglas, stc = c(1, 2, 1))
douglas_tm <- treeMean(douglas, douglas_id, na.rm = TRUE)
douglas_d <- detrend(douglas_tm, method = "Spline", nyrs = 30)
douglas_c <- chron(douglas_d)
plot(douglas_c)

# ingredient #2: climate data
# data source: DWD (German Weather Service) 1*1km grid
clim_fs <- read.csv2("data/clim_fs.csv")
head(clim_fs)
tail(clim_fs)

# plot(clim_fs$year, clim_fs$temp, type = "l")

# excursion: time series-plot
temp <- clim_fs$temp
temp <- ts(temp, start = 1901, frequency = 12)
plot(temp)

# 1. try and error

clim_fs_july <- clim_fs[clim_fs$month == 7, ]
clim_fs_july$month <- NULL
# combining tree-ring data and climate data in one data frame

# chronology needs to be transformed into "proper"
# data frame (= year is a real variable)

douglas_july <- data.frame(
  year = as.numeric(rownames(douglas_c)),
  rwi = douglas_c$std
)

douglas_july <- merge(douglas_july, clim_fs_july)

summary(lm(rwi ~ temp, data = douglas_july))
summary(lm(rwi ~ prec, data = douglas_july))

# rwi = a_0 + a_1*temp + a_2*prec
summary(lm(rwi ~ temp + prec, data = douglas_july))

library(treeclim)


