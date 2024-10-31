library(dplR)

douglas <- read.rwl("Data/douglas.rwl")
douglas_id <- read.ids(douglas, stc = c(1, 2, 1))
douglas_tm <- treeMean(douglas, douglas_id, na.rm = TRUE)
douglas_d <- detrend(douglas_tm, method = "Spline",
                     nyrs = 30)
douglas_c <- chron(douglas_d)



clim_fs <- read.csv2("Data/clim_fs.csv")
head(clim_fs)

library(treeclim)
my_calib <- dcc(douglas_c, clim_fs)
my_calib
plot(my_calib)

# Action 1
my_calib2 <- dcc(douglas_c, clim_fs,
                 selection = 3:9)
plot(my_calib2)

# seasonal targets with "modifiers"
my_calib3 <- dcc(douglas_c, clim_fs,
                 selection = .mean(-6:-8) + .mean(6:8))
plot(my_calib3)

my_calib4 <- dcc(douglas_c, clim_fs,
                 selection = .mean(-6:-8, "prec") + 
                   .mean(6:8, "prec"))
plot(my_calib4)

my_calib5 <- dcc(douglas_c, clim_fs,
                 selection = 
                   .range(-6:-8, "prec") +
                   .range(7:8, "prec") +
                   .range(2:3, "temp"))
plot(my_calib5)

# modifiers separately
.range(5:9)
.mean(5:9)
.sum(5:9)

plot(dcc(douglas_c, clim_fs, selection = -6:9))
plot(dcc(douglas_c, clim_fs, selection = .range(-6:9)))
plot(dcc(douglas_c, clim_fs, selection = .mean(-6:9)))

# dynamic perspective

my_calib6 <- dcc(douglas_c, clim_fs,
                 selection = 
                   .range(-6:-8, "prec") +
                   .range(7:8, "prec") +
                   .range(2:3, "temp"),
                 dynamic = "moving")
plot(my_calib6)


# Action 2
larch <- read.rwl("Data/larch.rwl")
larch_id <- read.ids(larch, stc = c(1, 2, 1))
larch_tm <- treeMean(larch, larch_id, na.rm = TRUE)
larch_d <- detrend(larch_tm, method = "Spline",
                   nyrs = 30)
larch_c <- chron(larch_d)

larch_calib <- dcc(larch_c, clim_fs, selection = 3:9,
                   dynamic = "moving", win_size = 30,
                   win_offset = 3)
plot(larch_calib)

# DLM: dendro-flavoured linear model
my_dlm <- dlm(douglas_c, clim_fs, 
              selection = .range(6:8, "prec"))
summary(my_dlm)
