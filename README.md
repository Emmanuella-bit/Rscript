##Extracting the exam data from the working directory

library(dplR)
#set a working directory for my assignment
setwd("C:/Rstudios_workspace/exams_data")

#Extract data
beech_data <- read.rwl("bausenberg_beech.rwl")
oak_data <- read.rwl("bausenberg_oak.rwl")


###### the tree means
beech_id <- read.ids(beech_data, stc = c(5, 2, 1))
oak_id <- read.ids(oak_data, stc = c(5, 2, 1))
beech_tm <- treeMean(beech_data, beech_id, na.rm = TRUE)
oak_tm <- treeMean(oak_data, oak_id, na.rm = TRUE)


####### detrending the data
beech_d <- detrend(beech_tm, method = "Spline",
                   nyrs = 32)
oak_d <- detrend(oak_tm, method = "Spline",
                 nyrs = 32)


#### Chroning of the data
beech_c <- chron(beech_d)
oak_c <- chron(oak_d)
plot(beech_c)
plot(oak_c)


#### pointer years with pointRes
# Assuming beech_d and oak_d are data frames with each column representing a tree's ring-width series

# Check the structure of the data
str(beech_d)
str(oak_d)

library(pointRes)
####for beech
p_norm <- pointer.norm(beech_tm, method.thresh = "Neuwirth")
p_rgc <- pointer.rgc(beech_tm)
p_zchron <- pointer.zchron(beech_d)

comparison <- list(p_norm, p_rgc, p_zchron)

pointer.plot(comparison, labels = c("norm", "rgc", "zchron"))
####for oak
p_norm <- pointer.norm(oak_tm, method.thresh = "Neuwirth")
p_rgc <- pointer.rgc(oak_tm)
p_zchron <- pointer.zchron(oak_d)

comparison <- list(p_norm, p_rgc, p_zchron)

pointer.plot(comparison, labels = c("norm", "rgc", "zchron"))



# Resilience analysis for beech and oak tree  species

res_beech <- res.comp(beech_d)
res_oak <- res.comp(oak_d)

years_beech <- as.numeric(rownames(res_beech$resist))
years_oak <- as.numeric(rownames(res_oak$resist))

beech_2003_rt <- res_beech$resist[years_beech == 2003, ]
beech_2003_rc <- res_beech$recov[years_beech == 2003, ]
beech_2003_rs <- res_beech$resil[years_beech == 2003, ]

oak_2003_rt <- res_oak$resist[years_oak == 2003, ]
oak_2003_rc <- res_oak$recov[years_oak == 2003, ]
oak_2003_rs <- res_oak$resil[years_oak == 2003, ]

##### visual representation 

boxplot(beech_2003_rt, oak_2003_rt, names = c("Beech",
                                                "Oak"),
        col = c("lightgreen", "lightblue"), ylab = "Resistance")


boxplot(beech_2003_rc, oak_2003_rc, names = c("Beech",
                                                "Oak"),
        col = c("lightgreen", "lightblue"), ylab = "Recovery")

boxplot(beech_2003_rs, oak_2003_rs, names = c("Beach",
                                                "Oak"),
        col = c("lightgreen", "lightblue"), ylab = "Resilience")



###### testing for finding difference between species (normality)

shapiro.test(beech_2003_rt)
shapiro.test(oak_2003_rt)
shapiro.test(beech_2003_rs)
shapiro.test(oak_2003_rs)
shapiro.test(beech_2003_rc)
shapiro.test(oak_2003_rc)

length(beech_2003_rt) ### my sample size is only 2, conducting a Shapiro-Wilk test may not be meaningful, as this test requires a minimum of 3 observations



######## to analyse which tree species have more resistance and recovery pattern
t.test(beech_2003_rt, oak_2003_rt)
text (-3.257, "p-value = 0.05069")

t.test(beech_2003_rs, oak_2003_rs)

t.test(beech_2003_rc, oak_2003_rc)



###########calibration of climate-growth relationships



# Read tree-ring data for beech
beech_data <- read.rwl("bausenberg_beech.rwl")
beech_id <- read.ids(beech_data, stc = c(1, 2, 1))
beech_tm <- treeMean(beech_data, beech_id, na.rm = TRUE)
beech_d <- detrend(beech_tm, method = "Spline",
                   nyrs = 30)
beech_c <- chron(beech_d)

# Read tree-ring data for oak
oak_data <- read.rwl("bausenberg_oak.rwl")
oak_id <- read.ids(oak_data, stc = c(1, 2, 1))
oak_tm <- treeMean(oak_data, oak_id, na.rm = TRUE)
oak_d <- detrend(oak_tm, method = "Spline",
                 nyrs = 30)
oak_c <- chron(oak_d)


clim_fs <- read.csv2("climate_bausenberg.csv")
head(clim_fs)
# Subset climate data to match the time span of tree-ring data
clim_fs_subset <- subset(clim_fs, year >= 1940 & year <= 2018)

#calibration of climate-growth relationships for beech
library(treeclim)
my_calib_beech <- dcc(beech_c, clim_fs_subset)
my_calib_beech
plot(my_calib_beech)

my_calib_beech2 <- dcc(beech_c, clim_fs,
                       selection = 8:9)
plot(my_calib_beech2)


#calibration of climate-growth relationships for oak
library(treeclim)
my_calib_oak <- dcc(oak_c, clim_fs_subset)
my_calib_oak
plot(my_calib_oak)

my_calib_oak2 <- dcc(oak_c, clim_fs,
                     selection = 3:9)
plot(my_calib_oak2)

# seasonal targets with "modifiers"
my_calib_beech3 <- dcc(beech_c, clim_fs,
                       selection = .mean(-6:-8, "prec") + 
                         .mean(6:8, "prec"))

plot(my_calib_beech3)


my_calib_oak3 <- dcc(oak_c, clim_fs,
                     selection = .mean(-6:-8, "prec") + 
                       .mean(6:8, "prec"))
plot(my_calib_oak3)


# plot dynamic for  beech
my_calib_beech4 <- dcc(beech_c, clim_fs,
                       selection = 
                         .range(6:8, "prec") +
                         .range(6:8, "tmean") +
                         .range(6:8, "spei_3") +
                         .range(6:8, "spi_3"),
                       dynamic = "moving")

plot(my_calib_beech4)

#plot dynamic for oak
my_calib_oak4 <- dcc(oak_c, clim_fs,
                     selection = 
                       .range(6:8, "prec") +
                       .range(6:8, "tmean") +
                       .range(6:8, "spei_3") +
                       .range(6:8, "spi_3"),
                     dynamic = "moving")

plot(my_calib_oak4)



