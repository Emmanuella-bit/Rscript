library(dplR)
setwd("C:/Rstudios_workspace/exams_data")

# repeating detrending with splines

all_fs <- read.rwl("data/all.rwl")
all_ids <- read.ids(all_fs, stc = c(1, 2, 1))
all_fs_tm <- treeMean(all_fs, all_ids)

spag.plot(all_fs_tm)

all_fs_d <- detrend(all_fs_tm, method = "Spline")
spag.plot(all_fs_d)

all_fs_ch <- chron(all_fs_d)


# repeating pointer years with pointRes

library(pointRes)

rtsl1 <- read.rwl("data/RTSL1.rwl")
rtsl1_ids <- read.ids(rtsl1, stc = c(5, 2, 1))
rtsl1_tm <- treeMean(rtsl1, rtsl1_ids)

rtsl1_d <- detrend(rtsl1_tm, method = "Spline", 
                   nyrs = 32)

p_norm <- pointer.norm(rtsl1_tm, method.thresh = "Neuwirth")
p_rgc <- pointer.rgc(rtsl1_tm)
p_zchron <- pointer.zchron(rtsl1_d)

comparison <- list(p_norm, p_rgc, p_zchron)

pointer.plot(comparison, labels = c("norm", "rgc", "zchron"))

# Excursion: Lists in R

l <- list(1, 2, 3)
l
l[1]
l[[1]]
l[[2]]
l[[3]]
l[[1]][1]

l <- list(a = 1, b = 2, c = 3)
l[[1]]
l$a

p_norm$EYvalues
p_norm[[2]]

# Resilience analysis

res <- res.comp(rtsl1_d, nb.yrs = c(3, 3))
res_resist <- res$resist
res_recov <- res$recov
res_resil <- res$resil

# only look at 1976 (drought year!!)
years <- as.numeric(rownames(res_resil))
years
res_recov[years == 1976, ]
res_resist[years == 1976, ]
res_resil[years == 1976, ]

# Commmunity action 1

# Part 1

angio <- read.rwl("data/angiosperms.rwl")
gymno <- read.rwl("data/conifers.rwl")

ids_angio <- read.ids(angio, stc = c(1, 2, 1))
ids_gymno <- read.ids(gymno, stc = c(1, 2, 1))

angio_tm <- treeMean(angio, ids_angio)
gymno_tm <- treeMean(gymno, ids_gymno)

angio_d <- detrend(angio_tm, method = "Spline", nyrs = 32)
gymno_d <- detrend(gymno_tm, method = "Spline", nyrs = 32)

res_angio <- res.comp(angio_d)
res_gymno <- res.comp(gymno_d)

years_angio <- as.numeric(rownames(res_angio$resist))
years_gymno <- as.numeric(rownames(res_gymno$resist))

angio_2003_rt <- res_angio$resist[years_angio == 2003, ]
angio_2003_rc <- res_angio$recov[years_angio == 2003, ]
angio_2003_rs <- res_angio$resil[years_angio == 2003, ]

gymno_2003_rt <- res_gymno$resist[years_gymno == 2003, ]
gymno_2003_rc <- res_gymno$recov[years_gymno == 2003, ]
gymno_2003_rs <- res_gymno$resil[years_gymno == 2003, ]

# Part 2

boxplot(angio_2003_rt, gymno_2003_rt, names = c("Angiosperms",
                                                "Gymnosperms"),
        col = c("lightgreen", "darkgreen"), ylab = "Resistance")


boxplot(angio_2003_rc, gymno_2003_rc, names = c("Angiosperms",
                                                "Gymnosperms"),
        col = c("lightgreen", "darkgreen"), ylab = "Recovery")

boxplot(angio_2003_rs, gymno_2003_rs, names = c("Angiosperms",
                                                "Gymnosperms"),
        col = c("lightgreen", "darkgreen"), ylab = "Resilience")

# Part 3

# check for normality

shapiro.test(angio_2003_rt)
shapiro.test(gymno_2003_rt)

t.test(angio_2003_rt, gymno_2003_rt)

shapiro.test(angio_2003_rs)
shapiro.test(gymno_2003_rs)

t.test(angio_2003_rs, gymno_2003_rs)

boxplot(angio_2003_rs, gymno_2003_rs, names = c("Angiosperms",
                                                "Gymnosperms"),
        col = c("lightgreen", "darkgreen"), ylab = "Resilience")

text(1, 1.2, "p-value = 0.7656")
