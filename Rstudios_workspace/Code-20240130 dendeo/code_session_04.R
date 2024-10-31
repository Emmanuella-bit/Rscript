library(dplR)
setwd("C:/Rstudios_workspace/Data-20240130 dendeo")

# Repeating concepts from last time ---- 

all_trees <- read.rwl("all.rwl")
head(all_trees)

stc <- c(1, 2, 1)
ids <- read.ids(all_trees, stc = stc)
all_tm <- treeMean(all_trees, ids)
head(all_tm)

# Action 1 ----

conifers <- read.rwl("data/conifers.rwl")
ids_conif <- read.ids(conifers, stc = stc)
conifers_tm <- treeMean(conifers, ids_conif)
spag.plot(conifers_tm)

# Descriptive statistics ----

# how many trees?
ncol(all_tm)

# how many years covered?
years <- rownames(all_tm)
years <- as.numeric(years) # recasting from char to num
min(years)
max(years)
range(years)
length(years)

# GLK
glk.legacy(all_tm)
mean(glk.legacy(all_tm))
mean(glk.legacy(all_tm), na.rm = TRUE)

# mean interseries correlation
all_cor <- cor(all_tm, use = "pairwise.complete")
all_cor <- all_cor[upper.tri(all_cor)]
rbar_all <- mean(all_cor)

# alternative to keep matrix structure
all_cor2 <- cor(all_tm, use = "pairwise.complete")
all_cor2[lower.tri(all_cor2, diag = TRUE)] <- NA
rbar_all2 <- mean(all_cor2, na.rm = TRUE)

# Action 2 ----

# conifers_tm
angio <- read.rwl("data/angiosperms.rwl")
ids_angio <- read.ids(angio, stc = stc)
angio_tm <- treeMean(angio, ids_angio)

# GLKs
glk_conifers <- mean(glk.legacy(conifers_tm), na.rm = TRUE)
glk_angio <- mean(glk.legacy(angio_tm), na.rm = TRUE)

glk_conifers
glk_angio

# Rbars
rbar_conifers <- cor(conifers_tm, use = "pairwise.complete")
rbar_conifers <- mean(rbar_conifers[upper.tri(rbar_conifers)])

rbar_angio <- cor(angio_tm, use = "pairwise.complete")
rbar_angio <- mean(rbar_angio[upper.tri(rbar_angio)])

rbar_conifers
rbar_angio
rbar_all
