library(dplR)

# set the working directory, that contains code and data
setwd("C:/Rstudios_workspace/Data-20240130 dendeo")

all_trees <- read.rwl("all.rwl")
head(all_trees)
tail(all_trees)

plot(all_trees)
spag.plot(all_trees)

series <- all_trees[,1] 
years <- rownames(all_trees)
plot(years, series, type = "l")

# tree-specific means

stc <- c(1, 2, 1)
ids <- read.ids(all_trees, stc = stc)
ids
all_tm <- treeMean(all_trees, ids)
head(all_tm)

spag.plot(all_tm)

# Action 1

conifers <- read.rwl("conifers.rwl")
# setup of series ID:
# example 625A
# 1. character/number: site/species; here: 6 (oak)
# 2. and 3. characters/numbers: tree no.; here: 25
# 4. character: core; here core A
stc <- c(1, 2, 1)
# supplying the stc argument by position:
conifers_ids <- read.ids(conifers, stc)
# supplying the stc argument by name:
conifers_ids <- read.ids(conifers, stc = stc)
# supplying the stc argument inline by name:
conifers_ids <- read.ids(conifers, stc = c(1, 2, 1))
conifers_tm <- treeMean(conifers, conifers_ids)
spag.plot(conifers_tm)
?read.ids



