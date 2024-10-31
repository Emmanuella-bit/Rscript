# Prerequesites of the t-test ----

set.seed(42)
all_fi <- rnorm(144, mean = 3, sd = 1)

hist(all_fi)

qqnorm(all_fi)
qqline(all_fi, col = "red")

all_fi2 <- rbeta(144, 4, 1)
hist(all_fi2)

qqnorm(all_fi2)
qqline(all_fi2)

shapiro.test(all_fi)
# alternative hypothesis: data is *not* normally distributed
# null hypothesis: data is normally distributed
# high p-value: we cannot safely reject the null hypothesis
# instead, we accept it! -> data is normally distributed!

shapiro.test(all_fi2)
# low p-value: we safely reject null hypothesis -> data is
# not normally distributed

beech <- read.csv2("rtsl_trw.csv")
dbh <- read.csv2("dbh.csv")

var(beech$trw1975) / var(beech$trw1976)
var(dbh$site1) / var(dbh$site2)

# Action 1 ----

par(mfrow = c(1, 2)) # splitting up the plotting area
hist(dbh$site1)
hist(dbh$site2)

qqnorm(dbh$site1)
qqline(dbh$site1)
qqnorm(dbh$site2)
qqline(dbh$site2)

shapiro.test(dbh$site1) # cannot reject H0 -> data is normal
shapiro.test(dbh$site2) # cannot reject H0 -> data is normal

# Conditional subsetting ----

fs_trees <- read.csv2("dendro_dbh.csv")
head(fs_trees)

fs_trees$species_common
fs_trees[1,]
fs_trees[1:10,]
fs_trees$species_common[3:9]

fs_trees[c(13, 14, 22, 29),]

fs_trees$species_common == "Scots pine"
fs_trees[fs_trees$species_common == "Scots pine", ]

big_trees <- fs_trees$dbh_cm >= 50
fs_trees_big <- fs_trees[big_trees, ]
nrow(fs_trees_big)

sum(big_trees)

# negative conditional subsetting / exclusion
no_spruce <- fs_trees$species_common != "Norway spruce"
fs_trees[no_spruce, ]

# logical AND / OR
big_douglas <- fs_trees$species_common == "Douglas fir" &
  fs_trees$dbh_cm >= 60
fs_trees[big_douglas, ]

pine_or_beech <- fs_trees$species_common == "European beech" |
  fs_trees$species_common == "Scots pine"
fs_trees[pine_or_beech,]

# alternative with %in%

pine_or_beech2 <- fs_trees$species_common %in%
  c("European beech", "Scots pine")
fs_trees[pine_or_beech2,]

# Action 2 ----

# 1)
fs_trees[fs_trees$species_common == "Sycamore maple", ]

# 2)
fs_trees[fs_trees$species_common == "Sycamore maple" &
  fs_trees$dbh_cm >= 50,]

# 3)
fs_trees[(fs_trees$species_common == "Sycamore maple" |
  fs_trees$species_common == "Douglas fir") &
  fs_trees$dbh_cm > 50,]
