# Conditional subsetting - repeat

set.seed(42)
g <- data.frame(
  x = rnorm(10),
  y = sample(c("a", "b", "c"), 10,
             replace = TRUE)
)

g

g2 <- g[g$x > 0, ]
nrow(g2)
g[g$y == "b", ]
g[g$y != "b", ]
g[g$y %in% c("a", "b"), ]
g[g$x < 0 & g$y == "c", ]

r <- data.frame(
  x = rnorm(10000),
  y = sample(c("a", "b", "c"), 10000,
             replace = TRUE)
)

nrow(r[r$x < 0, ])

# Selection and testing workflow ----

fs_trees <- read.csv2("dendro_dbh.csv")
head(fs_trees)

fs_trees$species_common
unique(fs_trees$species_common)
conifers <- c("Scots pine", "European larch",
              "Norway spruce", "Douglas fir")
fs_conif <- fs_trees[fs_trees$species_common %in%
                       conifers, ]
fs_angio <- fs_trees[!(fs_trees$species_common %in%
                         conifers), ]

dbh_conif <- fs_conif$dbh_cm
dbh_angio <- fs_angio$dbh_cm

shapiro.test(dbh_conif)
shapiro.test(dbh_angio)

var(dbh_conif) / var(dbh_angio)

t.test(dbh_angio, dbh_conif)

boxplot(dbh_conif, dbh_angio)

boxplot(dbh_conif, dbh_angio,
        col = c("darkgreen", "lightgreen"),
        names = c("Conifers", "Angiosperms"),
        ylab = "DBH (cm)")
text(2, 72, "p < 0.001")

# Non-parametric alternatives

set.seed(42)
z1 <- rgamma(20, 1.0, 1.0)
z2 <- rgamma(20, 1.2, 0.8)

mean(z1)
mean(z2)

qqnorm(z1)
qqline(z1)
shapiro.test(z1)

z1
z1_ord <- order(z1)
z1[z1_ord]

# paired two sample wilcoxon signed rank test
d <- z1 - z2
r <- order(abs(d))
r_plus <- sum(r[d > 0])
r_minus <- sum(r[d < 0])
z <- min(c(r_minus, r_plus))
z

wilcox.test(z1, z2, paired = TRUE)

wilcox.test(z1, mu = 1)
wilcox.test(z1, mu = 2)

wilcox.test(z1, z2)

# Action 1 ----

rtsl <- read.csv2("rtsl_trw.csv")
# two sample, paired Wilcoxon signed rank test
wilcox.test(rtsl$trw1975, rtsl$trw1976,
            paired = TRUE)

t.test(rtsl$trw1975, rtsl$trw1976,
            paired = TRUE)

