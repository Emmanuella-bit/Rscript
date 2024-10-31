# Action 1

file1 <- read.csv("file1.txt")
head(file1)
class(file1$year)
class(file1$month)
class(file1$tmp)

file2 <- read.delim("file2.txt", sep = "*")
class(file2$tmp)

file3 <- read.table("file3.txt", header = TRUE)
class(file3$tmp)

?read.table

read.csv("file1.txt",
         col.names = c("año", "mes", "temperatura"))


# Action 2

write.csv2(file3, "file3.csv")


# Speed

test <- read.csv2("nyc_street_trees_2015.csv")
test <- data.table::fread("nyc_street_trees_2015.csv")
test <- readr::read_csv2("nyc_street_trees_2015.csv")

# Community

# Part 1

nyctc <- read.csv2("nyc_street_trees_2015.csv")
head(nyctc)

# Part 2

dbh_brooklyn <- nyctc$tree_dbh[nyctc$borough == "Brooklyn"]
dbh_queens <- nyctc$tree_dbh[nyctc$borough == "Queens"]

shapiro.test(dbh_brooklyn)
shapiro.test(dbh_queens)

shapiro.test(sample(dbh_brooklyn, 5000))
shapiro.test(sample(dbh_queens, 5000))

hist(dbh_brooklyn)

# Part 3

wilcox.test(dbh_brooklyn, dbh_queens)

# Part 4

boxplot(dbh_brooklyn, dbh_queens,
        names = c("Brooklyn", "Queens"),
        ylab = "Tree Diameter at 130cm Height",
        col = c("lightblue", "darkred"), outline = FALSE)
text(1.5, 300, "p < 0.001")

# Basic simulation

d1 <- rnorm(1000000, mean = 1, sd = 1)
d2 <- rnorm(1000000, mean = 1.05, sd = 1)

t.test(d1, d2)
