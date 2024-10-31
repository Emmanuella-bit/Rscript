matrix(1:9, nrow = 3)
matrix(1:9, byrow = TRUE, nrow = 3)

cat1 <- c("mona", "bone", "jakon")
cat2 <- c("maybe", "you're", "right")
cat3 <- c("fill", "my", "eyes")

cat_stevens_songs <- c(cat1, cat2, cat3)
cat_stevens_songs
matrix(cat_stevens_songs, nrow = 3)
cat_stevens_matrix <- matrix(cat_stevens_songs,
                             byrow = TRUE,
                             nrow = 3)
cat_stevens_matrix

my_matrix <- matrix(c((1:3)^2, (1:3)^3),
                    byrow = TRUE,
                    ncol = 3)
my_matrix
colnames(my_matrix) <- c("one", "two", "three")
my_matrix
rownames(my_matrix) <- c("squared", "cubic")
my_matrix

my_matrix <- matrix(1:9, byrow = TRUE, nrow = 3)
rowMeans(my_matrix)
rowSums(my_matrix)


# using brackets with a matrix
my_matrix[1,1]
my_matrix[2,3]
my_matrix[2,]
my_matrix[,3]
my_matrix[1:2,2:3]

my_matrix
my_matrix + 2

mtcars
head(mtcars)
tail(mtcars)
str(mtcars)

name <- c("Shakira", "Serj Tankian",
          "Anthony Kiedis")
female <- c(TRUE, FALSE, FALSE)
year_of_birth <- c(1977, 1967, 1962)
born <- c("Colombia", "Libanon", "USA")
parental_grandpa <- c("Libanon", "Armenia",
                      "Lithuania")

singers <- data.frame(name, female,
                      year_of_birth,
                      born,
                      parental_grandpa)

singers

# Action 1

days <- c("Monday", "Tuesday", "Wednesday",
          "Thursday", "Friday")
species_1 <- c(140, 110, 88, 12, 9)
species_2 <- c(24, 35, 17, 62, 76)
rain <- c(FALSE, FALSE, FALSE, TRUE, TRUE)

ant_df <- data.frame(
  days,
  species_1,
  species_2,
  rain
)

ant_df[, 2]
ant_df[1, ]
ant_df[, "rain"]
ant_df[1:3, "rain"]

ant_df$days

# Action 2
# a)
ant_df[1:4, 2:3]
ant_df[1:4, c("species_1", "species_2")]
ant_df[-5, c("species_1", "species_2")]

# b)
ant_df$days
ant_df[,1]
ant_df[,"days"]

# c)
ant_df[1:2, "rain"]

# working with real data

beech <- read.csv2("rtsl_trw.csv")
beech
head(beech)
tail(beech)

par(las = 1)
boxplot(beech$trw1975,
        beech$trw1976,
        main = "Diameter growth",
        names = c(1975, 1976),
        ylab = "Growth in mm",
        col = c("lightsalmon", "purple"))
?par
colors()
