# selection from vectors

foo <- c(1, 2, 3, 4, 5)
foo[1]
foo[4]
bar <- foo[4]

# Action 1

ant_species1 <- c(140, 110, 88, 12, 9)
ant_species2 <- c(24, 35, 17, 62, 76)
days_vector <- c("Monday", "Tuesday",
                 "Wednesday", "Thursday",
                 "Friday")
names(ant_species1) <- days_vector
names(ant_species2) <- days_vector

# select multiple elements

my_vector <- c("fee", "fi", "foo", "fum")
my_vector[2]
my_vector[c(3, 4)]
new_vector <- my_vector[c(3, 4)]

indices <- c(1, 2)
indices <- indices + 1
my_vector[indices]

# Action 2

species_1_mon_wed <- ant_species1[c(1, 2, 3)]
species_1_thu_fri <- ant_species1[c(4, 5)]

species_1_mon_wed <- ant_species1[1:3]
species_1_thu_fri <- ant_species1[4:5]

species_2_mon_wed <- ant_species2[1:3]
species_2_thu_fri <- ant_species2[4:5]

mean(species_1_mon_wed)
mean(species_1_thu_fri)

mean(species_2_mon_wed)
mean(species_2_thu_fri)

mean_species_1_mon_wed <- mean(ant_species1[1:3])

# Excluding elements

foo <- 1:10
foo[-7]
foo[-c(7:9)]
index <- 7:9
foo[-index]

# Selecting by name

my_vector <- c(1, 2, 3)
names(my_vector) <- c("one", "two", "three")
my_vector
my_vector["two"]
my_vector[c("one", "three")]

# Action 3

ant_species1["Thursday"]

# Comparing samples

fi <- c(3.5, 3.2, 2.5, 2.9, 2.6,
        3, 2.5, 2.8, 2.2, 2.7)
ccm <- c(1.7, 3.7, 3.3, 2.2, 2.2,
         2.2, 2.3, 2.9, 1.4, 2.8)

mean(fi); mean(ccm)
sd(fi); sd(ccm)

hist(fi)
hist(ccm)
?hist

boxplot(fi)
?boxplot

boxplot(ccm)

boxplot(fi, ccm)
boxplot(fi, ccm, names = c("FI", "CCM"))
boxplot(fi, ccm, names = c("FI", "CCM"),
        col = c("#435573", "orange"))

# Action 5

boxplot(ant_species1, ant_species2,
        names = c("Species 1", "Species 2"),
        col = c("chartreuse", "papayawhip"))



