# Name: Yola Charara
# HW1

# Problem 1: Su_raw_matrix.txt data

# Libraries

library(ggplot2)
library(tidyverse)

# (a) Read Su_raw_matrix.txt dataset

su <- read.delim("Su_raw_matrix.txt", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# (b) Finding mean and standard deviation of Liver_2.CEL

mean_Liver2 <- mean(su$Liver_2.CEL)
sd_Liver2   <- sd(su$Liver_2.CEL) 

# Print results

cat("Mean of Liver_2.CEL:", mean_Liver2, "\n")
cat("SD of Liver_2.CEL:  ", sd_Liver2, "\n")

# (c) Average and total values of each column

col_means <- colMeans(su)
col_sums  <- colSums(su)

# Print summaries

print("Column means (numeric columns):")
print(col_means)
print("Column sums (numeric columns):")
print(col_sums)

# Problem 2: Normal distribution histograms

# (a) mean = 0, sigma = 0.2

x1 <- rnorm(10000, mean = 0, sd = 0.2)
hist(x1,
     main = "Histogram of N(0, 0.2)",
     xlab = "Value",
     xlim = c(-5, 5),
     col = "yellow",
     border = "black")

# (b) mean = 0, sigma = 0.5

x2 <- rnorm(10000, mean = 0, sd = 0.5)
hist(x2,
     main = "Histogram of N(0, 0.5)",
     xlab = "Value",
     xlim = c(-5, 5),
     col = "green",
     border = "black")

# Problem 3: ggplot with dat

#Observing commands 3b through 3e

dat <- data.frame(cond = factor(rep(c("A","B"), each=200)),
                  rating = c(rnorm(200),rnorm(200, mean=.8)))
# Overlaid histograms
ggplot(dat, aes(x=rating, fill=cond)) +
  geom_histogram(binwidth=.5, alpha=.5, position="identity")
# Interleaved histograms
ggplot(dat, aes(x=rating, fill=cond)) + geom_histogram(binwidth=.5, position="dodge")
# Density plots
ggplot(dat, aes(x=rating, colour=cond)) + geom_density()
# Density plots with semitransparent fill
ggplot(dat, aes(x=rating, fill=cond)) + geom_density(alpha=.3)

# (f) Applying ggplot to diabetes_train.csv

# Read diabetes_train.csv dataset

diabetes <- read.csv("diabetes_train.csv", header = TRUE, stringsAsFactors = FALSE)

# Checking column names

print("Columns in diabetes dataset:")
print(colnames(diabetes))

# Overlaid histograms for mass
ggplot(diabetes, aes(x = mass, fill = class)) + geom_histogram(binwidth = 2, alpha = 0.5, position = "identity")

# Interleaved histograms for mass
ggplot(diabetes, aes(x = mass, fill = class)) + geom_histogram(binwidth = 2, position = "dodge")

# Density plots for mass
ggplot(diabetes, aes(x = mass, colour = class)) + geom_density()

# Density plots with semitransparent fill for mass
ggplot(diabetes, aes(x = mass, fill = class)) + geom_density(alpha = 0.3)

# Problem 4: titanic.csv data using tidyverse

passengers <- read.csv("titanic.csv", header = TRUE, stringsAsFactors = FALSE)

# Checking column names

print("Titanic columns:")
print(colnames(passengers))

# (a) Drop NA rows and summarize

passengers %>% drop_na() %>% summary()

# (b) Filter only male passengers

passengers %>% filter(Sex == "male")

# (c) Arrange by Fare in descending order

passengers %>% arrange(desc(Fare))

# (d) Create new column FamSize that sums parents/children and siblings/spouses

passengers %>% mutate(FamSize = Parch + SibSp)

# (e) Group by Sex and calculate mean Fare and total Survived

passengers %>% group_by(Sex) %>% summarise(meanFare = mean(Fare), numSurv = sum(Survived))

# Problem 5: 10th, 30th, 50th, 60th percentiles of skin

quantile(diabetes$skin, probs = c(0.1, 0.3, 0.5, 0.6))





