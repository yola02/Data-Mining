# HW2
# Name: Yola Charara

# ============================================

# Problem 2

compute_distances <- function(vec1, vec2) {
  # Manhattan distance
  manhattan <- sum(abs(vec1 - vec2))
  
  # Euclidean distance
  euclidean <- sqrt(sum((vec1 - vec2)^2))
  
  return(list(
    manhattan = manhattan,
    euclidean = euclidean
  ))
}

# Example vectors
v1 <- c(4, 10, 8, 1)
v2 <- c(6, 3, 4, 9)

# Compute distances
distances <- compute_distances(v1, v2)

# Display results
cat("Vector 1:", v1, "\n")
cat("Vector 2:", v2, "\n")
cat("Manhattan Distance:", distances$manhattan, "\n")
cat("Euclidean Distance:", round(distances$euclidean, 3), "\n")

# ========================================================

# Problem 4

# Load built-in dataset
data(mtcars)

# Calculate correlation
correlation <- cor(mtcars$mpg, mtcars$wt)
cat("Correlation between mpg and wt:", round(correlation, 3), "\n")

# Scatter plot
plot(mtcars$wt, mtcars$mpg)

# ======================================================

#Problem 5

# Load library
library(tidyverse)

# Load the dataset
metabolite <- read.csv("~/Downloads/metabolite.csv")

# Remove columns with more than 75% missing values
missing_rate <- colMeans(is.na(metabolite)) * 100
metabolite_clean <- metabolite[, missing_rate <= 75]

# Replace remaining missing values with the column median
for (col in names(metabolite_clean)) {
  if (is.numeric(metabolite_clean[[col]])) {
    med <- median(metabolite_clean[[col]], na.rm = TRUE)
    metabolite_clean[[col]][is.na(metabolite_clean[[col]])] <- med
  }
}

# Checking that no missing values remain (output should be 0)
sum(is.na(metabolite_clean))

# ================================================================

# Problem 6

# Exclude Label column from PCA
features <- metabolite_clean %>% select(-Label)

# Apply PCA
pca_result <- prcomp(features, scale. = TRUE)

# Combine PCA results with labels
pca_df <- data.frame(pca_result$x[, 1:2], Label = metabolite_clean$Label)

# Scatter plot using first two principal components
plot(pca_df$PC1, pca_df$PC2,
     col = as.factor(pca_df$Label),
     pch = 19,
     xlab = "Principal Component 1",
     ylab = "Principal Component 2",
     main = "PCA Scatter Plot Colored by Label")

legend("topright", legend = unique(pca_df$Label),
       col = unique(as.factor(pca_df$Label)), pch = 19)
