library(tidyverse)

# ------------------------------------------------------------
# 1. Import the red and white wine datasets
# ------------------------------------------------------------
wine_red <- read_delim(
  "data/winequality-red.csv",
  delim = ";"
) %>%
  mutate(wine_type = "Red")

wine_white <- read_delim(
  "data/winequality-white.csv",
  delim = ";"
) %>%
  mutate(wine_type = "White")

# ------------------------------------------------------------
# 2. Merge the two datasets and adjust variable types
# ------------------------------------------------------------
prop.table(table(wine_data$quality))

wine_data <- bind_rows(wine_red, wine_white) %>%
  mutate(
    wine_type = factor(wine_type),
    quality = as.integer(quality),
    # Create a binary outcome for logistic regression
    # Wines with a quality score of 6 or above are classified as high quality.
    higher_quality = if_else(quality >= 6, 1L, 0L)
  )

# ------------------------------------------------------------
# 3. Check for missing values and duplicated observations
# ------------------------------------------------------------
colSums(is.na(wine_data))
sum(duplicated(wine_data))

# ------------------------------------------------------------
# 4. Verify the merged dataset
# ------------------------------------------------------------
dim(wine_data)
table(wine_data$wine_type)
table(wine_data$quality)
table(wine_data$higher_quality)
prop.table(table(wine_data$quality))

# Preview the datasets
View(wine_red)
View(wine_white)
View(wine_data)