# ============================================================
# 02_eda.R
# Exploratory Data Analysis
# ============================================================

library(tidyverse)

# Run the data preparation script to create wine_data
source("code/01_import_clean_merge.R")


# ============================================================
# 1. Dataset Overview
# ============================================================

# Display the dimensions of the merged dataset
dim(wine_data)

# Display variable names
names(wine_data)

# Display the structure and variable types
str(wine_data)

# Preview the first few observations
head(wine_data)

# Display a general summary of all variables
summary(wine_data)


# ============================================================
# 2. Sample Distribution
# ============================================================

# Number of red and white wine observations
table(wine_data$wine_type)

# Proportion of red and white wine observations
prop.table(table(wine_data$wine_type))

# Number of observations for each quality score
table(wine_data$quality)

# Proportion of observations for each quality score
prop.table(table(wine_data$quality))

# Distribution of the binary higher-quality outcome
table(wine_data$higher_quality)

# Proportion of the binary higher-quality outcome
prop.table(table(wine_data$higher_quality))


# ============================================================
# 3. Summary Statistics for Continuous Variables
# ============================================================

wine_summary <- wine_data %>%
  summarise(
    across(
      where(is.numeric),
      list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd = ~ sd(.x, na.rm = TRUE),
        min = ~ min(.x, na.rm = TRUE),
        max = ~ max(.x, na.rm = TRUE)
      )
    )
  )

wine_summary


# ============================================================
# 4. Summary Statistics by Wine Type
# ============================================================

wine_summary_by_type <- wine_data %>%
  group_by(wine_type) %>%
  summarise(
    n = n(),
    mean_quality = mean(quality, na.rm = TRUE),
    sd_quality = sd(quality, na.rm = TRUE),
    mean_alcohol = mean(alcohol, na.rm = TRUE),
    sd_alcohol = sd(alcohol, na.rm = TRUE),
    mean_volatile_acidity = mean(
      `volatile acidity`,
      na.rm = TRUE
    ),
    sd_volatile_acidity = sd(
      `volatile acidity`,
      na.rm = TRUE
    ),
    mean_sulphates = mean(sulphates, na.rm = TRUE),
    sd_sulphates = sd(sulphates, na.rm = TRUE)
  )

wine_summary_by_type


# ============================================================
# 5. Distribution of Wine Quality
# ============================================================

quality_distribution_plot <- ggplot(
  wine_data,
  aes(x = factor(quality))
) +
  geom_bar() +
  labs(
    title = "Distribution of Wine Quality Scores",
    x = "Quality Score",
    y = "Number of Observations"
  ) +
  theme_minimal()

quality_distribution_plot


# ============================================================
# 6. Quality Distribution by Wine Type
# ============================================================

quality_by_type_plot <- ggplot(
  wine_data,
  aes(
    x = factor(quality),
    fill = wine_type
  )
) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of Wine Quality Scores by Wine Type",
    x = "Quality Score",
    y = "Number of Observations",
    fill = "Wine Type"
  ) +
  theme_minimal()

quality_by_type_plot


# ============================================================
# 7. Comparison of Quality Scores by Wine Type
# ============================================================

quality_boxplot <- ggplot(
  wine_data,
  aes(
    x = wine_type,
    y = quality,
    fill = wine_type
  )
) +
  geom_boxplot() +
  labs(
    title = "Wine Quality Scores by Wine Type",
    x = "Wine Type",
    y = "Quality Score"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"
  )

quality_boxplot


# ============================================================
# 8. Relationship Between Alcohol and Quality
# ============================================================

alcohol_quality_plot <- ggplot(
  wine_data,
  aes(
    x = alcohol,
    y = quality,
    color = wine_type
  )
) +
  geom_jitter(
    height = 0.12,
    alpha = 0.30
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Alcohol Content and Wine Quality",
    x = "Alcohol Content",
    y = "Quality Score",
    color = "Wine Type"
  ) +
  theme_minimal()

alcohol_quality_plot


# ============================================================
# 9. Relationship Between Volatile Acidity and Quality
# ============================================================

volatile_acidity_quality_plot <- ggplot(
  wine_data,
  aes(
    x = `volatile acidity`,
    y = quality,
    color = wine_type
  )
) +
  geom_jitter(
    height = 0.12,
    alpha = 0.30
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Volatile Acidity and Wine Quality",
    x = "Volatile Acidity",
    y = "Quality Score",
    color = "Wine Type"
  ) +
  theme_minimal()

volatile_acidity_quality_plot


# ============================================================
# 10. Relationship Between Sulphates and Quality
# ============================================================

sulphates_quality_plot <- ggplot(
  wine_data,
  aes(
    x = sulphates,
    y = quality,
    color = wine_type
  )
) +
  geom_jitter(
    height = 0.12,
    alpha = 0.30
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Sulphates and Wine Quality",
    x = "Sulphates",
    y = "Quality Score",
    color = "Wine Type"
  ) +
  theme_minimal()

sulphates_quality_plot


# ============================================================
# 11. Relationship Between Density and Quality
# ============================================================

density_quality_plot <- ggplot(
  wine_data,
  aes(
    x = density,
    y = quality,
    color = wine_type
  )
) +
  geom_jitter(
    height = 0.12,
    alpha = 0.30
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Density and Wine Quality",
    x = "Density",
    y = "Quality Score",
    color = "Wine Type"
  ) +
  theme_minimal()

density_quality_plot


# ============================================================
# 12. Relationship Between Residual Sugar and Quality
# ============================================================

residual_sugar_quality_plot <- ggplot(
  wine_data,
  aes(
    x = `residual sugar`,
    y = quality,
    color = wine_type
  )
) +
  geom_jitter(
    height = 0.12,
    alpha = 0.30
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  labs(
    title = "Relationship Between Residual Sugar and Wine Quality",
    x = "Residual Sugar",
    y = "Quality Score",
    color = "Wine Type"
  ) +
  theme_minimal()

residual_sugar_quality_plot


# ============================================================
# 13. Correlation Matrix for Continuous Variables
# ============================================================

wine_numeric <- wine_data %>%
  select(
    `fixed acidity`,
    `volatile acidity`,
    `citric acid`,
    `residual sugar`,
    chlorides,
    `free sulfur dioxide`,
    `total sulfur dioxide`,
    density,
    pH,
    sulphates,
    alcohol,
    quality
  )

correlation_matrix <- cor(
  wine_numeric,
  use = "complete.obs"
)

round(correlation_matrix, 2)


# ============================================================
# 14. Correlations With Quality
# ============================================================

quality_correlations <- correlation_matrix[, "quality"]

quality_correlations <- sort(
  quality_correlations,
  decreasing = TRUE
)

quality_correlations


# ============================================================
# 15. Optional: Save Figures
# ============================================================

# Before running this section, make sure a figures folder exists.

# ggsave(
#   filename = "figures/quality_distribution.png",
#   plot = quality_distribution_plot,
#   width = 8,
#   height = 5
# )

# ggsave(
#   filename = "figures/quality_by_type.png",
#   plot = quality_by_type_plot,
#   width = 8,
#   height = 5
# )

# ggsave(
#   filename = "figures/alcohol_quality_relationship.png",
#   plot = alcohol_quality_plot,
#   width = 8,
#   height = 5
# )