# ============================================================
# 02_eda.R
# Exploratory Data Analysis
# ============================================================

library(tidyverse)
library(corrplot)

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
# 4. Distribution of Wine Quality
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
# 5. Relationship Between Alcohol and Quality
# ============================================================

alcohol_quality_plot <- ggplot(
  wine_data,
  aes(
    x = alcohol,
    y = quality
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
    y = "Quality Score"
  ) +
  theme_minimal()

alcohol_quality_plot


# ============================================================
# 6. Relationship Between Volatile Acidity and Quality
# ============================================================

volatile_acidity_quality_plot <- ggplot(
  wine_data,
  aes(
    x = `volatile acidity`,
    y = quality
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
    y = "Quality Score"
  ) +
  theme_minimal()

volatile_acidity_quality_plot



# ============================================================
# 7. Relationship Between Density and Quality
# ============================================================

density_quality_plot <- ggplot(
  wine_data,
  aes(
    x = density,
    y = quality
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
    y = "Quality Score"
  ) +
  theme_minimal()

density_quality_plot


# ============================================================
# 8. Distribution of Predictors
# ============================================================

main_predictor_boxplots <- wine_data %>%
  select(
    alcohol,
    `volatile acidity`,
    density
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "value"
  ) %>%
  ggplot(
    aes(
      x = variable,
      y = value
    )
  ) +
  geom_boxplot() +
  facet_wrap(
    ~ variable,
    scales = "free"
  ) +
  labs(
    title = "Distributions of Main Continuous Predictors",
    x = NULL,
    y = "Value"
  ) +
  theme_minimal()

main_predictor_boxplots


# ============================================================
# 9. Correlation Matrix for Continuous Variables
# ============================================================

wine_numeric <- wine_data %>%
  select(
    `volatile acidity`,
    density,
    alcohol,
    quality
  )

correlation_matrix <- cor(
  wine_numeric,
  use = "complete.obs"
)

round(correlation_matrix, 2)


# ============================================================
# 10. Correlation Heatmap
# ============================================================

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.cex = 0.7,
  addCoef.col = "black",
  number.cex = 0.5
)


# ============================================================
# 11. Correlation with the Binary Higher-Quality Outcome
# ============================================================

wine_logistic_numeric <- wine_data %>%
  select(
    `volatile acidity`,
    density,
    alcohol,
    higher_quality
  )

higher_quality_correlations <- cor(
  wine_logistic_numeric,
  use = "complete.obs"
)[, "higher_quality"]

higher_quality_correlations <-
  higher_quality_correlations[
    names(higher_quality_correlations) != "higher_quality"
  ]

higher_quality_correlations <- sort(
  higher_quality_correlations,
  decreasing = TRUE
)

round(higher_quality_correlations, 2)


# ============================================================
# 12. Plot Correlations with Higher Quality
# ============================================================

higher_quality_correlation_data <- tibble(
  variable = names(higher_quality_correlations),
  correlation = as.numeric(higher_quality_correlations)
)

higher_quality_correlation_plot <-
  higher_quality_correlation_data %>%
  ggplot(
    aes(
      x = reorder(variable, correlation),
      y = correlation
    )
  ) +
  geom_col() +
  coord_flip() +
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  labs(
    title = "Correlation of Wine Characteristics with Higher Quality",
    x = "Wine Characteristic",
    y = "Correlation with Higher Quality"
  ) +
  theme_minimal()

higher_quality_correlation_plot


# ============================================================
# 13. Save Figures
# ============================================================

dir.create(
  "figures",
  showWarnings = FALSE,
  recursive = TRUE
)

# Quality distribution
ggsave(
  "figures/quality_distribution.png",
  plot = quality_distribution_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# Alcohol vs quality
ggsave(
  "figures/alcohol_vs_quality.png",
  plot = alcohol_quality_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# Volatile acidity vs quality
ggsave(
  "figures/volatile_acidity_vs_quality.png",
  plot = volatile_acidity_quality_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# Density vs quality
ggsave(
  "figures/density_vs_quality.png",
  plot = density_quality_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# Predictor distributions
ggsave(
  "figures/main_predictor_boxplots.png",
  plot = main_predictor_boxplots,
  width = 10,
  height = 7,
  dpi = 300
)

# Correlation heatmap
png(
  "figures/correlation_heatmap.png",
  width = 1800,
  height = 1500,
  res = 200
)

corrplot(
  correlation_matrix,
  method = "color",
  type = "upper",
  tl.col = "black",
  tl.cex = 0.7,
  addCoef.col = "black",
  number.cex = 0.5
)

dev.off()

# Correlations with the binary higher-quality outcome
ggsave(
  "figures/higher_quality_correlations.png",
  plot = higher_quality_correlation_plot,
  width = 8,
  height = 5,
  dpi = 300
)

