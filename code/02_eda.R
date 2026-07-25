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
# 9. Alcohol-Quality Correlation by Wine Type
# ============================================================

alcohol_quality_by_type <- wine_data %>%
  group_by(wine_type) %>%
  summarise(
    n = n(),
    correlation = cor(
      alcohol,
      quality,
      use = "complete.obs"
    )
  )

alcohol_quality_by_type


# ============================================================
# 10. Relationship Between Volatile Acidity and Quality
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
# 11. Relationship Between Sulphates and Quality
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
# 12. Relationship Between Density and Quality
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
# 13. Relationship Between Residual Sugar and Quality
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



# ============================================================
# 13A. Distribution of Main Predictors
# ============================================================

main_predictor_boxplots <- wine_data %>%
  select(
    alcohol,
    `volatile acidity`,
    sulphates,
    density,
    `residual sugar`
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
# 14. Correlation Matrix for Continuous Variables
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
# 15. Correlation Heatmap
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
# 16. Correlation with the Binary Higher-Quality Outcome
# ============================================================

wine_logistic_numeric <- wine_data %>%
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
# 17. Plot Correlations with Higher Quality
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
# 18. Save Figures
# ============================================================

dir.create(
  "figures",
  showWarnings = FALSE,
  recursive = TRUE
)

ggsave(
  "figures/quality_by_wine_type.png",
  plot = quality_by_type_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/alcohol_vs_quality.png",
  plot = alcohol_quality_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/volatile_acidity_vs_quality.png",
  plot = volatile_acidity_quality_plot,
  width = 8,
  height = 5,
  dpi = 300
)

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

ggsave(
  "figures/quality_distribution.png",
  plot = quality_distribution_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/quality_boxplot.png",
  plot = quality_boxplot,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/sulphates_vs_quality.png",
  plot = sulphates_quality_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/density_vs_quality.png",
  plot = density_quality_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/residual_sugar_vs_quality.png",
  plot = residual_sugar_quality_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/main_predictor_boxplots.png",
  plot = main_predictor_boxplots,
  width = 10,
  height = 7,
  dpi = 300
)

ggsave(
  "figures/higher_quality_correlations.png",
  plot = higher_quality_correlation_plot,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  "figures/higher_quality_correlations.png",
  plot = higher_quality_correlation_plot,
  width = 8,
  height = 5,
  dpi = 300
)

dev.off()


