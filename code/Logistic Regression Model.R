library(broom)
library(tidyverse)
library(car)
library(knitr)
library(modelr)


## Model 2: Logistic Regression Model
## Research Question
## ------------------------------------------------------------
# Investigate if alcohol content may be related to whether wine quality
# is high (>= 6) or not, adjusted for volatile acidity.
# y = 1 if quality >= 6, y = 0 if quality < 6 (higher_quality variable)
## ------------------------------------------------------------

## Step 1: Fit the model
logit_model <- glm(
  higher_quality ~ alcohol + `volatile acidity`,
  data = wine_data,
  family = binomial
)

logit_results <- tidy(logit_model, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 3)
logit_results

## Step 2: Multicollinearity
vif(logit_model)
# both VIFs are essentially 1 - no multicollinearity between alcohol and
# volatile acidity. 

## Step 3: Fitted values
fitted_probs <- fitted(logit_model)
summary(fitted_probs)

## Step 4: Residuals table
logit_model %>%
  augment() %>%
  dplyr::select(higher_quality, alcohol, `volatile acidity`, .fitted, .resid, .std.resid) %>%
  mutate(
    pred_prob          = logit_model$fitted,
    resid_raw          = residuals(logit_model, type = "response"),
    raw_byhand         = higher_quality - pred_prob,
    resid_deviance     = residuals(logit_model),
    resid_pearson      = residuals(logit_model, "pearson"),
    pearson_byhand     = resid_raw / sqrt(pred_prob * (1 - pred_prob)),
    resid_standardized = rstandard(logit_model)
  ) %>%
  mutate_if(is.numeric, round, 3) %>%
  head(3) %>%
  knitr::kable()

## Step 5: Residual plot

# raw residual
logit_model %>%
  augment() %>%
  mutate(
    pred_prob = logit_model$fitted,
    resid_raw = residuals(logit_model, type = "response")
  ) %>%
  ggplot(aes(x = pred_prob, y = resid_raw)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Raw Residuals vs Fitted Probabilities",
    x = "Fitted probability",
    y = "Raw residual"
  ) +
  theme_minimal()
# Upper line: wines where higher_quality = 1 → residual = 1 − fitted prob 
# (positive, shrinking toward 0 as fitted prob → 1)
# Lower line: wines where higher_quality = 0 → residual = 0 − fitted prob 
# (negative, shrinking toward 0 as fitted prob → 0)

# pearson residual
logit_model %>%
  augment() %>%
  mutate(
    pred_prob = logit_model$fitted,
    resid_pearson = residuals(logit_model, type = "pearson")
  ) %>%
  ggplot(aes(x = pred_prob, y = resid_pearson)) +
  geom_point(alpha = 0.3) +
  geom_smooth(se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  labs(
    title = "Pearson Residuals vs Fitted Probabilities",
    x = "Fitted probability",
    y = "Pearson residual"
  ) +
  theme_minimal()

y_resid <- residuals(logit_model)
x_fit <- logit_model$fitted

residual_plot <- binnedplot(x_fit, y_resid)
# The binned residual plot shows most bins falling within the expected 
# confidence bands, this means model fits overall 

## Step 6: Overdispersion check
dispersion_ratio <- summary(logit_model)$deviance / summary(logit_model)$df.residual
dispersion_ratio
# ratio very close to 1 -> no evidence of overdispersion

summary(glm(
  formula = higher_quality ~ alcohol + `volatile acidity`,
  data = wine_data,
  family = quasibinomial
))
# Dispersion parameter for quasibinomial family taken to be 0.998
# essentially 1, confirming the binomial variance assumption is appropriate.

## Step 7: Interaction model 
logit_interaction_av <- glm(
  higher_quality ~ alcohol * `volatile acidity`,
  data = wine_data,
  family = binomial
)

tidy(logit_interaction_av, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 3)
# The interaction term was statistically significant
# the positive effect of alcohol on high-quality odds becomes stronger as 
# volatile acidity increases
anova(logit_model, logit_interaction_av, test = "Chisq")

## bin volatile acidity into groups so we can color by it
va_levels <- quantile(wine_data$`volatile acidity`, probs = c(0.1, 0.5, 0.9))

pred_grid <- expand.grid(
  alcohol = seq(min(wine_data$alcohol), 
                max(wine_data$alcohol), 
                length.out = 100),
  `volatile acidity` = va_levels
)

pred_grid <- pred_grid %>%
  add_predictions(logit_interaction_av, 
                  var = "pred_logit_interaction", 
                  type = "response") %>%
  mutate(
    va_group = factor(`volatile acidity`, levels = va_levels,
                      labels = c("Low VA", "Median VA", "High VA"))
  )


wine_data <- wine_data %>%
  add_predictions(logit_interaction_av, 
                  var = "pred_logit_interaction", 
                  type = "response")

## plot
logit_interaction_plot <-
  pred_grid %>%
  ggplot(aes(x = alcohol, y = pred_logit_interaction, color = va_group)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Logistic Regression with Interaction",
    x = "Alcohol (%)",
    y = "Predicted Probability of High Quality",
    color = "Volatile Acidity"
  ) +
  theme(
    text = element_text(size = 10.5),
    plot.title = element_text(face = "bold"),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold")
  )

logit_interaction_plot
