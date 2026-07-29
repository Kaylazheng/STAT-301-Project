library(broom)
library(tidyverse)
library(car）

## Justifications of the choices of the models
# Our response variable is quality, which is a numeric/continuous variable.
# In this analysis, we treated quality as a continuous variable, then we use multiple linear regression model 
# to analyze the relationship between 12 input variables and quality variable.

## Parameter estimates: Model1 - Multiple Linear regression model
# We use alcohol, density, chlorides and volatile acidity as predictors.
# The response variable is quality.

# select data except higher_quality variable, this variable will be used for the second model.
wine1 <- wine_data %>%
  select(-higher_quality)

# model for all potential input variables:
MLR_wine <- lm(quality ~ ., wine1)
MLR_wine_results <- tidy(MLR_wine) %>%
  mutate_if(is.numeric, round, 3)
MLR_wine_results
# all variables are associated with the response variable except citric acid variable.

## Model Diagnostics

## 1.Residual plot (for Linearity and Normality Assumption)
options(repr.plot.width = 8, repr.plot.height = 6)
plot(MLR_wine, 1)
# the residuals are around line r=0 and don't have any pattern, so the model fits data well with constant variance of errors.

## 2.Q-Q plot (for Constant Variance Assumption)
plot(MLR_wine, 2)
# Although there are some outliers on the tails, the points still roughly on an straight line.
# For the large sample size, the normality assumption is adequately satisfied.

## 3.Multicollinearity
# Use VIF to check the multicollinearity
vif(MLR_wine)
# density variable has the largest VIF, so we need to ignore this one.

# model except density variable
MLR_wine1 <- lm(quality ~ .-density, wine1)
MLR_wine1_results <- tidy(MLR_wine1) %>%
  mutate_if(is.numeric, round, 3)
MLR_wine1_results
# all variables are associated with the response variable except fixed acidity, citric acid and pH variables.

#Use VIF to check the multicollinearity
vif(MLR_wine1)
# wine_type has the largest VIF, but it's between 5 and 10 and this variable is useful for us to find the interaction,
# so we don't ignore this variable, and the multicollinearity disappears

## Model with interaction
# Since we have one categorical variable, so we need to check whether there is a interaction term.
MLR_interaction_wine <- lm(quality ~ (. - density - wine_type)*wine_type, wine1)
summary(MLR_interaction_wine)
# the relationship between variables fixed acidity, volatile acidity, total sulfur dioxide, pH, sulphates, alcohol and quality depend on the wine type.

## 