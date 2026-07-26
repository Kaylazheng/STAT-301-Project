library(broom)
library(tidyverse)
library(car)

## Justifications of the choices of the models
# Response variable: higher_quality (binary: 1 = quality >= 6, 0 = otherwise).
# Since the outcome is binary, linear regression isn't appropriate - it can predict
# probabilities outside [0, 1] and violates constant variance assumptions.
# Logistic regression models log-odds instead, keeping predictions between 0 and 1.

## Step 1: Fit logistic regression with all predictors
# remove quality, since higher_quality is derived directly from it
wine2 <- wine_data %>%
  select(-quality)

logit_wine <- glm(higher_quality ~ ., data = wine2, family = binomial)

# exponentiate = TRUE converts log-odds coefficients into odds ratios (OR),
# which are easier to interpret than raw logistic coefficients
logit_wine_results <- tidy(logit_wine, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 3)
logit_wine_results
# interpretation: OR > 1 means the variable increases the odds of high quality,
# OR < 1 means it decreases the odds, holding all other variables constant

## Step 2: Check multicollinearity
vif(logit_wine)
# density has by far the highest VIF (~18.6), meaning it's highly correlated
# with other predictors (this matches what we saw in the MLR model too).
# high multicollinearity inflates standard errors and makes coefficients
# unstable, so we remove density and refit.

## Step 3: Refit without density
logit_wine1 <- glm(higher_quality ~ . - density, data = wine2, family = binomial)
logit_wine1_results <- tidy(logit_wine1, exponentiate = TRUE, conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 3)
logit_wine1_results
# after removing density: fixed acidity and pH become non-significant, while
# citric acid becomes significant.

vif(logit_wine1)
# wine_type VIF drops from ~7.47 to ~5.57 once density is removed. it's still
# the largest, but we keep wine_type in the model since it's central to the
# interaction question we test in Step 5.

## Step 4: Assess model fit
# (a) McFadden's pseudo R-squared
# unlike MLR, there's no R-squared for logistic regression
# McFadden's pseudo R2 instead compares the log-likelihood of our fitted model 
# to an intercept-only ("null") model
# "how much better do our predictors do than guessing
# the overall average rate of high quality for every wine?"
null_model <- glm(higher_quality ~ 1, data = wine2, family = binomial)
pseudo_r2 <- 1 - logLik(logit_wine1) / logLik(null_model)
pseudo_r2
# values of 0.2-0.4 are considered a good fit for McFadden's R2

# (b) Confusion matrix and classification accuracy
# how often does the model correctly classify a wine as high or low quality?
pred_prob <- fitted(logit_wine1)
pred_class <- ifelse(pred_prob > 0.5, 1, 0)

confusion_matrix <- table(predicted = pred_class, actual = wine2$higher_quality)
confusion_matrix
# rows = what the model predicted, columns = the true class
# diagonal entries = correct predictions, off-diagonal = misclassifications

accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy
# proportion of wines correctly classified overall

## Step 5: Test for interaction with wine_type
# research question: does the relationship between chemical properties and
# the odds of high quality differ between red and white wine? 
logit_interaction_wine <- glm(
  higher_quality ~ (. - density - wine_type) * wine_type,
  data = wine2,
  family = binomial
)
logit_interaction_results <- tidy(logit_interaction_wine, exponentiate = TRUE, 
                                  conf.int = TRUE) %>%
  mutate_if(is.numeric, round, 3)
logit_interaction_results

anova(logit_wine1, logit_interaction_wine, test = "Chisq")
# result: deviance difference = 97.082 on 10 df, p < 2.2e-16
# since p < 0.05, the interaction terms is statistically significant.