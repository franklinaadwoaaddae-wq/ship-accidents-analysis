# ============================================================
# SRM 611 - Project 1
# Ship Accidents: Data Diagnostics and Model Repair
# ============================================================

# Required package
library(lmtest)

# Load the data
# ShipAccidents.csv was provided by the SRM 611 course professor.
# Place the file in a folder named "data" before running the analysis.
ship <- read.csv("data/ShipAccidents.csv")


# ============================================================
# Section 2: Description and Exploration of the Data
# ============================================================

# ------------------------------------------------------------
# 2.1 Basic Data Information
# ------------------------------------------------------------

# Dimensions of the dataset
dim(ship)

# Variable names
names(ship)

# Structure of the dataset
str(ship)

# Summary statistics
summary(ship)


# ------------------------------------------------------------
# 2.2 Missing Values
# ------------------------------------------------------------

# Number of missing values for each variable
colSums(is.na(ship))

# Percentage of missing values for each variable
round(colMeans(is.na(ship)) * 100, 2)

# Display observations with missing exposure
ship[is.na(ship$exposure), ]


# ------------------------------------------------------------
# 2.3 Descriptive Statistics for Main Quantitative Variables
# ------------------------------------------------------------

# Accidents
c(
  n = sum(!is.na(ship$accidents)),
  mean = mean(ship$accidents, na.rm = TRUE),
  sd = sd(ship$accidents, na.rm = TRUE),
  median = median(ship$accidents, na.rm = TRUE),
  min = min(ship$accidents, na.rm = TRUE),
  max = max(ship$accidents, na.rm = TRUE)
)

# Exposure
c(
  n = sum(!is.na(ship$exposure)),
  mean = mean(ship$exposure, na.rm = TRUE),
  sd = sd(ship$exposure, na.rm = TRUE),
  median = median(ship$exposure, na.rm = TRUE),
  min = min(ship$exposure, na.rm = TRUE),
  max = max(ship$exposure, na.rm = TRUE)
)

# Service months
c(
  n = sum(!is.na(ship$service_months)),
  mean = mean(ship$service_months, na.rm = TRUE),
  sd = sd(ship$service_months, na.rm = TRUE),
  median = median(ship$service_months, na.rm = TRUE),
  min = min(ship$service_months, na.rm = TRUE),
  max = max(ship$service_months, na.rm = TRUE)
)


# ------------------------------------------------------------
# 2.4 Construction-Era Coding
# ------------------------------------------------------------

# Examine combinations of the three construction indicators
table(
  ship$construction1,
  ship$construction2,
  ship$construction3,
  useNA = "ifany"
)

# Sum of construction indicators for each observation
table(
  rowSums(
    ship[, c(
      "construction1",
      "construction2",
      "construction3"
    )]
  )
)

# Number of observations in the reference construction category
sum(
  ship$construction1 == 0 &
    ship$construction2 == 0 &
    ship$construction3 == 0
)


# ------------------------------------------------------------
# 2.5 Operational Status
# ------------------------------------------------------------

table(ship$operational, useNA = "ifany")


# ------------------------------------------------------------
# 2.6 Zero Accident Counts
# ------------------------------------------------------------

# Number of observations with zero accidents
sum(ship$accidents == 0)

# Percentage with zero accidents
round(mean(ship$accidents == 0) * 100, 2)


# ------------------------------------------------------------
# 2.7 Relationship Between Exposure and Accidents
# ------------------------------------------------------------

# Correlation using observations with non-missing exposure
cor(
  ship$exposure,
  ship$accidents,
  use = "complete.obs"
)


# ------------------------------------------------------------
# 2.8 Exploratory Visualizations
# ------------------------------------------------------------

# Distribution of accident counts
hist(
  ship$accidents,
  breaks = 10,
  main = "Distribution of Accident Counts",
  xlab = "Number of Accidents",
  ylab = "Frequency"
)

# Accidents versus exposure
plot(
  ship$exposure,
  ship$accidents,
  main = "Number of Accidents versus Exposure Score",
  xlab = "Exposure Score",
  ylab = "Number of Accidents",
  pch = 19
)

# Add a simple fitted line for exploratory purposes
abline(
  lm(accidents ~ exposure, data = ship),
  lwd = 2
)

# Create a readable construction-category variable
ship$construction <- factor(
  ifelse(
    ship$construction1 == 1,
    "Construction 1",
    ifelse(
      ship$construction2 == 1,
      "Construction 2",
      ifelse(
        ship$construction3 == 1,
        "Construction 3",
        "Reference"
      )
    )
  ),
  levels = c(
    "Reference",
    "Construction 1",
    "Construction 2",
    "Construction 3"
  )
)

# Accident counts by construction category
boxplot(
  accidents ~ construction,
  data = ship,
  main = "Accident Counts by Construction Category",
  xlab = "Construction Category",
  ylab = "Number of Accidents"
)


# ============================================================
# Section 3: Baseline Model and Diagnostics
# ============================================================

# ------------------------------------------------------------
# 3.1 Create the Analysis Dataset
# ------------------------------------------------------------

# Keep observations with complete data for all variables
# included in the baseline model
ship_complete <- ship[
  complete.cases(
    ship[, c(
      "accidents",
      "exposure",
      "construction1",
      "construction2",
      "construction3"
    )]
  ),
]

# Check the number of observations retained
dim(ship_complete)

# Confirm that there are no missing values
colSums(
  is.na(
    ship_complete[, c(
      "accidents",
      "exposure",
      "construction1",
      "construction2",
      "construction3"
    )]
  )
)


# ------------------------------------------------------------
# 3.2 Fit the Baseline Normal Linear Regression Model
# ------------------------------------------------------------

baseline_model <- lm(
  accidents ~ exposure +
    construction1 +
    construction2 +
    construction3,
  data = ship_complete
)

# Full regression results
summary(baseline_model)


# ------------------------------------------------------------
# 3.3 Confidence Intervals for Regression Coefficients
# ------------------------------------------------------------

confint(baseline_model)


# ------------------------------------------------------------
# 3.4 Overall Model ANOVA Table
# ------------------------------------------------------------

anova(baseline_model)


# ------------------------------------------------------------
# 3.5 Basic Model Fit Information
# ------------------------------------------------------------

# Number of observations used
nobs(baseline_model)

# R-squared
summary(baseline_model)$r.squared

# Adjusted R-squared
summary(baseline_model)$adj.r.squared

# Residual standard error
sigma(baseline_model)


# ------------------------------------------------------------
# 3.6 Residuals versus Fitted Values
# Check: Functional Form and Constant Variance
# ------------------------------------------------------------

plot(
  fitted(baseline_model),
  residuals(baseline_model),
  main = "Residuals versus Fitted Values",
  xlab = "Fitted Values",
  ylab = "Residuals",
  pch = 19
)

abline(h = 0, lty = 2)


# ------------------------------------------------------------
# 3.7 Normal Q-Q Plot
# Check: Normality of Residuals
# ------------------------------------------------------------

qqnorm(
  residuals(baseline_model),
  main = "Normal Q-Q Plot of Baseline Model Residuals",
  pch = 19
)

qqline(
  residuals(baseline_model),
  lwd = 2
)


# ------------------------------------------------------------
# 3.8 Scale-Location Plot
# Check: Constant Variance
# ------------------------------------------------------------

standardized_residuals <- rstandard(baseline_model)

plot(
  fitted(baseline_model),
  sqrt(abs(standardized_residuals)),
  main = "Scale-Location Plot",
  xlab = "Fitted Values",
  ylab = expression(sqrt("|Standardized Residuals|")),
  pch = 19
)


# ------------------------------------------------------------
# 3.9 Residuals versus Observation Order
# Check: Potential Independence Patterns
# ------------------------------------------------------------

plot(
  seq_along(residuals(baseline_model)),
  residuals(baseline_model),
  type = "b",
  pch = 19,
  main = "Residuals versus Observation Order",
  xlab = "Observation Order",
  ylab = "Residuals"
)

abline(h = 0, lty = 2)


# ------------------------------------------------------------
# 3.10 Standardized Residuals
# Check: Unusual Response Observations
# ------------------------------------------------------------

standardized_residuals

# Observations with |standardized residual| > 2
which(abs(standardized_residuals) > 2)


# ------------------------------------------------------------
# 3.11 Leverage
# Check: Unusual Predictor Values
# ------------------------------------------------------------

leverage_values <- hatvalues(baseline_model)
leverage_values

# Common screening guideline: 2p/n
# p includes the intercept
p <- length(coef(baseline_model))
n <- nobs(baseline_model)

leverage_cutoff <- 2 * p / n
leverage_cutoff

# Observations exceeding the guideline
which(leverage_values > leverage_cutoff)


# ------------------------------------------------------------
# 3.12 Cook's Distance
# Check: Potentially Influential Observations
# ------------------------------------------------------------

cooks_values <- cooks.distance(baseline_model)
cooks_values

# Common screening guideline: 4/n
cooks_cutoff <- 4 / n
cooks_cutoff

# Observations exceeding the guideline
which(cooks_values > cooks_cutoff)

plot(
  cooks_values,
  type = "h",
  main = "Cook's Distance",
  xlab = "Observation",
  ylab = "Cook's Distance"
)

abline(
  h = cooks_cutoff,
  lty = 2
)


# ------------------------------------------------------------
# 3.13 Identify Potentially Influential Observations
# ------------------------------------------------------------

influential_rows <- which(
  abs(standardized_residuals) > 2 |
    leverage_values > leverage_cutoff |
    cooks_values > cooks_cutoff
)

influential_rows
ship_complete[influential_rows, ]


# ------------------------------------------------------------
# 3.14 Shapiro-Wilk Test
# Formal Check of Residual Normality
# ------------------------------------------------------------

shapiro.test(
  residuals(baseline_model)
)


# ------------------------------------------------------------
# 3.15 Breusch-Pagan Test
# Formal Check of Constant Variance
# ------------------------------------------------------------

bptest(baseline_model)


# ============================================================
# Section 4: Investigation of Model Modification
# ============================================================

# ------------------------------------------------------------
# 4.1 Quadratic Pattern in Exposure
# ------------------------------------------------------------

quadratic_model <- lm(
  accidents ~ exposure + I(exposure^2) +
    construction1 + construction2 + construction3,
  data = ship_complete
)

summary(quadratic_model)

# Compare baseline and quadratic models
anova(baseline_model, quadratic_model)

# Model fit measures
c(
  Baseline_R2 = summary(baseline_model)$r.squared,
  Quadratic_R2 = summary(quadratic_model)$r.squared,
  Baseline_Adj_R2 = summary(baseline_model)$adj.r.squared,
  Quadratic_Adj_R2 = summary(quadratic_model)$adj.r.squared,
  Baseline_AIC = AIC(baseline_model),
  Quadratic_AIC = AIC(quadratic_model)
)


# ------------------------------------------------------------
# 4.2 Log-Transformed Response Model
# log(accidents + 1) is used because accidents contains zeros
# ------------------------------------------------------------

log_model <- lm(
  log(accidents + 1) ~ exposure +
    construction1 + construction2 + construction3,
  data = ship_complete
)

summary(log_model)


# ------------------------------------------------------------
# 4.3 Diagnostics for Log-Transformed Model
# ------------------------------------------------------------

# Residuals versus fitted values
plot(
  fitted(log_model),
  residuals(log_model),
  main = "Residuals versus Fitted Values: Log Model",
  xlab = "Fitted Values",
  ylab = "Residuals",
  pch = 19
)

abline(h = 0, lty = 2)

# Normal Q-Q plot
qqnorm(
  residuals(log_model),
  main = "Normal Q-Q Plot: Log Model",
  pch = 19
)

qqline(
  residuals(log_model),
  lwd = 2
)

# Scale-location plot
log_standardized <- rstandard(log_model)

plot(
  fitted(log_model),
  sqrt(abs(log_standardized)),
  main = "Scale-Location Plot: Log Model",
  xlab = "Fitted Values",
  ylab = expression(sqrt("|Standardized Residuals|")),
  pch = 19
)


# ------------------------------------------------------------
# 4.4 Formal Diagnostics for Log Model
# ------------------------------------------------------------

shapiro.test(residuals(log_model))
bptest(log_model)


# ------------------------------------------------------------
# 4.5 Influence Diagnostics for Log Model
# ------------------------------------------------------------

log_cooks <- cooks.distance(log_model)
log_cooks_cutoff <- 4 / nobs(log_model)

log_cooks_cutoff
which(log_cooks > log_cooks_cutoff)
log_cooks[log_cooks > log_cooks_cutoff]


# ------------------------------------------------------------
# 4.6 Diagnostics for Quadratic Model
# ------------------------------------------------------------

plot(
  fitted(quadratic_model),
  residuals(quadratic_model),
  main = "Residuals versus Fitted Values: Quadratic Model",
  xlab = "Fitted Values",
  ylab = "Residuals",
  pch = 19
)

abline(h = 0, lty = 2)

qqnorm(
  residuals(quadratic_model),
  main = "Normal Q-Q Plot: Quadratic Model",
  pch = 19
)

qqline(
  residuals(quadratic_model),
  lwd = 2
)


# ------------------------------------------------------------
# 4.7 Formal Diagnostics for Quadratic Model
# ------------------------------------------------------------

shapiro.test(residuals(quadratic_model))
bptest(quadratic_model)


# ============================================================
# Section 4: Final Model Comparison
# ============================================================

# ------------------------------------------------------------
# 4.8 Log Model with Quadratic Exposure Term
# ------------------------------------------------------------

log_quadratic_model <- lm(
  log(accidents + 1) ~ exposure + I(exposure^2) +
    construction1 + construction2 + construction3,
  data = ship_complete
)

summary(log_quadratic_model)


# ------------------------------------------------------------
# 4.9 Compare the Two Log-Response Models
# ------------------------------------------------------------

anova(log_model, log_quadratic_model)

c(
  Log_Adj_R2 = summary(log_model)$adj.r.squared,
  Log_Quadratic_Adj_R2 =
    summary(log_quadratic_model)$adj.r.squared,
  Log_AIC = AIC(log_model),
  Log_Quadratic_AIC = AIC(log_quadratic_model)
)


# ------------------------------------------------------------
# 4.10 Diagnostics for Log-Quadratic Model
# ------------------------------------------------------------

shapiro.test(
  residuals(log_quadratic_model)
)

bptest(
  log_quadratic_model
)


# ------------------------------------------------------------
# 4.11 Residual Plot for Log-Quadratic Model
# ------------------------------------------------------------

plot(
  fitted(log_quadratic_model),
  residuals(log_quadratic_model),
  main = "Residuals versus Fitted Values: Log-Quadratic Model",
  xlab = "Fitted Values",
  ylab = "Residuals",
  pch = 19
)

abline(h = 0, lty = 2)


# ------------------------------------------------------------
# 4.12 Q-Q Plot for Log-Quadratic Model
# ------------------------------------------------------------

qqnorm(
  residuals(log_quadratic_model),
  main = "Normal Q-Q Plot: Log-Quadratic Model",
  pch = 19
)

qqline(
  residuals(log_quadratic_model),
  lwd = 2
)


# ------------------------------------------------------------
# 4.13 Final Confidence Intervals
# ------------------------------------------------------------

confint(log_model)
confint(log_quadratic_model)


# ============================================================
# Section 4: Interpretation of the Final Model
# ============================================================

# ------------------------------------------------------------
# 4.14 Representative Exposure Values
# ------------------------------------------------------------

exposure_values <- c(
  4,
  6,
  8,
  10
)

# Predictions for the reference construction category
prediction_data <- data.frame(
  exposure = exposure_values,
  construction1 = 0,
  construction2 = 0,
  construction3 = 0
)

# Predicted values on the log(accidents + 1) scale
prediction_data$predicted_log <- predict(
  log_quadratic_model,
  newdata = prediction_data
)

# Back-transform to the original accident-count scale
prediction_data$predicted_accidents <-
  exp(prediction_data$predicted_log) - 1

prediction_data


# ------------------------------------------------------------
# 4.15 Estimated Turning Point of Exposure Curve
# ------------------------------------------------------------

b1 <- coef(log_quadratic_model)["exposure"]
b2 <- coef(log_quadratic_model)["I(exposure^2)"]

turning_point <- -b1 / (2 * b2)
turning_point


# ------------------------------------------------------------
# 4.16 Construction Effects on Multiplicative Scale
# ------------------------------------------------------------

exp(
  coef(log_quadratic_model)[
    c(
      "construction1",
      "construction2",
      "construction3"
    )
  ]
)
