# =========================================================
# MCS 206 Final Course Output
# Project Title: Route Cost Impact: Airline Ticket Prices and Fuel-Related Costs
#
# Description:
# This script investigates whether fuel-related costs are associated
# with airline ticket prices. It performs:
#   1. package loading and project setup
#   2. data import and cleaning
#   3. feature engineering
#   4. exploratory data analysis
#   5. correlation analysis
#   6. regression modeling
#   7. model diagnostics and evaluation
#   8. export of figures, tables, and model outputs
#
# Notes:
# - The main response variable is total_ticket_price_usd.
# - The main explanatory variable of interest is total_fuel_cost_usd.
# - Additional predictors are included to control for operational
#   and contextual factors that may also influence ticket pricing.
# =========================================================


# 1. LOADING AND SETUP ----------------------------------------------------

## 1.1 Load required packages
# These packages support data wrangling, visualization,
# modeling, and file path management.
packages <- c(
  "tidyverse",
  "tidymodels",
  "broom",
  "here"
)

installed <- rownames(installed.packages())

for (p in packages) {
  if (!(p %in% installed)) install.packages(p, dependencies = TRUE)
  library(p, character.only = TRUE)
}

## 1.2 Create output folders
# These folders store exported figures, tables, and model objects
# so that results are organized outside the script console.
dir.create(here("output"), showWarnings = FALSE)
dir.create(here("output", "figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output", "tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(here("output", "models"), showWarnings = FALSE, recursive = TRUE)

## 1.3 Import data
# Read the dataset from the project data folder.
df_raw <- read_csv(here("data", "route_cost_impact.csv"))

## 1.4 Global options
# Apply a clean plotting theme and reduce scientific notation
# to make outputs easier to read.
theme_set(theme_minimal())
options(scipen = 999)


# 2. DATA CLEANING AND FEATURE ENGINEERING -------------------------------

## 2.1 Convert data types
# Convert the month column into a proper Date variable so that
# time-based summaries and plots work correctly.
#
# Convert categorical variables into factors so they are treated
# as groups in plots and as categorical predictors in regression.
df <- df_raw |>
  mutate(
    month = as.Date(paste0(month, "-01")),
    across(
      c(
        conflict_phase, airline, iata_code,
        origin_city, destination_city, aircraft_type,
        rerouted, flight_cancelled
      ),
      as.factor
    )
  )

## 2.2 Filter invalid observations
# Remove rows with zero or negative values in variables where such
# values would make the analysis invalid or misleading.
#
# Reasons:
# - actual_distance_km must be positive to compute per-km measures
# - total_ticket_price_usd must be positive because ticket price is
#   the main response variable
# - estimated_passengers must be positive to compute revenue per passenger
# - total_fuel_cost_usd must be positive to avoid division-by-zero
#   when calculating fuel_pass_through
df <- df |>
  filter(
    actual_distance_km > 0,
    total_ticket_price_usd > 0,
    estimated_passengers > 0,
    total_fuel_cost_usd > 0
  )

## 2.3 Create derived variables
# These engineered variables make the analysis more interpretable by
# standardizing route-level costs and revenues.
#
# route:
#   readable route label combining origin and destination
# ticket_price_per_km:
#   allows price comparison across short and long routes
# fuel_cost_per_km:
#   standardizes fuel cost by travel distance
# fuel_pass_through:
#   measures how much fuel cost is reflected in the fuel surcharge
# revenue_per_passenger:
#   approximates revenue generated per passenger on a route
df <- df |>
  mutate(
    route = paste(origin_city, destination_city, sep = " -> "),
    ticket_price_per_km = total_ticket_price_usd / actual_distance_km,
    fuel_cost_per_km = total_fuel_cost_usd / actual_distance_km,
    fuel_pass_through = fuel_surcharge_usd / total_fuel_cost_usd,
    revenue_per_passenger = route_revenue_usd / estimated_passengers
  )

## 2.4 Data quality summary
# Store a compact summary table for quick inspection.
data_summary <- tibble(
  n_rows = nrow(df),
  n_columns = ncol(df),
  missing_ticket_price = sum(is.na(df$total_ticket_price_usd)),
  missing_fuel_cost = sum(is.na(df$total_fuel_cost_usd)),
  missing_distance = sum(is.na(df$actual_distance_km)),
  missing_passengers = sum(is.na(df$estimated_passengers))
)

write_csv(data_summary, here("output", "tables", "data_summary.csv"))


# 3. EXPLORATORY DATA ANALYSIS -------------------------------------------

## 3.1 Distribution of ticket prices
# This histogram checks whether ticket prices are concentrated,
# spread out, skewed, or affected by extreme values.
p1 <- ggplot(df, aes(x = total_ticket_price_usd)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Total Ticket Prices",
    x = "Total Ticket Price (USD)",
    y = "Count"
  )

ggsave(
  filename = here("output", "figures", "01_distribution_ticket_prices.png"),
  plot = p1,
  width = 8,
  height = 5,
  dpi = 300
)

## 3.2 Fuel cost vs ticket price
# This plot examines the central research relationship:
# whether higher fuel costs are associated with higher ticket prices.
# Coloring by rerouting status helps reveal whether disrupted flights
# follow a different price pattern.
p2 <- ggplot(df, aes(x = total_fuel_cost_usd, y = total_ticket_price_usd, color = rerouted)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Fuel Cost vs Ticket Price",
    x = "Total Fuel Cost (USD)",
    y = "Total Ticket Price (USD)",
    color = "Rerouted"
  )

ggsave(
  filename = here("output", "figures", "02_fuel_cost_vs_ticket_price.png"),
  plot = p2,
  width = 8,
  height = 5,
  dpi = 300
)

## 3.3 Distance vs ticket price
# This plot checks whether longer routes tend to have higher ticket prices,
# which is important because distance may confound the effect of fuel cost.
p3 <- ggplot(df, aes(x = actual_distance_km, y = total_ticket_price_usd, color = rerouted)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Distance vs Ticket Price",
    x = "Actual Distance (km)",
    y = "Total Ticket Price (USD)",
    color = "Rerouted"
  )

ggsave(
  filename = here("output", "figures", "03_distance_vs_ticket_price.png"),
  plot = p3,
  width = 8,
  height = 5,
  dpi = 300
)

## 3.4 Ticket price by rerouting status
# This boxplot compares the distribution of ticket prices between
# rerouted and non-rerouted flights.
p4 <- ggplot(df, aes(x = rerouted, y = total_ticket_price_usd, fill = rerouted)) +
  geom_boxplot() +
  labs(
    title = "Ticket Price by Rerouting Status",
    x = "Rerouted",
    y = "Total Ticket Price (USD)"
  )

ggsave(
  filename = here("output", "figures", "04_ticket_price_by_rerouting.png"),
  plot = p4,
  width = 8,
  height = 5,
  dpi = 300
)

## 3.5 Fuel cost by conflict phase
# This boxplot shows how fuel costs vary across conflict phases.
# This matters because external conflict conditions may influence
# fuel prices, route planning, and airline operating conditions.
p5 <- ggplot(df, aes(x = conflict_phase, y = total_fuel_cost_usd, fill = rerouted)) +
  geom_boxplot() +
  coord_flip() +
  labs(
    title = "Fuel Cost by Conflict Phase",
    x = "Conflict Phase",
    y = "Total Fuel Cost (USD)",
    fill = "Rerouted"
  )

ggsave(
  filename = here("output", "figures", "05_fuel_cost_by_conflict_phase.png"),
  plot = p5,
  width = 8,
  height = 6,
  dpi = 300
)

## 3.6 Monthly average ticket price and fuel price
# This plot compares average monthly ticket price and average monthly
# jet fuel price. Fuel price is rescaled for visual comparison only.
monthly_summary <- df |>
  group_by(month) |>
  summarise(
    avg_ticket = mean(total_ticket_price_usd, na.rm = TRUE),
    avg_fuel = mean(jet_fuel_usd_barrel, na.rm = TRUE),
    .groups = "drop"
  )

write_csv(monthly_summary, here("output", "tables", "monthly_summary.csv"))

p6 <- ggplot(monthly_summary, aes(x = month)) +
  geom_line(aes(y = avg_ticket), linewidth = 1) +
  geom_line(aes(y = avg_fuel * 10), linewidth = 1) +
  labs(
    title = "Monthly Average Ticket Price and Fuel Price",
    subtitle = "Fuel price is rescaled for visual comparison",
    x = "Month",
    y = "Value"
  )

ggsave(
  filename = here("output", "figures", "06_monthly_ticket_and_fuel_trends.png"),
  plot = p6,
  width = 9,
  height = 5,
  dpi = 300
)


# 4. CORRELATION ANALYSIS ------------------------------------------------

## 4.1 Select key numeric variables
# These variables are chosen to inspect linear relationships among
# ticket price, fuel-related measures, distance, revenue, and demand.
numeric_df <- df |>
  select(
    total_ticket_price_usd,
    total_fuel_cost_usd,
    fuel_surcharge_usd,
    actual_distance_km,
    extra_distance_km,
    fuel_consumption_bbl,
    jet_fuel_usd_barrel,
    estimated_passengers,
    route_revenue_usd,
    fuel_pct_of_cost,
    ticket_price_per_km,
    fuel_cost_per_km,
    fuel_pass_through,
    revenue_per_passenger
  ) |>
  mutate(across(everything(), ~ ifelse(is.infinite(.x), NA, .x)))

## 4.2 Compute correlation matrix
cor_matrix <- cor(numeric_df, use = "complete.obs")

## 4.3 Save correlation matrix as a table
cor_table <- as.data.frame(cor_matrix) |>
  rownames_to_column("variable")

write_csv(cor_table, here("output", "tables", "correlation_matrix.csv"))

## 4.4 Create correlation heatmap
cor_df <- as.data.frame(cor_matrix) |>
  rownames_to_column("var1") |>
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

p7 <- ggplot(cor_df, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile() +
  geom_text(aes(label = round(correlation, 2)), size = 3) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Correlation Heatmap of Key Numeric Variables",
    x = NULL,
    y = NULL
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(
  filename = here("output", "figures", "07_correlation_heatmap.png"),
  plot = p7,
  width = 10,
  height = 8,
  dpi = 300
)


# 5. MODELING ------------------------------------------------------------

## 5.1 Train-test split
# Split the data so the model is trained on one subset and evaluated
# on a separate unseen subset. This gives a more honest measure of
# predictive performance.
set.seed(123)

split <- initial_split(df, prop = 0.8)
train <- training(split)
test <- testing(split)

## 5.2 Define model
# A multiple linear regression model is used because the response
# variable is continuous and the goal is to estimate how ticket price
# changes with fuel-related and operational predictors.
lm_model <- linear_reg() |>
  set_engine("lm")

## 5.3 Model formula
# Variable selection rationale:
#
# total_fuel_cost_usd
#   Main explanatory variable of interest. The project investigates
#   whether higher fuel-related route costs are associated with
#   higher ticket prices.
#
# fuel_surcharge_usd
#   Represents a direct pricing mechanism through which airlines may
#   pass fuel costs on to passengers.
#
# actual_distance_km
#   Controls for route length, since longer flights usually require
#   more fuel and often have higher ticket prices.
#
# rerouted
#   Captures operational disruption. Rerouted flights may travel
#   farther or incur higher costs.
#
# conflict_phase
#   Represents external conditions that may influence fuel prices,
#   route efficiency, and airline operations.
#
# airline
#   Controls for airline-specific pricing strategies, market position,
#   and operating structure.
#
# estimated_passengers
#   Serves as a proxy for route demand and scale.
lm_formula <- total_ticket_price_usd ~
  total_fuel_cost_usd +
  fuel_surcharge_usd +
  actual_distance_km +
  rerouted +
  conflict_phase +
  airline +
  estimated_passengers

## 5.4 Fit the model
lm_fit <- lm_model |>
  fit(lm_formula, data = train)

## 5.5 Save fitted model object
saveRDS(lm_fit, here("output", "models", "linear_reg_model.rds"))


# 6. MODEL EVALUATION ----------------------------------------------------

## 6.1 Generate test-set predictions
# Predictions are created using unseen test data to evaluate how well
# the fitted model generalizes beyond the training sample.
predictions <- predict(lm_fit, test) |>
  bind_cols(test) |>
  mutate(
    residual = total_ticket_price_usd - .pred,
    abs_error = abs(total_ticket_price_usd - .pred)
  )

write_csv(predictions, here("output", "tables", "test_predictions.csv"))

## 6.2 Compute performance metrics
# These metrics summarize predictive accuracy:
# - RMSE: penalizes larger errors more heavily
# - MAE: average absolute prediction error
# - R-squared: proportion of variance explained
model_metrics <- metrics(predictions, truth = total_ticket_price_usd, estimate = .pred)

write_csv(model_metrics, here("output", "tables", "model_metrics.csv"))

## 6.3 Save coefficient table
coef_table <- tidy(lm_fit)
write_csv(coef_table, here("output", "tables", "model_coefficients.csv"))

## 6.4 Save glance table
# glance() provides overall model fit statistics such as R-squared,
# adjusted R-squared, sigma, AIC, BIC, and sample size.
model_summary <- glance(lm_fit)
write_csv(model_summary, here("output", "tables", "model_summary.csv"))


# 7. DIAGNOSTICS ---------------------------------------------------------

## 7.1 Residuals vs predicted values
# This checks whether residuals are randomly scattered around zero.
# A clear pattern may suggest nonlinearity, unequal variance,
# or model misspecification.
p8 <- ggplot(predictions, aes(x = .pred, y = residual)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Residuals vs Predicted Ticket Price",
    x = "Predicted Ticket Price (USD)",
    y = "Residual"
  )

ggsave(
  filename = here("output", "figures", "08_residuals_vs_predicted.png"),
  plot = p8,
  width = 8,
  height = 5,
  dpi = 300
)

## 7.2 Residual distribution
# This histogram checks whether residuals are roughly centered around zero.
# Strong skewness or extreme tails may indicate poor model fit or outliers.
p9 <- ggplot(predictions, aes(x = residual)) +
  geom_histogram(bins = 30, fill = "gray70", color = "white") +
  labs(
    title = "Distribution of Residuals",
    x = "Residual",
    y = "Count"
  )

ggsave(
  filename = here("output", "figures", "09_residual_distribution.png"),
  plot = p9,
  width = 8,
  height = 5,
  dpi = 300
)

## 7.3 Actual vs predicted values
# This plot compares observed ticket prices with model predictions.
# Points closer to the 45-degree line indicate better predictive accuracy.
p10 <- ggplot(predictions, aes(x = total_ticket_price_usd, y = .pred)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    title = "Actual vs Predicted Ticket Prices",
    x = "Actual Ticket Price (USD)",
    y = "Predicted Ticket Price (USD)"
  )

ggsave(
  filename = here("output", "figures", "10_actual_vs_predicted.png"),
  plot = p10,
  width = 8,
  height = 5,
  dpi = 300
)

## 7.4 Largest prediction errors
# This table highlights the cases where the model performed worst.
# Reviewing these can help identify unusual routes or outliers.
largest_errors <- predictions |>
  select(
    month, airline, origin_city, destination_city,
    total_ticket_price_usd, .pred, residual, abs_error
  ) |>
  arrange(desc(abs_error)) |>
  slice_head(n = 10)

write_csv(largest_errors, here("output", "tables", "largest_prediction_errors.csv"))


# 8. SHORT WRITTEN INTERPRETATION ----------------------------------------

# Interpretation guide:
#
# 1. If the coefficient of total_fuel_cost_usd is positive, this suggests
#    that higher fuel costs are associated with higher ticket prices,
#    holding the other predictors constant.
#
# 2. If fuel_surcharge_usd is also positive, this supports the idea that
#    some fuel-related costs are passed on directly to passengers.
#
# 3. If actual_distance_km is positive, longer routes tend to have
#    higher ticket prices after controlling for the other variables.
#
# 4. Differences across airline, rerouted status, and conflict_phase
#    indicate that ticket pricing is influenced not only by fuel costs
#    but also by operational and contextual conditions.
#
# 5. The model metrics and diagnostic plots should be reviewed together.
#    A reasonable R-squared and low prediction error support model usefulness,
#    while random residual scatter and a roughly centered residual distribution
#    suggest that the linear model is acceptable.
#
# 6. If strong residual patterns, large outliers, or weak predictive accuracy
#    appear, the results should be interpreted with caution and may suggest
#    that additional variables or a different model form are needed.


# 9. PRINT KEY OUTPUTS TO CONSOLE ----------------------------------------

print(data_summary)
print(model_metrics)
print(coef_table)
print(model_summary)
print(largest_errors)