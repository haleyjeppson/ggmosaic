# Debug Script: Understanding the Model Fitting Issue
# =====================================================

library(dplyr)

# Load the titanic data
data(titanic, package = "ggmosaic")

cat("=== Titanic Data Structure ===\n")
cat("Dimensions:", dim(titanic), "\n")
cat("First few rows:\n")
print(head(titanic))
cat("\nStructure:\n")
str(titanic)

# The ggmosaic::titanic is in case form (each row = 1 passenger)
# Let's create a contingency table like prodcalc() does

cat("\n=== Creating Contingency Table ===\n")

# This mimics what margin() does in prodcalc
contingency <- titanic %>%
  group_by(Class, Sex) %>%
  summarise(.n = n(), .groups = "drop")

print(contingency)
cat("\nContingency table dimensions:", dim(contingency), "\n")

# Now fit the independence model
cat("\n=== Fitting Independence Model ===\n")
cat("Formula: .n ~ Class + Sex\n\n")

model <- glm(.n ~ Class + Sex, data = contingency, family = poisson())

cat("Model summary:\n")
print(summary(model))

cat("\n=== Predicted Values ===\n")
contingency$.expected <- predict(model, type = "response")
print(contingency)

cat("\n=== Pearson Residuals ===\n")
contingency$.residual <- (contingency$.n - contingency$.expected) / sqrt(contingency$.expected)
print(contingency)

cat("\n=== Residual Summary ===\n")
print(summary(contingency$.residual))

# Now let's trace what happens in the actual ggmosaic pipeline
cat("\n\n=== Tracing ggmosaic Pipeline ===\n")

# Simulate what prodcalc does
library(ggmosaic)

# Parse the formula
test_formula <- as.formula("weight ~ x__Class + x__Sex")
vars <- parse_product_formula(test_formula)
cat("Parsed variables:\n")
print(str(vars))

# Create test data with the x__ prefixes (like stat_mosaic does)
test_data <- titanic %>%
  rename(x__Class = Class, x__Sex = Sex) %>%
  mutate(weight = 1)

cat("\nTest data with prefixes:\n")
print(head(test_data))

# Now aggregate it (mimicking what margin() does)
aggregated <- test_data %>%
  group_by(x__Class, x__Sex) %>%
  summarise(.wt = sum(weight), .groups = "drop") %>%
  rename(.n = .wt)

cat("\nAggregated data:\n")
print(aggregated)
cat("Dimensions:", dim(aggregated), "\n")

# Try to fit the model with the x__ prefixed names
cat("\n=== Fitting with prefixed variable names ===\n")
model2 <- glm(.n ~ x__Class + x__Sex, data = aggregated, family = poisson())
print(summary(model2))

aggregated$.expected <- predict(model2, type = "response")
aggregated$.residual <- (aggregated$.n - aggregated$.expected) / sqrt(aggregated$.expected)

cat("\nAggregated data with residuals:\n")
print(aggregated)

# Now simulate the divide() operation which creates multiple levels
cat("\n\n=== Simulating divide() with multiple levels ===\n")

# divide() creates parent and child rectangles
# Let's create a simplified version
multilevel_data <- bind_rows(
  # Level 1: top-level rectangles (by Class only)
  aggregated %>%
    group_by(x__Class) %>%
    summarise(.n = sum(.n), .groups = "drop") %>%
    mutate(level = 1),

  # Level 2: leaf-level rectangles (by Class x Sex)
  aggregated %>%
    mutate(level = 2)
)

cat("Multi-level data (simulating divide output):\n")
print(multilevel_data)
cat("Dimensions:", dim(multilevel_data), "\n")

# This is what gets passed to fit_loglinear_model!
# Now let's see what happens when we try to fit and join

cat("\n=== Testing fit_loglinear_model logic ===\n")

# Select vars for the model
vars_to_use <- c("x__Class", "x__Sex")

# Create mod_data (this is what the function does)
mod_data <- multilevel_data %>%
  select(all_of(c(vars_to_use, ".n"))) %>%
  distinct()

cat("mod_data after distinct:\n")
print(mod_data)
cat("Dimensions:", dim(mod_data), "\n")

# Fit the model
model3 <- glm(.n ~ x__Class + x__Sex, data = mod_data, family = poisson())

# Get predictions
mod_data$.expected <- predict(model3, type = "response")
mod_data$.residual <- (mod_data$.n - mod_data$.expected) / sqrt(mod_data$.expected)

cat("\nmod_data with predictions:\n")
print(mod_data)

# Now try the join
cat("\n=== Testing the join operation ===\n")
cat("Original multilevel_data dimensions:", dim(multilevel_data), "\n")
cat("mod_data dimensions:", dim(mod_data), "\n")

result <- multilevel_data %>%
  left_join(
    mod_data[, c(vars_to_use, ".expected", ".residual")],
    by = vars_to_use
  )

cat("\nResult after join:\n")
print(result)
cat("Dimensions:", dim(result), "\n")

cat("\n=== DIAGNOSIS ===\n")
if (nrow(result) == nrow(multilevel_data)) {
  cat("✓ Join successful! Same number of rows.\n")
} else {
  cat("✗ Join changed number of rows!\n")
}

# Check for NAs in residuals
if (any(is.na(result$.residual))) {
  cat("⚠ Warning: Some residuals are NA (expected for parent-level rows)\n")
  cat("Rows with NA residuals:\n")
  print(result[is.na(result$.residual), ])
}

cat("\n=== Summary ===\n")
cat("The issue is likely that:\n")
cat("1. prodcalc() returns data with multiple levels (parent + leaf rectangles)\n")
cat("2. fit_loglinear_model() receives this multi-level data\n")
cat("3. When we distinct() by variables + .n, we get fewer rows\n")
cat("4. The join should work, but something is failing\n")
cat("\nCheck if the actual error is in the column selection or assignment.\n")
