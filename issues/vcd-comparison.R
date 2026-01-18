# VCD Package Comparison for Residual Shading
# =============================================
#http://127.0.0.1:12714/graphics/plot_zoom_png?width=1315&height=956
# This script creates mosaic plots using vcd/vcdExtra for comparison
# with the ggmosaic implementation

library(vcd)
library(vcdExtra)

# ==============================================================================
# Part 1: Using datasets::Titanic (4-way contingency table)
# ==============================================================================

cat("=== Part 1: datasets::Titanic (4-way array) ===\n\n")

data(Titanic, package = "datasets")

cat("Structure of datasets::Titanic:\n")
str(Titanic)

cat("\n--- Test 1a: Independence model (Class, Sex, Survived) ---\n")
cat("Formula: ~ Class + Sex + Survived (all independent)\n\n")

# Create a 3-way table by marginalizing over Age
titanic_3way <- margin.table(Titanic, margin = c(1, 2, 4))  # Class, Sex, Survived
cat("3-way table dimensions:\n")
print(dim(titanic_3way))
print(titanic_3way)

# Mosaic plot with independence model
# In vcd, the expected parameter specifies the model
par(mfrow = c(1, 1))
mosaic(titanic_3way,
       expected = ~ Class + Sex + Survived,
       main = "VCD: Independence Model (Class + Sex + Survived)",
       gp = shading_max,
       legend = TRUE)

cat("\n--- Test 1b: Custom model (Class and Sex independent, Survived separate) ---\n")
cat("Formula: ~ Class + Sex\n")
cat("This tests if Survived is independent of Class and Sex\n\n")

mosaic(titanic_3way,
       expected = ~ Class + Sex,
       main = "VCD: Model ~ Class + Sex",
       gp = shading_max,
       legend = TRUE)

cat("\n--- Test 1c: Interaction model (Class:Sex + Survived) ---\n")
cat("Formula: ~ Class * Sex + Survived\n\n")

mosaic(titanic_3way,
       expected = ~ Class * Sex + Survived,
       main = "VCD: Model ~ Class * Sex + Survived",
       gp = shading_max,
       legend = TRUE)

cat("\n--- Test 1d: Saturated model (should show no residuals) ---\n")
cat("Formula: ~ Class * Sex * Survived\n\n")

mosaic(titanic_3way,
       expected = ~ Class * Sex * Survived,
       main = "VCD: Saturated Model (all interactions)",
       gp = shading_max,
       legend = TRUE)

# ==============================================================================
# Part 2: Using ggmosaic::titanic (case form data)
# ==============================================================================

cat("\n\n=== Part 2: ggmosaic::titanic (case form) ===\n\n")

data(titanic, package = "ggmosaic")

cat("Structure of ggmosaic::titanic:\n")
str(titanic)
cat("\nFirst few rows:\n")
print(head(titanic))

# Convert to contingency table
titanic_table <- xtabs(~ Class + Sex + Survived, data = titanic)

cat("\nContingency table from ggmosaic::titanic:\n")
print(titanic_table)

cat("\n--- Test 2a: Independence model ---\n")
cat("Formula: ~ Class + Sex + Survived\n\n")

mosaic(titanic_table,
       expected = ~ Class + Sex + Survived,
       main = "VCD: ggmosaic::titanic - Independence Model",
       gp = shading_max,
       legend = TRUE)

cat("\n--- Test 2b: Custom model (Class + Sex) ---\n")
cat("Formula: ~ Class + Sex\n\n")

mosaic(titanic_table,
       expected = ~ Class + Sex,
       main = "VCD: ggmosaic::titanic - Model ~ Class + Sex",
       gp = shading_max,
       legend = TRUE)

# ==============================================================================
# Part 3: Using vcdExtra::mosaic.glm() for more control
# ==============================================================================

cat("\n\n=== Part 3: Using vcdExtra::mosaic.glm() ===\n\n")

# First, convert table to data frame for GLM fitting
titanic_df <- as.data.frame(titanic_table)
names(titanic_df)[4] <- "Freq"  # xtabs names the count column "Freq"

cat("Data frame for GLM:\n")
print(head(titanic_df))

cat("\n--- Test 3a: Fit independence model with glm() ---\n")

# Fit Poisson GLM for independence
mod_indep <- glm(Freq ~ Class + Sex + Survived,
                 data = titanic_df,
                 family = poisson)

cat("\nModel summary:\n")
print(summary(mod_indep))

cat("\nDeviance table:\n")
print(anova(mod_indep, test = "Chisq"))

# Plot using mosaic.glm
mosaic(mod_indep,
       main = "VCD: mosaic.glm() - Independence Model",
       gp = shading_max,
       legend = TRUE)

cat("\n--- Test 3b: Fit model ~ Class + Sex ---\n")

mod_cs <- glm(Freq ~ Class + Sex,
              data = titanic_df,
              family = poisson)

cat("\nModel summary:\n")
print(summary(mod_cs))

mosaic(mod_cs,
       main = "VCD: mosaic.glm() - Model ~ Class + Sex",
       gp = shading_max,
       legend = TRUE)

cat("\n--- Test 3c: Fit interaction model ---\n")

mod_interact <- glm(Freq ~ Class * Sex + Survived,
                    data = titanic_df,
                    family = poisson)

cat("\nModel summary:\n")
print(summary(mod_interact))

mosaic(mod_interact,
       main = "VCD: mosaic.glm() - Model ~ Class * Sex + Survived",
       gp = shading_max,
       legend = TRUE)

# ==============================================================================
# Part 4: Extract and display residuals for validation
# ==============================================================================

cat("\n\n=== Part 4: Residual Comparison ===\n\n")

cat("--- Independence Model Residuals ---\n")
titanic_df$expected <- fitted(mod_indep)
titanic_df$pearson_resid <- residuals(mod_indep, type = "pearson")

cat("\nData with fitted values and Pearson residuals:\n")
print(titanic_df)

cat("\nSummary of Pearson residuals:\n")
print(summary(titanic_df$pearson_resid))

cat("\nResiduals by combination:\n")
print(titanic_df[order(-abs(titanic_df$pearson_resid)),
                  c("Class", "Sex", "Survived", "Freq", "expected", "pearson_resid")])

# ==============================================================================
# Part 5: Different shading schemes
# ==============================================================================

cat("\n\n=== Part 5: Different Shading Schemes ===\n\n")

par(mfrow = c(2, 2))

cat("Comparing shading schemes for independence model:\n\n")

# Default shading
mosaic(titanic_table,
       expected = ~ Class + Sex + Survived,
       main = "Default Shading",
       legend = TRUE)

# Friendly shading
mosaic(titanic_table,
       expected = ~ Class + Sex + Survived,
       main = "Friendly Shading",
       gp = shading_Friendly,
       legend = TRUE)

# Max shading (used above)
mosaic(titanic_table,
       expected = ~ Class + Sex + Survived,
       main = "Max Shading",
       gp = shading_max,
       legend = TRUE)

# HSV shading
mosaic(titanic_table,
       expected = ~ Class + Sex + Survived,
       main = "HSV Shading",
       gp = shading_hsv,
       legend = TRUE)

par(mfrow = c(1, 1))

# ==============================================================================
# Part 6: Interpretation Guide
# ==============================================================================

cat("\n\n=== Interpretation Guide ===\n\n")

cat("Color interpretation in vcd::mosaic():\n")
cat("- Blue tiles: Fewer observations than expected (negative residuals)\n")
cat("- White/grey tiles: Observed matches expected (residuals near 0)\n")
cat("- Red tiles: More observations than expected (positive residuals)\n")
cat("- Color intensity: Magnitude of residual (stronger color = larger |residual|)\n")
cat("\nSignificance levels (shading_max):\n")
cat("- |residual| < 2: Light shading (not significant)\n")
cat("- |residual| > 2: Medium shading (p < 0.05)\n")
cat("- |residual| > 4: Dark shading (p < 0.001)\n")

cat("\n\nExpected values and residuals are calculated as:\n")
cat("- Expected: fitted values from Poisson GLM\n")
cat("- Pearson residuals: (Observed - Expected) / sqrt(Expected)\n")

cat("\n\n=== Comparison Summary ===\n\n")
cat("These vcd plots should match the ggmosaic implementation:\n")
cat("1. Same Pearson residuals for the same models\n")
cat("2. Similar color scheme (blue=negative, red=positive)\n")
cat("3. Same interpretation of residual magnitudes\n\n")

cat("Key differences:\n")
cat("- vcd uses base graphics, ggmosaic uses ggplot2\n")
cat("- vcd has multiple shading schemes, ggmosaic uses one diverging scale\n")
cat("- Exact colors may differ, but the pattern should be the same\n\n")

cat("To validate ggmosaic implementation:\n")
cat("1. Compare residual values in the data frame above\n")
cat("2. Check that strong positive residuals (red in vcd) match red in ggmosaic\n")
cat("3. Check that strong negative residuals (blue in vcd) match blue in ggmosaic\n")
cat("4. Verify independence model shows same pattern of deviations\n")
