# Loglinear Model Residual Shading Examples for ggmosaic
# =========================================================
#
# This script demonstrates the new residual shading functionality
# added to geom_mosaic() for visualizing deviations from loglinear models.

library(ggmosaic)
library(ggplot2)

# Load example data
data(titanic)
data(happy)

# Example 1: Independence Model (Shortcut)
# -----------------------------------------
# Tests complete independence between Class and Sex
# Blue = fewer than expected, Red = more than expected under independence

p1 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  labs(title = "Titanic: Class × Sex",
       subtitle = "Independence model (shortcut)") +
  theme_mosaic()

print(p1)

# Example 2: Custom Formula
# --------------------------
# Test a specific loglinear model using formula syntax
# Model: Class and Sex are independent (conditioning on nothing)

p2 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = ~ Class + Sex) +
  scale_fill_residual() +
  labs(title = "Titanic: Class × Sex × Survived",
       subtitle = "Model: ~ Class + Sex (no interaction)") +
  theme_mosaic()

print(p2)

# Example 3: Saturated Model
# ---------------------------
# Saturated model should show all residuals ≈ 0
# This is a diagnostic check - if you see non-zero residuals, something is wrong

p3 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "saturated") +
  scale_fill_residual() +
  labs(title = "Titanic: Saturated Model",
       subtitle = "Should show residuals near zero") +
  theme_mosaic()

print(p3)

# Example 4: Conditional Independence
# ------------------------------------
# Tests if health and marital status are independent given sex
# Uses the conditional shortcut

p4 <- ggplot(data = happy) +
  geom_mosaic(aes(weight = wtssall,
                  x = product(health, marital),
                  conds = sex),
              expected = "conditional",
              na.rm = TRUE) +
  scale_fill_residual() +
  labs(title = "Happy: Health × Marital | Sex",
       subtitle = "Conditional independence model") +
  theme_mosaic()

print(p4)

# Example 5: Custom Color Scale
# ------------------------------
# Customize the diverging color scheme

p5 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual(low = "dodgerblue4",
                      mid = "gray95",
                      high = "darkred",
                      limits = c(-5, 5),
                      name = "Pearson\nResid.") +
  labs(title = "Titanic: Class × Survived",
       subtitle = "Custom color scheme") +
  theme_mosaic()

print(p5)

# Example 6: Manual Residual Mapping
# -----------------------------------
# Explicitly map residuals to fill aesthetic (override auto-mapping)
# Useful when you want more control over the scale

p6 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = after_stat(.residual)),
              expected = "independence") +
  scale_fill_gradient2(low = "purple", mid = "white", high = "orange",
                       midpoint = 0,
                       name = "Custom\nResidual") +
  labs(title = "Titanic: Manual Residual Mapping",
       subtitle = "Using after_stat(.residual)") +
  theme_mosaic()

print(p6)

# Example 7: Comparing with Existing Fill Aesthetic
# --------------------------------------------------
# When both expected and fill are specified, fill takes precedence
# But residuals are still calculated and available

p7a <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived),
              expected = "independence") +
  labs(title = "Fill aesthetic specified",
       subtitle = "Residuals calculated but fill shows Survived") +
  theme_mosaic()

print(p7a)

# Compare with automatic residual mapping (no fill specified)
p7b <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  labs(title = "No fill aesthetic",
       subtitle = "Residuals automatically mapped to fill") +
  theme_mosaic()

print(p7b)

# Example 8: Three-way Table
# ---------------------------
# More complex contingency table with three factors

p8 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Age)),
              expected = "independence") +
  scale_fill_residual() +
  labs(title = "Titanic: Class × Sex × Age",
       subtitle = "Complete independence model") +
  theme_mosaic()

print(p8)

# Example 9: Interaction Model
# -----------------------------
# Test a model with one interaction term

p9 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = ~ Class * Sex + Survived) +
  scale_fill_residual() +
  labs(title = "Titanic: Testing Survived independence",
       subtitle = "Model: Class * Sex + Survived") +
  theme_mosaic()

print(p9)

# Example 10: Weighted Data
# --------------------------
# Works with weighted survey data (like happy dataset)

p10 <- ggplot(data = happy) +
  geom_mosaic(aes(weight = wtssall,
                  x = product(happy, health)),
              expected = "independence",
              na.rm = TRUE) +
  scale_fill_residual() +
  labs(title = "Happy: Happiness × Health",
       subtitle = "Weighted data with survey weights") +
  theme_mosaic()

print(p10)

# Interpretation Guide
# ====================
#
# Pearson Residuals interpretation:
# - Residual ≈ 0: Observed count matches expected count under the model
# - Residual > 0 (RED): More observations than expected (positive association)
# - Residual < 0 (BLUE): Fewer observations than expected (negative association)
# - |Residual| > 2: Statistically significant deviation at ~5% level
# - |Residual| > 3: Strong evidence against the model
#
# Model Shortcuts:
# - "independence": All variables are independent (main effects only)
# - "saturated": Perfect fit (all possible interactions)
# - "conditional": Conditional independence (requires conds aesthetic)
#
# Formula Syntax:
# - ~ A + B: Main effects only (independence)
# - ~ A * B: Main effects + interaction (equivalent to ~ A + B + A:B)
# - ~ A + B + C + A:B: Main effects + one specific interaction
