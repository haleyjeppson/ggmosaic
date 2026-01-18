# Side-by-Side Comparison: vcd vs ggmosaic
# ==========================================
#
# This script creates matching plots using both vcd and ggmosaic
# for direct visual comparison

library(vcd)
library(ggmosaic)
library(ggplot2)

# Prepare data
data(titanic, package = "ggmosaic")
titanic_table <- xtabs(~ Class + Sex + Survived, data = titanic)

# ==============================================================================
# Comparison 1: Independence Model (Class + Sex + Survived)
# ==============================================================================

cat("=== Comparison 1: Independence Model ===\n\n")

# VCD version
par(mfrow = c(1, 2))
mosaic(titanic_table,
       expected = ~ Class + Sex + Survived,
       main = "VCD: Independence Model\n~ Class + Sex + Survived",
       gp = shading_max,
       legend = TRUE)

# Extract residuals for display
titanic_df <- as.data.frame(titanic_table)
names(titanic_df)[4] <- "Freq"
mod1 <- glm(Freq ~ Class + Sex + Survived, data = titanic_df, family = poisson)
titanic_df$resid <- residuals(mod1, type = "pearson")

cat("VCD Residuals for Independence Model:\n")
print(titanic_df[, c("Class", "Sex", "Survived", "Freq", "resid")])

# ggmosaic version
p1 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = "independence") +
  scale_fill_residual() +
  labs(title = "ggmosaic: Independence Model",
       subtitle = "expected = 'independence'") +
  theme_mosaic()

# Display ggmosaic plot
par(mfrow = c(1, 1))
print(p1)

cat("\n\nPress [Enter] to continue to next comparison...")
readline()

# ==============================================================================
# Comparison 2: Custom Formula ~ Class + Sex
# ==============================================================================

cat("\n=== Comparison 2: Custom Formula ~ Class + Sex ===\n\n")

# VCD version
par(mfrow = c(1, 2))
mosaic(titanic_table,
       expected = ~ Class + Sex,
       main = "VCD: Model ~ Class + Sex",
       gp = shading_max,
       legend = TRUE)

# Extract residuals
mod2 <- glm(Freq ~ Class + Sex, data = titanic_df, family = poisson)
titanic_df$resid2 <- residuals(mod2, type = "pearson")

cat("VCD Residuals for ~ Class + Sex:\n")
print(titanic_df[, c("Class", "Sex", "Survived", "Freq", "resid2")])

# ggmosaic version
p2 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = ~ Class + Sex) +
  scale_fill_residual() +
  labs(title = "ggmosaic: Model ~ Class + Sex",
       subtitle = "expected = ~ Class + Sex") +
  theme_mosaic()

par(mfrow = c(1, 1))
print(p2)

cat("\n\nPress [Enter] to continue to next comparison...")
readline()

# ==============================================================================
# Comparison 3: Just Class and Sex (2-way table)
# ==============================================================================

cat("\n=== Comparison 3: Two-way table (Class × Sex) ===\n\n")

titanic_2way <- xtabs(~ Class + Sex, data = titanic)

# VCD version
par(mfrow = c(1, 2))
mosaic(titanic_2way,
       expected = ~ Class + Sex,
       main = "VCD: Class × Sex\nIndependence Model",
       gp = shading_max,
       legend = TRUE)

# Extract residuals
df_2way <- as.data.frame(titanic_2way)
names(df_2way)[3] <- "Freq"
mod3 <- glm(Freq ~ Class + Sex, data = df_2way, family = poisson)
df_2way$resid <- residuals(mod3, type = "pearson")

cat("VCD Residuals for Class × Sex:\n")
print(df_2way)

# ggmosaic version
p3 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  labs(title = "ggmosaic: Class × Sex",
       subtitle = "expected = 'independence'") +
  theme_mosaic()

par(mfrow = c(1, 1))
print(p3)

cat("\n\nPress [Enter] to continue to next comparison...")
readline()

# ==============================================================================
# Comparison 4: Saturated Model (should show ~0 residuals)
# ==============================================================================

cat("\n=== Comparison 4: Saturated Model ===\n\n")

# VCD version
par(mfrow = c(1, 2))
mosaic(titanic_table,
       expected = ~ Class * Sex * Survived,
       main = "VCD: Saturated Model\n(all interactions)",
       gp = shading_max,
       legend = TRUE)

# ggmosaic version
p4 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = "saturated") +
  scale_fill_residual() +
  labs(title = "ggmosaic: Saturated Model",
       subtitle = "expected = 'saturated'") +
  theme_mosaic()

par(mfrow = c(1, 1))
print(p4)

# ==============================================================================
# Summary
# ==============================================================================

cat("\n\n=== Validation Checklist ===\n\n")
cat("Compare the vcd and ggmosaic plots:\n\n")
cat("✓ Do the same cells show positive residuals (red)?\n")
cat("✓ Do the same cells show negative residuals (blue)?\n")
cat("✓ Are the magnitudes similar (color intensity)?\n")
cat("✓ Does the saturated model show near-zero residuals (mostly white)?\n")
cat("\nNote: Exact colors will differ due to different color scales,\n")
cat("but the pattern of positive/negative should match.\n")
