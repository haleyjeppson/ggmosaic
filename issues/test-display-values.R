# Testing display_values Parameter in geom_mosaic_text()
# ========================================================
#
# This demonstrates the new functionality for displaying
# observed counts, expected values, and residuals in cells

library(ggmosaic)
library(ggplot2)

data(titanic)

# Example 1: Display Observed Counts
# -----------------------------------
cat("Example 1: Display observed counts in cells\n\n")

p1 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "observed",
                   format_digits = 0) +
  labs(title = "Titanic: Class × Sex with Observed Counts",
       subtitle = "display_values = 'observed'") +
  theme_mosaic()

print(p1)

cat("\nPress [Enter] for next example...")
readline()

# Example 2: Display Residuals from Independence Model
# -----------------------------------------------------
cat("\nExample 2: Display Pearson residuals with residual shading\n\n")

p2 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   expected = "independence",  # Must specify in both layers!
                   display_values = "residual",
                   format_digits = 2,
                   colour = "black",
                   size = 3) +
  labs(title = "Titanic: Class × Sex - Independence Model",
       subtitle = "Residuals shown in cells with diverging color scale") +
  theme_mosaic()

print(p2)

cat("\nPress [Enter] for next example...")
readline()

# Example 3: Display Expected Values
# -----------------------------------
cat("\nExample 3: Display expected values from model\n\n")

p3 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   expected = "independence",
                   display_values = "expected",
                   format_digits = 1,
                   colour = "black") +
  labs(title = "Titanic: Expected Values Under Independence",
       subtitle = "display_values = 'expected'") +
  theme_mosaic()

print(p3)

cat("\nPress [Enter] for next example...")
readline()

# Example 4: Factor Labels (Default)
# -----------------------------------
cat("\nExample 4: Traditional factor labels (default behavior)\n\n")

p4 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   expected = "independence",
                   display_values = "label",  # default
                   colour = "black") +
  labs(title = "Titanic: With Factor Labels",
       subtitle = "display_values = 'label' (default)") +
  theme_mosaic()

print(p4)

cat("\nPress [Enter] for next example...")
readline()

# Example 5: Three-way Table with Residuals
# ------------------------------------------
cat("\nExample 5: Three-way table with residuals\n\n")

p5 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex, Survived)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 1,
                   colour = "white",
                   size = 2.5) +
  labs(title = "Titanic: Class × Sex × Survived",
       subtitle = "Residuals from complete independence model") +
  theme_mosaic()

print(p5)

cat("\nPress [Enter] for next example...")
readline()

# Example 6: Custom Model with Observed Counts
# ---------------------------------------------
cat("\nExample 6: Custom model (~ Class + Sex) with observed counts\n\n")

p6 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = ~ Class + Sex) +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex, Survived)),
                   expected = ~ Class + Sex,
                   display_values = "observed",
                   format_digits = 0,
                   colour = "black",
                   size = 2) +
  labs(title = "Titanic: Model ~ Class + Sex",
       subtitle = "Observed counts shown, colored by residuals") +
  theme_mosaic()

print(p6)

cat("\nPress [Enter] for next example...")
readline()

# Example 7: Comparison - Observed vs Expected
# ---------------------------------------------
cat("\nExample 7: Side-by-side comparison of observed vs expected\n\n")

p7a <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   expected = "independence",
                   display_values = "observed",
                   format_digits = 0) +
  labs(title = "Observed Counts",
       subtitle = "Actual data") +
  theme_mosaic()

p7b <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   expected = "independence",
                   display_values = "expected",
                   format_digits = 1) +
  labs(title = "Expected Counts",
       subtitle = "Under independence model") +
  theme_mosaic()

# Display side by side (requires gridExtra or patchwork)
if (requireNamespace("gridExtra", quietly = TRUE)) {
  gridExtra::grid.arrange(p7a, p7b, ncol = 2)
} else {
  print(p7a)
  cat("\nShowing expected plot...\n")
  readline()
  print(p7b)
}

cat("\nPress [Enter] to continue...")
readline()

# Example 8: Using with repel and as.label
# -----------------------------------------
cat("\nExample 8: Residuals with repel labels\n\n")

p8 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 2,
                   repel = TRUE,
                   as.label = TRUE,
                   size = 3) +
  labs(title = "Titanic: Residuals with Repelled Labels",
       subtitle = "Using repel = TRUE, as.label = TRUE") +
  theme_mosaic()

print(p8)

# Summary
cat("\n\n=== Summary ===\n\n")
cat("The display_values parameter allows you to show:\n\n")
cat("1. 'label' (default): Factor labels (original behavior)\n")
cat("2. 'observed': Observed counts (.n column)\n")
cat("3. 'expected': Expected values under fitted model\n")
cat("4. 'residual': Pearson residuals\n\n")
cat("Use format_digits to control decimal places (default: 1)\n\n")
cat("This feature is especially useful for:\n")
cat("- Displaying counts in cells (like vcd::labeling_values)\n")
cat("- Showing residual magnitudes alongside color shading\n")
cat("- Comparing observed vs expected values\n")
cat("- Creating publication-ready annotated mosaic plots\n")
