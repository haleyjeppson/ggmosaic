# Quick Test Script for Residual Shading Implementation
# ========================================================
#
# Run this script to verify the basic functionality works.
# This is a minimal test - see residual-shading-examples.R for comprehensive examples.

# Load the package (assumes you're in the package directory)
devtools::load_all()

# Load ggplot2 if not already loaded
library(ggplot2)

# Test 1: Basic independence model with titanic data
# ---------------------------------------------------
cat("Test 1: Independence model with titanic data...\n")

data(titanic)

# fit the glm model directly
#

p1 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  labs(title = "Test 1: Independence Model",
       subtitle = "Should show diverging colors (blue/white/red)") +
  theme_mosaic()

print(p1)
cat("✓ Test 1 completed\n\n")

# Test 2: Backward compatibility (no model)
# ------------------------------------------
cat("Test 2: Backward compatibility (expected = NULL)...\n")

p2 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived)) +
  labs(title = "Test 2: No Model (Default Behavior)",
       subtitle = "Should work exactly as before") +
  theme_mosaic()

print(p2)
cat("✓ Test 2 completed\n\n")

# Test 3: Custom formula
# -----------------------
cat("Test 3: Custom formula specification...\n")

p3 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = ~ Class + Sex) +
  scale_fill_residual() +
  labs(title = "Test 3: Custom Formula",
       subtitle = "Model: ~ Class + Sex; Survived") +
  theme_mosaic()

print(p3)
cat("✓ Test 3 completed\n\n")

p4 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = ~ Class + Sex) +
  scale_fill_residual() +
  labs(title = "Test 3: Custom Formula",
       subtitle = "Model: ~ Class + Sex") +
  theme_mosaic()

print(p4)


# Test 4: Weighted data
# ----------------------
cat("Test 4: Weighted data (happy dataset)...\n")

data(happy)

p4 <- ggplot(data = happy) +
  geom_mosaic(aes(weight = wtssall,
                  x = product(happy, health)),
              expected = "independence",
              na.rm = TRUE) +
  scale_fill_residual() +
  labs(title = "Test 4: Weighted Data",
       subtitle = "Happy dataset with survey weights") +
  theme_mosaic()

print(p4)
cat("✓ Test 4 completed\n\n")

# Test 5: Saturated model (diagnostic)
# -------------------------------------
cat("Test 5: Saturated model (should show near-zero residuals)...\n")

p5 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "saturated") +
  scale_fill_residual() +
  labs(title = "Test 5: Saturated Model",
       subtitle = "Residuals should be near zero (mostly white)") +
  theme_mosaic()

print(p5)
cat("✓ Test 5 completed\n\n")

# Summary
cat("===============================================\n")
cat("All basic tests completed successfully!\n")
cat("===============================================\n")
cat("\nNext steps:\n")
cat("1. Visually inspect the plots for correct residual shading\n")
cat("2. Verify colors: blue = negative, white = zero, red = positive\n")
cat("3. Check that Test 2 looks identical to old behavior\n")
cat("4. Check that Test 5 shows mostly white (near-zero residuals)\n")
cat("\nFor more examples, see: issues/residual-shading-examples.R\n")
cat("For implementation details, see: issues/implementation-summary.md\n")
