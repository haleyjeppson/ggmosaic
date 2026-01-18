# Testing Text Size Control in geom_mosaic_text()
# ==================================================

library(ggmosaic)
library(ggplot2)

data(titanic)

# Example 1: Default size (2.7)
# -----------------------------
p1 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "observed",
                   format_digits = 0) +
  labs(title = "Default Size",
       subtitle = "size not specified (uses default 2.7)") +
  theme_mosaic()

print(p1)

cat("\nPress [Enter] for next example...")
readline()

# Example 2: Small text
# ---------------------
p2 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "observed",
                   format_digits = 0,
                   size = 2) +
  labs(title = "Small Text",
       subtitle = "size = 2") +
  theme_mosaic()

print(p2)

cat("\nPress [Enter] for next example...")
readline()

# Example 3: Large text
# ---------------------
p3 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "observed",
                   format_digits = 0,
                   size = 5) +
  labs(title = "Large Text",
       subtitle = "size = 5") +
  theme_mosaic()

print(p3)

cat("\nPress [Enter] for next example...")
readline()

# Example 4: Bold text with custom aesthetics
# ---------------------------------------------
p4 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "observed",
                   format_digits = 0,
                   size = 4,
                   colour = "white",
                   fontface = "bold") +
  labs(title = "Larger Bold White Text",
       subtitle = "size = 4, fontface = 'bold', colour = 'white'") +
  theme_mosaic()

print(p4)

cat("\nPress [Enter] for next example...")
readline()

# Example 5: Size with residuals
# -------------------------------
p5 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 2,
                   size = 4,
                   colour = "black",
                   fontface = "bold") +
  labs(title = "Residuals with Custom Size",
       subtitle = "size = 4, bold font") +
  theme_mosaic()

print(p5)

cat("\nPress [Enter] for next example...")
readline()

# Example 6: Advanced text aesthetics
# ------------------------------------
p6 <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   display_values = "observed",
                   format_digits = 0,
                   size = 3.5,
                   colour = "black",
                   fontface = "italic",
                   family = "serif") +
  labs(title = "Custom Font Aesthetics",
       subtitle = "size = 3.5, fontface = 'italic', family = 'serif'") +
  theme_mosaic()

print(p6)

cat("\n\n=== Summary ===\n\n")
cat("Text size in geom_mosaic_text() is controlled via the 'size' parameter:\n\n")
cat("  geom_mosaic_text(..., size = 3)\n\n")
cat("Default size: 2.7 (defined in default_aes)\n")
cat("Typical range: 2 to 6\n\n")
cat("You can also control:\n")
cat("  - colour: Text color\n")
cat("  - fontface: 'plain', 'bold', 'italic', 'bold.italic'\n")
cat("  - family: Font family name (e.g., 'serif', 'sans', 'mono')\n")
cat("  - angle: Text rotation angle (in degrees)\n")
cat("  - hjust, vjust: Horizontal/vertical justification (0-1)\n")
cat("  - lineheight: Line height for multi-line text\n")
