# Test key code from frequency-table-forms vignette
# to verify it works before knitting

library(ggmosaic)
library(ggplot2)
library(vcdExtra)
library(dplyr)

# Create 2-way table
HairEye_table <- margin.table(HairEyeColor, margin = c(1, 2))
HairEye_table

# Frequency form
HairEye_freq <- as.data.frame(HairEye_table)
head(HairEye_freq)

# Case form
HairEye_case <- expand.dft(HairEye_freq, freq = "Freq")
head(HairEye_case)

# Test plot from frequency form
p1 <- ggplot(data = HairEye_freq) +
  geom_mosaic(aes(weight = Freq, x = product(Hair, Eye), fill = Eye)) +
  labs(title = "From Frequency Form") +
  theme_mosaic()

print(p1)

# Test plot from case form
p2 <- ggplot(data = HairEye_case) +
  geom_mosaic(aes(x = product(Hair, Eye), fill = Eye)) +
  labs(title = "From Case Form") +
  theme_mosaic()

print(p2)

# Test 3-way with original data
hair_freq <- as.data.frame(HairEyeColor)

p3 <- ggplot(data = hair_freq) +
  geom_mosaic(aes(weight = Freq, x = product(Hair, Eye, Sex), fill = Eye)) +
  labs(title = "Three-Way") +
  theme_mosaic()

print(p3)

# Test residual shading
p4 <- ggplot(data = HairEye_freq) +
  geom_mosaic(aes(weight = Freq, x = product(Hair, Eye)),
              expected = "independence") +
  scale_fill_residual() +
  labs(title = "Independence Model") +
  theme_mosaic()

print(p4)

cat("All tests completed successfully!\n")
