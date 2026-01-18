# Examples using HairEyeColor dataset
# Demonstrating case form vs. frequency form for mosaic plots

library(ggplot2)
library(ggmosaic)
library(vcdExtra)

# The HairEyeColor dataset is a 3-way contingency table
str(HairEyeColor)
# table [1:4, 1:4, 1:2] 32 53 10 3 11 50 10 30 10 25 ...
# - attr(*, "dimnames")=List of 3
#   ..$ Hair: chr [1:4] "Black" "Brown" "Red" "Blond"
#   ..$ Eye : chr [1:4] "Brown" "Blue" "Hazel" "Green"
#   ..$ Sex : chr [1:2] "Male" "Female"

# =============================================================================
# FREQUENCY FORM: Using weight aesthetic
# =============================================================================

# Convert the table to a data frame (frequency form)
# This creates one row per cell in the table with a "Freq" column
hair_freq <- as.data.frame(HairEyeColor)
head(hair_freq)
#    Hair   Eye    Sex Freq
# 1 Black Brown   Male   32
# 2 Brown Brown   Male   53
# 3   Red Brown   Male   10
# 4 Blond Brown   Male    3
# 5 Black  Blue   Male   11
# 6 Brown  Blue   Male   50

# Example 1: Simple 2-way mosaic - Hair by Eye
ggplot(data = hair_freq) +
  geom_mosaic(aes(weight = Freq, x = product(Hair, Eye), fill = Eye)) +
  labs(title = "Hair and Eye Color",
       subtitle = "Using frequency form with weight aesthetic")

# Example 2: 2-way with different fill
ggplot(data = hair_freq) +
  geom_mosaic(aes(weight = Freq, x = product(Eye, Hair), fill = Hair)) +
  labs(title = "Eye and Hair Color",
       subtitle = "Order matters: Eye then Hair")

# Example 3: 3-way mosaic - Hair x Eye, conditioned on Sex
ggplot(data = hair_freq) +
  geom_mosaic(aes(weight = Freq, x = product(Hair, Eye),
                  conds = product(Sex), fill = Eye)) +
  labs(title = "Hair and Eye Color by Sex",
       subtitle = "Conditioned on Sex using conds aesthetic")

# Example 4: 3-way mosaic - all variables in product()
ggplot(data = hair_freq) +
  geom_mosaic(aes(weight = Freq, x = product(Sex, Hair, Eye), fill = Eye)) +
  labs(title = "Three-way Mosaic: Sex, Hair, Eye",
       subtitle = "All three variables in product()")

# Example 5: Focus on one sex - filter the data
hair_male <- subset(hair_freq, Sex == "Male")
ggplot(data = hair_male) +
  geom_mosaic(aes(weight = Freq, x = product(Hair, Eye), fill = Eye)) +
  labs(title = "Hair and Eye Color - Males Only")

# Example 6: Using y aesthetic to control layout
ggplot(data = hair_freq) +
  geom_mosaic(aes(weight = Freq, x = product(Hair, Sex),
                  y = product(Sex), fill = Eye)) +
  labs(title = "Hair by Sex, colored by Eye",
       subtitle = "Using y aesthetic for explicit partitioning")

# =============================================================================
# CASE FORM: Expanding to individual observations (for comparison)
# =============================================================================

# Convert frequency form to case form (one row per observation)
# Use vcdExtra::expand.dft() to expand the frequency table
hair_case <- vcdExtra::expand.dft(hair_freq, freq = "Freq")
nrow(hair_case)  # 592 observations
head(hair_case)

# Example 7: Same plot as Example 1, but using case form (no weight needed)
ggplot(data = hair_case) +
  geom_mosaic(aes(x = product(Hair, Eye), fill = Eye)) +
  labs(title = "Hair and Eye Color - Case Form",
       subtitle = "No weight needed: one row per observation")

# =============================================================================
# COMPARISON: Both forms produce identical plots
# =============================================================================

# Frequency form with weight
p1 <- ggplot(data = hair_freq) +
  geom_mosaic(aes(weight = Freq, x = product(Eye), fill = Eye)) +
  labs(title = "Frequency Form")

# Case form without weight
p2 <- ggplot(data = hair_case) +
  geom_mosaic(aes(x = product(Eye), fill = Eye)) +
  labs(title = "Case Form")

# Both plots should be identical
print(p1)
print(p2)

# =============================================================================
# NOTES:
# =============================================================================
#
# FREQUENCY FORM:
# - One row per cell in the contingency table
# - Requires explicit Freq/count column
# - Use weight aesthetic to specify the frequency column
# - More compact storage for large datasets
# - Common output from table(), xtabs(), or as.data.frame() on tables
#
# CASE FORM:
# - One row per individual observation
# - No weight column needed
# - Each observation contributes count of 1
# - More intuitive for raw data
# - Default format for data collection
#
# CONVERTING BETWEEN FORMS:
# - Frequency to case: use vcdExtra::expand.dft(df, freq = "Freq")
# - Case to frequency: use table(), xtabs(), or count() from dplyr
#
# The weight aesthetic allows geom_mosaic() to work with frequency form data,
# making it flexible for both aggregated and case-level data.
