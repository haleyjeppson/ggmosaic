# Debug Script: Label Issues
# ===========================

library(ggmosaic)
library(dplyr)

data(titanic)

cat("=== Original titanic data ===\n")
cat("Class levels:\n")
print(levels(titanic$Class))
cat("\nSex levels:\n")
print(levels(titanic$Sex))

cat("\n=== Testing prodcalc directly ===\n")

# Simulate what stat_mosaic does
test_data <- titanic %>%
  rename(x__Class = Class, x__Sex = Sex) %>%
  mutate(weight = 1)

# Run prodcalc with expected model
result <- prodcalc(
  data = test_data,
  formula = as.formula("weight ~ x__Class + x__Sex"),
  divider = mosaic(),
  expected = "independence"
)

cat("\nColumn names in result:\n")
print(names(result))

cat("\nUnique values of x__Class:\n")
print(unique(result$x__Class))
cat("\nClass of x__Class:\n")
print(class(result$x__Class))

cat("\nUnique values of x__Sex:\n")
print(unique(result$x__Sex))
cat("\nClass of x__Sex:\n")
print(class(result$x__Sex))

# Check if there are weird characters
cat("\nChecking for non-standard characters in x__Class:\n")
class_vals <- as.character(unique(result$x__Class))
for (i in seq_along(class_vals)) {
  cat("Value", i, ":", sQuote(class_vals[i]),
      " | nchar:", nchar(class_vals[i]),
      " | bytes:", paste(charToRaw(class_vals[i]), collapse=" "),
      "\n")
}

cat("\nChecking for non-standard characters in x__Sex:\n")
sex_vals <- as.character(unique(result$x__Sex))
for (i in seq_along(sex_vals)) {
  cat("Value", i, ":", sQuote(sex_vals[i]),
      " | nchar:", nchar(sex_vals[i]),
      " | bytes:", paste(charToRaw(sex_vals[i]), collapse=" "),
      "\n")
}

cat("\n=== Testing the full ggplot ===\n")

p <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual()

# Build the plot to access the data
built <- ggplot_build(p)

cat("\nBuilt data layer 1:\n")
print(head(built$data[[1]]))

cat("\nUnique x values (for axis):\n")
if ("x" %in% names(built$data[[1]])) {
  print(unique(built$data[[1]]$x))
}

cat("\nChecking for label column:\n")
if ("label" %in% names(built$data[[1]])) {
  cat("Labels found:\n")
  print(unique(built$data[[1]]$label))
}
