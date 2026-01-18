# Loglinear Models with ggmosaic - Quick Reference

## Essential Patterns

### 1. Basic Residual Shading (Independence Model)

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual()
```

**When to use**: Test if two variables are independent.

**Interpretation**:
- Blue tiles = Observed > Expected (positive association)
- Red tiles = Observed < Expected (negative association)
- |residual| > 2 ≈ statistically significant

---

### 2. Show Residual Values in Cells

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   expected = "independence",  # MUST MATCH
                   display_values = "residual",
                   format_digits = 2)
```

**Key point**: `expected` must be specified in BOTH layers.

---

### 3. Show Observed Counts in Cells

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   display_values = "observed",
                   format_digits = 0)
```

**When to use**: Show exact frequencies alongside proportions.

---

### 4. Three-Way Independence Model

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex, Survived)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 1,
                   size = 2.5)
```

**When to use**: Test complete independence of three variables.

---

### 5. Custom Formula Model

```r
# Test if Survival is independent given Class and Sex
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Survived)),
              expected = ~ Class + Sex) +  # Custom formula
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex, Survived)),
                   expected = ~ Class + Sex,  # MUST MATCH
                   display_values = "residual",
                   format_digits = 1,
                   size = 2.5)
```

**When to use**: Test specific hypotheses about independence/association.

---

### 6. Customize Colors and Text

```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual(
    low = "steelblue",
    mid = "white",
    high = "firebrick",
    limits = c(-5, 5)
  ) +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 2,
                   size = 4,
                   colour = "black",
                   fontface = "bold")
```

---

## Model Specification Shortcuts

| Shortcut | Formula Equivalent | Meaning |
|----------|-------------------|---------|
| `"independence"` | `~ A + B + C` | Complete independence (main effects only) |
| `"saturated"` | `~ A * B * C` | Saturated model (perfect fit, no residuals) |
| `"conditional"` | `~ A + B + C + A:C + B:C` | Margins independent given conditions |

---

## Display Values Options

| Option | Shows | Use Case |
|--------|-------|----------|
| `"label"` | Factor level labels | Default behavior |
| `"observed"` | Observed counts | Show exact frequencies |
| `"expected"` | Expected from model | Compare with observed |
| `"residual"` | Pearson residuals | Show deviation magnitude |

---

## Text Aesthetics

All standard ggplot2 text aesthetics work:

```r
geom_mosaic_text(...,
  size = 3,                # Text size (default: 2.7)
  colour = "white",        # Text color
  fontface = "bold",       # "plain", "bold", "italic", "bold.italic"
  family = "serif",        # Font family
  angle = 0,               # Rotation angle
  hjust = 0.5,            # Horizontal justification (0-1)
  vjust = 0.5             # Vertical justification (0-1)
)
```

---

## Interpreting Pearson Residuals

$$r = \frac{\text{observed} - \text{expected}}{\sqrt{\text{expected}}}$$

| Magnitude | Interpretation | Color |
|-----------|---------------|-------|
| \|r\| < 2 | Not significant | White/light |
| 2 < \|r\| < 4 | Significant (α ≈ 0.05) | Moderate blue/red |
| \|r\| > 4 | Highly significant | Dark blue/red |

**Positive residuals** (blue): Combination occurs MORE than expected
**Negative residuals** (red): Combination occurs LESS than expected

---

## Common Patterns and Their Formulas

### Two-way independence
```r
expected = "independence"  # or ~ A + B
```
Tests: Are A and B independent?

### Three-way independence
```r
expected = "independence"  # or ~ A + B + C
```
Tests: Are A, B, and C mutually independent?

### No three-way interaction
```r
expected = ~ A + B + C + A:B + A:C + B:C
```
Tests: Is the three-way interaction zero?

### One variable independent of others
```r
expected = ~ A + B  # when variables are A, B, C
```
Tests: Is C independent of the A×B cross-classification?

### Joint independence
```r
expected = ~ A + B:C  # A independent of B×C
```

---

## Checklist for Residual Shading

- [ ] Specify `expected` parameter in `geom_mosaic()`
- [ ] Add `scale_fill_residual()` for color scale
- [ ] If using `geom_mosaic_text()` with residuals/expected:
  - [ ] Specify same `expected` in `geom_mosaic_text()`
  - [ ] Set `display_values = "residual"` or `"expected"`
  - [ ] Choose appropriate `format_digits`
- [ ] Interpret residuals: |r| > 2 is significant

---

## Common Mistakes

### ❌ Missing `expected` in text layer
```r
# WRONG - will show warning
geom_mosaic(aes(...), expected = "independence") +
geom_mosaic_text(aes(...), display_values = "residual")  # Missing expected!
```

### ✅ Correct
```r
geom_mosaic(aes(...), expected = "independence") +
geom_mosaic_text(aes(...), expected = "independence", display_values = "residual")
```

---

### ❌ Mismatched `expected` parameters
```r
# WRONG - different models in two layers
geom_mosaic(aes(...), expected = "independence") +
geom_mosaic_text(aes(...), expected = ~ A + B, ...)  # Different model!
```

### ✅ Correct
```r
geom_mosaic(aes(...), expected = "independence") +
geom_mosaic_text(aes(...), expected = "independence", ...)
```

---

## Workflow Template

```r
# 1. Explore data
data(titanic)
str(titanic)
table(titanic$Class, titanic$Survived)

# 2. Create basic mosaic
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived), fill = Survived))

# 3. Add independence model and residual shading
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual()

# 4. Add residual values for interpretation
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 2)

# 5. Customize for publication
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual(limits = c(-5, 5)) +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 2,
                   size = 4,
                   fontface = "bold") +
  labs(title = "Titanic Survival by Class",
       subtitle = "Pearson residuals from independence model") +
  theme_mosaic()
```

---

## Quick Diagnostics

### Check model fit
```r
# Fit model directly
tab <- xtabs(~ Class + Survived, data = titanic)
tab_df <- as.data.frame(tab)
model <- glm(Freq ~ Class + Survived, data = tab_df, family = poisson())

# Goodness of fit test
deviance(model)  # G² statistic
df.residual(model)  # df
pchisq(deviance(model), df.residual(model), lower.tail = FALSE)  # p-value
```

### Extract residuals
```r
residuals(model, type = "pearson")
```

---

## See Also

- Main vignette: `vignette("loglinear-models", package = "ggmosaic")`
- Function help: `?geom_mosaic`, `?geom_mosaic_text`, `?scale_fill_residual`
- Basic mosaic plots: `vignette("ggmosaic", package = "ggmosaic")`
- VCD package: `vignette("strucplot", package = "vcd")`

---

## Examples by Use Case

### Publication figure with counts
```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived), fill = Survived)) +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   display_values = "observed",
                   format_digits = 0,
                   size = 4) +
  labs(title = "Titanic Survival by Class") +
  theme_mosaic()
```

### Statistical analysis with residuals
```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex)),
                   expected = "independence",
                   display_values = "residual",
                   format_digits = 1,
                   colour = "white",
                   size = 3.5,
                   fontface = "bold") +
  labs(title = "Class × Sex Association Test",
       subtitle = "Independence model") +
  theme_mosaic()
```

### Comparing observed vs expected
```r
library(patchwork)

p_obs <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   expected = "independence",
                   display_values = "observed",
                   format_digits = 0) +
  labs(title = "Observed")

p_exp <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)),
              expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   expected = "independence",
                   display_values = "expected",
                   format_digits = 1) +
  labs(title = "Expected")

p_obs + p_exp
```
