# Extended Topics for Loglinear Models Vignette

This document outlines additional topics that could be developed for the "ggmosaic and Loglinear Models" vignette.

## 1. Statistical Testing and Inference

### 1.1 Goodness-of-Fit Tests

**Concept**: Testing whether a loglinear model fits the data adequately.

**Content**:
- Extract deviance statistics from `glm()` fit
- Likelihood ratio G² test
- Pearson X² test
- Degrees of freedom calculation
- P-value interpretation

**Example code**:
```r
# Fit independence model directly
tab <- xtabs(~ Class + Survived, data = titanic)
model <- glm(Freq ~ Class + Survived, data = as.data.frame(tab), family = poisson())

# Extract test statistics
deviance(model)  # G² statistic
df.residual(model)  # Degrees of freedom
pchisq(deviance(model), df.residual(model), lower.tail = FALSE)  # p-value

# Visualize with test results in subtitle
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)), expected = "independence") +
  scale_fill_residual() +
  labs(title = "Independence Model",
       subtitle = sprintf("G² = %.2f, df = %d, p < .001",
                         deviance(model), df.residual(model)))
```

### 1.2 Model Comparison

**Concept**: Comparing nested loglinear models using likelihood ratio tests.

**Content**:
- Hierarchical model testing
- `anova()` for nested model comparison
- Information criteria (AIC, BIC)
- Choosing between competing models

**Example**:
```r
# Three nested models
m1 <- glm(Freq ~ Class + Sex + Survived, family = poisson())
m2 <- glm(Freq ~ Class + Sex + Survived + Class:Survived, family = poisson())
m3 <- glm(Freq ~ Class * Sex * Survived, family = poisson())

anova(m1, m2, m3, test = "Chisq")

# Visualize the preferred model
# ... ggplot code ...
```

### 1.3 Residual Types and Interpretation

**Concept**: Understanding different types of residuals and when to use them.

**Content**:
- **Pearson residuals**: (O - E) / √E (used in ggmosaic)
- **Standardized residuals**: Pearson / √(1 - h_ii)
- **Adjusted residuals**: Account for multiple testing
- **Deviance residuals**: Based on likelihood contributions
- **Connection to significance levels**: |r| > 2 ≈ α = 0.05

**Visualization**:
```r
# Show relationship between residual magnitude and color intensity
# Create a reference plot showing residual scale
```

## 2. Advanced Visualization Techniques

### 2.1 Avoiding Overlapping Labels

**Concept**: Using ggrepel to prevent text overlap in crowded plots.

**Example**:
```r
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex, Age)), expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Sex, Age)),
                   expected = "independence",
                   display_values = "residual",
                   repel = TRUE,        # Avoid overlaps
                   as.label = TRUE,     # Box around text
                   size = 2.5)
```

### 2.2 Small Multiples with Faceting

**Concept**: Combining mosaic plots with faceting for conditioning.

**Example**:
```r
# Compare residuals across different subgroups
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)), expected = "independence") +
  scale_fill_residual() +
  facet_wrap(~ Sex) +
  labs(title = "Independence Model by Sex")
```

### 2.3 Overlaying Additional Information

**Concept**: Adding marginal frequencies, percentages, or other annotations.

**Example**:
```r
# Add marginal totals as annotations
# Show row/column percentages
# Overlay confidence intervals for proportions
```

### 2.4 Interactive Plots

**Concept**: Creating interactive mosaic plots with plotly.

**Example**:
```r
library(plotly)

p <- ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)), expected = "independence") +
  scale_fill_residual()

ggplotly(p)
```

## 3. Model Diagnostics and Assessment

### 3.1 Handling Zero Cells

**Concept**: Dealing with structural and sampling zeros.

**Content**:
- **Structural zeros**: Impossible combinations (e.g., male pregnancy)
- **Sampling zeros**: Possible but unobserved
- **Solutions**:
  - Quasi-independence models
  - Collapsing categories
  - Adding small constant (0.5)
  - Exact tests for sparse tables

**Example**:
```r
# Identify zero cells
# Fit quasi-independence model
# Compare with and without zero adjustment
```

### 3.2 Outlier and Influential Cell Detection

**Concept**: Identifying cells that don't fit the model or strongly influence it.

**Content**:
- Cells with |residual| > 4 (extreme outliers)
- Influence diagnostics (hat values, Cook's D)
- Visualizing outliers with special highlighting

**Example**:
```r
# Highlight extreme residuals
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Sex), alpha = abs(.residual) > 4),
              expected = "independence") +
  scale_fill_residual()
```

### 3.3 Model Adequacy Checks

**Concept**: Assessing whether model assumptions are reasonable.

**Content**:
- Residual distribution plots (should be ~ N(0,1))
- QQ plots for residuals
- Patterns in residual plots
- Overdispersion detection

**Example**:
```r
# Extract residuals from fitted model
residuals_df <- data.frame(residual = residuals(model, type = "pearson"))

ggplot(residuals_df, aes(sample = residual)) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "QQ Plot of Pearson Residuals")
```

## 4. Real-World Applications

### 4.1 Survey Data Analysis

**Dataset**: General Social Survey (GSS)

**Analysis**:
- Political affiliation × education × income
- Testing for conditional independence
- Handling survey weights

**Example**:
```r
# Weighted analysis with survey package
library(survey)
# ... survey design ...
# ... weighted mosaic plot ...
```

### 4.2 Clinical Trials

**Dataset**: Hypothetical treatment × outcome × covariate data

**Analysis**:
- Treatment effectiveness by subgroup
- Interaction testing
- Simpson's paradox illustration

### 4.3 Market Segmentation

**Dataset**: Customer demographics × product preferences

**Analysis**:
- Identifying market segments
- Association patterns
- Target group identification

### 4.4 Educational Assessment

**Dataset**: Student performance × demographics

**Analysis**:
- Achievement gaps across groups
- Differential item functioning
- Equity analysis

## 5. Connection to Other Packages

### 5.1 Comparison with vcd Package

**Content**:
- Equivalent functionality: `vcd::mosaic()` vs `geom_mosaic()`
- Shading schemes: `vcd::shading_max()` vs `scale_fill_residual()`
- Labeling: `vcd::labeling_values()` vs `display_values`
- When to use each package

**Side-by-side comparison**:
```r
# vcd version
library(vcd)
mosaic(~ Class + Survived, data = Titanic,
       shade = TRUE,
       labeling = labeling_values)

# ggmosaic version
ggplot(data = titanic) +
  geom_mosaic(aes(x = product(Class, Survived)), expected = "independence") +
  scale_fill_residual() +
  geom_mosaic_text(aes(x = product(Class, Survived)),
                   expected = "independence",
                   display_values = "observed")
```

### 5.2 Integration with broom

**Content**: Tidying model output for analysis and visualization.

**Example**:
```r
library(broom)

model <- glm(Freq ~ Class * Survived, family = poisson(), data = tab_df)
tidy(model)
glance(model)
augment(model)
```

### 5.3 Working with Survey Weights

**Content**: Handling complex survey designs.

**Example**:
```r
library(survey)
# Create survey design
# Fit weighted loglinear model
# Visualize with ggmosaic
```

### 5.4 Large and Sparse Tables

**Content**: Strategies for high-dimensional tables.

**Topics**:
- Collapsing categories
- Marginal and partial associations
- Graphical model selection
- Dimension reduction before visualization

## 6. Methodological Extensions

### 6.1 Hierarchical Loglinear Models

**Concept**: Testing specific patterns of association.

**Content**:
- Graphical models
- Decomposable models
- Marginal and conditional independence
- Model search procedures

**Example**:
```r
# Test specific independence hypothesis
# [AB][AC][BC] model (no three-way interaction)
```

### 6.2 Quasi-Independence

**Concept**: Models for square tables with structural zeros.

**Application**: Social mobility tables, agreement tables

**Example**:
```r
# Father's occupation × Son's occupation
# Fit quasi-independence (off-diagonal only)
```

### 6.3 RC (Row-Column) Association Models

**Concept**: Reduced-rank models for ordered categories.

**Content**:
- Linear-by-linear association
- Row effects and column effects
- Score parameters

### 6.4 Correspondence Analysis Connection

**Concept**: Relating mosaic plots to dimension reduction.

**Content**:
- CA scores as coordinates
- Residuals in reduced space
- Visualizing deviations from independence in 2D

**Example**:
```r
library(ca)
# Run correspondence analysis
# Compare with residual mosaic
```

## 7. Case Studies

### Case Study 1: Hair Color and Eye Color

**Data**: HairEyeColor dataset

**Questions**:
- Are hair color and eye color independent?
- Does the association differ by sex?
- Which combinations are over/under-represented?

**Complete analysis with**:
- Model fitting
- Residual visualization
- Interpretation
- Comparison with vcd results

### Case Study 2: Graduate Admissions (Berkeley)

**Data**: UCBAdmissions dataset

**Questions**:
- Simpson's paradox illustration
- Gender bias in admissions?
- Department-specific patterns
- Conditional independence testing

### Case Study 3: Arthritis Treatment

**Data**: Arthritis dataset from vcd

**Questions**:
- Treatment effectiveness
- Interaction with baseline severity
- Marginal vs conditional effects

## Implementation Notes

Each extended topic should include:

1. **Clear learning objectives**: What should readers understand?
2. **Conceptual explanation**: Why is this important?
3. **Working code examples**: Copy-paste ready
4. **Visualizations**: Annotated plots showing key points
5. **Interpretation guidelines**: How to read the results
6. **Common pitfalls**: What to avoid
7. **Further reading**: References for deeper study

## Suggested Structure for Each Topic

```r
## Topic Name

### What and Why

[Brief introduction to the concept and its importance]

### Example

[Motivating dataset and research question]

### Code

```{r}
# Step-by-step implementation
```

### Interpretation

[How to read and understand the results]

### Try It Yourself

[Exercise for readers]
```

## Priority Topics for Next Version

Based on user needs, prioritize:

1. **Statistical testing** (Section 1) - Most requested
2. **Comparison with vcd** (Section 5.1) - Helps migration
3. **Case studies** (Section 7) - Concrete examples
4. **Avoiding overlaps** (Section 2.1) - Practical need
5. **Zero cells** (Section 3.1) - Common issue

## References for Extended Topics

- Agresti, A. (2013). *Categorical Data Analysis* (3rd ed.). Wiley.
- Bishop, Y. M., Fienberg, S. E., & Holland, P. W. (1975). *Discrete Multivariate Analysis*. MIT Press.
- Friendly, M. (2000). *Visualizing Categorical Data*. SAS Institute.
- Wickham, H. (2016). *ggplot2: Elegant Graphics for Data Analysis* (2nd ed.). Springer.
