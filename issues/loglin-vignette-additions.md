## Additional Topics

This vignette covers the fundamentals of using loglinear models with ggmosaic. Additional topics that could be explored include:

### Statistical Testing
- **Goodness-of-fit tests**: Using `glm()` to extract deviance and likelihood ratio statistics
- **Model comparison**: Comparing nested models with `anova()`
- **Residual interpretation**: Connecting Pearson residuals to standardized residuals and adjusted residuals

### Advanced Visualization
- **Repelling labels**: Using `repel = TRUE` to avoid overlapping text in dense plots
- **Label boxes**: Using `as.label = TRUE` for rounded rectangle backgrounds
- **Faceting**: Combining mosaic plots with `facet_wrap()` and `facet_grid()`
- **Overlaying statistics**: Adding marginal frequencies or percentages

### Model Diagnostics
- **Zero cells**: Handling structural and sampling zeros in sparse tables
- **Outlier detection**: Identifying cells with extreme residuals
- **Model adequacy**: Assessing whether the independence assumption is reasonable
- **Influential observations**: Detecting cells that strongly influence model fit

### Real-World Applications
- **Survey data analysis**: Examining relationships in survey responses
- **Clinical trials**: Analyzing treatment × outcome × covariate associations
- **Market research**: Understanding customer segmentation patterns
- **Educational assessment**: Studying performance across demographic groups

### Connection to Other Packages
- **Comparison with vcd**: Differences and similarities with `vcd::mosaic()`
- **Integration with broom**: Tidying model output with `broom::tidy()`
- **Working with survey weights**: Handling complex survey designs
- **Large tables**: Strategies for visualizing high-dimensional contingency tables

### Methodological Extensions
- **Hierarchical models**: Testing specific patterns of association
- **Quasi-independence**: Models for square tables with diagonal structure
- **RC models**: Row-column association models
- **Correspondence analysis**: Connecting mosaic plots to dimension reduction

## Summary

Extended mosaic plots with residual shading provide a powerful tool for exploring associations in categorical data:

1. **Specify a model** using `expected = "independence"`, `"saturated"`, `"conditional"`, or a custom formula
2. **Apply residual shading** with `scale_fill_residual()` to highlight deviations
3. **Label cells** with `geom_mosaic_text()` to show observed, expected, or residual values
4. **Customize aesthetics** using standard ggplot2 parameters

This approach combines the strengths of:
- **Statistical modeling** (loglinear models)
- **Visual exploration** (mosaic plots)
- **ggplot2 flexibility** (layers, scales, themes)

The result is an intuitive yet rigorous way to understand categorical data relationships.

