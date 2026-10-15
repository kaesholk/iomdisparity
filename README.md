---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



# iomdisparity

<!-- badges: start -->
[![R-CMD-check](https://img.shields.io/badge/R--CMD--check-passing-brightgreen.svg)](#)
[![CRAN status](https://www.r-pkg.org/badges/version/iomdisparity)](https://cran.r-project.org/package=iomdisparity)
[![License: MIT](https://img.shields.io/badge/License-MIT-blue.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

**iomdisparity** is an R package that implements the **Institute of Medicine (IOM)** definition of racial and ethnic healthcare disparities using the **rank-and-replace** methodology described by McGuire et al. (2006) and Cook et al. (2010, 2012).

---

## Conceptual Framework

Under the IOM (*Unequal Treatment*, 2002) definition, a healthcare disparity is defined as:

> A difference in treatment provided to members of different racial or ethnic groups that is not justified by underlying clinical health conditions or treatment preferences of patients.

Because socioeconomic status (SES) differences (e.g., lower income, lack of private insurance, structural discrimination) are key pathways through which the healthcare system produces disparities, **SES differences are not adjusted away**.

`iomdisparity` constructs a counterfactual minority population that possesses the **clinical need distribution of the reference (white) population**, while strictly **preserving the minority population's socioeconomic distribution and rank correlations**.

---

## Features

- **Quantile Mapping Rank-and-Replace**: Exact quantile transformation without data loss, artificial trimming, or dropping of unmatched individuals.
- **Single-Part and Two-Part Models**: Native support for standard GLMs (`gaussian`, `binomial`, `poisson`) and two-part hurdle/expenditure models (`logit`/`probit` + log-link `Gamma`).
- **Multi-Method Comparative Reporting**: Automatic side-by-side comparison of:
  1. **IOM Rank-and-Replace** (concordant with IOM principles)
  2. **Residual Direct Effect (RDE)** (regression-adjusted predictive margins)
  3. **Unadjusted Difference** (raw difference in group means)
- **Robust Stratified Bootstrapping**: Proper stratified resampling within racial/ethnic groups to capture first-stage and rank-and-replace sampling variance.
- **Publication-Ready S3 Visualizations**:
  - `plot(fit, type = "distributions")`: Replicates Figure 2 from McGuire et al. (2006), visualizing factual reference, factual comparison, and counterfactual need distributions.
  - `plot(fit, type = "disparity")`: Forest plot with 95% bootstrap confidence intervals across methods.

---

## Installation

Install the package directly from GitHub:

```r
# install.packages("devtools")
devtools::install_github("example/iomdisparity")
```

---

## Quickstart

```r
library(iomdisparity)

# Load the built-in synthetic Medical Expenditure Panel Survey (MEPS) dataset
data(meps_synthetic)

# Estimate disparities in any mental health care use
fit <- iom_disparity(
  formula = any_mental_health_use ~ age + female + chronic_conditions +
    sf12_physical + sf12_mental + activity_limitations +
    income_poverty_ratio + insurance + race,
  data = meps_synthetic,
  race_col = "race",
  ref_race = "White",
  comp_race = "Black",
  need_vars = c("age", "female", "chronic_conditions", "sf12_physical",
                "sf12_mental", "activity_limitations"),
  model_type = "glm",
  family = binomial(),
  n_boot = 100,
  seed = 42
)

# Display summary results
print(fit)
```

### Visualizing the Counterfactual Transformation

```r
# Density plot showing how Black clinical need is aligned with White clinical need
plot(fit, type = "distributions")

# Forest plot of disparity estimates with 95% bootstrap confidence intervals
plot(fit, type = "disparity")
```

---

## Two-Part Expenditure Models

For healthcare expenditures or visit counts characterized by zero-inflation and right-skewed positive costs:

```r
fit_exp <- iom_disparity(
  formula = mental_health_expenditure ~ age + female + chronic_conditions +
    sf12_physical + sf12_mental + activity_limitations +
    income_poverty_ratio + insurance + race,
  data = meps_synthetic,
  race_col = "race",
  ref_race = "White",
  comp_race = "Hispanic",
  need_vars = c("age", "female", "chronic_conditions", "sf12_physical",
                "sf12_mental", "activity_limitations"),
  model_type = "two_part",
  family = binomial(link = "logit"),
  part2_family = Gamma(link = "log"),
  n_boot = 50,
  seed = 42
)

summary(fit_exp)
```

---

## References

- **McGuire, T. G., Alegria, M., Cook, B. L., Wells, K. B., & Zaslavsky, A. M. (2006).** Implementing the Institute of Medicine definition of disparities: an application to mental health care. *Health Services Research*, 41(5), 1979–2005. [doi:10.1111/j.1475-6773.2006.00583.x](https://doi.org/10.1111/j.1475-6773.2006.00583.x)
- **Cook, B. L., McGuire, T. G., Lock, K., & Zaslavsky, A. M. (2010).** Comparing methods of racial and ethnic disparities measurement across different settings of mental health care. *Health Services Research*, 45(3), 825–847. [doi:10.1111/j.1475-6773.2010.01100.x](https://doi.org/10.1111/j.1475-6773.2010.01100.x)
- **Cook, B. L., McGuire, T. G., & Zaslavsky, A. M. (2012).** Measuring racial/ethnic disparities in health care: methods and practical issues. *Health Services Research*, 47(3 Pt 2), 1232–1254. [doi:10.1111/j.1475-6773.2012.01387.x](https://doi.org/10.1111/j.1475-6773.2012.01387.x)
- **Institute of Medicine (IOM). (2002).** *Unequal Treatment: Confronting Racial and Ethnic Disparities in Health Care*. Washington, DC: The National Academies Press. [doi:10.17226/10260](https://doi.org/10.17226/10260)
- **Mahmoudi, E., & Jensen, G. A. (2012).** Diverging racial and ethnic disparities in access to physician care: comparing 2000 and 2007. *Medical Care*, 50(4), 327–334. [doi:10.1097/MLR.0b013e318245a111](https://doi.org/10.1097/MLR.0b013e318245a111)
