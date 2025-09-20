# File: R/rep_colcode.R
# Purpose: Color-coding section for sem4all reports

#' Generate Color-Coded Thresholds Section
#'
#' Creates the color-coded thresholds and cutoffs section for the sem4all report.
#'
#' @return Character vector containing the formatted color coding section.
generate_color_coding_section <- function() {
  colcode_section <- c(
    "## Color-Coded Thresholds and Cutoffs",
    "",
    "The following table defines the color scheme used throughout this report for all statistical thresholds. All values are reported in thousandths (e.g., 832 = 0.832).",
    "",
    "| Statistic | Criterion | Value | Color Code | Interpretation |",
    "|-----------|-----------|-------|------------|---------------|",
    "| **Loadings (λ)** | Excellent | ≥ 700 | <span style=\"color:green; font-weight:bold\">Green</span> | Strong indicator-construct relationship |",
    "| | Acceptable | ≥ 500 | <span style=\"color:orange\">Orange</span> | Adequate relationship, theoretically justified |",
    "| | Poor | < 500 | <span style=\"color:red; font-weight:bold\">Red</span> | Weak relationship, consider removal |",
    "| **CI Low / High** | Stable Loading | ≥ 500 | <span style=\"color:green; font-weight:bold\">Green</span> | Lower bound is acceptable |",
    "| | Questionable Stability | < 500 | <span style=\"color:orange\">Orange</span> | Lower bound is weak, interpret with caution |",
    "| **Composite Reliability (ρDG)** | Good | ≥ 700 | <span style=\"color:green; font-weight:bold\">Green</span> | High internal consistency |",
    "| | Questionable | < 700 | <span style=\"color:red; font-weight:bold\">Red</span> | Low reliability, needs improvement |",
    "| **Cronbach's Alpha (α)** | Good | ≥ 700 | <span style=\"color:green; font-weight:bold\">Green</span> | High internal consistency |",
    "| | Questionable | < 700 | <span style=\"color:red; font-weight:bold\">Red</span> | Low reliability, needs improvement |",
    "| **AVE** | Good | ≥ 500 | <span style=\"color:green; font-weight:bold\">Green</span> | Construct captures more variance than error |",
    "| | Poor | < 500 | <span style=\"color:red; font-weight:bold\">Red</span> | More error than valid variance |",
    "| **F² Effect Size** | Large | ≥ 350 | <span style=\"color:green; font-weight:bold\">Green</span> | Substantial impact |",
    "| | Medium | ≥ 150 | <span style=\"color:orange\">Orange</span> | Moderate impact |",
    "| | Small | ≥ 20 | <span style=\"color:blue\">Blue</span> | Minor but potentially meaningful impact |",
    "| | None | < 20 | <span style=\"color:red; font-weight:bold\">Red</span> | Negligible impact |",
    "| **SRMR** | Good | ≤ 80 | <span style=\"color:green; font-weight:bold\">Green</span> | Close model-data fit |",
    "| | Questionable | > 80 | <span style=\"color:red; font-weight:bold\">Red</span> | Model misfit |",
    "| **GFI/FIT** | Good | ≥ 930 | <span style=\"color:green; font-weight:bold\">Green</span> | Good overall fit |",
    "| | Questionable | < 930 | <span style=\"color:orange\">Orange</span> | Marginal fit |",
    "| **Fornell-Larcker** | Met | Diagonal > Off-diag | <span style=\"color:green; font-weight:bold\">Green</span> | Discriminant validity established |",
    "| | Not Met | Diagonal ≤ Off-diag | <span style=\"color:red; font-weight:bold\">Red</span> | Discriminant validity violated |",
    "| **HTMT** | Good | ≤ 850 | <span style=\"color:green; font-weight:bold\">Green</span> | Discriminant validity established |",
    "| | Questionable | > 850 | <span style=\"color:red; font-weight:bold\">Red</span> | Discriminant validity violated |",
    "| **Mahalanobis Outliers** | Acceptable | ≤ 5% | <span style=\"color:green; font-weight:bold\">Green</span> | Minimal influence from outliers |",
    "| | Caution | > 5% & ≤ 10% | <span style=\"color:orange\">Orange</span> | Moderate outlier presence |",
    "| | Problematic | > 10% | <span style=\"color:red; font-weight:bold\">Red</span> | Severe outlier problem |",
    "",
    "*Note: For GSCA, skewness and kurtosis values outside typical PLS-SEM thresholds do not invalidate results due to GSCA's least-squares estimation method which does not assume multivariate normality (Hwang & Takane, 2014). The standard PLS-SEM thresholds are |skewness| < 3 and |kurtosis| < 10 (reported as 3000 and 10000 in thousandths), but these are not required for GSCA validity. We use red coloring for values exceeding these PLS-SEM thresholds to help the researcher identify rare descriptive statistical results even when GSCA is used.*"
  )
  
  return(colcode_section)
}