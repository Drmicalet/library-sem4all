# File: R/rep_outliers.R
# Purpose: Generate Mahalanobis outlier analysis section for sem4all reports

#' Generate Mahalanobis Outlier Analysis Section
#'
#' Creates the Mahalanobis outlier analysis section for the sem4all report.
#'
#' @param results The model results object.
#' @param model_name Name of the model being reported.
#' @return Character vector containing the formatted outlier analysis section, or NULL if not available.
generate_outlier_analysis_section <- function(results, model_name) {
  if (is.null(results$outliers)) {
    return(NULL)
  }
  
  outliers_section <- c(
    "", 
    "### Mahalanobis Outlier Analysis - Model ", model_name,
    "",
    "| Metric | Value |",
    "|--------|-------|"
  )
  
  outliers_section <- c(outliers_section, sprintf("| **Number of Outliers** | %d |", results$outliers$n_outliers))
  outliers_section <- c(outliers_section, sprintf("| **Percentage of Outliers** | %.2f%% |", results$outliers$percent_outliers))
  outliers_section <- c(outliers_section, sprintf("| **Critical Chi-Square Value (α=0.001)** | %.2f |", results$outliers$critical_value))
  
  # Format the max distance with color based on whether it exceeds critical value
  max_distance <- results$outliers$max_distance
  if (max_distance < results$outliers$critical_value) {
    max_distance_formatted <- paste0("**<span style=\"color:green; font-weight:bold\">", round(max_distance, 2), "</span>**")
  } else {
    max_distance_formatted <- paste0("<span style=\"color:red; font-weight:bold\">", round(max_distance, 2), "</span>")
  }
  
  outliers_section <- c(outliers_section, sprintf("| **Max Mahalanobis Distance** | %s |", max_distance_formatted))
  
  outliers_section <- c(outliers_section, "", "*Note: Outliers identified using Mahalanobis distance with a significance level of α=0.001 based on the chi-square distribution with degrees of freedom equal to the number of indicators (Hair et al., 2022). Source: Own elaboration with sem4all package.*")
  
  return(outliers_section)
}