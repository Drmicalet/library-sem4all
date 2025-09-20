# File: R/rep_desc.R
# Purpose: Generate descriptive statistics table section for sem4all reports

#' Generate Descriptive Statistics Table Section
#'
#' Creates the descriptive statistics table section for the sem4all report.
#'
#' @param results The model results object.
#' @param model_name Name of the model being reported.
#' @return Character vector containing the formatted descriptive statistics section, or NULL if not available.
generate_descriptive_stats_section <- function(results, model_name) {
  if (is.null(results$descriptive_stats) || nrow(results$descriptive_stats) == 0) {
    return(NULL)
  }
  
  desc_section <- c(
    "", 
    "### Descriptive Statistics - Model ", model_name, " (in thousandths)",
    "",
    "| Indicator | Mean | SD | Skew | Kurt | Min | Max |",
    "|---------|------|----|------|------|-----|-----|"
  )
  
  for (i in 1:nrow(results$descriptive_stats)) {
    row <- results$descriptive_stats[i, ]
    skew_val <- as.numeric(row$Skew)
    kurt_val <- as.numeric(row$Kurt)
    
    # Format values as thousandths
    mean_val <- round(row$Mean * 1000)
    sd_val <- round(row$SD * 1000)
    min_val <- round(row$Min * 1000)
    max_val <- round(row$Max * 1000)
    
    # Format skew and kurt (no special coloring in MD for this version)
    skew_formatted <- round(skew_val * 1000)
    kurt_formatted <- round(kurt_val * 1000)
    
    md_row <- sprintf("| %s | %d | %d | %d | %d | %d | %d |", 
                      row$Indicator, mean_val, sd_val, 
                      skew_formatted, kurt_formatted, 
                      min_val, max_val)
    desc_section <- c(desc_section, md_row)
  }
  
  desc_section <- c(desc_section, "", "*Note: Values reported in thousandths (e.g., 342 = 0.342). 'Skew' = Skewness, 'Kurt' = Kurtosis. GSCA is robust to violations of normality assumptions (Hwang & Takane, 2014), making these metrics informative rather than restrictive for model validity.*")
  
  return(desc_section)
}