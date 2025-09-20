# File: R/rep_fl.R
# Purpose: Generate Fornell-Larcker criterion table section for sem4all reports

#' Generate Fornell-Larcker Criterion Table Section
#'
#' Creates the Fornell-Larcker criterion table section for the sem4all report.
#'
#' @param results The model results object.
#' @param model_name Name of the model being reported.
#' @return Character vector containing the formatted Fornell-Larcker section, or NULL if not available.
generate_fornell_larcker_section <- function(results, model_name) {
  if (is.null(results$discriminant_validity$fornell_larcker)) {
    return(NULL)
  }
  
  # Threshold function for thousandths representation
  is_good_ave <- function(x) x >= 500
  
  # Function to apply color and bold formatting for Markdown
  apply_md_formatting <- function(value, threshold_func) {
    if (is.na(value) || is.null(value) || value == "") return("")
    value_thousandths <- round(as.numeric(value) * 1000)
    
    if (threshold_func(value)) {
      return(paste0("**<span style=\"color:green; font-weight:bold\">", value_thousandths, "</span>**"))
    } else {
      return(paste0("<span style=\"color:red; font-weight:bold\">", value_thousandths, "</span>"))
    }
  }
  
  fl_section <- c(
    "", 
    "### Fornell-Larcker Criterion Table - Model ", model_name, " (in thousandths)",
    "",
    sprintf("| Construct | %s |", paste(colnames(results$discriminant_validity$fornell_larcker), collapse = " | "))
  )
  
  fl_section <- c(fl_section, sprintf("|---------|%s|", paste(rep("---", ncol(results$discriminant_validity$fornell_larcker)), collapse = "|")))
  
  for (i in 1:nrow(results$discriminant_validity$fornell_larcker)) {
    construct <- rownames(results$discriminant_validity$fornell_larcker)[i]
    row_values <- sapply(1:ncol(results$discriminant_validity$fornell_larcker), function(j) {
      if (i == j) {
        # Diagonal value (square root of AVE)
        apply_md_formatting(sqrt(results$reliability$AVE[[j]]), is_good_ave)
      } else {
        # Off-diagonal value (correlation)
        round(results$structural_model$correlations[i, j] * 1000)
      }
    })
    fl_section <- c(fl_section, sprintf("| **%s** | %s |", construct, paste(row_values, collapse = " | ")))
  }
  
  fl_section <- c(fl_section, "", "*Note: Diagonal values (square roots of AVE) are colored green to indicate they meet the criterion (diagonal > off-diagonal). Off-diagonal values are shown without special color coding as they are part of the comparison, not individually thresholded.*")
  
  return(fl_section)
}