# File: R/rep_htmt.R
# Purpose: Generate HTMT matrix table section for sem4all reports

#' Generate HTMT Matrix Table Section
#'
#' Creates the HTMT matrix table section for the sem4all report.
#'
#' @param results The model results object.
#' @param model_name Name of the model being reported.
#' @return Character vector containing the formatted HTMT section, or NULL if not available.
generate_htmt_section <- function(results, model_name) {
  if (is.null(results$discriminant_validity$htmt)) {
    return(NULL)
  }
  
  htmt_section <- c(
    "", 
    "### HTMT Matrix Table - Model ", model_name, " (in thousandths)",
    "",
    sprintf("| Construct | %s |", paste(rownames(results$discriminant_validity$htmt), collapse = " | "))
  )
  
  htmt_section <- c(htmt_section, sprintf("|---------|%s|", paste(rep("---", ncol(results$discriminant_validity$htmt)), collapse = "|")))
  
  for (i in 1:nrow(results$discriminant_validity$htmt)) {
    construct <- rownames(results$discriminant_validity$htmt)[i]
    row_values <- sapply(1:ncol(results$discriminant_validity$htmt), function(j) {
      if (i == j) {
        "-"
      } else {
        htmt_val <- results$discriminant_validity$htmt[i, j]
        if (htmt_val > 0.85) {
          paste0("<span style=\"color:red; font-weight:bold\">", round(htmt_val * 1000), "</span>")
        } else {
          paste0("<span style=\"color:green; font-weight:bold\">", round(htmt_val * 1000), "</span>")
        }
      }
    })
    htmt_section <- c(htmt_section, sprintf("| **%s** | %s |", construct, paste(row_values, collapse = " | ")))
  }
  
  htmt_section <- c(htmt_section, "", "*Note: HTMT values < 850 indicate adequate discriminant validity (green); values > 850 indicate a violation (red in bold).*")
  
  return(htmt_section)
}