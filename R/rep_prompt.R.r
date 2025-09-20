# File: R/rep_prompt.R
# Purpose: Generate AI prompt section for sem4all reports

#' Generate Critical Value Commentary Section
#'
#' Creates the critical value commentary section with AI prompts for the sem4all report.
#'
#' @param results The model results object.
#' @param model_name Name of the model being reported.
#' @return Character vector containing the formatted prompt section, or NULL if no issues found.
generate_prompt_section <- function(results, model_name) {
  issues <- c()
  
  # Check HTMT values
  if (!is.null(results$discriminant_validity$htmt)) {
    htmt_max <- max(results$discriminant_validity$htmt, na.rm = TRUE)
    if (htmt_max > 0.85) {
      issues <- c(issues, paste0("<!-- AI_QUERY: In ", model_name, ", HTMT = ", round(htmt_max, 3), 
                                 " exceeds 0.85. Does this invalidate discriminant validity? ",
                                 "Cite Henseler et al. (2015) and Hair et al. (2022). References:",
                                 "Henseler, J., Ringle, C. M., & Sarstedt, M. (2015).",
                                 "Hair, J. F., Hult, G. T. M., Ringle, C., & Sarstedt, M. (2022). -->"))
    }
  }
  
  # Check SRMR
  if (!is.null(results$model_fit$SRMR) && results$model_fit$SRMR > 0.08) {
    issues <- c(issues, paste0("<!-- AI_QUERY: In ", model_name, ", SRMR = ", round(results$model_fit$SRMR, 3), 
                               " exceeds 0.08. Does this indicate a poor model fit? ",
                               "Cite Cho et al. (2020). Reference: Cho, G., Hwang, H., Sarstedt, M., & Ringle, C. M. (2020). -->"))
  }
  
  # Append issues to the report
  if (length(issues) > 0) {
    prompt_section <- c("", "## Critical Value Commentary")
    prompt_section <- c(prompt_section, issues)
    return(prompt_section)
  }
  
  return(NULL)
}