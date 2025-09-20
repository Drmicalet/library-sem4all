# File: R/rep_mm.R
# Purpose: Generate measurement model assessment table section for sem4all reports

#' Generate Measurement Model Assessment Table Section
#'
#' Creates the measurement model assessment table section for the sem4all report.
#'
#' @param results The model results object.
#' @param model_name Name of the model being reported.
#' @return Character vector containing the formatted measurement model section.
generate_measurement_model_section <- function(results, model_name) {
  # Ensure AVE is calculated if not provided
  if (is.null(results$reliability$AVE) || length(results$reliability$AVE) == 0) {
    results$reliability$AVE <- calculate_ave_from_results(results)
  }
  
  # Ensure rhoDG and alpha are calculated if not provided
  if (is.null(results$reliability$rhoDG) || length(results$reliability$rhoDG) == 0) {
    results$reliability$rhoDG <- calculate_rhoDG_from_results(results)
  }
  
  if (is.null(results$reliability$alpha) || length(results$reliability$alpha) == 0) {
    results$reliability$alpha <- calculate_alpha_from_results(results)
  }
  
  # Threshold functions for thousandths representation
  is_good_loading <- function(x) x >= 700
  is_good_rhoDG_alpha <- function(x) x >= 700
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
  
  mm_section <- c(
    "", 
    "### Measurement Model Assessment - Model ", model_name, " (in thousandths)",
    "",
    "||||95%|CI||||",
    "|---|---|---|---|---|---|---|---|",
    "| **Construct & Components** | **λ** | **SD** | **low** | **up** | **α** | **ρDG** | **AVE** |"
  )
  
  # Process each row of the measurement model results
  if (!is.null(results$measurement_model) && nrow(results$measurement_model) > 0) {
    for (i in 1:nrow(results$measurement_model)) {
      row <- results$measurement_model[i, ]
      construct_component <- row$Construct.Components
      
      # Format values
      lambda_val <- if (!is.na(row$lambda)) as.numeric(row$lambda) else NA
      alpha_val <- if (!is.na(row$alpha)) as.numeric(row$alpha) else NA
      rhoDG_val <- if (!is.na(row$rhoDG)) as.numeric(row$rhoDG) else NA
      ave_val <- if (!is.na(row$AVE)) as.numeric(row$AVE) else NA
      
      # Apply formatting
      formatted_lambda <- if (!is.na(lambda_val)) apply_md_formatting(lambda_val, is_good_loading) else ""
      formatted_alpha <- if (!is.na(alpha_val)) apply_md_formatting(alpha_val, is_good_rhoDG_alpha) else ""
      formatted_rhoDG <- if (!is.na(rhoDG_val)) apply_md_formatting(rhoDG_val, is_good_rhoDG_alpha) else ""
      formatted_ave <- if (!is.na(ave_val)) apply_md_formatting(ave_val, is_good_ave) else ""
      
      # Format CI values
      ci_low <- if (!is.na(row$CI_low)) round(row$CI_low * 1000) else ""
      ci_up <- if (!is.na(row$CI_up)) round(row$CI_up * 1000) else ""
      sd_val <- if (!is.na(row$SD)) round(row$SD * 1000) else ""
      
      # Determine if this is a construct header row
      if (construct_component %in% names(results$reliability$AVE)) {
        md_row <- sprintf("| **%s** | | | | | %s | %s | %s |", 
                          construct_component, 
                          formatted_alpha, 
                          formatted_rhoDG, 
                          formatted_ave)
      } else {
        md_row <- sprintf("| %s | %s | %s | %s | %s | | | |", 
                          construct_component, 
                          formatted_lambda, 
                          sd_val, 
                          ci_low, 
                          ci_up)
      }
      
      mm_section <- c(mm_section, md_row)
    }
    
    mm_section <- c(mm_section, "", "*Note: Values reported in thousandths (e.g., 832 = 0.832). Source: Own elaboration with sem4all package (DOI: 10.5281/zenodo.17164443)*")
  }
  
  return(mm_section)
}