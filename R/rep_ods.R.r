# File: R/rep_ods.R
# Purpose: ODS output generation for sem4all reports

#' Generate ODS Output File
#'
#' Creates an ODS file with colored formatting for each model's results.
#'
#' @param model_results A list containing results from one or more SEM models.
#' @param output_dir Directory to save the ODS file.
#' @return Invisibly returns NULL. Outputs .ods file.
generate_ods_output <- function(model_results, output_dir) {
  # Check if openxlsx is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    warning("Package 'openxlsx' not available. ODS output was not generated.")
    return()
  }
  
  # Create workbook
  wb <- openxlsx::createWorkbook()
  
  # Define color styles
  green_style <- openxlsx::createStyle(fontColour = "#006400", halign = "center", textBold = TRUE)
  red_style <- openxlsx::createStyle(fontColour = "#DC143C", halign = "center")
  header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#E6E6FA")
  
  # Threshold functions for thousandths representation
  is_good_loading <- function(x) x >= 700
  is_good_rhoDG_alpha <- function(x) x >= 700
  is_good_ave <- function(x) x >= 500
  
  # Add sheets for each model
  for (model_name in names(model_results)) {
    results <- model_results[[model_name]]
    
    # Add Measurement Model sheet
    sheet_name <- paste0(model_name, " - Measurement")
    if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)
    openxlsx::addWorksheet(wb, sheet_name)
    
    if (!is.null(results$measurement_model) && nrow(results$measurement_model) > 0) {
      # Convert to thousandths for ODS
      ods_mm <- results$measurement_model
      if (!is.null(ods_mm$lambda)) ods_mm$lambda <- round(ods_mm$lambda * 1000)
      if (!is.null(ods_mm$SD)) ods_mm$SD <- round(ods_mm$SD * 1000)
      if (!is.null(ods_mm$CI_low)) ods_mm$CI_low <- round(ods_mm$CI_low * 1000)
      if (!is.null(ods_mm$CI_up)) ods_mm$CI_up <- round(ods_mm$CI_up * 1000)
      if (!is.null(ods_mm$alpha)) ods_mm$alpha <- round(ods_mm$alpha * 1000)
      if (!is.null(ods_mm$rhoDG)) ods_mm$rhoDG <- round(ods_mm$rhoDG * 1000)
      if (!is.null(ods_mm$AVE)) ods_mm$AVE <- round(ods_mm$AVE * 1000)
      
      openxlsx::writeData(wb, sheet_name, ods_mm, startRow = 2, startCol = 1)
      
      # Apply styles to Measurement Model sheet
      # Lambda column (column 2)
      for (i in 2:(nrow(results$measurement_model)+1)) {
        lambda_val <- results$measurement_model$lambda[i-1]
        if (!is.na(lambda_val)) {
          cell_color <- if (is_good_loading(lambda_val)) green_style else red_style
          openxlsx::addStyle(wb, sheet_name, cell_color, rows = i, cols = 2, gridExpand = TRUE)
        }
      }
      
      # Alpha column (column 6)
      for (i in 2:(nrow(results$measurement_model)+1)) {
        alpha_val <- results$measurement_model$alpha[i-1]
        if (!is.na(alpha_val)) {
          cell_color <- if (is_good_rhoDG_alpha(alpha_val)) green_style else red_style
          openxlsx::addStyle(wb, sheet_name, cell_color, rows = i, cols = 6, gridExpand = TRUE)
        }
      }
      
      # rhoDG column (column 7)
      for (i in 2:(nrow(results$measurement_model)+1)) {
        rhoDG_val <- results$measurement_model$rhoDG[i-1]
        if (!is.na(rhoDG_val)) {
          cell_color <- if (is_good_rhoDG_alpha(rhoDG_val)) green_style else red_style
          openxlsx::addStyle(wb, sheet_name, cell_color, rows = i, cols = 7, gridExpand = TRUE)
        }
      }
      
      # AVE column (column 8)
      for (i in 2:(nrow(results$measurement_model)+1)) {
        ave_val <- results$measurement_model$AVE[i-1]
        if (!is.na(ave_val)) {
          cell_color <- if (is_good_ave(ave_val)) green_style else red_style
          openxlsx::addStyle(wb, sheet_name, cell_color, rows = i, cols = 8, gridExpand = TRUE)
        }
      }
      
      # Add header style
      openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:ncol(results$measurement_model), gridExpand = TRUE)
    }
    
    # Add Fornell-Larcker sheet if available
    if (!is.null(results$discriminant_validity$fornell_larcker)) {
      sheet_name <- paste0(model_name, " - Fornell-Larcker")
      if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)
      openxlsx::addWorksheet(wb, sheet_name)
      
      fl_matrix <- results$discriminant_validity$fornell_larcker
      openxlsx::writeData(wb, sheet_name, fl_matrix, startRow = 1, startCol = 1)
      
      # Apply diagonal green coloring for Fornell-Larcker
      n <- nrow(fl_matrix)
      for (i in 1:n) {
        # Diagonal values (should be green if positive)
        if (fl_matrix[i, i] > 0) {
          openxlsx::addStyle(wb, sheet_name, green_style, rows = i, cols = i+1, gridExpand = TRUE)
        } else {
          openxlsx::addStyle(wb, sheet_name, red_style, rows = i, cols = i+1, gridExpand = TRUE)
        }
      }
      
      # Add header style to Fornell-Larcker sheet
      openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:(n+1), gridExpand = TRUE)
    }
    
    # Add HTMT sheet if available
    if (!is.null(results$discriminant_validity$htmt)) {
      sheet_name <- paste0(model_name, " - HTMT")
      if (nchar(sheet_name) > 31) sheet_name <- substr(sheet_name, 1, 31)
      openxlsx::addWorksheet(wb, sheet_name)
      
      htmt_matrix <- results$discriminant_validity$htmt
      openxlsx::writeData(wb, sheet_name, htmt_matrix, startRow = 1, startCol = 1)
      
      # Apply coloring for HTMT values
      n <- nrow(htmt_matrix)
      for (i in 1:n) {
        for (j in 1:n) {
          if (i != j) {
            htmt_val <- htmt_matrix[i, j]
            if (!is.na(htmt_val)) {
              if (htmt_val > 0.85) {
                openxlsx::addStyle(wb, sheet_name, red_style, rows = i, cols = j+1, gridExpand = TRUE)
              } else {
                openxlsx::addStyle(wb, sheet_name, green_style, rows = i, cols = j+1, gridExpand = TRUE)
              }
            }
          }
        }
      }
      
      # Add header style to HTMT sheet
      openxlsx::addStyle(wb, sheet_name, header_style, rows = 1, cols = 1:(n+1), gridExpand = TRUE)
    }
  }
  
  # Save the workbook
  openxlsx::saveWorkbook(wb, file.path(output_dir, "sem4all_report.ods"), overwrite = TRUE)
}