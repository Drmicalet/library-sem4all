# File: R/rep_csv.R
# Purpose: CSV output generation for sem4all reports

#' Generate CSV Output Files
#'
#' Creates CSV files for each model's results.
#'
#' @param model_results A list containing results from one or more SEM models.
#' @param output_dir Directory to save the CSV files.
#' @return Invisibly returns NULL. Outputs .csv files.
generate_csv_output <- function(model_results, output_dir) {
  # Create CSV Output for each model
  for (model_name in names(model_results)) {
    results <- model_results[[model_name]]
    
    # Write Measurement Model table to CSV
    if (!is.null(results$measurement_model) && nrow(results$measurement_model) > 0) {
      # Convert to thousandths for CSV
      csv_mm <- results$measurement_model
      if (!is.null(csv_mm$lambda)) csv_mm$lambda <- round(csv_mm$lambda * 1000)
      if (!is.null(csv_mm$SD)) csv_mm$SD <- round(csv_mm$SD * 1000)
      if (!is.null(csv_mm$CI_low)) csv_mm$CI_low <- round(csv_mm$CI_low * 1000)
      if (!is.null(csv_mm$CI_up)) csv_mm$CI_up <- round(csv_mm$CI_up * 1000)
      if (!is.null(csv_mm$alpha)) csv_mm$alpha <- round(csv_mm$alpha * 1000)
      if (!is.null(csv_mm$rhoDG)) csv_mm$rhoDG <- round(csv_mm$rhoDG * 1000)
      if (!is.null(csv_mm$AVE)) csv_mm$AVE <- round(csv_mm$AVE * 1000)
      
      write.csv(csv_mm, file.path(output_dir, paste0("measurement_model_", model_name, ".csv")), row.names = FALSE)
    }
    
    # Write Descriptive Statistics table to CSV
    if (!is.null(results$descriptive_stats) && nrow(results$descriptive_stats) > 0) {
      # Convert to thousandths for CSV
      csv_ds <- results$descriptive_stats
      if (!is.null(csv_ds$Mean)) csv_ds$Mean <- round(csv_ds$Mean * 1000)
      if (!is.null(csv_ds$SD)) csv_ds$SD <- round(csv_ds$SD * 1000)
      if (!is.null(csv_ds$Min)) csv_ds$Min <- round(csv_ds$Min * 1000)
      if (!is.null(csv_ds$Max)) csv_ds$Max <- round(csv_ds$Max * 1000)
      
      write.csv(csv_ds, file.path(output_dir, paste0("descriptive_stats_", model_name, ".csv")), row.names = FALSE)
    }
    
    # Write Fornell-Larcker table to CSV
    if (!is.null(results$discriminant_validity$fornell_larcker)) {
      fl_matrix <- results$discriminant_validity$fornell_larcker
      write.csv(fl_matrix, file.path(output_dir, paste0("fornell_larcker_", model_name, ".csv")), row.names = TRUE)
    }
    
    # Write HTMT table to CSV
    if (!is.null(results$discriminant_validity$htmt)) {
      htmt_matrix <- results$discriminant_validity$htmt
      write.csv(htmt_matrix, file.path(output_dir, paste0("htmt_", model_name, ".csv")), row.names = TRUE)
    }
    
    # Write Outlier Analysis to CSV
    if (!is.null(results$outliers)) {
      outlier_data <- data.frame(
        Metric = c("Number of Outliers", "Percentage of Outliers", 
                  "Critical Chi-Square Value (α=0.001)", "Max Mahalanobis Distance"),
        Value = c(results$outliers$n_outliers, 
                 results$outliers$percent_outliers,
                 results$outliers$critical_value,
                 results$outliers$max_distance)
      )
      write.csv(outlier_data, file.path(output_dir, paste0("outlier_analysis_", model_name, ".csv")), row.names = FALSE)
    }
  }
}