# File: R/reporting.R
# Purpose: Main reporting function for sem4all that calls modular components

#' @importFrom methods is
#' @importFrom stats sd
#' @importFrom e1071 skewness kurtosis
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook createStyle addStyle
#' @import sem4all

#' Generate Enhanced Report for sem4all with Multiple Output Formats
#'
#' This function generates comprehensive reports from SEM model results in multiple formats.
#' It handles both GSCA and PLS-SEM results, adds missing metrics, and provides detailed diagnostics.
#'
#' @param model_results A list containing results from one or more SEM models.
#' @param demographics Data frame of demographic data (optional).
#' @param scales Data frame of scale definitions (optional).
#' @param questionnaire Data frame of questionnaire items (optional).
#' @param output_dir Directory to save the output files.
#' @return Invisibly returns NULL. Outputs .md, .csv, and .ods files.
generate_enhanced_report <- function(model_results, 
                                    demographics = NULL, scales = NULL, questionnaire = NULL,
                                    output_dir = ".") {
  # Create output directory if it doesn't exist
  dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Start building the markdown report
  md_report <- c("# sem4all: Comprehensive Output Report")
  md_report <- c(md_report, "")
  
  # Add color coding section
  colcode_section <- generate_color_coding_section()
  md_report <- c(md_report, colcode_section)

  # Process each model in the results
  for (model_name in names(model_results)) {
    results <- model_results[[model_name]]
    
    # Determine SEM method used
    sem_method <- ifelse(grepl("GSCA", model_name, ignore.case = TRUE), "GSCA", "PLS-SEM")
    
    # Add model section header with formula section first
    formula_section <- generate_formula_section(results, model_name, sem_method)
    md_report <- c(md_report, formula_section)
    
    # Add demographics section
    if (!is.null(demographics)) {
      demo_section <- generate_demographics_section(demographics)
      md_report <- c(md_report, demo_section)
    }
    
    # Add scales section
    if (!is.null(scales)) {
      scales_section <- generate_scales_section(scales)
      md_report <- c(md_report, scales_section)
    }
    
    # Add questionnaire section
    if (!is.null(questionnaire)) {
      quest_section <- generate_questionnaire_section(questionnaire)
      md_report <- c(md_report, quest_section)
    }
    
    # Add measurement model assessment section
    mm_section <- generate_measurement_model_section(results, model_name)
    md_report <- c(md_report, mm_section)
    
    # Add Fornell-Larcker criterion section
    fl_section <- generate_fornell_larcker_section(results, model_name)
    if (!is.null(fl_section)) {
      md_report <- c(md_report, fl_section)
    }
    
    # Add HTMT matrix section
    htmt_section <- generate_htmt_section(results, model_name)
    if (!is.null(htmt_section)) {
      md_report <- c(md_report, htmt_section)
    }
    
    # Add descriptive statistics section
    desc_section <- generate_descriptive_stats_section(results, model_name)
    if (!is.null(desc_section)) {
      md_report <- c(md_report, desc_section)
    }
    
    # Add outlier analysis section
    outliers_section <- generate_outlier_analysis_section(results, model_name)
    if (!is.null(outliers_section)) {
      md_report <- c(md_report, outliers_section)
    }
    
    # Add prompt section
    prompt_section <- generate_prompt_section(results, model_name)
    if (!is.null(prompt_section)) {
      md_report <- c(md_report, prompt_section)
    }
  }
  
  # Add references section
  ref_section <- generate_references_section()
  md_report <- c(md_report, ref_section)

  # Write Markdown file
  writeLines(md_report, con = file.path(output_dir, "sem4all_report.md"))

  # Generate CSV output
  generate_csv_output(model_results, output_dir)
  
  # Generate ODS output
  generate_ods_output(model_results, output_dir)
}