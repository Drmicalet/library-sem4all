# File: R/rep_demo.R
# Purpose: Generate demographics table section for sem4all reports

#' Generate Demographics Table Section
#'
#' Creates the demographics table section for the sem4all report.
#'
#' @param demographics Data frame of demographic data (optional).
#' @return Character vector containing the formatted demographics section, or NULL if no demographics provided.
generate_demographics_section <- function(demographics) {
  if (is.null(demographics)) {
    return(NULL)
  }
  
  demo_section <- c(
    "", 
    "### Demographics Table",
    "",
    "| Characteristic | Category | Frequency | Percentage |",
    "|----------------|--------|---------|----------|"
  )
  
  for (i in 1:nrow(demographics)) {
    demo_section <- c(demo_section, sprintf("| %s | %s | %d | %.1f%% |", 
                        demographics$Characteristic[i], demographics$Category[i],
                        demographics$Frequency[i], demographics$Percentage[i]))
  }
  
  demo_section <- c(demo_section, "", "*Note: Source: User-provided demographic data*")
  
  return(demo_section)
}