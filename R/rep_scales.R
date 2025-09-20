# File: R/rep_scales.R
# Purpose: Generate measurement scales table section for sem4all reports

#' Generate Measurement Scales Table Section
#'
#' Creates the measurement scales table section for the sem4all report.
#'
#' @param scales Data frame of scale definitions (optional).
#' @return Character vector containing the formatted scales section, or NULL if no scales provided.
generate_scales_section <- function(scales) {
  if (is.null(scales)) {
    return(NULL)
  }
  
  scales_section <- c(
    "", 
    "### Measurement Scales Table",
    "",
    "| Construct & Indicators | Scale Definition | Sources |",
    "|----------------------|------------------|---------|"
  )
  
  current_construct <- ""
  for (i in 1:nrow(scales)) {
    construct <- scales[i, "Construct & Indicators"]
    if (construct != current_construct) {
      scales_section <- c(scales_section, sprintf("| **%s** | | |", construct))
      current_construct <- construct
    }
    
    if (construct != "") {
      scales_section <- c(scales_section, sprintf("| %s | %s | %s |", 
                                  scales[i, "Construct & Indicators"], 
                                  scales[i, "Scale Definition"], 
                                  scales[i, "Sources"]))
    }
  }
  
  scales_section <- c(scales_section, "", "*Note: Source: User-provided scale definitions*")
  
  return(scales_section)
}