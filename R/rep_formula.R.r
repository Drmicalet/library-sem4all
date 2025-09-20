# File: R/rep_formula.R
# Purpose: Generate structural equation model formula section for sem4all reports

#' Generate Structural Equation Model Formula Section
#'
#' Creates the structural equation model formula section for the sem4all report.
#'
#' @param results The model results object.
#' @param model_name Name of the model being reported.
#' @param sem_method SEM method used (GSCA or PLS-SEM).
#' @return Character vector containing the formatted formula section.
generate_formula_section <- function(results, model_name, sem_method) {
  formula_section <- c(
    "", 
    paste0("## Model ", model_name, " Results"),
    paste0("**Best-Fitting SEM Method:** ", sem_method),
    "",
    "### Structural Equation Model Representation",
    "",
    "The structural model can be represented as:"
  )
  
  # Generate matrix equation based on structural model
  if (!is.null(results$structural_model$beta)) {
    constructs <- rownames(results$structural_model$beta)
    structural_eqs <- c()
    
    for (i in 1:length(constructs)) {
      eq <- paste0(constructs[i], " = ")
      paths <- c()
      
      for (j in 1:length(constructs)) {
        if (i != j && !is.na(results$structural_model$beta[i, j]) && 
            results$structural_model$beta[i, j] != 0) {
          paths <- c(paths, paste0(round(results$structural_model$beta[i, j], 3), 
                                  " × ", constructs[j]))
        }
      }
      
      if (length(paths) > 0) {
        eq <- paste0(eq, paste(paths, collapse = " + "))
        structural_eqs <- c(structural_eqs, eq)
      }
    }
    
    if (length(structural_eqs) > 0) {
      formula_section <- c(formula_section, "````")
      formula_section <- c(formula_section, structural_eqs)
      formula_section <- c(formula_section, "````")
    } else {
      formula_section <- c(formula_section, "No significant structural paths identified.")
    }
  } else {
    formula_section <- c(formula_section, "No structural model available for representation.")
  }
  
  formula_section <- c(formula_section, "", "*Note: This matrix representation shows the final structural relationships in the model.*")
  
  return(formula_section)
}