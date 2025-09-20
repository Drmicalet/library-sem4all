# File: R/utils.R
# Purpose: Utility functions for validation and metric calculation across sem4all

#' Validate Model Results Structure
#'
#' Checks if model results have the expected structure for reporting.
#'
#' @param results The model results object to validate.
#' @return Logical indicating if results structure is valid.
validate_model_results <- function(results) {
  required_components <- c("measurement_model", "structural_model", "model_fit", 
                          "reliability", "discriminant_validity", "descriptive_stats", "outliers")
  
  missing_components <- setdiff(required_components, names(results))
  if (length(missing_components) > 0) {
    warning("Model results missing components: ", paste(missing_components, collapse = ", "))
    # Try to initialize missing components
    for (comp in missing_components) {
      results[[comp]] <- list()
    }
  }
  
  return(results)
}

#' Calculate AVE from model results
#'
#' @param results The model results object.
#' @return A named vector of AVE values.
calculate_ave_from_results <- function(results) {
  # If AVE is already calculated in reliability section, use that
  if (!is.null(results$reliability$AVE) && length(results$reliability$AVE) > 0) {
    return(results$reliability$AVE)
  }
  
  # If measurement model exists, calculate AVE from loadings
  if (!is.null(results$measurement_model) && nrow(results$measurement_model) > 0) {
    # Identify construct rows (where alpha/rhoDG/AVE is not NA)
    construct_rows <- !is.na(results$measurement_model$alpha)
    
    # For each construct, get its indicators and calculate AVE
    ave_values <- sapply(which(construct_rows), function(i) {
      construct <- results$measurement_model$Construct.Components[i]
      indicators <- results$measurement_model$Construct.Components[
        results$measurement_model$Construct.Components == construct]
      
      # Get loadings for these indicators
      loadings <- results$measurement_model$lambda[
        results$measurement_model$Construct.Components %in% indicators]
      
      # Calculate AVE
      sum_squared_loadings <- sum(loadings^2)
      sum_error_variance <- sum(1 - loadings^2)
      sum_squared_loadings / (sum_squared_loadings + sum_error_variance)
    })
    
    names(ave_values) <- results$measurement_model$Construct.Components[construct_rows]
    return(ave_values)
  }
  
  # If we can't calculate it, return NULL
  return(NULL)
}

#' Calculate rhoDG from model results
#'
#' @param results The model results object.
#' @return A named vector of rhoDG values.
calculate_rhoDG_from_results <- function(results) {
  # If rhoDG is already calculated in reliability section, use that
  if (!is.null(results$reliability$rhoDG) && length(results$reliability$rhoDG) > 0) {
    return(results$reliability$rhoDG)
  }
  
  # If measurement model exists, calculate rhoDG from loadings
  if (!is.null(results$measurement_model) && nrow(results$measurement_model) > 0) {
    # Identify construct rows (where alpha/rhoDG/AVE is not NA)
    construct_rows <- !is.na(results$measurement_model$rhoDG)
    
    # For each construct, get its indicators and calculate rhoDG
    rhoDG_values <- sapply(which(construct_rows), function(i) {
      construct <- results$measurement_model$Construct.Components[i]
      indicators <- results$measurement_model$Construct.Components[
        results$measurement_model$Construct.Components == construct]
      
      # Get loadings for these indicators
      loadings <- results$measurement_model$lambda[
        results$measurement_model$Construct.Components %in% indicators]
      
      # Calculate rhoDG
      sum_lambda_sq <- sum(loadings^2)
      sum_error_var <- sum(1 - loadings^2)
      sum_lambda_sq / (sum_lambda_sq + sum_error_var)
    })
    
    names(rhoDG_values) <- results$measurement_model$Construct.Components[construct_rows]
    return(rhoDG_values)
  }
  
  # If we can't calculate it, return NULL
  return(NULL)
}

#' Calculate Cronbach's Alpha from model results
#'
#' @param results The model results object.
#' @return A named vector of alpha values.
calculate_alpha_from_results <- function(results) {
  # If alpha is already calculated in reliability section, use that
  if (!is.null(results$reliability$alpha) && length(results$reliability$alpha) > 0) {
    return(results$reliability$alpha)
  }
  
  # If measurement model exists, calculate alpha from loadings
  if (!is.null(results$measurement_model) && nrow(results$measurement_model) > 0 && 
      !is.null(results$structural_model$correlations)) {
    # Identify construct rows (where alpha/rhoDG/AVE is not NA)
    construct_rows <- !is.na(results$measurement_model$alpha)
    
    # For each construct, get its indicators and calculate alpha
    alpha_values <- sapply(which(construct_rows), function(i) {
      construct <- results$measurement_model$Construct.Components[i]
      indicators <- results$measurement_model$Construct.Components[
        results$measurement_model$Construct Components == construct]
      
      # Get correlations for these indicators
      k <- length(indicators)
      if (k <= 1) return(NA)
      
      corr_matrix <- results$structural_model$correlations[indicators, indicators]
      avg_corr <- mean(corr_matrix[upper.tri(corr_matrix)])
      
      # Calculate Cronbach's Alpha
      (k * avg_corr) / (1 + (k - 1) * avg_corr)
    })
    
    names(alpha_values) <- results$measurement_model$Construct.Components[construct_rows]
    return(alpha_values)
  }
  
  # If we can't calculate it, return NULL
  return(NULL)
}

#' Ensure Proper Structure for Model Results
#'
#' Standardizes model results to have the expected structure.
#'
#' @param results The model results object to standardize.
#' @return Standardized model results object.
standardize_model_results <- function(results) {
  # Initialize missing components
  if (is.null(results$measurement_model)) results$measurement_model <- data.frame()
  if (is.null(results$structural_model)) results$structural_model <- list(correlations = matrix())
  if (is.null(results$model_fit)) results$model_fit <- list()
  if (is.null(results$reliability)) results$reliability <- list()
  if (is.null(results$discriminant_validity)) results$discriminant_validity <- list()
  if (is.null(results$descriptive_stats)) results$descriptive_stats <- data.frame()
  if (is.null(results$outliers)) results$outliers <- list()
  
  # Calculate missing metrics
  if (is.null(results$reliability$AVE) || length(results$reliability$AVE) == 0) {
    results$reliability$AVE <- calculate_ave_from_results(results)
  }
  
  if (is.null(results$reliability$rhoDG) || length(results$reliability$rhoDG) == 0) {
    results$reliability$rhoDG <- calculate_rhoDG_from_results(results)
  }
  
  if (is.null(results$reliability$alpha) || length(results$reliability$alpha) == 0) {
    results$reliability$alpha <- calculate_alpha_from_results(results)
  }
  
  # Calculate Fornell-Larcker if missing
  if (is.null(results$discriminant_validity$fornell_larcker) && 
      !is.null(results$reliability$AVE) && 
      !is.null(results$structural_model$correlations)) {
    results$discriminant_validity$fornell_larcker <- 
      sqrt(diag(results$reliability$AVE)) - results$structural_model$correlations
  }
  
  # Calculate HTMT if missing
  if (is.null(results$discriminant_validity$htmt) && 
      !is.null(results$measurement_model) && 
      !is.null(results$structural_model$correlations)) {
    results$discriminant_validity$htmt <- 
      calculate_htmt_from_results(results)
  }
  
  return(results)
}

#' Calculate HTMT from model results
#'
#' @param results The model results object.
#' @return HTMT matrix.
calculate_htmt_from_results <- function(results) {
  if (is.null(results$measurement_model) || nrow(results$measurement_model) == 0 || 
      is.null(results$structural_model$correlations)) {
    return(NULL)
  }
  
  # Identify constructs
  constructs <- unique(results$measurement_model$Construct.Components[!is.na(results$measurement_model$Construct.Components)])
  n_constructs <- length(constructs)
  
  # Initialize HTMT matrix
  htmt_matrix <- matrix(NA, nrow = n_constructs, ncol = n_constructs, 
                        dimnames = list(constructs, constructs))
  
  # Calculate HTMT for each pair of constructs
  for (i in 1:(n_constructs-1)) {
    for (j in (i+1):n_constructs) {
      construct_i <- constructs[i]
      construct_j <- constructs[j]
      
      # Get indicators for each construct
      indicators_i <- results$measurement_model$Construct.Components[
        results$