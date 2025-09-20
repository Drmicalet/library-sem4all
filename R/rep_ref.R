# File: R/rep_ref.R
# Purpose: References section for sem4all reports

#' Generate References Section
#'
#' Creates the references section for the sem4all report.
#'
#' @return Character vector containing the formatted references section.
generate_references_section <- function() {
  references <- c(
    "", 
    "## References",
    "",
    "**Software and Libraries:**",
    "*   R Core Team. (2023). R: A language and environment for statistical computing. R Foundation for Statistical Computing. https://www.R-project.org/",
    "*   van der Loo, M. P. J. (2020). *magrittr*: A Forward-Pipe Operator for R.",
    "*   Wickham, H. (2023). *tidyverse*: Easily Install and Load the 'Tidyverse'.",
    "*   Wickham, H., François, R., Henry, L., & Müller, K. (2023). *dplyr*: A Grammar of Data Manipulation.",
    "*   Wickham, H. (2023). *ggplot2*: Create Elegant Data Visualisations Using the Grammar of Graphics.",
    "*   Talbot, M. (2021). *DiagrammeR*: Graph and Network Visualization.",
    "*   Ushey, K., Allaire, J., Tang, Y., & McGratten, D. (2023). *reticulate*: Interface to 'Python'.",
    "*   Pandas Development Team. (2023). Pandas: powerful Python data analysis toolkit. https://pandas.pydata.org/",
    "*   Hunter, J. D. (2007). Matplotlib: A 2D graphics environment. *Computing in Science & Engineering*, 9(3), 90–95.",
    "*   Virtanen, P., et al. (2020). SciPy 1.0: Fundamental Algorithms for Scientific Computing in Python. *Nature Methods*, 17, 261–272.",
    "*   Afthanorhan, A., Awang, Z., & Mamat, M. (2016). A comparative study between GSCA-SEM and PLS-SEM. *MJ Journal on Statistics and Probability*, 1, 63–72.",
    "*   Rosseel, Y. (2012). lavaan: An R package for structural equation modeling. *Journal of Statistical Software*, 48(2), 1-36.",
    "*   Talbot, M. (2021). DiagrammeR: Graph and Network Visualization. R package version 1.0.10.",
    "*   Ushey, K., Allaire, J., Tang, Y., & McGratten, D. (2023). reticulate: Interface to 'Python'. R package version 1.34.",
    "*   Wickham, H., Chan, T., Seidel, D., & Csárdi, G. (2011). testthat: Get Started with Testing. *The R Journal*, 3(1), 5-10.",
    "*   Mayol-Tur, M. (2025). gsca_ridge: Generalized Structured Component Analysis with Ridge Regularization (Version 1.0.0) [Computer software]. https://doi.org/10.5281/zenodo.17155474",
    "*   Rönkkö, M. (2021). cSEM: Composite-Based Structural Equation Modeling (Version 1.0.13) [Computer software]. https://github.com/mronkko/csem",
    "*   Rönkkö, M. (2021). matrixpls: Matrix-Based Partial Least Squares Estimation (Version 1.0.13) [Computer software]. https://cran.r-project.org/package=matrixpls",
    "*   Hennig, P.-G., Rademaker, C., Sarstedt, M., & Schuberth, F. (2021). gesca: Generalized Structure Component Analysis (Version 1.0.1) [Computer software]. https://cran.r-project.org/package=gesca",
    "*   Sanchez, G. (2021). plspm: Tools for Partial Least Squares Path Modeling (PLS-PM) (Version 0.4.9) [Computer software]. https://cran.r-project.org/package=plspm",
    "*   Sanchez, G. (2021). semPLS: Structural Equation Modeling Using Partial Least Squares (Version 1.3-4) [Computer software]. https://cran.r-project.org/package=semPLS",
    "*   Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization paths for generalized linear models via coordinate descent. *Journal of Statistical Software*, 33(1), 1–22.",
    "*   Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D., François, R., ... & Yutani, H. (2019). Welcome to the tidyverse. *Journal of Open Source Software*, 4(43), 1686.",
    "*   Yoon, C., & Hwang, H. (2021). GSCA Pro: Software for generalized structured component analysis. *Multivariate Behavioral Research*, 56(4), 678–685.",
    "*   Ringle, C. M., Wende, S., & Becker, J. M. (2015). SmartPLS 3. Boenningstedt: SmartPLS GmbH. http://www.smartpls.com",
    "*   IBM Corp. Released 2021. IBM SPSS Statistics for Windows (Version 28.0) [Computer software]. Armonk, NY: IBM Corp.",
    "*   Free Software Foundation. (2021). GNU PSPP: Statistical analysis software (Version 1.5.0) [Computer software]. https://www.gnu.org/software/pspp/",
    "*   Doornik, J. A. (2021). OxMetrics: An Object-Oriented Econometric Software Package (Version 8) [Computer software]. London: Timberlake Consultants Ltd.",
    "*   SAS Institute Inc. (2021). SAS/STAT 9.4 User's Guide [Computer software]. Cary, NC: SAS Institute Inc.",
    "*   StataCorp. (2021). Stata Statistical Software: Release 17 [Computer software]. College Station, TX: StataCorp LLC.",
    "*   SAS Institute Inc. (2021). JMP 16 [Computer software]. Cary, NC: SAS Institute Inc.",
    "",
    "**Methodological Foundations and Thresholds:**",
    "*   Hwang, H., & Takane, Y. (2014). *Generalized structured component analysis: A component-based approach to structural equation modeling*. CRC Press.",
    "*   Cho, G., Hwang, H., Sarstedt, M., & Ringle, C. M. (2020). Cutoff criteria for overall model fit indexes in generalized structured component analysis. *Journal of Marketing Analytics*, 8(4), 189–202.",
    "*   Fornell, C., & Larcker, D. F. (1981). Evaluating structural equation models with unobservable variables and measurement error. *Journal of Marketing Research*, 18(1), 39–50.",
    "*   Henseler, J., Ringle, C. M., & Sarstedt, M. (2015). A new criterion for assessing discriminant validity in variance-based structural equation modeling. *Journal of the Academy of Marketing Science*, 43(1), 115–142.",
    "*   Cohen, J. (1988). *Statistical power analysis for the behavioral sciences* (2nd ed.). Lawrence Erlbaum Associates.",
    "*   Hair, J. F., Hult, G. T. M., Ringle, C., & Sarstedt, M. (2022). *A primer on partial least squares structural equation modeling (PLS-SEM)* (3rd ed.). Sage Publications.",
    "*   Nunnally, J. C., & Bernstein, I. H. (1994). *Psychometric theory* (3rd ed.). McGraw-Hill.",
    "*   Faul, F., Erdfelder, E., Buchner, A. and Lang, A.-G. (2009) 'Statistical power analyses using G\\*Power 3.1: Tests for correlation and regression analyses', *Behavior Research Methods*, 41(4), pp. 1149–1160.",
    "*   Tanaka, J. S.,& Huba, G. J. (1985). A fit index for covariance structure models under arbitrary GLS estimation. *British Journal of Mathematical and Statistical Psychology*, 38(2), 197–201.",
    "*   Tarafdar, M., Tu, Q., Ragu-Nathan, T.S. (2010). Impact of Technostress on End-User Satisfaction and Performance. *Journal of Management Information Systems* 27, 303–334."
  )
  
  return(references)
}