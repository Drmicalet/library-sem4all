# File: R/rep_quest.R
# Purpose: Generate questionnaire items table section for sem4all reports

#' Generate Questionnaire Items Table Section
#'
#' Creates the questionnaire items table section for the sem4all report.
#'
#' @param questionnaire Data frame of questionnaire items (optional).
#' @return Character vector containing the formatted questionnaire section, or NULL if no questionnaire provided.
generate_questionnaire_section <- function(questionnaire) {
  if (is.null(questionnaire)) {
    return(NULL)
  }
  
  quest_section <- c(
    "", 
    "### Questionnaire Items",
    "",
    "| Item | Text |",
    "|------|------|"
  )
  
  for (i in 1:nrow(questionnaire)) {
    quest_section <- c(quest_section, sprintf("| %s | %s |", 
                        questionnaire$Item[i], questionnaire$Text[i]))
  }
  
  quest_section <- c(quest_section, "", "*Note: Source: User-provided questionnaire items*")
  
  return(quest_section)
}