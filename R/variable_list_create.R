#' Create a dataframe with standard variable names and definitions
#'
#' Variable list should be used as an input to the apmx::pk_define() function.
#' The user should add additional definitions to the file for custom columns with apmx::variable_list_add().
#'
#' @param variable vector of variable names
#' @param categorization vector of category names
#' @param description vector of variable descriptions
#' @param comment vector of variable comments (can be left NA)
#'
#' @return dataframe of standard variable definitions
#'
#' @examples
#' vl <- variable_list_create(variable = c("WEIGHT", "HEIGHT"),
#'                            categorization = rep("Covariate", 2),
#'                            description = c("weight", "height"))
#'
#' @export
variable_list_create <- function(variable=NULL, categorization=NULL, description=NULL, comment=NA) {
  VL <- as.data.frame(VL)

  if(is.null(variable)) {
    return(VL)
  }

  if(FALSE %in% is.na(comment)) {
    if(length(variable)!=length(categorization) | length(variable)!=length(description) | length(variable)!=length(comment)) {
      stop("variable, categorization, description, and comment must be the same length")
    }
  }

  else {
    if(length(variable)!=length(categorization) | length(variable)!=length(description)) {
      stop("variable, categorization, and description must be the same length")
    }
  }

  temp <- data.frame("Variable" = variable,
                     "Categorization" = categorization,
                     "Description" = description,
                     "Comment" = comment)

  df <- dplyr::bind_rows(VL, temp)

  return(df)
}
