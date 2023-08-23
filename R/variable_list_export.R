#' Export a standard variable list to support the pk_define() function.
#'
#' Variable list outputted as a .csv file
#' The user should add additional definitions to the file for custom covariates.
#'
#' @param file desired filepath for the variable list
#'
#' @return standard variable list as a .csv file
#'
#' @examplesIf exists("vl_path")
#' ## Write variable list template to server with variable_list_export()
#' vl_path ##User designated filepath for variable list "C:/.../variable.list.csv"
#' if (file.exists(vl_path)) {
#'   variable_list_export(vl_path)
#' }
#'
#' @export
variable_list_export <- function(file) {
  browser()
  dir <- this.path::dirname2(file) #directory of the dataset

  if (dir==".") {
    stop(paste(file, "is not a valid filepath."))
  }

  name <- this.path::basename2(file) #dataset name including extension

  if (!grepl(".csv$", name)) {
    stop(paste("filepath must include document name and .csv suffix."))
  }

  VL <- as.data.frame(VL)
  utils::write.csv(VL, file, row.names = F, na="")
}
