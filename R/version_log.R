#' Create and maintain a dataset version log
#'
#' Version log is outputted as a .docx file.
#' Document tracks changes in subject count, record count, new variables, and changing variables.
#' User comments in the word document are preserved between versions.
#'
#' @param file filepath of new dataset
#' @param orig original dataset flag
#' @param outdir output directory, defaults to dataset directory
#' @param prevdata comparison dataset filepath
#' @param template template docx filepath
#' @param comp_var grouping variables for comparison
#' @param src_data string to describe source data
#' @param font font style
#' @param size font size
#' @param orient document orientation
#'
#' @return version log as a .docx file
#'
#' @examplesIf exists("df_path")
#' ## Simple ex domain with 1 subject and 1 dose
#' ex <- data.frame(STUDYID = "ABC101",
#'                  USUBJID = "ABC101-001",
#'                  EXSTDTC = "2000-01-01 10:00:00",
#'                  EXSTDY = 1,
#'                  EXTPTNUM = 0,
#'                  EXDOSE = 100,
#'                  CMT = 1,
#'                  EXTRT = "ABC",
#'                  EXDOSU = "mg",
#'                  VISIT = "Day 1",
#'                  EXTPT = "Dose",
#'                  EXDOSFRQ = "Once",
#'                  EXROUTE = "Oral")
#'
#' ## Simple pc domain with 1 subject and 3 observations
#' pc <- data.frame(USUBJID = "ABC101-001",
#'                  PCDTC = c("2000-01-01 09:40:00",
#'                            "2000-01-01 10:29:00",
#'                            "2000-01-01 12:05:00"),
#'                  PCDY = 1,
#'                  PCTPTNUM = c(0, ##Units of hours
#'                               0.021,
#'                               0.083),
#'                  PCSTRESN = c(NA,
#'                               469,
#'                               870),
#'                  PCLLOQ = 25,
#'                  CMT = 2,
#'                  VISIT = "Day 1",
#'                  PCTPT = c("Pre-dose",
#'                            "30-min post-dose",
#'                            "2-hr post-dose"),
#'                  PCTEST = "ABC",
#'                  PCSTRESU = "ug/mL")
#'
#' ## Create with pk_build()
#' df <- pk_build(ex, pc)
#'
#' ## Write with pk_write()
#' df_path ##User designated filepath "C:/.../dataset.csv"
#' if (file.exists(df_path)) {
#'   pk_write(df, df_path)
#' }
#'
#' ## Document with version_log()
#' if (file.exists(df_path)) {
#'   version_log(df_path, orig = T, comp_var = c("USUBJID", "ATFD", "CMT"))
#' }
#'
#' @export
#'
version_log <- function(file, orig = F, outdir = NULL,
                        prevdata = NULL, template = NULL,
                        comp_var, src_data = "",
                        font = "Times New Roman", size = 9,
                        orient = "landscape") {
  content_type <- is_header <- row_id <- cell_id <- text <- NULL
  DATASET <- COMMENTS <- ROW <- NULL

  # QC: If file doesn't exist, inform user.
  if (!file.exists(file)) {
    stop(paste("Error,", file, "does not exist!"))
  }

  data <- utils::read.csv(file, na.strings=".")
  name <- basename(file)

  # QC: If file is not .csv, throw error.
  if (!grepl("\\.csv$", name)) {
    stop("There is no csv file. Need a csv file.")
  }
  if(is.null(outdir)) {
    outpath <- paste0(dirname(file), "/")
  }
  else {
    outpath <- outdir
  }
  if(orig == T) {
    VersionLog <- data.frame(
      ROW = c("1"),
      DATASET = c(name),
      NSTUD = c(as.character(length(unique(data$NSTUDY)))),
      NSUB = c(as.character(length(unique(data$USUBJID)))),
      NROW = c(as.character(nrow(data))),
      NEW_VAR = c("Original Dataset"),
      CHG_VAR = c("Original Dataset"),
      REF_ROW = c("-"),
      SRCDATA = c(src_data),
      COMMENTS = c("")
    )
    VersionLog2 <- flextable::flextable(VersionLog) #creates flextable object
    VersionLog2 <- flextable::border_inner_h(VersionLog2,
                                             part = "body", #removes inside borders
                                             border = officer::fp_border(color = "grey",
                                                                         width = 0.1,
                                                                         style="solid"))
    VersionLog2 <- flextable::font(VersionLog2,
                                   fontname = font, part = "all") #declare font name
    VersionLog2 <- flextable::fontsize(VersionLog2,
                                       size = size, part = "all")#declare font size
    VersionLog2 <- flextable::bold(VersionLog2, part = "header") #bold the header
    VersionLog2 <- flextable::align(VersionLog2,
                                    align = "center")
    VersionLog2 <- flextable::align(VersionLog2,
                                    align = "center", part = "header")

    if(orient == "landscape") {
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(1), width = 0.46, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(2), width = 1.51, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(3), width = 0.57, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(4), width = 0.48, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(5), width = 0.55, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(6), width = 1.03, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(7), width = 1.03, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(8), width = 0.77, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(9), width = 1.3, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(10), width = 1.3, unit = "in")
      VersionLog2 <- flextable::height(VersionLog2,
                                       height = 0.3, unit = "in") #set row height
    }
    else {
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(1), width = 0.3, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(2), width = 1.2, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(3), width = 0.4, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(4), width = 0.3, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(5), width = 0.4, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(6), width = 0.7, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(7), width = 0.7, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(8), width = 0.5, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(9), width = 1, unit = "in")
      VersionLog2 <- flextable::width(VersionLog2,
                                      j = c(10), width = 1, unit = "in")
      VersionLog2 <- flextable::height(VersionLog2,
                                       height = 0.3, unit = "in") #set row height
    }
    if(is.null(template)) {
      sect_properties <- officer::prop_section(
        page_size = officer::page_size(
          orient = "landscape"))
      flextable::save_as_docx(VersionLog2, path = paste0(outpath, "/VersionLog.docx"), pr_section = sect_properties)
    }
    else {
      tmplt <- officer::read_docx(path = template) #read in template form
      tmplt <- flextable::body_add_flextable(tmplt, VersionLog2) #add flextable to the document
      print(tmplt, target = paste0(outpath, "/VersionLog.docx"))
    }
  }
  else {
    if(orig==F & !is.null(outdir) &
       !(file.exists(paste0(outpath, "/VersionLog.docx")))) {
      stop("There is no preexisting version log in the outpath provided")
    }
    VersionLogdoc <- officer::read_docx(paste0(outpath, "/VersionLog.docx"))
    VersionLogsum <- officer::docx_summary(VersionLogdoc)
    table_cells <- dplyr::filter(VersionLogsum, content_type == "table cell")
    table_data <- dplyr::filter(table_cells, !is_header)
    table_data <- dplyr::select(table_data, row_id, cell_id, text)
    # split data into individual columns
    splits <- split(table_data, table_data$cell_id)
    splits <- lapply(splits, function(x) x$text)
    # combine columns back together in wide format
    table_result <- dplyr::bind_cols(splits)
    # get table headers
    cols <- dplyr::filter(table_cells, is_header)
    names(table_result) <- cols$text
    VersionLog <- table_result
    comments <- dplyr::select(VersionLog, DATASET, COMMENTS)
    for(i in 1:nrow(VersionLog)) {
      if(VersionLog$DATASET[i] == basename(file) & i < nrow(VersionLog)) {
        stop("This file is not the most recent dataset version")
      }
    }
    if(nrow(VersionLog) > 1) {
      VersionLog2 <- dplyr::filter(VersionLog, DATASET != basename(file))
      VersonLog2 <- dplyr::select(VersionLog2, -COMMENTS)
    } else {
      VersionLog2 <- dplyr::select(VersionLog, -COMMENTS)
    }
    if(is.null(prevdata)) {
      prev_data <- utils::read.csv(gsub(basename(file), VersionLog2$DATASET[nrow(VersionLog2)], file), na.strings = ".")
    } else {
      prev_data <- utils::read.csv(prevdata, na.strings = ".")
    }
    check1 <- dplyr::group_by_at(prev_data, comp_var)
    check1 <- dplyr::mutate(check1, COMPROWN = dplyr::row_number())
    check1 <- dplyr::ungroup(check1)

    check2 <- dplyr::group_by_at(data, comp_var)
    check2 <- dplyr::mutate(check2, COMPROWN = dplyr::row_number())
    check2 <- dplyr::ungroup(check2)

    if(max(check1$COMPROWN) > 1 | max(check2$COMPROWN) > 1) {
      stop("The compare variables do not provide unique records")
    }
    table <- summary(arsenal::comparedf(prev_data, data, by = comp_var, int.as.num = TRUE))
    new_var <- table$vars.ns.table$variable
    new_var2 <- paste(new_var, collapse=", ")
    changes <- table$diffs.table$var.y
    changes2 <- unique(changes)
    changes3 <- paste(changes2, collapse=", ")
    if(is.null(prevdata)) {
      ref_row <- dplyr::mutate(VersionLog2, ROW = max(ROW))
    } else {
      ref_row <- dplyr::filter(VersionLog2, DATASET == basename(prevdata))
    }
    VersionLog3 <- dplyr::add_row(VersionLog2,
                                  DATASET = name,
                                  NSTUD = as.character(length(unique(data$NSTUDY))),
                                  NSUB = as.character(length(unique(data$USUBJID))),
                                  NROW = as.character(nrow(data)),
                                  NEW_VAR = new_var2,
                                  CHG_VAR = changes3,
                                  REF_ROW = ref_row$ROW,
                                  SRCDATA = src_data)
    VersionLog3 <- dplyr::mutate(VersionLog3,
                                 ROW = as.character(dplyr::row_number()))
    VersionLog3 <- dplyr::left_join(VersionLog3, comments, by = "DATASET")
    VersionLog3 <- dplyr::filter(VersionLog3, !duplicated(DATASET))

    VersionLog4 <- flextable::flextable(VersionLog3) #creates flextable object
    VersionLog4 <- flextable::border_inner_h(VersionLog4,
                                             part = "body", #removes inside borders
                                             border = officer::fp_border(color = "grey",
                                                                         width = 0.1,
                                                                         style="solid"))
    VersionLog4 <- flextable::font(VersionLog4,
                                   fontname = font, part = "all") #declare font name
    VersionLog4 <- flextable::fontsize(VersionLog4, size = size, part = "all") #declare font size
    VersionLog4 <- flextable::bold(VersionLog4, part = "header") #bold the header
    VersionLog4 <- flextable::align(VersionLog4, align = "center")
    VersionLog4 <- flextable::align(VersionLog4, align = "center", part = "header")
    if(orient == "landscape") {
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(1), width = 0.46, unit = "in") #set column width for particular columns
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(2), width = 1.51, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(3), width = 0.57, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(4), width = 0.48, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(5), width = 0.55, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(6), width = 1.03, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(7), width = 1.03, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(8), width = 0.77, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(9), width = 1.3, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(10), width = 1.3, unit = "in")
      VersionLog4 <- flextable::height(VersionLog4,
                                       height = 0.3, unit = "in") #set row height
    }
    else {
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(1), width = 0.3, unit = "in") #set column width for particular columns
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(2), width = 1.2, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(3), width = 0.4, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(4), width = 0.3, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(5), width = 0.4, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(6), width = 0.7, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(7), width = 0.7, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(8), width = 0.5, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(9), width = 1, unit = "in")
      VersionLog4 <- flextable::width(VersionLog4,
                                      j = c(10), width = 1, unit = "in")
      VersionLog4 <- flextable::height(VersionLog4,
                                       height = 0.3, unit = "in") #set row height
    }
    if(is.null(template)) {
      sect_properties <- officer::prop_section(
        page_size = officer::page_size(
          orient = "landscape"))
      flextable::save_as_docx(VersionLog4, path = paste0(outpath, "/VersionLog.docx"), pr_section = sect_properties)
    }
    else {
      tmplt <- officer::read_docx(path = template) #read in template form
      tmplt <- flextable::body_add_flextable(tmplt, VersionLog4) #add flextable to the document
      print(tmplt, target = paste0(outpath, "/VersionLog.docx"))
    }
  }
}
