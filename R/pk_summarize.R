#' Produce summary tables for a PK(PD) dataset
#'
#' Summarize BLQ distributions, categorical covariates, and continuous covariates in three tables.
#' Outputs are default .csv files, but can also be .docx and/or .pptx
#' Tables are default stratified by study, but can be stratified by any variable requested by the user.
#'
#' @param df dataset produced by pk_build().
#' @param dir filepath for output directory.
#' @param strat.by vector of variables names to stratify the summary tables.
#' @param ignore.c ignores records flagged in the C column when TRUE.
#' @param na numeric value to be interpreted as NA or missing.
#' @param docx creates summary tables as a Word document when TRUE.
#' @param pptx creates summary tables as a PowerPoint document when TRUE.
#' @param docx.font font for the summary tables in the Word document.
#' @param docx.size font size for the summary tables in the Word document.
#' @param docx.template filepath for template .docx file. When NULL, the summary tables print to a blank document.
#' @param pptx.template filepath for template .pptx file. When NULL, the summary tables print to a blank slide.
#' @param pptx.font font for the summary tables in the PowerPoint document.
#' @param pptx.size font size for the summary tables in the PowerPoint document.
#' @param docx.orientation orientation of .docx files.
#' @param ignore.request vector of additional logical expressions to filter the datase prior to summary.
#'
#' @return summary tables as .csv, .docx, and .pptx files
#'
#' @examples
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
#'
#' ## Generate summary statistics with pk_summarize()
#' pk_summarize(df)
#'
#'
#'
#' @export
pk_summarize <- function(df, dir = NA, strat.by = "NSTUDYC",
                         ignore.c = TRUE, na = -999,
                         docx = FALSE, pptx = FALSE,
                         docx.font="Times New Roman", docx.size=9,
                         docx.template=NULL, pptx.template=NULL,
                         pptx.font="Times New Roman", pptx.size=12,
                         docx.orientation = "portrait", ignore.request = c()) {

  EVID <- Covariate <- Order <- ID <- NSTUDYC <- NULL

  orig <- df
  out <- list()

  ###QC###
  #make sure strat.by is a character or vector of characters
  if(!is.character(strat.by)) {
    stop("strat.by must be in character form only.")
  }

  #ignore.c must be logical
  if(!is.logical(ignore.c)) {
    stop("ignore.c parameter must be TRUE or FALSE.")
  }

  #na must be numeric
  if(!is.numeric(na)) {
    stop("na parameter must be numeric")
  }

  #docx must be logical
  if(!is.logical(docx)) {
    stop("docx parameter must be TRUE or FALSE")
  }

  if(!is.null(docx.template)) {
    docx.dir <- this.path::dirname2(docx.template)
    if (docx.dir==".") {
      stop(paste(docx.template, "is not a valid filepath."))
    }

    docx.name <- this.path::basename2(docx.template) #dataset name including extension
    if (!grepl(".docx$", docx.name)) {
      stop(paste(docx.template, "must include document name and .docx suffix."))
    }
  }

  #pptx must be logical
  if(!is.logical(pptx)) {
    stop("pptx parameter must be TRUE or FALSE")
  }

  if(!is.null(pptx.template)) {
    pptx.dir <- this.path::dirname2(pptx.template)
    if (pptx.dir==".") {
      stop(paste(pptx.template, "is not a valid filepath."))
    }

    pptx.name <- this.path::basename2(pptx.template) #dataset name including extension
    if (!grepl(".pptx$", pptx.name)) {
      stop(paste(pptx.template,  "must include document name and .pptx suffix."))
    }
  }

  if(!is.character(docx.font)) {
    stop("docx.font parameter must be a character expression.")
  }
  if(!is.character(pptx.font)) {
    stop("pptx.font parameter must be a character expression.")
  }

  if(!is.numeric(docx.size)) {
    stop("docx.size parameter must be a number")
  }
  if(!is.numeric(pptx.size)) {
    stop("pptx.size parameter must be a number")
  }

  if(!docx.orientation %in% c("portrait", "landscape")) {
    stop("docx.orientation parameter must be portrait or landscape.")
  }

  # START:  MICHAEL'S ADDITIONS
  # Filter will happen before any kind of processing.
  # DESCRIPTION: The first thing that will happen is that we will see if there are any records to be
  # ignored, and if there is we will places the "*Ignores records flagged by ...", and then after
  # that we see if ignore.c is true, appending to the list of ignored records. We read from the
  # CSV to edit the data that the user has specified. Next, we break apart the vector of characters.
  # It's critical that the user only puts the operations in only the pattern of column operations and conditions.
  # After all of the QA, we pass the arguments into the filter function, and all of the operations
  # are flipped to make it easier for the user to see what actions have been performed on their data.
  ignored_records <- c()

  if (ignore.c == TRUE || (length(ignore.request) != 0)) {
    ignored_records <- c(ignored_records, "*Ignores records flagged by")
  }
  if (ignore.c == TRUE) {
    ignored_records <- c(ignored_records, "C")
  }
  if (length(ignore.request) != 0) {
    valid_operations <- c("==", "<", "<=", ">=", ">", "!=")
    for (item in ignore.request) {

      # Break apart item to get colnames, operation, and value.
      item_separated_by_spaces <- strsplit(item, " ")
      column <- item_separated_by_spaces[[1]][1]
      operation <- item_separated_by_spaces[[1]][2]
      condition <- item_separated_by_spaces[[1]][3]
      # QA for requied items.
      if (is.na(column)) {
        stop("A column is required for this ignore request.")
      }
      else if (is.na(operation)) {
        stop("An operation is required for this ignore request.")
      }
      else if (is.na(condition)) {
        stop("A condition is required for this ignore request.")
      }
      # Check to see if column is valid.
      if (!any(column %in% names(df))) {
        stop(column, " is not a column in the dataset.")
      }
      # Check to see if the operation is valid.
      if (!any(operation %in% valid_operations)) {
        stop(operation, " is not a valid operation. Must be one of the following: ==, <, <=, >, >=, !=")
      }

      # Check if this is a valid record.
      if (!any(condition %in% unlist(df[, column])) && (operation == "==" || operation == "!=")) {
        stop(condition, " is not a record in the dataset.")
      }

      # df <- df %>% filter(eval(parse(text = item)))
      df <- dplyr::filter(df, !eval(parse(text = item)))




      # # This will flip all of the operations that are done.
      # if (operation == "==") {
      #   operation <- "!="
      #   item = paste(column, operation, condition)
      # }
      # else if (operation == "!=") {
      #   operation <- "=="
      #   item = paste(column, operation, condition)
      # }
      # else if (operation == "<") {
      #   operation <- ">"
      #   item = paste(column, operation, condition)
      # }
      # else if (operation == "<=") {
      #   operation <- ">="
      #   item = paste(column, operation, condition)
      # }
      # else if (operation == ">") {
      #   operation <- "<"
      #   item = paste(column, operation, condition)
      # }
      # else if (operation == ">=") {
      #   operation <- "<="
      #   item = paste(column, operation, condition)
      # }

       # Add item to the ignored footer.
      ignored_records <- c(ignored_records, item)

    }
  }
  # END:  MICHAEL'S ADDITIONS.

  if (docx.orientation=="portrait") {
    maxwidth <- 7
  }
  else if (docx.orientation=="landscape") {
    maxwidth <- 10
  }

  #dir must be a filepath
  if (!is.na(dir)) {
    if (!dir.exists(dir)) {
      stop(paste(dir, "is not a valid filepath."))
    }
  }
  if (length(ignore.request)==0) {
    orig <- orig
  }

  else {
    orig <- df
  }

  for (i in strat.by) {
    if (!i %in% colnames(orig)) {
      stop(paste("Column", i, "does not exist in the PK(PD) dataset"))
    }
  }

  orig <- dplyr::mutate(orig, NSTUDYC = as.character(NSTUDYC))

  for (i in strat.by) {
    if (ignore.c==TRUE) {
      df <- orig[is.na(orig$C),]
    }
    else {
      df <- orig
    }

    ### Dataset summary ###
    df.total <- data.frame(Item = c("Subjects", "Total Records", "Dose Records",
                                    "Observation Records", "Other Records"),
                           "Total" = NA)

    df.total$Total[1] <- length(unique(df$ID))
    df.total$Total[2] <- nrow(df)
    df.total$Total[3] <- nrow(df[df$EVID==1,])
    df.total$Total[4] <- nrow(df[df$EVID==0,])
    df.total$Total[5] <- nrow(df[df$EVID==2,])
    df.total$Total <- as.character(df.total$Total)

    for(j in sort(unique(unlist(df[, i])))) {
      df.temp <- data.frame(Item = c("Subjects", "Total Records", "Dose Records",
                                     "Observation Records", "Other Records"),
                            "Total" = NA)
      colnames(df.temp) <- c("Item", j)

      df.temp[1, 2] <- length(unique(df[df[,i]==j, "ID"]))
      df.temp[2, 2] <- nrow(df[df[,i]==j,])
      df.temp[3, 2] <- nrow(df[df$EVID==1 & df[,i]==j,])
      df.temp[4, 2] <- nrow(df[df$EVID==0 & df[,i]==j,])
      df.temp[5, 2] <- nrow(df[df$EVID==2 & df[,i]==j,])
      df.temp[, 2] <- as.character(df.temp[, 2])

      df.total <- dplyr::left_join(df.total, df.temp, by="Item")
    }

    df.total$DVIDC = "General"
    df.total <- df.total[, c(ncol(df.total), 1, 3:(ncol(df.total)-1), 2)]

    df.total.f <- df.total[1,]
    df.total.f[1,] <- rep("General", ncol(df.total))
    df.total <- dplyr::bind_rows(df.total.f, df.total)

    ###BLQ summary###
    df.blq <- data.frame(DVIDC = unique(df$DVIDC[df$EVID==0]),
                         Total = NA)
    df.blq <- tidyr::crossing(df.blq, Item = c(0, 1, 2, 3)) #1 = total, #2 = quantifiable, # = post-dose BLQ

    df.blq <- df.blq[,c(1, 3, 2)]

    for (k in 1:nrow(df.blq)) {
      if (df.blq$Item[k]==1) {
        df.blq$Total[k] <- nrow(df[df$DVIDC==df.blq$DVIDC[k] & df$EVID==0, ])
      }
      else if (df.blq$Item[k]==2) {
        df.blq$Total[k] <- nrow(df[df$DVIDC==df.blq$DVIDC[k] & df$EVID==0 & df$BLQ==0, ])
      }
      else if (df.blq$Item[k]==3) {
        df.blq$Total[k] <- nrow(df[df$DVIDC==df.blq$DVIDC[k] & df$EVID==0 & df$BLQ==2, ])
      }
    }

    for (k in 1:nrow(df.blq)) {
      if (df.blq$Item[k]==1) {

      }
      else if (df.blq$Item[k]==2) {
        df.blq$Total[k] <- paste0(df.blq$Total[k], " (", round(100*as.numeric(df.blq$Total[k])/as.numeric(df.blq$Total[k-1]), 1), "%)")
      }
      else if (df.blq$Item[k]==3) {
        df.blq$Total[k] <- paste0(df.blq$Total[k], " (", round(100*as.numeric(df.blq$Total[k])/as.numeric(df.blq$Total[k-2]), 1), "%)")
      }
    }

    for (j in sort(unique(unlist(df[, i])))) {
      df.temp <- df.blq[, 1:2]
      df.temp$Temp <- NA

      for (k in 1:nrow(df.temp)) {
        if (df.temp$Item[k]==1) {
          df.temp$Temp[k] <- nrow(df[df[,i]==j & df$DVIDC==df.temp$DVIDC[k] & df$EVID==0, ])
        }
        else if (df.temp$Item[k]==2) {
          df.temp$Temp[k] <- nrow(df[df[,i]==j & df$DVIDC==df.temp$DVIDC[k] & df$EVID==0 & df$BLQ==0, ])
        }
        else if (df.temp$Item[k]==3) {
          df.temp$Temp[k] <- nrow(df[df[,i]==j & df$DVIDC==df.temp$DVIDC[k] & df$EVID==0 & df$BLQ==2, ])
        }
      }

      for (k in 1:nrow(df.temp)) {
        if (df.temp$Item[k]==1) {

        }
        else if (df.temp$Item[k]==2) {
          df.temp$Temp[k] <- paste0(df.temp$Temp[k], " (", round(100*as.numeric(df.temp$Temp[k])/as.numeric(df.temp$Temp[k-1]), 1), "%)")
        }
        else if (df.temp$Item[k]==3) {
          df.temp$Temp[k] <- paste0(df.temp$Temp[k], " (", round(100*as.numeric(df.temp$Temp[k])/as.numeric(df.temp$Temp[k-2]), 1), "%)")
        }
      }

      colnames(df.temp) <- c("DVIDC", "Item", j)
      df.blq <- dplyr::left_join(df.blq, df.temp, by=c("DVIDC", "Item"))
    }

    df.blq <- dplyr::mutate_all(df.blq, function(x) gsub("NaN%", "0%", x))

    df.blq <- df.blq[,c(1, 2, 4:ncol(df.blq), 3)]
    df.blq <- dplyr::mutate(df.blq,
                            Item = dplyr::case_when(Item==0 ~ "overwrite",
                                                    Item==1 ~ "Total",
                                                    Item==2 ~ "Quantifiable",
                                                    Item==3 ~ "BLQ"))

    for (j in 1:nrow(df.blq)) {
      if(df.blq$Item[j]=="overwrite") {
        df.blq[j,] <- df.blq[j, 1]
      }
    }

    df.summary <- dplyr::bind_rows(df.total, df.blq)
    merge.col <- c(1)
    for (j in 2:nrow(df.summary)) {
      if (df.summary[j,1]!=df.summary[j-1,1]) {
        merge.col <- c(merge.col, j)
      }
    }
    df.summary <- df.summary[, -1]

    for (j in merge.col) {
      df.summary[j,] <- paste(df.summary[j,], "Observations")
    }

    out <- c(out, list(df.summary))

    if(!is.na(dir)) {
      utils::write.csv(df.summary, file.path(dir, paste0("BLQ_by_", i, ".csv")), row.names = FALSE, quote = FALSE, na=".")
    }

    if(!is.na(dir) & docx==TRUE) {
      flextable::set_flextable_defaults(
        font.size = docx.size,
        font.family = docx.font)

      df.summary1 <- flextable::flextable(df.summary)
      df.summary1 <- flextable::merge_h(df.summary1,
                                        i = merge.col)
      df.summary1 <- flextable::align(df.summary1,
                                      i = merge.col, align="center")
      df.summary1 <- flextable::bold(df.summary1,
                                     i = merge.col)
      df.summary1 <- flextable::bold(df.summary1,
                                     part = "header")

      widths <- flextable::dim_pretty(df.summary1)
      if (sum(unlist(widths[1])) <= maxwidth) {
        df.summary1 <- flextable::autofit(df.summary1)
      }

      if (sum(unlist(widths[1])) > maxwidth) {
        result.col <- max(unlist(widths[1])[2:length(unlist(widths[1]))])
        if (result.col * (ncol(df.summary)-1) < (maxwidth - 1)) {
          df.summary1 <- flextable::width(df.summary1,
                                          j = 2:ncol(df.summary), width = result.col)
          df.summary1 <- flextable::width(df.summary1,
                                          j = 1, width = maxwidth - result.col * (ncol(df.summary)-1))
        }
      }

      # START: Michael's additions
      # DESCRIPTION: This just addes to the footer of all of the operations on the data.
      if (length(ignore.request) != 0) {
        ignored_string <- paste(ignored_records, collapse = ", ")
      }
      else {
        ignored_string <- ignored_records
      }

      if (ignore.c==TRUE || length(ignore.request) != 0) {
        # df.summary1 <- df.summary1 %>%
        #   flextable::add_footer_lines(values = ignored_string)
        df.summary1 <- flextable::add_footer_lines(df.summary1, values = ignored_string)

      }
      # END:   Michael's additions

      # if (ignore.c==T) {
      #   df.summary1 <- flextable::add_footer_lines(df.summary1,
      #                                              values = "*Ignores records flagged by C")
      # }

      tmplt <- officer::read_docx(path=docx.template)
      tmplt <- flextable::body_add_flextable(tmplt, df.summary1)
      print(tmplt, target = file.path(dir, paste0("BLQ_by_", i, ".docx")))
    }

    if(!is.na(dir) & pptx==TRUE) {
      flextable::set_flextable_defaults(
        font.size = pptx.size,
        font.family = pptx.font)

      df.summary1 <- flextable::flextable(df.summary)
      df.summary1 <- flextable::merge_h(df.summary1,
                                        i = merge.col)
      df.summary1 <- flextable::align(df.summary1,
                                      i = merge.col, align="center")
      df.summary1 <- flextable::bold(df.summary1,
                                     i = merge.col)
      df.summary1 <- flextable::bold(df.summary1,
                                     part = "header")

      tmplt <- officer::read_pptx(path=pptx.template)
      tmplt <- officer::add_slide(tmplt)
      tmplt <- officer::ph_with(tmplt,
                                value = df.summary1, location = officer::ph_location_fullsize())
      print(tmplt, target = file.path(dir, paste0("BLQ_by_", i, ".pptx")))
    }

    ###Cat covariate summary###
    cov.c <- cov_find(df, cov="categorical", type="character")
    cov.n <- cov_find(df, cov="categorical", type="numeric")

    cov <- data.frame("Covariate" = NA,
                      "ValueN" = NA,
                      "Value" = NA)

    df <- dplyr::filter(df, EVID<2)

    for (j in cov.n) {
      df.temp <- df[, c(j, paste0(j, "C"))]
      df.temp <- dplyr::mutate(df.temp, Covariate = j)
      df.temp <- dplyr::distinct(df.temp)

      colnames(df.temp) <- c("ValueN", "Value", "Covariate")
      cov <- dplyr::bind_rows(cov, df.temp)
      cov <- dplyr::filter(cov, !is.na(Covariate))
    }

    nsub <- length(unique(df$USUBJID))

    cov$Total <- NA
    for (k in 1:nrow(cov)) {
      cov$Total[k] <- as.character(length(unique(df$USUBJID[df[,cov$Covariate[k]]==cov$ValueN[k]])))
    }

    for (k in 1:nrow(cov)) {
      cov$Total[k] <- paste0(cov$Total[k], " (", round(100*as.numeric(cov$Total[k])/nsub, 1), "%)")
    }

    for (j in sort(unique(unlist(df[,i])))) {
      df.temp <- cov[, 1:3]
      df.temp$Temp <- NA

      nsub <- length(unique(df$USUBJID[df[,i]==j]))

      for (k in 1:nrow(df.temp)) {
        df.temp$Temp[k] <- length(unique(df$USUBJID[df[,df.temp$Covariate[k]]==df.temp$ValueN[k] & df[,i]==j]))
      }

      for (k in 1:nrow(df.temp)) {
        df.temp$Temp[k] <- paste0(df.temp$Temp[k], " (", round(100*as.numeric(df.temp$Temp[k])/nsub, 1), "%)")
      }

      colnames(df.temp) <- c("Covariate", "ValueN", "Value", j)
      cov <- dplyr::left_join(cov, df.temp, by=c("Covariate", "ValueN", "Value"))
    }

    cov <- cov[,c(1, 3, 5:ncol(cov), 4)]
    cov$Value[is.na(cov$Value)] <- "MISSING"

    df.temp <- cov
    for (j in 1:nrow(df.temp)) {
      df.temp[j,] <- df.temp[j, 1]
    }
    df.temp <- dplyr::distinct(df.temp)
    df.temp <- dplyr::mutate(df.temp, Order = 0)

    cov <- dplyr::mutate(cov, Order = 1)
    cov <- dplyr::bind_rows(cov, df.temp)
    cov <- dplyr::arrange(cov, Covariate, Order)
    cov <- dplyr::select(cov, -Order)

    merge.col <- c()
    for (j in 1:nrow(cov)) {
      if (cov[j,1]==cov[j,ncol(cov)]) {
        merge.col <- c(merge.col, j)
      }
    }
    cov <- cov[,-1]

    out <- c(out, list(cov))

    if(!is.na(dir)) {
      utils::write.csv(cov, file.path(dir, paste0("CATCOV_by_", i, ".csv")), row.names = FALSE, quote = FALSE, na=".")
    }

    if(!is.na(dir) & docx==TRUE) {
      flextable::set_flextable_defaults(
        font.size = docx.size,
        font.family = docx.font)

      cov1 <- flextable::flextable(cov)
      cov1 <- flextable::merge_h(cov1,
                                 i = merge.col)
      cov1 <- flextable::align(cov1,
                               i = merge.col, align="center")
      cov1 <- flextable::bold(cov1,
                              i = merge.col)
      cov1 <- flextable::bold(cov1,
                              part = "header")
      cov1 <- flextable::width(cov1,
                               j = 1, width = 2, unit="in")

      widths <- flextable::dim_pretty(cov1)
      if (sum(unlist(widths[1])) <= maxwidth) {
        cov1 <- flextable::autofit(cov1)
      }

      if (sum(unlist(widths[1])) > maxwidth) {
        result.col <- max(unlist(widths[1])[2:length(unlist(widths[1]))])
        if (result.col * (ncol(cov)-1) < (maxwidth - 1)) {
          cov1 <- flextable::width(cov1,
                                   j = 2:ncol(cov), width = result.col)
          cov1 <- flextable::width(cov1,
                                   j = 1, width = maxwidth - result.col * (ncol(cov)-1))
        }
      }

      # if (ignore.c==T) {
      #   cov1 <- flextable::add_footer_lines(cov1, values = "*Ignores records flagged by C")
      # }

      # START: Michael's additions
      # DESCRIPTION: This just addes to the footer of all of the operations on the data.
      if (ignore.c==TRUE || length(ignore.request) != 0) {
        cov1 <- flextable::add_footer_lines(cov1, values = ignored_string)
      }
      # END:   Michael's additions

      tmplt <- officer::read_docx(path=docx.template)
      tmplt <- flextable::body_add_flextable(tmplt, cov1)
      print(tmplt, target = file.path(dir, paste0("CATCOV_by_", i, ".docx")))
    }

    if(!is.na(dir) & pptx==TRUE) {
      flextable::set_flextable_defaults(
        font.size = pptx.size,
        font.family = pptx.font)

      cov1 <- flextable::flextable(cov)
      cov1 <- flextable::merge_h(cov1,
                                 i = merge.col)
      cov1 <- flextable::align(cov1,
                               i = merge.col, align="center")
      cov1 <- flextable::bold(cov1, i = merge.col)
      cov1 <- flextable::bold(cov1, part = "header")

      tmplt <- officer::read_pptx(path=pptx.template)
      tmplt <- officer::add_slide(tmplt)
      tmplt <- officer::ph_with(tmplt,
                                value = cov1, location = officer::ph_location_fullsize())
      print(tmplt, target = file.path(dir, paste0("CATCOV_by_", i, ".pptx")))
    }

    ###Cont cov summary###
    if(length(cov_find(df, cov="continuous", type="numeric"))>0) {
      df.cont <- dplyr::select(df,
                               ID, dplyr::all_of(i), dplyr::all_of(cov_find(df, cov="continuous", type="numeric")))

      cov <- data.frame(Covariate = colnames(df.cont)[3:ncol(df.cont)],
                        Total = NA)
      cov <- tidyr::crossing(cov, Measure = c(0, 1, 2, 3, 4, 5, 6))

      cov <- cov[,c(1, 3, 2)]

      for (k in 1:nrow(cov)) {
        df.temp <- dplyr::select(df.cont,
                                 ID, dplyr::all_of(cov$Covariate[k]))
        df.temp <- dplyr::distinct(df.temp)

        df.temp.1 <- df.temp[df.temp[, cov$Covariate[k]]!=na, ]
        df.temp.2 <- df.temp[df.temp[, cov$Covariate[k]]==na, ]

        if (cov$Measure[k]==1) {
          cov$Total[k] <- as.character(length(unique(df.temp.1$ID)))
        }
        else if (cov$Measure[k]==2) {
          cov$Total[k] <- paste0(round(mean(unlist(df.temp.1[,2])), 2), " (", round(stats::sd(unlist(df.temp.1[,2])), 2), ")")
        }
        else if (cov$Measure[k]==3) {
          cov$Total[k] <- paste0(round(stats::median(unlist(df.temp.1[,2])), 2), " (", round(stats::quantile(unlist(df.temp.1[,2]), probs = 0.25), 2), "; ", round(stats::quantile(unlist(df.temp.1[,2]), probs = 0.75), 2), ")")
        }
        else if (cov$Measure[k]==4) {
          cov$Total[k] <- paste0(round(stats::quantile(unlist(df.temp.1[,2]), probs = 0.05), 2), "; ", round(stats::quantile(unlist(df.temp.1[,2]), probs = 0.95), 2))
        }
        else if (cov$Measure[k]==5) {
          cov$Total[k] <- paste0(round(min(df.temp.1[,2]), 2), "; ", round(max(df.temp.1[,2]), 2))
        }
        else if (cov$Measure[k]==6) {
          cov$Total[k] <- as.character(length(unique(df.temp.2$ID)))
        }
      }

      for (j in sort(unique(unlist(df.cont[,i])))) {
        df.temp <- cov[, 1:2]
        df.temp$Temp <- NA

        for (k in 1:nrow(cov)) {
          df.temp.1 <- df.cont[df.cont[,i]==j, ]
          df.temp.1 <- dplyr::select(df.temp.1,
                                     ID, dplyr::all_of(cov$Covariate[k]))
          df.temp.1 <- dplyr::distinct(df.temp.1)

          df.temp.1.1 <- df.temp.1[df.temp.1[, df.temp$Covariate[k]]!=na, ]
          df.temp.1.2 <- df.temp.1[df.temp.1[, df.temp$Covariate[k]]==na, ]

          if (df.temp$Measure[k]==1) {
            df.temp$Temp[k] <- as.character(length(unique(df.temp.1.1$ID)))
          }
          else if (df.temp$Measure[k]==2 & nrow(df.temp.1.1)==0) {
            df.temp$Temp[k] <- "--"
          }
          else if (df.temp$Measure[k]==2) {
            df.temp$Temp[k] <- paste0(round(mean(unlist(df.temp.1.1[,2])), 2), " (", round(stats::sd(unlist(df.temp.1.1[,2])), 2), ")")
          }
          else if (df.temp$Measure[k]==3 & nrow(df.temp.1.1)==0) {
            df.temp$Temp[k] <- "--"
          }
          else if (df.temp$Measure[k]==3) {
            df.temp$Temp[k] <- paste0(round(stats::median(unlist(df.temp.1.1[,2])), 2), " (", round(stats::quantile(unlist(df.temp.1.1[,2]), probs = 0.25), 2), "; ", round(stats::quantile(unlist(df.temp.1[,2]), probs = 0.75), 2), ")")
          }
          else if (df.temp$Measure[k]==4 & nrow(df.temp.1.1)==0) {
            df.temp$Temp[k] <- "--"
          }
          else if (df.temp$Measure[k]==4) {
            df.temp$Temp[k] <- paste0(round(stats::quantile(unlist(df.temp.1.1[,2]), probs = 0.05), 2), "; ", round(stats::quantile(unlist(df.temp.1.1[,2]), probs = 0.95), 2))
          }
          else if (df.temp$Measure[k]==5 & nrow(df.temp.1.1)==0) {
            df.temp$Temp[k] <- "--"
          }
          else if (df.temp$Measure[k]==5) {
            df.temp$Temp[k] <- paste0(round(min(df.temp.1.1[,2]), 2), "; ", round(max(df.temp.1.1[,2]), 2))
          }
          else if (df.temp$Measure[k]==6) {
            df.temp$Temp[k] <- as.character(length(unique(df.temp.1.2$ID)))
          }
        }

        colnames(df.temp) <- c("Covariate", "Measure", j)
        cov <- dplyr::left_join(cov, df.temp, by=c("Covariate", "Measure"))
      }

      cov <- cov[,c(1:2, 4:ncol(cov), 3)]
      cov <- dplyr::mutate(cov, Measure = dplyr::case_when(Measure==0 ~ "overwrite",
                                                           Measure==1 ~ "NSUB",
                                                           Measure==2 ~ "MEAN (SD)",
                                                           Measure==3 ~ "MEDIAN (IQR)",
                                                           Measure==4 ~ "P05-P95",
                                                           Measure==5 ~ "MIN-MAX",
                                                           Measure==6 ~ "MISSING"))

      for (j in 1:nrow(cov)) {
        if (cov$Measure[j]=="overwrite") {
          cov[j,] <- cov$Covariate[j]
        }
      }

      merge.col <- c(1)
      for (j in 2:nrow(cov)) {
        if (cov[j,1]!=cov[j-1,1]) {
          merge.col <- c(merge.col, j)
        }
      }
      cov <- cov[, -1]

      out <- append(out, list(cov))

      if(!is.na(dir)) {
        utils::write.csv(cov, file.path(dir, paste0("CONTCOV_by_", i, ".csv")), row.names = FALSE, quote = FALSE, na=".")
      }

      if(!is.na(dir) & docx==TRUE) {
        flextable::set_flextable_defaults(
          font.size = docx.size,
          font.family = docx.font)

        cov1 <- flextable::flextable(cov)
        cov1 <- flextable::merge_h(cov1,
                                   i = merge.col)
        cov1 <- flextable::align(cov1,
                                 i = merge.col, align="center")
        cov1 <- flextable::bold(cov1,
                                i = merge.col)
        cov1 <- flextable::bold(cov1,
                                part = "header")

        widths <- flextable::dim_pretty(cov1)
        if (sum(unlist(widths[1])) <= maxwidth) {
          cov1 <- flextable::autofit(cov1)
        }

        if (sum(unlist(widths[1])) > maxwidth) {
          result.col <- max(unlist(widths[1])[2:length(unlist(widths[1]))])
          if (result.col * (ncol(cov)-1) < (maxwidth-1)) {
            cov1 <- flextable::width(cov1,
                                     j = 2:ncol(cov), width = result.col)
            cov1 <- flextable::width(cov1,
                                     j = 1, width = maxwidth - result.col * (ncol(cov)-1))
          }
        }

        # if (ignore.c==T) {
        #   cov1 <- flextable::add_footer_lines(cov1, values = "*Ignores records flagged by C")
        # }
      # START: Michael's additions
      # DESCRIPTION: This just addes to the footer of all of the operations on the data.
      if (ignore.c==TRUE || length(ignore.request) != 0) {
        cov1 <- flextable::add_footer_lines(cov1, values = ignored_string)

      }
      # END:   Michael's additions

        tmplt <- officer::read_docx(path=docx.template)
        tmplt <- flextable::body_add_flextable(tmplt, cov1)
        print(tmplt, target = file.path(dir, paste0("CONTCOV_by_", i, ".docx")))
      }

      if(!is.na(dir) & pptx==TRUE) {
        flextable::set_flextable_defaults(
          font.size = pptx.size,
          font.family = pptx.font)

        cov1 <- flextable::flextable(cov)
        cov1 <- flextable::merge_h(cov1,
                                   i = merge.col)
        cov1 <- flextable::align(cov1,
                                 i = merge.col, align="center")
        cov1 <- flextable::bold(cov1,
                                i = merge.col)
        cov1 <- flextable::bold(cov1,
                                part = "header")

        tmplt <- officer::read_pptx(path=pptx.template)
        tmplt <- officer::add_slide(tmplt)
        tmplt <- officer::ph_with(tmplt,
                                  value = cov1, location = officer::ph_location_fullsize())
        print(tmplt, target = file.path(dir, paste0("CONTCOV_by_", i, ".pptx")))
      }
    }
  }

  #out <- out[2:length(out)]
  return(out)
}
