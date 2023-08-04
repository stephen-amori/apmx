# This is for debugging. Need this library.
# To download this library we need to use:
# devtools::install_github("ManuelHentschel/vscDebugger")
# library(vscDebugger)
# When debugging we need to load in all of the libraries. Below is the R code does just that.
# pkgload::load_all()
#  library(vscDebugger)
# install.packages("tidyr")
# library(tidyr)
# pkgload::load_all()
# rm(list = c("pk_write"))
library(testthat)
library(tidyr)
# library(dplyr)
# library(haven)
# devtools::install_github("stephen-amori/apmx")
library(apmx)
library(tibble)
library(dplyr)
# library(flextable)
# library(xfun)



# START of Global variables
ex_iso_dates <- c( "2023-05-17T08:30:00Z",
                "2025-12-25T12:00:00Z",
                "2021-01-01T00:00:01Z",
                "2022-11-11T11:11:11Z",
                "2024-02-29T14:29:00Z",
                "2020-07-04T18:00:00Z",
                "2023-10-31T23:59:59Z",
                "2022-02-14T20:00:00Z",
                "2021-12-31T23:59:59Z",
                "2025-06-30T13:00:00Z")

pd_iso_dates <- c("2026-03-17T09:30:00Z",
               "2027-07-21T14:00:00Z",
               "2028-01-15T00:00:02Z",
               "2029-09-09T12:12:12Z",
               "2030-04-04T15:30:00Z",
               "2031-08-15T19:00:00Z",
               "2032-11-01T00:59:59Z",
               "2033-03-03T21:00:00Z",
               "2034-12-30T23:59:59Z",
               "2035-05-31T14:00:00Z")

pc_iso_dates <- c("2036-04-15T10:33:00Z",
               "2037-08-19T15:07:00Z",
               "2038-02-17T01:01:03Z",
               "2039-10-08T13:14:13Z",
               "2040-05-05T16:35:00Z",
               "2041-09-16T20:07:00Z",
               "2042-11-02T01:59:58Z",
               "2043-04-04T22:07:00Z",
               "2044-12-29T00:59:58Z",
               "2045-07-01T15:07:00Z")

tv.cov_iso_dates <- c("2046-08-01T08:45:00Z",
               "2047-03-12T18:05:00Z",
               "2048-06-14T02:15:03Z",
               "2049-10-08T04:44:13Z",
               "2050-01-01T10:15:00Z",
               "2051-11-16T13:05:00Z",
               "2052-05-02T04:19:58Z",
               "2053-04-04T16:25:00Z",
               "2054-12-20T05:59:58Z",
               "2055-07-07T09:05:00Z")



test_ex_cols <- c(  "USUBJID", 
                    "DTIM", 
                    "NDAY",
                    "TPT", 
                    "AMT", 
                    "STUDY", 
                    "VISIT",
                    "TPTC", 
                    "DVID", 
                    "DVIDU", 
                    "ROUTE", 
                    "FRQ",
                    "ADDL",
                    "II",
                    "CMT")
# test_ex <- data.frame(usubjid    = letters[1:10],
#                     dtim       = iso_dates,
#                     nday       = 1:10,
#                     tpt        = 1:10,
#                     amt        = 1:10,
#                     study      = letters[1:10],
#                     visit      = letters[1:10],
#                     tptc       = letters[1:10],
#                     dvid       = letters[1:10],
#                     divdu      = letters[1:10],
#                     route      = letters[1:10],
#                     frq        = letters[1:10],
#                     addl       = 1:10,
#                     ii = 1:10,
#                     cmt = 11:20)

test_ex <- data.frame(usubjid    = letters[1:10],
                    dtim       = ex_iso_dates,
                    nday       = 1:10,
                    tpt        = 1:10,
                    amt        = 1:10,
                    study      = letters[1:10],
                    visit      = letters[1:10],
                    tptc       = letters[1:10],
                    dvid       = letters[1:10],
                    divdu      = letters[1:10],
                    route      = letters[1:10],
                    frq        = letters[1:10],
                    addl       = 1:10,
                    ii = 1:10,
                    # cmt = 11:20
                    cmt = c(0,0,0,0,0,0,0,0,0,0))

names(test_ex) <- test_ex_cols

pc_test_cols <- c(  "USUBJID",
                    "DTIM",
                    "NDAY",
                    "TPT",
                    "ODV",
                    "LLOQ",
                    "STUDYID",
                    "VISIT",
                    "TPTC",
                    "DVID",
                    "DVIDU",
                    "CMT"
)

test_pc <- data.frame(usubjid   = letters[1:10],
                    dtim        = pc_iso_dates,
                    nday        = 1:10,
                    tpt         = 1:10,
                    odv         = 2:11,
                    lloq        = 1:10,
                    studyid     = 1:10,
                    visit       = letters[1:10],
                    tptc        = letters[1:10],
                    dvid        = letters[1:10],
                    dvidu       = letters[1:10],
                    cmt = 1:10)
names(test_pc) <- pc_test_cols

pd_test_columns <- c(   "USUBJID",
                        "DTIM",
                        "NDAY",
                        "TPT",
                        "ODV",
                        "LLOQ",
                        "CMT",
                        "VISIT",
                        "TPTC",
                        "DVID",
                        "DVIDU")

test_pd <- data.frame(  usubjid      = letters[1:10],
                    dtim        = pd_iso_dates,
                    nday        = 1:10,
                    tpt         = 1:10,
                    odv         = 1:10,
                    lloq        = 1:10,
                    cmt         = 1:10,
                    visit       = letters[1:10],
                    tptc        = letters[1:10],
                    dvid        = letters[1:10],
                    dvidu       = letters[1:10])
names(test_pd) <- pd_test_columns

test_tv.cov <- data.frame(
    USUBJID = letters[1:10],
    DTIM    = tv.cov_iso_dates[1:10],
    AST     = c(19, 13, 16, 20, 21, 25, 21, 18, 22, 20),
    ALT     = c(60, 32, 20, 41, 30, 62, 50, 28, 33, 46)
)

dm <- tibble(
    USUBJID = letters[1:10],
    SEX = "F",
    RACE = c("ASIAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "AMERICAN INDIAN OR ALASKA NATIVE", "WHITE", "ASIAN", "ASIAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "WHITE"),
    ETHNIC = c("NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "HISPANIC OR LATINO","NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO","HISPANIC OR LATINO"),
    AGE = c(5, 20, 41, 90, 71, 52, 34, 15, 4, 20)
)

lb <- tibble(
    USUBJID = letters[1:10],
    ALT = c(21,25,30,26,27,21,23,22,22,27),
    AST = c(22,18,13,19,22,20,25,21,15,16),
    BILI = c(5,7,10,15,17,9,10,8,14,11),
    CREAT = c(60,70,61,73,80,81,69,68,71,83)
)

vs <- tibble(
    USUBJID = letters[1:10],
    HEIGHT = c(180,150,170,161,181,167,187,150,164,190),
    WEIGHT = c(80.2,70.1,63.7,45.7,90.6,70.2,80.4,90,99.2,100)
)

test_sl_cov_list <- list(dm, lb, vs)

test_sl_cov_df_with_units <- data.frame(
    USUBJID = letters[1:10],
    ALT     = c(20, 33, 25, 26, 21, 24, 30, 32, 26, 29),
    AST     = c(17, 29, 23, 23, 17, 19, 28, 29, 24, 21),
    BILI    = c(8, 10, 12, 7, 9, 10, 7, 13, 12, 11),
    CREAT   = c(57, 59, 81, 66, 71, 76, 71, 82, 66, 64),
    ASTU    = rep("IU/L", 10),
    ALTU    = rep("IU/L", 10),
    BILIU   = rep("mg/dL", 10),
    CREATU  = rep("mg/dL", 10)
)

test_tv_col_df_with_units <- data.frame(
    USUBJID = letters[1:10],
    DTIM    = tv.cov_iso_dates[1:10],
    ALT     = c(20, 33, 25, 26, 21, 24, 30, 32, 26, 29),
    AST     = c(17, 29, 23, 23, 17, 19, 28, 29, 24, 21),
    BILI    = c(8, 10, 12, 7, 9, 10, 7, 13, 12, 11),
    CREAT   = c(57, 59, 81, 66, 71, 76, 71, 82, 66, 64),
    ASTU    = rep("IU/L", 10),
    ALTU    = rep("IU/L", 10),
    BILIU   = rep("mg/dL", 10),
    CREATU  = rep("mg/dL", 10)
)
# END of Global variables

# # testing column names
# test_that("testing USUBJID", {
#     test_ex_cols <- c("USUBJID_test", 
#                 "EXSTDTC_test", 
#                 "EXSTDY_test",
#                 "EXTPTNUM_test", 
#                 "EXDOSE_test", 
#                 "STUDYID_test", 
#                 "VISIT_test",
#                 "EXTPT_test", 
#                 "EXTRT_test", 
#                 "EXDOSU_test", 
#                 "EXROUTE_test", 
#                 "EXDOSFRQ_test")
#     df <- data.frame(a = 1:10,
#                      b = 1:10,
#                      c = 1:10,
#                      d = 1:10,
#                      e = 1:10,
#                      f = 1:10,
#                      g = 1:10,
#                      h = 1:10,
#                      i = 1:10,
#                      j = 1:10,
#                      k = 1:10,
#                      l = 1:10)
#     names(df) <- test_ex_cols
#     expect_error(pk_build(df), "Column USUBJID is missing from the ex dataset.")
# })


# #'  This test checks to see that regex correctly reassigns
# #'  the values.
# test_that("column test for character type", {
#     test_ex_cols <- c("my_USUBJID", 
#                 "my_EXSTDTC", 
#                 "my_EXSTDY",
#                 "my_EXTPTNUM", 
#                 "my_EXDOSE", 
#                 "my_STUDYID", 
#                 "my_VISIT",
#                 "my_EXTPT", 
#                 "my_EXTRT", 
#                 "my_EXDOSU", 
#                 "my_EXROUTE", 
#                 "my_EXDOSFRQ")
#     df <- data.frame(usubjid    = 1:10,
#                      exstdtc    = 1:10,
#                      exstdy     = 1:10,
#                      extptnum   = 1:10,
#                      exdose     = 1:10,
#                      studyid    = 1:10,
#                      visit      = 1:10,
#                      extpt      = 1:10,
#                      extrt      = 1:10,
#                      exdose     = 1:10,
#                      exroute    = 1:10,
#                      exdosefrq  = 1:10)
#     names(df) <- test_ex_cols

#     expect_error(pk_build(df), "Column USUBJID in ex is not character type.")
#     df$my_USUBJID <- letters[1:10]
    
#     expect_error(pk_build(df), "Column VISIT in ex is not character type.")
#     df$my_VISIT <- letters[1:10]

#     df$CMT <- 11:20

#     expect_error(pk_build(df), "Column TPTC in ex is not character type.")
#     df$my_EXTPT <- letters[1:10]

        

#         expect_error(pk_build(df), "Column DVID in ex is not character type.")
#     df$my_EXTRT <- letters[1:10]
#     #   Issue, cannot change DVIDU. IDK what changes it.

#     expect_error(pk_build(df), "Column DVIDU in ex is not character type.")
#     df$my_EXDOSU <- letters[1:10]

#         expect_error(pk_build(df), "Column ROUTE in ex is not character type.")
#     df$my_EXROUTE <- letters[1:10]

#         expect_error(pk_build(df), "Column FRQ in ex is not character type.")
#     df$my_EXDOSFRQ <- letters[1:10]
# })

# # '  Check to see if column is missing.
# test_that("Testing for missing column", {
# test_ex_cols <- c("my_USUBJID", 
#                 "my_EXSTDTC", 
#                 "my_EXSTDY",
#                 "my_EXTPTNUM", 
#                 "my_EXDOSE", 
#                 "my_STUDYID", 
#                 # "my_VISIT",
#                 "my_EXTPT", 
#                 "my_EXTRT", 
#                 "my_EXDOSU", 
#                 "my_EXROUTE", 
#                 "my_EXDOSFRQ")
#     df <- data.frame(usubjid    = letters[1:10],
#                      exstdtc    = 1:10,
#                      exstdy     = 1:10,
#                      extptnum   = 1:10,
#                      exdose     = 1:10,
#                      studyid    = 1:10,
#                     #  visit      = letters[1:10],
#                      extpt      = letters[1:10],
#                      extrt      = letters[1:10],
#                      exdose     = letters[1:10],
#                      exroute    = letters[1:10],
#                      exdosefrq  = letters[1:10])
#     names(df) <- test_ex_cols
#     expect_error(pk_build(df), "Column VISIT is missing from the ex dataset.")
# })

# test_that("Checking that all numeric columns are numeric.", {
#     test_ex_cols <- c("USUBJID", 
#                 "DTIM", 
#                 "NDAY",
#                 "TPT", 
#                 "AMT", 
#                 "STUDY", 
#                 "VISIT",
#                 "TPTC", 
#                 "DVID", 
#                 "DVIDU", 
#                 "ROUTE", 
#                 "FRQ")
#     df <- data.frame(usubjid    = letters[1:10],
#                      dtim       = 1:10,
#                      nday       = "a",
#                      tpt        = 1:10,
#                      amt        = 1:10,
#                      study      = 1:10,
#                      visit      = letters[1:10],
#                      tptc       = letters[1:10],
#                      dvid       = letters[1:10],
#                      divdu      = letters[1:10],
#                      route      = letters[1:10],
#                      frq        = letters[1:10])
#     names(df) <- test_ex_cols
#     expect_error(pk_build(df), "Column NDAY in ex is not numeric type.")

#     df$NDAY <- 1:10
#     df$TPT <- "character"
#     expect_error(pk_build(df), "Column TPT in ex is not numeric type.")

#     df$TPT <- 1:10
#     df$AMT <- "character"
#     expect_error(pk_build(df), "Column AMT in ex is not numeric type.")
# })

# test_that("checking for missing rows.", {
#     test_ex_cols <- c(  "USUBJID", 
#                         "DTIM", 
#                         "NDAY",
#                         "TPT", 
#                         "AMT", 
#                         "STUDY", 
#                         "VISIT",
#                         "TPTC", 
#                         "DVID", 
#                         "DVIDU", 
#                         "ROUTE", 
#                         "FRQ",
#                         "CMT")
#     df <- data.frame(usubjid    = letters[1:10],
#                      dtim       = 1:10,
#                      nday       = 1:10,
#                      tpt        = 1:10,
#                      amt        = 1:10,
#                      study      = 1:10,
#                      visit      = letters[1:10],
#                      tptc       = letters[1:10],
#                      dvid       = letters[1:10],
#                      divdu      = letters[1:10],
#                      route      = letters[1:10],
#                      frq        = letters[1:10],
#                      cmt        = 11:20)
#     names(df) <- test_ex_cols
#     # Passes error checking.
#     expect_error(pk_build(df), "STUDY in ex must be character type.")

#     # Introduce a missing row in USUBJID.
#     df$USUBJID[4] = NA
#     expect_error(pk_build(df), "USUBJID missing in ex for at least 1 row.")

#     # Introduce multiple missing rows in USUBJID.
#     df$USUBJID[7] = NA
#     df$USUBJID[9] = NA
#     expect_error(pk_build(df), "USUBJID missing in ex for at least 1 row.")

#     # Introduce multiple missing rows in CMT.
#     # TODO: check with Stephen and Ethan.
#     # df$USUBJID = letters[1:10]
#     # df$CMT[2] = NA
#     # expect_error(pk_build(df), "CMT missing in ex for at least 1 row.")
# })

# test_that("Check for STUDY are character types", {
#     test_ex_cols <- c(  "USUBJID", 
#                         "DTIM", 
#                         "NDAY",
#                         "TPT", 
#                         "AMT", 
#                         "STUDY", 
#                         "VISIT",
#                         "TPTC", 
#                         "DVID", 
#                         "DVIDU", 
#                         "ROUTE", 
#                         "FRQ",
#                         "CMT")
#     df <- data.frame(usubjid    = letters[1:10],
#                      dtim       = 1:10,
#                      nday       = 1:10,
#                      tpt        = 1:10,
#                      amt        = 1:10,
#                      study      = 1:10,
#                      visit      = letters[1:10],
#                      tptc       = letters[1:10],
#                      dvid       = letters[1:10],
#                      divdu      = letters[1:10],
#                      route      = letters[1:10],
#                      frq        = letters[1:10],
#                      cmt        = 11:20)
#     names(df) <- test_ex_cols
#     expect_error(pk_build(df), "STUDY in ex must be character type.")

#     df$STUDY = letters[1:10]
# })

# test_that("DTIM in ISO 8601 Format", {
#     test_ex_cols <- c(  "USUBJID", 
#                         "DTIM", 
#                         "NDAY",
#                         "TPT", 
#                         "AMT", 
#                         "STUDY", 
#                         "VISIT",
#                         "TPTC", 
#                         "DVID", 
#                         "DVIDU", 
#                         "ROUTE", 
#                         "FRQ",
#                         "CMT")
#     df <- data.frame(usubjid    = letters[1:10],
#                      dtim       = 1:10,
#                      nday       = 1:10,
#                      tpt        = 1:10,
#                      amt        = 1:10,
#                      study      = letters[1:10],
#                      visit      = letters[1:10],
#                      tptc       = letters[1:10],
#                      dvid       = letters[1:10],
#                      divdu      = letters[1:10],
#                      route      = letters[1:10],
#                      frq        = letters[1:10],
#                      cmt        = 11:20)
#     names(df) <- test_ex_cols
#     expect_error(pk_build(df), "DTIM in ex is not ISO 8601 format.")

#     # Now putting correcting the issue.
#     # df$DTIM <- iso_dates
#     # pk_build(df)
# })

# test_that("If ADDL exists so must II", {
#     test_ex_cols <- c(  "USUBJID", 
#                         "DTIM", 
#                         "NDAY",
#                         "TPT", 
#                         "AMT", 
#                         "STUDY", 
#                         "VISIT",
#                         "TPTC", 
#                         "DVID", 
#                         "DVIDU", 
#                         "ROUTE", 
#                         "FRQ",
#                         "ADDL",
#                         "CMT")
#     df <- data.frame(usubjid    = letters[1:10],
#                      dtim       = ex_iso_dates,
#                      nday       = 1:10,
#                      tpt        = 1:10,
#                      amt        = 1:10,
#                      study      = letters[1:10],
#                      visit      = letters[1:10],
#                      tptc       = letters[1:10],
#                      dvid       = letters[1:10],
#                      divdu      = letters[1:10],
#                      route      = letters[1:10],
#                      frq        = letters[1:10],
#                      addl       = 1:10,
#                      cmt        = 11:20)
#     names(df) <- test_ex_cols

#     # Testing if ADDL exists, then II should, but in this
#     # case, it does not.
#     expect_error(pk_build(df), "If ex contains ADDL, it must contain II")

#     df <- dplyr::rename(df, II = ADDL)
#     expect_error(pk_build(df), "If ex contains II, it must contain ADDL")
# })

# test_that("Testing NDAY with 0, II documented while ADDL is NA, and ADDL documented and II NA", {
#     test_ex_cols <- c(  "USUBJID", 
#                         "DTIM", 
#                         "NDAY",
#                         "TPT", 
#                         "AMT", 
#                         "STUDY", 
#                         "VISIT",
#                         "TPTC", 
#                         "DVID", 
#                         "DVIDU", 
#                         "ROUTE", 
#                         "FRQ",
#                         "ADDL",
#                         "II",
#                         "CMT")
#     df <- data.frame(usubjid    = letters[1:10],
#                      dtim       = ex_iso_dates,
#                      nday       = 1:10,
#                      tpt        = 1:10,
#                      amt        = 1:10,
#                      study      = letters[1:10],
#                      visit      = letters[1:10],
#                      tptc       = letters[1:10],
#                      dvid       = letters[1:10],
#                      divdu      = letters[1:10],
#                      route      = letters[1:10],
#                      frq        = letters[1:10],
#                      addl       = 1:10,
#                      ii         = NA,
#                      cmt        = 11:20)
#     names(df) <- test_ex_cols

#     expect_error(pk_build(df), "At least one row in ex has a documented ADDL when II is NA.")

#     df$ADDL <- NA
#     df$II <- 1:10
#     expect_error(pk_build(df), "At least one row in ex has a documented II when ADDL is NA.")

#     # Now we are going to change II to all documented, and ADDL to one NA. 
#     # This does not work! Either I have a misunderstanding, or this is not correctly implemented.
#     # df$ADDL[5] <- NA
#     # df$II <- 1:10
#     # expect_error(pk_build(df), "At least one row in ex has a documented II when ADDL is NA.")
# })

# test_that("Testing NDAY with invalid values.", {
#     invalid_day <- c(-5, 0, 5, 1, 3, 5, 6, 9, -10, -4)
#     test_ex_cols <- c(  "USUBJID", 
#                         "DTIM", 
#                         "NDAY",
#                         "TPT", 
#                         "AMT", 
#                         "STUDY", 
#                         "VISIT",
#                         "TPTC", 
#                         "DVID", 
#                         "DVIDU", 
#                         "ROUTE", 
#                         "FRQ",
#                         "ADDL",
#                         "II",
#                         "CMT")
#     df <- data.frame(usubjid    = letters[1:10],
#                      dtim       = ex_iso_dates,
#                      nday       = invalid_day,
#                      tpt        = 1:10,
#                      amt        = 1:10,
#                      study      = letters[1:10],
#                      visit      = letters[1:10],
#                      tptc       = letters[1:10],
#                      dvid       = letters[1:10],
#                      divdu      = letters[1:10],
#                      route      = letters[1:10],
#                      frq        = letters[1:10],
#                      addl       = 1:10,
#                      ii         = 1:10,
#                      cmt        = 11:20)
#     names(df) <- test_ex_cols
#     expect_error(pk_build(df), "NDAY in ex has a 0 measurement. Please confirm day of first dose is nominal day 1 and the day prior to first dose is nominal day -1.")
#     # Works with all negatives.
#     # df$NDAY <- -10:-1
#     # pk_build(df)
# })

# test_that("Required PD Columns", {
# # source("R//PK_ASSEMBLY.R")
#         iso_dates <- c( "2023-05-17T08:30:00Z",
#                     "2025-12-25T12:00:00Z",
#                     "2021-01-01T00:00:00Z",
#                     "2022-11-11T11:11:11Z",
#                     "2024-02-29T14:29:00Z",
#                     "2020-07-04T18:00:00Z",
#                     "2023-10-31T23:59:59Z",
#                     "2022-02-14T20:00:00Z",
#                     "2021-12-31T23:59:59Z",
#                     "2025-06-30T13:00:00Z")
#     test_ex_cols <- c(  "USUBJID", 
#                         "DTIM", 
#                         "NDAY",
#                         "TPT", 
#                         "AMT", 
#                         "STUDY", 
#                         "VISIT",
#                         "TPTC", 
#                         "DVID", 
#                         "DVIDU", 
#                         "ROUTE", 
#                         "FRQ",
#                         "ADDL",
#                         "II",
#                         "CMT")
#     ex <- data.frame(usubjid    = letters[1:10],
#                      dtim       = ex_iso_dates,
#                      nday       = 1:10,
#                      tpt        = 1:10,
#                      amt        = 1:10,
#                      study      = letters[1:10],
#                      visit      = letters[1:10],
#                      tptc       = letters[1:10],
#                      dvid       = letters[1:10],
#                      divdu      = letters[1:10],
#                      route      = letters[1:10],
#                      frq        = letters[1:10],
#                      addl       = 1:10,
#                      ii         = 1:10,
#                      cmt        = 11:20)
#     names(ex) <- test_ex_cols
#     # Building the pc, but this one is missing ODV.
#     pc_test_cols <- c(  "USUBJID",
#                         # "test_PCDTC",
#                         "test_PCDY",
#                         "test_PCTPTNUM",
#                         # "test_AVAL",
#                         "test_PCLLOQ",
#                         "test_STUDYID",
#                         "test_VISIT",
#                         "test_PCTPT"
#                         # "test_PCTESTCD",
#                         # "test_PCORRESU"
#                         )
#     pc <- data.frame(   usubjid     = letters[1:10],
#                         # pcdtc       = 1:10,
#                         pdcy        = 1:10,
#                         pctptnum    = 1:10,
#                         # aval        = 1:10,
#                         pclloq      = 1:10,
#                         studyid     = 1:10,
#                         visit       = letters[1:10],
#                         pctpt       = letters[1:10]
#                         # pctestcd    = 1:10,
#                         # corresu     = 1:10
#                         )
#     names(pc) <- pc_test_cols
#     expect_error(pk_build(ex, pc), "Column DTIM is missing from the pc dataset.")
#     pc$test_PCDTC <- 1:10
#     expect_error(pk_build(ex, pc), "Column ODV is missing from the pc dataset.")
#     pc$test_AVAL <- 1:10
#     # Adding CMT
#     expect_error(pk_build(ex, pc), "Column CMT is missing from the pc dataset.")
#     pc$CMT <- 1:10
#     expect_error(pk_build(ex, pc), "Column DVID is missing from the pc dataset.")
#     pc$test_PCTESTCD <- letters[1:10]
#     expect_error(pk_build(ex, pc), "Column DVIDU is missing from the pc dataset.")
# })

# test_that("ODV at least one is equal to zero", {
#     # source("R//PK_ASSEMBLY.R")
#     pc_test_cols <- c(  "USUBJID",
#                         "DTIM",
#                         "NDAY",
#                         "TPT",
#                         "ODV",
#                         "LLOQ",
#                         "STUDYID",
#                         "VISIT",
#                         "TPTC",
#                         "DVID",
#                         "DVIDU",
#                         "CMT")
#     pc <- data.frame(usubjid     = letters[1:10],
#                      dtim        = 1:10,
#                      nday        = c(1,2,3,4,0,6,7,8,9,10),
#                      tpt         = 1:10,
#                      odv         = c(1,2,3,4,5,0,7,8,9,10),
#                      lloq        = 1:10,
#                      studyid     = 1:10,
#                      visit       = letters[1:10],
#                      tptc        = letters[1:10],
#                      dvid        = letters[1:10],
#                      dvidu       = letters[1:10],
#                      cmt = 1:10)
#     names(pc) <- pc_test_cols
#     expect_error(pk_build(test_ex, pc), "At least one dependent variable in PC is less than or equal to 0.")
#     # Fix ODV.
#     pc$ODV <- 1:10
#     expect_error(pk_build(test_ex, pc), "DTIM in pc is not ISO 8601 format for at least one row.")
#     # Fix ISO dates.
#     pc$DTIM <- ex_iso_dates
#     expect_error(pk_build(test_ex, pc), "NDAY in pc has a 0 measurement. Please confirm day of first dose is nominal day 1 and the day prior to first dose is nominal day -1")

# })

# test_that("PD required cols", {
#     # Need this for debugging.
#     # source("R//PK_ASSEMBLY.R")
#     pd_test_columns <- c(   "USUBJID",
#                             "DTIM",
#                             "NDAY",
#                             "TPT",
#                             "ODV",
#                             "LLOQ",
#                             # "CMT",
#                             "VISIT",
#                             "TPTC",
#                             "DVID",
#                             "DVIDU")
#     pd <- data.frame(  usubjid     = 1:10,
#                         dtim        = ex_iso_dates,
#                         nday        = 1:10,
#                         tpt         = 1:10,
#                         odv         = 1:10,
#                         lloq        = 1:10,
#                         # cmt         = 1:10,
#                         visit       = letters[1:10],
#                         tptc        = letters[1:10],
#                         dvid        = letters[1:10],
#                         dvidu       = letters[1:10])
#     names(pd) <- pd_test_columns
#     # test column that is supposed to be character.
#     expect_error(pk_build(test_ex, test_pc, pd), "Column USUBJID in pd is not character type.")
#     pd$USUBJID <- letters[1:10]
#     # test for a required column.
#     expect_error(pk_build(test_ex, test_pc, pd), "Column CMT is missing from the pd dataset.")
#     pd$CMT <- letters[1:10]
#     # test column that is supposed to be numeric.
#     expect_error(pk_build(test_ex, test_pc, pd), "Column CMT in pd is not numeric type.")
#     pd$CMT <- 11:20
#     # test missing row in pd.
#     pd$CMT[4] <- NA
#     expect_error(pk_build(test_ex, test_pc, pd), "CMT missing in pd for at least 1 row.")
# })

# test_that("PD, NDAY in pd has 0 measurement.", {
#     # Need this for debugging.
#     # source("R//PK_ASSEMBLY.R")
#     pd_test_columns <- c(   "USUBJID",
#                             "DTIM",
#                             "NDAY",
#                             "TPT",
#                             "ODV",
#                             "LLOQ",
#                             "CMT",
#                             "VISIT",
#                             "TPTC",
#                             "DVID",
#                             "DVIDU")
#     pd <- data.frame(  usubjid      = letters[1:10],
#                         dtim        = ex_iso_dates,
#                         nday        = 1:10,
#                         tpt         = 1:10,
#                         odv         = 1:10,
#                         lloq        = 1:10,
#                         cmt         = 1:10,
#                         visit       = letters[1:10],
#                         tptc        = letters[1:10],
#                         dvid        = letters[1:10],
#                         dvidu       = letters[1:10])
#     names(pd) <- pd_test_columns
#     pd$NDAY[3] <- 0
#     expect_error(pk_build(test_ex, test_pc, pd), "NDAY in pd has a 0 measurement. Please confirm day of first dose is nominal day 1 and the day prior to first dose is nominal day -1.") 
#     # pd$NDAY[3] <- 1341
#     # ayo <- pk_build(test_ex, test_pc, pd)
# })

# test_that("sl.cov Tests", { 
#     # Need this for debugging.  
#     # source("R//PK_ASSEMBLY.R")
#     dm <- tibble(
#         USUBJID = letters[1:10],
#         SEX = "F",
#         RACE = c("ASIAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "AMERICAN INDIAN OR ALASKA NATIVE", "WHITE", "ASIAN", "ASIAN", "WHITE", "AMERICAN INDIAN OR ALASKA NATIVE", "WHITE"),
#         ETHNIC = c("NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "NOT HISPANIC OR LATINO", "HISPANIC OR LATINO","NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO","NOT HISPANIC OR LATINO","HISPANIC OR LATINO"),
#         AGE = c(5, 20, 41, 90, 71, 52, 34, 15, 4, 20)
#     )

#     lb <- tibble(
#         USUBJID = letters[1:10],
#         ALT = c(21,25,30,26,27,21,23,22,22,27),
#         AST = c(22,18,13,19,22,20,25,21,15,16),
#         BILI = c(5,7,10,15,17,9,10,8,14,11),
#         CREAT = c(60,70,61,73,80,81,69,68,71,83)
#     )

#     vs <- tibble(
#         USUBJID = letters[1:10],
#         HEIGHT = c(180,150,170,161,181,167,187,150,164,190),
#         WEIGHT = c(80.2,70.1,63.7,45.7,90.6,70.2,80.4,90,99.2,NA)
#     )

#     sl_cov_list <- list(dm, lb, vs)
#     # Check for unique rows.
#     sl_cov_list[[1]]$USUBJID <- "a"
#     sl_cov_list[[2]]$USUBJID <- "a"
#     sl_cov_list[[3]]$USUBJID <- "a"
#     expect_error(pk_build(test_ex, test_pc, sl.cov = sl_cov_list), "sl.cov has duplicate USUBJID rows.")
#     # Now making the subjects unique again.
#     sl_cov_list[[1]]$USUBJID <- letters[1:10]
#     sl_cov_list[[2]]$USUBJID <- letters[1:10]
#     sl_cov_list[[3]]$USUBJID <- letters[1:10]
#     sl_cov_list[[3]]$CMT <- 1:10
#     # expect_error(pk_build(test_ex, test_pc, sl.cov = sl_cov_list), "CMT column is duplicated in sl.cov and another dataset. Please include this column in one dataset only.")
#     # Removing CMT column from the last tibble in the list.
#     sl_cov_list[[3]] <- sl_cov_list[[3]] %>% select(-CMT)

#     # Make sl.cov a dataframe 
#     sl_cov_df <- data.frame(
#         USUBJID = letters[11:16],
#         RANDOM   = c(1:6)
#     )
#     # This will build a PK(PD) data frame, but will proceed with TWO warnings:
#     # Warning messages:
#     # 1: In pk_build(test_ex, test_pc, sl.cov = sl_cov_df) :
#     # The following USUBJID(s) have PKPD events but are not in sl.cov: a, b, c, d, e, f, g, h, i, j
#     # 2: In pk_build(test_ex, test_pc, sl.cov = sl_cov_df) :
#     # The following USUBJID(s) have at least one event with missing ATFD: c
#     #     expect_warning(pk_build(test_ex, test_pc, sl.cov = sl_cov_df))
# })

# test_that("Test tv.cov (Time Varying Covariates)", {
#     # Need this for debugging.
#     # source("R//PK_ASSEMBLY.R")
#     tv_cov_df <- data.frame()
#     expect_error(pk_build(test_ex, test_pc, tv.cov = tv_cov_df), "Column USUBJID is missing from the tv.cov dataset.")
#     tv_cov_df <- data.frame(
#         USUBJID = 1:5
#     )
#     expect_error(pk_build(test_ex, test_pc, tv.cov = tv_cov_df), "Column USUBJID in tv.cov is not character type.")
#     tv_cov_df$USUBJID <- letters[17:21]
#     # missing for at least 1 row.
#     tv_cov_df$DTIM <- c(1,2,NA,4,5)
#     expect_error(pk_build(test_ex, test_pc, tv.cov = tv_cov_df), "DTIM missing in tv.cov for at least 1 row.")
#     tv_cov_df$DTIM <- 1:5
#     expect_error(pk_build(test_ex, test_pc, tv.cov = tv_cov_df), "DTIM in tv.cov is not ISO 8601 format.")
#     tv_cov_df$DTIM <- ex_iso_dates[1:5]
#     tv_cov_df$AST <- c(19,13,16,20,21)
#     tv_cov_df$ALT <- c(60,30,20,41,32)
#     # # Purposeful dupe column (STEPHEN IS INFORMED)
#     tv_cov_df$STUDY <- 10:14
#     # expect_error(pk_build(test_ex, test_pc, tv.cov = tv_cov_df), "STUDY column is duplicated in tl.cov and another dataset. Please include this column in one dataset only")

#     # Removing dupe column
#     tv_cov_df <- subset(tv_cov_df, select = -c(STUDY))
#     new_row <- data.frame(
#     USUBJID = "q",
#     DTIM = "2023-05-17T08:30:00Z",
#     AST = 22,
#     ALT = 33
#     )
#     tv_cov_df <- rbind(tv_cov_df, new_row)
#     expect_error(pk_build(test_ex, test_pc, tv.cov = tv_cov_df), "tv.cov has duplicate USUBJID-DTIM rows")
#     # Removing the last column.
#     tv_cov_df <- tv_cov_df[-6, ]
#     # What this does is creates a list of the outputs that are returned from this function, and
#     # I check how long the warnings will be.
#     warning_messages <- purrr::quietly(pk_build)(test_ex, test_pc, tv.cov = tv_cov_df, time.rnd = 1)
#     expect_length(warning_messages$warnings, 2)
# })

# test_that("Testing Time Units.", {
#     expect_error(pk_build(test_ex, test_pc, time.units = "weeks"), "time.units parameter must be in days or hours.")
# })

# test_that("Rounding QC", {
#     # Note: For some reason, when I put the error comment, it would not let me check it.
#     # It always came up as an error. 
#     # Need this for debugging.
#     # source("R//PK_ASSEMBLY.R")
#     # pk_build(ex = test_ex, pc = test_pc, time.rnd = 0.25)
#     expect_error(pk_build(ex = test_ex, pc = test_pc, time.rnd = 0.25), "time.rnd parameter must an integer \\(the number of rounded decimal points\\).")
#     # expect_error(pk_build(test_ex, test_pc, time.rnd = T), "time.rnd parameter must be FALSE or integer \\(the number of rounded decimal points\\).")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, amt.rnd  = 0.25))
#     # expect_error(pk_build(test_ex, test_pc, amt.rnd  = T), "amt.rnd parameter must be FALSE or integer (the number of rounded decimal points).")
#     expect_error(pk_build(test_ex, test_pc, dv.rnd   = 0.25))
#     # expect_error(pk_build(test_ex, test_pc, dv.rnd   = T), "dv.rnd parameter must be FALSE or integer (the number of rounded decimal points).")
#     expect_error(pk_build(test_ex, test_pc, cov.rnd  = 0.25))
#     # expect_error(pk_build(test_ex, test_pc, cov.rnd  = T), "cov.rnd parameter must be FALSE or integer (the number of rounded decimal points).")
# })

# test_that("BDV/DDV/PDV QC", {
#     # Need this for debugging.
#     # source("R//PK_ASSEMBLY.R")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, pd = test_pd, BDV = F, DDV = T), "BDV parameter must be TRUE or BDV column must be included in pd to create DDV.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, pd = test_pd, BDV = F, PDV = T), "BDV parameter must be TRUE or BDV column must be included in pd to create PDV.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, pd = test_pd, DDV = F, PDV = T, BDV = T), "DDV parameter must be TRUE or BDV & DDV columns must be included in pd to create PDV.")
# })

# test_that("Other Argument QC", {
#     # Need this for debugging.
#     # source("R//PK_ASSEMBLY.R")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, tv.cov.fill = "ok"), 
#                  "tv.cov.fill parameter must be a tidy direction \\(down, up, downup, updown\\).")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, sparse = T), 
#         "sparse parameter must be numeric to set the threshold for sparse flag.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, sparse = -1), 
#         "sparse parameter must be greater than 0.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, cycle.length = "test"),
#         "cycle.length parameter must be numeric or NA.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, cycle.length = T),
#         "cycle.length parameter must be numeric or NA.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, cycle.length = -1),
#         "cycle.length parameter must be greater than 0.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, cycle.length = 0),
#         "cycle.length parameter must be greater than 0.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, na = T),
#         "na parameter must be numeric.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, na = "test"),
#         "na parameter must be numeric.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, impute = 0),
#         "impute parameter must be method 1, method 2, or NA.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, demo.map = NA),
#         "demo.map parameter must be TRUE or FALSE.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, demo.map = 2),
#         "demo.map parameter must be TRUE or FALSE.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, keep.other = 1),
#         "keep.other parameter must be TRUE or FALSE.")
#     expect_error(pk_build(ex = test_ex, pc = test_pc, keep.other = "test"),
#         "keep.other parameter must be TRUE or FALSE.")
# })
# # source("R//PK_ASSEMBLY.R")
# # pk_build(ex = test_ex, pc = test_pc)
# test_that("Testing Actual and Nominal Time for NTFD", {
#     # source("R//PK_ASSEMBLY.R")
#     # Need rounding.
#     test_ex$NDAY <-  -1
#     test_ex$TPT  <-  0
#     test_pc$TPT  <-  0
#     test_pc$NDAY <-  -1
#     df <- pk_build(ex = test_ex, pc = test_pc, time.rnd = 2)
#     # Expecting NDAY + TPT, where NDAY = -1 AND TPT = -999
#     expect_equal(df$NTFD[1], -1)

#     test_ex$NDAY <-  1
#     test_pc$NDAY <-  1
#     df <- pk_build(ex = test_ex, pc = test_pc, time.rnd = 2)
#     # Expect NTFD = NDAY - 1 + TPT, NDAY = 1, TPT = -999
#     # These are the columns after "BIND EVENTS TOGETHER" are ran.
#     expect_equal(df$NTFD[4], 0)

#     test_ex$NDAY <-  c(-2:-1, 1:8)
#     test_ex$TPT  <-  1:10
#     test_pc$TPT  <-  1:10
#     test_pc$NDAY <-  c(-2:-1, 1:8)
#     df <- pk_build(ex = test_ex, pc = test_pc, time.rnd = 2, time.units = "hours")
#     expect_equal(df$NTFD[10], 53)
#     expect_equal(df$NTFD[1], -47)
# })

# test_that("Actual + Nominal Time Calculations with NDOSE and LDOSE", {
#     # source("R//PK_ASSEMBLY.R")
#     df <- pk_build(ex = test_ex, pc = test_pc, tv.cov = test_tv.cov, time.rnd = 2, sl.cov = test_sl_cov_list)
#     # expect_equal(df$FDOSE[5], "2025-12-25 12:00:00")
#     # expect_equal(df$ATFD[5], 0)
#     # expect_equal(df$ATLD[5], 0)
#     expect_equal(df$NTFD[5], 3)
#     expect_equal(df$NTLD[5], 0)
#     expect_equal(df$NTLD[8], 0)
    
# })

# test_that("Imput Method 1", {
#     # source("R//PK_ASSEMBLY.R")
#     # With impute method 1, IMPDV == IMPEX == 0, and
#     # ATFD == ATFD and ATLD == ATLD
#     dfpk <- pk_build(ex = test_ex, pc = test_pc, tv.cov = test_tv.cov, time.rnd = 2, impute = 1)
#     # Where EVID == 0 is where IMPDV is 0, other than that
#     # IMPDV will be NA.
#     expect_equal(dfpk$IMPDV[3], as.double(NA))
#     expect_true(!is.na(dfpk$IMPDV[2]))

#     # Testing IMPEX. ATFD will never be na, so we will test where IMPDV is 0 and where the EVID is 0 which is when it's not in the EX domain (EX domain, EVID = 1).
#     expect_equal(dfpk$IMPEX[3], as.double(NA))
#     expect_true(!is.na(dfpk$IMPEX[1]))

#     # ATFD for EVID 2, 1, and 0.
#     expect_equal(dfpk$ATFD[1], 0.00)
#     expect_equal(dfpk$ATFD[2], 4717.09)
#     expect_equal(dfpk$ATFD[3], 8477)

#     # ATLD will be NA because the EVID == 2. (Also, because rows are combined.)
#     expect_true(!is.na(dfpk$ATLD[1]))
#     # ATLD will be 0 for EVID == 1 which leads to LDOSE1 == DTIM and then they are subtracted.
#     expect_equal(dfpk$ATLD[3], as.double(NA))
#     # ATLD will be 0 because DTIM and LDOSE2 will be equal.
#     expect_equal(dfpk$ATLD[2], 4716.09)

# })

# test_that("Imput Method 2", {
#     # source("R//PK_ASSEMBLY.R")
#     # NEED TO CHECK FOR THESE COMMENTED OUT TESTS.
#     dfpk <- pk_build(ex = test_ex, pc = test_pc, tv.cov = test_tv.cov, time.rnd = 2, impute = 2)
#     # expect_equal(dfpk$DTIM, dfpk$FDOSE)
#     # Time dose measurements chain.
#     dose_validation <- rep(c(0, 0, NA), 10)
#     expect_equal(dose_validation, dfpk$IMPFEX)
#     expect_equal(dose_validation, dfpk$IMPEX)
#     # expect_equal(dose_validation, dfpk$ATLD)
#     expect_equal(dose_validation, dfpk$NTLD)
#     dose_validation <- rep(c(-0.01, 0 ,0), 10)
#     # expect_equal(dose_validation, dfpk$ATFD)

#     test_ex$DTIM = NA 
#     # Checking to see if correct events are triggered when DTIM is gone.
#     expect_warning(dfpk_na_dtim <- pk_build(ex = test_ex, pc = test_pc, tv.cov = test_tv.cov, time.rnd = 2, impute = 2))
#     dose_validation <- rep(c(1, 1 ,NA), 10)
#     expect_equal(dose_validation, dfpk_na_dtim$IMPEX)
#     expect_equal(dose_validation, dfpk_na_dtim$IMPFEX)
#     dose_validation <- rep(c(0, 0 ,NA), 10)
#     expect_equal(dose_validation, dfpk_na_dtim$ATLD)
#     expect_equal(dose_validation, dfpk_na_dtim$NTLD)
#     expect_equal(dfpk_na_dtim$ATFD[10], 4)
# })

# test_that("Dose and obsevation calculations", {
#     # source("R//PK_ASSEMBLY.R")
#     pk <- pk_build(ex = test_ex, pc = test_pc, time.rnd = 2)
#     pattern <- rep(1:10, each = 2)
#     expect_equal(pattern, pk$DOSEA)
#     pattern <- rep(c(NA, 0), 10)
#     expect_equal(pattern, pk$BLQ)
#     pattern <- rep(c(1, 0), 10)
#     expect_equal(pattern, pk$MDV)
#     pattern <- rep(seq(1, 19, 2), each = 2)
#     expect_equal(pattern, pk$NTLC)
#     pattern <- rep(letters[1:10], each = 2)
#     expect_equal(pattern, pk$DVIDC)
#     pattern <- rep(1:10, each = 2)
#     expect_equal(pattern, pk$DVID)
#     # with DUR column.
#     test_ex <- test_ex %>% dplyr::mutate(DUR = 90)
#     pk <- pk_build(ex = test_ex, pc = test_pc, time.rnd = 2)
#     # TODO: Finish writing tests.
#     pattern <- rep(1:10, each = 2)
#     expect_equal(pk$DOSEA, pattern)
#     pattern <- rep(c(NA, 0), 10)
#     expect_equal(pk$BLQ, pattern)
#     pattern <- rep(c(1, 0), 10)
#     expect_equal(pk$MDV, pattern)
#     pattern <- rep(seq(1, 19, 2), each = 2)
#     expect_equal(pk$NTLC, pattern)
#     pattern <- rep(letters[1:10], each = 2)
#     expect_equal(pk$DVIDC, pattern)
#     pattern <- rep(1:10, each = 2)
#     expect_equal(pk$DVID, pattern)
# })

# test_that("PD Processing", {
#     # source("R//PK_ASSEMBLY.R")
#     pk <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, BDV = T, DDV = T, PDV = T)
#     pattern <- rep(NA, 20)
#     expect_equal(pk$BDV, pattern)
#     class(pattern) <- "numeric"
#     expect_equal(pk$DDV, pattern)
#     # expect_equal(pk$PDV, pattern)
#     # Testing when PDOS is 1 and BDV is not NA.
#     # test_ex$DTIM <- NA
#     # Need to try and get ATFD <= 0 and EVID = 0 because
#     # I need BDV and what not to be not NA! :)
#     # pk <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, impute = 2, BDV = F)

#     # pk_write(pk, "C://Users//michael.dick//Documents//apmx//test.csv")
     
#     # pk_summarize(file = "C://Users//michael.dick//Documents//apmx//test.csv",
#     #                     strat.by = "NSTUDYC",
#     #                     ignore.c = T, 
#     #                     na = -999,
#     #                     docx = T)
# })

# test_that("Pre-Processing Covariates", {
#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                         sl.cov = test_sl_cov_list, demo.map = F)
#     # With all of females and demographic map off there should be all 1s,
#     # and an NA from combining the time-varying covariets. 
#     expect_equal(pkdf$NSEX, rep(c(1,1,NA), 10))
#     # Since demographic map is off, we need to map based on what the function says.
#     # In this case, we will match the columns to what they are supposed to be.
#     expect_equal(pkdf$NRACE, c(2,2,NA,3,3,NA,1,1,NA,1,1,NA,3,3,NA,2,2,NA,2,2,NA,3,3,NA,1,1,NA,3,3,NA))

#     # Now we will test them with demo.map to True.
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                         sl.cov = test_sl_cov_list, demo.map = T)
#     expect_equal(pkdf$NSEX, rep(c(1,1,NA), 10))
#     expect_equal(pkdf$NRACE, c(3,3,NA,1,1,NA,4,4,NA,4,4,NA,1,1,NA,3,3,NA,3,3,NA,1,1,NA,4,4,NA,1,1,NA))
#     expect_equal(pkdf$NETHNIC,c(0,0,NA, 0,0,NA,0,0,NA,1,1,NA,0,0,NA,0,0,NA,0,0,NA,0,0,NA,0,0,NA,1,1,NA))

#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                         sl.cov = test_sl_cov_list, demo.map = F)
#     # There is no SEX, RACE, or any other catagotical covariets that would be appended a T
#     # with this toy dataset. Thus, the only thing for time-varying covariets will be
#     # AST and ATL, and these will have the each number repeated three times.
#     expect_equal(pkdf$TAST, rep(c(19,13,16,20,21,25,21,18,22,20), each = 3))
#     expect_equal(pkdf$TALT, rep(c(60,32,20,41,30,62,50,28,33,46), each = 3))
#     # expect_snapshot(print(pkdf, n = Inf, width = Inf))
# })

# test_that("Join Subject-Level Covariates", {
#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                         sl.cov = test_sl_cov_list, demo.map = T)
#     # Checking to see if the two dataframes, sl.cov and df combine.
#     expect_equal(names(pkdf[27]), "NSEX")
# })

# test_that("Join Time-Varying Covariates", {

#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                         sl.cov = test_sl_cov_list, demo.map = T)
#     evid_is_2_pkdf <- pkdf %>% filter(EVID == 2)
#     expect_equal(evid_is_2_pkdf$ATFD, c(8477.000,7747.240, 10026.080,  9827.720,  9437.810, 11456.790, 10410.170, 11371.840, 12041.240, 10963.830))
# })

# test_that("NODV_F", {
#     #     source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                     sl.cov = test_sl_cov_list, demo.map = T)
#     pattern_vector <- c(1:10)
#     count <- 1
#     for (i in 38:47)
#     {
#         expect_equal(names(pkdf[i]), paste0("NODV", pattern_vector[count], "F"))
#         count <- count + 1
#     }
# })

# test_that("DUPF", {
#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                     sl.cov = test_sl_cov_list, demo.map = T)
#     # This column will be all 0s because of the the grouping of 
#     # USUBJID, ATFD, EVID, CMT will not be unique.
#     expect_equal(pkdf$DUPF, rep(c(0), 30))
# })

# test_that("AMTF", {
#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                     sl.cov = test_sl_cov_list, demo.map = T)
#     # This will return all 0s because all the of AMT column is present, 
#     # and if not present, its EVID is not 1.
#     expect_equal(pkdf$AMTF, rep(0, 30))
# })

# test_that("NOEXF", {
#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                     sl.cov = test_sl_cov_list, demo.map = T)
#     # All values in this column will be 0 because there is at least 1 EVID in this
#     # present in this data.
#     expect_equal(pkdf$NOEXF, rep(0, 30))
# })

# test_that("SPARSEF", {
#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                     sl.cov = test_sl_cov_list, demo.map = T)
#     # First two will be one because they point to the row number that have EVID == 0,
#     # and have the minimum compartment.
#     pattern <- rep(c(0, 1, NA), 10)
#     pattern[c(FALSE, TRUE, FALSE)] <- 1:10
#     expect_equal(pkdf$CMT, pattern)

#     # Making same test, but instead of PD we are going to use PC.
#     pkdf <- pk_build(ex = test_ex, pc = test_pc, time.rnd = 2, tv.cov = test_tv.cov, 
#                 sl.cov = test_sl_cov_list, demo.map = T)
#     pattern <- rep(c(0, 1, NA), 10)
#     pattern[c(FALSE, TRUE, FALSE)] <- 1:10
#     expect_equal(pkdf$CMT, pattern)
# })

# test_that("PDOSEF, TIMEF, PLBOF", {
#     # source("R//PK_ASSEMBLY.R")

#     #####################################
#     # Would like to test when test when #
#     # ATFD < 0.                         #
#     #####################################

#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                     sl.cov = test_sl_cov_list, demo.map = T)

#     v <- rep(0, 30)
#     # Will be all 0s because ATFD > 0.
#     expect_equal(pkdf$PDOSEF, v)
#     # Same reason as above.
#     expect_equal(pkdf$TIMEF, v)
#     # Fits the pattern 0, 0, NA because DOSE == 0 is not applicable for when 
#     # the value is NA.
#     expect_equal(pkdf$PLBOF, rep(c(0, 0, NA), 10))
#     # ALL NA because there is no 1s present in any of these columns.
#     expect_equal(pkdf$C, as.character(rep(NA, 30)))
# })

# test_that("SFF and TREXF", {
#     # source("R//PK_ASSEMBLY.R")

#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                     sl.cov = test_sl_cov_list, demo.map = T)
#     expect_equal(pkdf$SDF, rep(0, 30))
#     expect_equal(pkdf$TREXF, rep(0, 30))
# })

# test_that("FIX NA Items", {
#     # source("R//PK_ASSEMBLY.R")

#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                     sl.cov = test_sl_cov_list, demo.map = T)
#     # All of the following will remain unchanged because each NA value
#     # appears when the record contains EVID = 2.
#     pat <- rep(seq(1, 19, 2), each = 3)
#     pat[seq(from = 3, to = length(pat), by = 3)] <- NA
#     expect_equal(pkdf$NTFD, pat)
#     expect_equal(pkdf$NTLC, pat)
#     pat <- rep(0, 30)
#     pat[seq(from = 3, to = length(pat), by = 3)] <- NA
#     expect_equal(pkdf$NTLD, pat)
# })

# test_that("Rounding", {

#     # source("R//PK_ASSEMBLY.R")

#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                     sl.cov = test_sl_cov_list, demo.map = T)
#     # ATFD rounding (time.rnd = 2)
#     expect_equal(pkdf$ATFD[2], 1035.04)
    
#     # AMT rounding (amt.rnd = NA)
#     expect_equal(pkdf$AMT[1], 1)
    
#     # ODV rounding (dv.rnd = NA)
#     expect_equal(pkdf$ODV[2], 1)

#     # LDV rounding (dv.rnd = NA)
#     expect_equal(pkdf$LDV[2], 0.0000000)
#     expect_equal(pkdf$LDV[8], 1.0986123)

#     # LDV rounding (dv.rnd = 2)
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                 sl.cov = test_sl_cov_list, demo.map = T, dv.rnd = 2)

#     expect_equal(pkdf$LDV[5], 0.69)

#     # BWEIGHT, TAST, TALT with cov.rnd = NA
#     expect_equal(pkdf$BWEIGHT[7], 63.7)
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                 sl.cov = test_sl_cov_list, demo.map = T, dv.rnd = 2)
#     # cov.rnd = 2
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#             sl.cov = test_sl_cov_list, demo.map = T, dv.rnd = 2, cov.rnd = 2)
#     expect_equal(pkdf$BWEIGHT[7], 63.70)
#     expect_equal(pkdf$BWEIGHT[10], 45.70)
#     expect_equal(pkdf$TAST[4], 13.00)
#     expect_equal(pkdf$TALT[7], 20.00)
# })

# test_that("Final Sorting Arrangement", {
#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, 
#                     sl.cov = test_sl_cov_list, demo.map = T)
#     expect_snapshot(pkdf, variant = "final-sorting-arrangment")
# })

# # Creating special datasets to trigger warnings.

# test_that("Warnings", {
#     # source("R//PK_ASSEMBLY.R")
#     warnings_ex <- data.frame(
#         STUDYID = rep("STUDYID", 6),
#         USUBJID = c("A", "A", "B", "B", "C", "C"),
#         DTIM = ex_iso_dates[1:6],
#         NDAY = c(1, 2, 3, 2, 5, 4),
#         TPT = rep(1, 6),
#         AMT = c(87,34,13,65,23,53),
#         VISIT = c("2023-02-05", "2023-02-03","2023-02-04","2023-02-05","2023-02-01","2023-02-10"),
#         CMT = c(6,0,5,1,2,3),
#         TPTC = c("Baseline", "Week 1", "Week 2", "Week 3", "Week 4", "Week 5"),
#         DVID = c("BP", "BP", "WT", "CHOL", "WT", "BP"),
#         DVIDU = c("mg/dL", "mg/dL", "kg", "mmHg", "kg", "mg/dL"),
#         ROUTE = rep("oral", 6),
#         FRQ = rep("QD", 6)
#     ) 
#     warnings_pc <- data.frame(
#         USUBJID = c("A", "A", "B", "B", "C", "C"),
#         DTIM = ex_iso_dates[1:6],
#         NDAY = c(1, 2, 3, 2, 5, 4),
#         DOMAIN = rep("PC", 6),
#         TPT = rep(1, 6), 
#         ODV = rep(1, 6),
#         LLOQ = rep(0.05, 6),
#         CMT = c(6,0,5,1,2,3),
#         VISIT = c("2023-02-05", "2023-02-03","2023-02-04","2023-02-05","2023-02-01","2023-02-10"),
#         TPTC = c("Baseline", "Week 1", "Week 2", "Week 3", "Week 4", "Week 5"),
#         DVID = c("BP", "BP", "WT", "CHOL", "WT", "BP"),
#         DVIDU = c("mg/dL", "mg/dL", "kg", "mmHg", "kg", "mg/dL")
#     )
#     pkdf <- pk_build(warnings_ex, pc = warnings_pc, time.units = "days")
#     expect_warning(pk_build(warnings_ex, pc = warnings_pc, time.units = "days"), "The following USUBJID\\(s\\) have at least one event that occurred out of protocol order \\(NTFD is not strictly increasing\\): B")

#     # Changing NDAY to a negative value to trigger another warning.
#     warnings_pc$NDAY[1] = -5
#     # Capture all of the output and then we will check if the correct error arose.
#     warning_messages <- purrr::quietly(pk_build)(warnings_ex, pc = warnings_pc, time.units = "days")
#     expect_equal(warning_messages$warnings[2], "The following USUBJID(s) have at least one negative NTLD value after first dose: A")
#     expect_equal(length(warning_messages$warnings), 2)
#     warnings_ex$DTIM[1] = NA
#     warning_messages <- purrr::quietly(pk_build)(warnings_ex, pc = warnings_pc, time.units = "days")
#     expect_equal(warning_messages$warnings[2], "The following USUBJID(s) have at least one event with missing ATFD: A")

#     warnings_ex$DTIM = rep(ex_iso_dates[1], 6)
#     warnings_pc$DTIM = rep(ex_iso_dates[1], 6)
#     warnings_ex$NDAY = rep(1, 6)
#     warnings_pc$NDAY = rep(1, 6)
#     warnings_ex$AT
#         #####################################
#         # TODO:                             #
#         # Need line 1255 to trigger!        #
#         #####################################

#     warnings_ex$LONGCOLNAME = 1:6
#     warning_messages <- purrr::quietly(pk_build)(warnings_ex, pc = warnings_pc, time.units = "days")
#     expect_equal(warning_messages$warnings[1], "The following column name(s) are longer than 8 characters: LONGCOLNAME")
#     # pk_build(warnings_ex, pc = warnings_pc, time.units = "days", impute = 2)
# })

# test_that("Warning messages for covariates", {
#     # source("R//PK_ASSEMBLY.R")
#     test_tv.cov$SEX <- c("F", "F", "F", "M", "F", "M", "M", "M", "ASFAFS", "M")
#     # test_tv.cov$SEX <- 3
#     test_ex$NDAY = -1
#     # test_pc$NDAY = 1
#     test_ex$TPT = 1
#     test_tv.cov$WEIGHT = 20
#     warning_messages <- purrr::quietly(pk_build)(ex = test_ex, pd = test_pd, time.rnd = 2, tv.cov = test_tv.cov, sl.cov = test_sl_cov_list, demo.map = T, na = -999)
#     expect_equal(warning_messages$warnings[3], "NSEX and TSEX are not equivalent at first dose (baseline).")
#     expect_equal(warning_messages$warnings[4], "BWEIGHT and TWEIGHT are not equivalent at first dose (baseline).")
# })

# # This is to check if the pk_build() function still works.

# test_that("Snapshots", {
#     pkdf <- pk_build(ex = test_ex, pc = test_pc, tv.cov = test_tv.cov, time.rnd = 2, impute = 2)
#     expect_snapshot(print(pkdf, n = Inf, width = Inf))
# })
