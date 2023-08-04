library(testthat)
library(tidyr)
library(apmx)
library(tibble)
library(dplyr)

#################### START: DUMMY DATA ####################
ex_iso_dates <- c( "2023-05-17T08:30:00Z",
            "2025-12-25T12:00:00Z",
            "2021-01-01T00:00:01Z",
            "2022-11-11T11:11:11Z",
            "2024-02-29T14:29:00Z",
            "2020-07-04T18:00:00Z",
            "2023-10-31T23:59:59Z",
            "2022-02-14T20:00:00Z",
            "2021-12-31T23:59:59Z",
            "2025-06-30T13:00:00Z"
)
EX <- data.frame(
    STUDYID = rep("STUDYID", 6),
    USUBJID = c("A1", "A1", "B2", "B2", "C3", "C3"),
    DTIM = ex_iso_dates[1:6],
    NDAY = c(1, 2, 3, 2, 5, 4),
    TPT = rep(1, 6),
    AMT = c(87,34,13,65,23,53),
    VISIT = c("2023-02-05", "2023-02-03","2023-02-04","2023-02-05","2023-02-01","2023-02-10"),
    CMT = c(6,0,5,1,2,3),
    TPTC = c("Baseline", "Week 1", "Week 2", "Week 3", "Week 4", "Week 5"),
    DVID = c("BP", "BP", "WT", "CHOL", "WT", "BP"),
    DVIDU = c("mg/dL", "mg/dL", "kg", "mmHg", "kg", "mg/dL"),
    ROUTE = rep("oral", 6),
    FRQ = rep("QD", 6),
    BAGE = c(41,31,42,25,27,29)
)
PC <- data.frame(
    USUBJID = c("A1", "A1", "B2", "B2", "C3", "C3"),
    DTIM = ex_iso_dates[1:6],
    NDAY = c(1, 2, 3, 2, 5, 4),
    DOMAIN = rep("PC", 6),
    TPT = rep(1, 6), 
    ODV = rep(1, 6),
    LLOQ = rep(0.05, 6),
    CMT = c(6,0,5,1,2,3),
    VISIT = c("2023-02-05", "2023-02-03","2023-02-04","2023-02-05","2023-02-01","2023-02-10"),
    TPTC = c("Baseline", "Week 1", "Week 2", "Week 3", "Week 4", "Week 5"),
    DVID = c("BP", "BP", "WT", "CHOL", "WT", "BP"),
    DVIDU = c("mg/dL", "mg/dL", "kg", "mmHg", "kg", "mg/dL")
)

#################### END: DUMMY DATA ####################

# test_that("Filter out (new addition!)", {
#     # source("R//PK_ASSEMBLY.R")

#     pkdf <- pk_build(ex = EX, pc = PC)
#     dir <- "C://Users//michael.dick//Documents//apmx//temp-csvs//test1.csv"
#     pk_write(pkdf, dir)
    
#     expect_error(pk_summarize(file = dir, ignore_request = c("USUBJID == D")), regexp = "D is not a record in the dataset.")
#     expect_error(pk_summarize(file = dir, ignore_request = c("NOT == D")), regexp = "NOT is not a column in the dataset.")
#     expect_error(pk_summarize(file = dir, ignore_request = c("SUBJID = 2")), regexp = "= is not a valid operation. Must be one of the following: ==, <, <=, >, >=, !=")
#     expect_error(pk_summarize(file = "not-a-valid-path-no-slashes"), "not-a-valid-path-no-slashes is not a valid filepath.")
#     expect_error(pk_summarize(file = "seems//legit//but-no-csv"), "filepath must include document name and .csv suffix.")
#     expect_error(pk_summarize(file = dir, strat.by = 91191), regexp = "strat.by must be in character form only")
#     expect_error(pk_summarize(file = dir, ignore.c = "2"), regexp = "ignore.c parameter must be TRUE or FALSE.")
#     expect_error(pk_summarize(file = dir, docx.template = "some-file-path//fail.pdf"), regexp = "fail.pdf must include document name and .docx suffix.")
# })


# This writes to a file and we check if they exist.
# test_that("Writing CSVs", {
#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = EX, pc = PC)
#     working_dir <- getwd()
#     working_dir <- paste0(working_dir, "/test-pk-summarize-files/test1.csv")
#     pk_write(pkdf, working_dir)
#     pk_summarize(file = working_dir)
    
#     working_dir <- getwd()
#     blq_check_path <- paste0(working_dir, "/test-pk-summarize-files/BLQ_by_NSTUDYC.csv")
#     contcov_check_path <- paste0(working_dir, "/test-pk-summarize-files/CONTCOV_by_NSTUDYC.csv")
#     catcov_check_path <- paste0(working_dir, "/test-pk-summarize-files/CATCOV_by_NSTUDYC.csv")
#     expect_true(file.exists(blq_check_path))
#     expect_true(file.exists(contcov_check_path))
#     expect_true(file.exists(catcov_check_path))
#     # Remove creates CSVs.
#     unlink(blq_check_path)
#     unlink(contcov_check_path)
#     unlink(catcov_check_path)
# })

# # Testing Word and PowerPoint Docs.
# test_that("Testing Word and PowerPoint", {
#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = EX, pc = PC)
#     working_dir <- getwd()
#     # DEBUG:
#     # data_dir <- paste0(working_dir, "/tests/testthat/test-pk-summarize-files/test1.csv")
    
#     data_dir <- paste0(working_dir, "/test-pk-summarize-files/BLQ_by_NSTUDYC.csv")
#     pk_write(pkdf, data_dir)
#     pk_summarize(file = data_dir)
#     # Now read in file and check certain rec

#     # DEBUG:
#     # check_blq <- read.csv(paste0(working_dir, "/tests/testthat/test-pk-summarize-files/BLQ_by_NSTUDYC.csv"))

#     check_blq <- read.csv(paste0(working_dir, "/test-pk-summarize-files/BLQ_by_NSTUDYC.csv"))
#     expect_equal(check_blq$STUDYID[4], '5')
#     expect_equal(check_blq$STUDYID[9], "3 (100%)")
#     expect_equal(check_blq$STUDYID[13], "1 (100%)")
#     expect_equal(check_blq$STUDYID[17], "1 (100%)")

#     # DEBUG:
#     # check_cat_cov <- read.csv(paste0(working_dir, "/tests/testthat/test-pk-summarize-files/CATCOV_by_NSTUDY.csv"))

#     check_cat_cov <- read.csv(paste0(working_dir, "/test-pk-summarize-files/CATCOV_by_NSTUDYC.csv"))
#     expect_equal(check_cat_cov$Value[2], "QD")
#     expect_equal(check_cat_cov$STUDYID[2], "3 (100%)")
#     expect_equal(check_cat_cov$STUDYID[3], "NROUTE")
#     expect_equal(check_cat_cov$Total[1], "NFRQ")
#     expect_equal(check_cat_cov$Total[6], "3 (100%)")

#     # DEBUG:
#     # check_cat_cov <- read.csv(paste0(working_dir, "/tests/testthat/test-pk-summarize-files/CONT_COV_by_NSTUDY.csv"))
    
#     check_cont_cov <- read.csv(paste0(working_dir, "/test-pk-summarize-files/CONTCOV_by_NSTUDYC.csv"))
#     expect_equal(check_cont_cov$STUDYID[1], "BAGE")
#     expect_equal(check_cont_cov$STUDYID[3], "30.6 (6.23)")
#     expect_equal(check_cont_cov$STUDYID[4], "29 (27; 29.5)")
#     expect_equal(check_cont_cov$Total[5], "25.4; 39")
#     expect_equal(check_cont_cov$Total[6], "25; 41")

# })