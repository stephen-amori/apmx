# LIBRARIES FOR DEBUGGING PURPOSES:
# library(testthat)
# library(apmx)
library(tidyr)
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
    DVIDU = "mg/dl",
    ROUTE = rep("oral", 6),
    FRQ = rep("QD", 6),
    BAGE = c(41,31,42,25,27,29),
    BAGEU = "Years"
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
    DVIDU = "mg/dl"
)

second_iso_dates <- c( 
  "2022-08-17T12:40:30Z",
  "2020-11-25T14:50:00Z",
  "2021-05-01T07:20:01Z",
  "2023-07-11T09:15:11Z",
  "2021-03-19T13:59:00Z",
  "2020-12-04T16:00:00Z",
  "2022-04-21T21:49:59Z",
  "2021-09-24T18:00:00Z",
  "2023-10-11T22:39:59Z",
  "2022-06-20T11:00:00Z"
)

EX2 <- data.frame(
    STUDYID = c("STUDY1", "STUDY2", "STUDY1", "STUDY2", "STUDY1", "STUDY2"),
    USUBJID = c("D1", "D1", "E2", "E2", "F3", "F3"),
    DTIM = second_iso_dates[1:6],
    NDAY = c(2,3,4,2,3,8),
    TPT = c(2,1,2,1,3,1),
    AMT = c(52,32,22,62,32,25),
    VISIT = c("2023-03-05", "2023-03-03","2023-03-04","2023-03-05","2023-03-01","2023-03-10"),
    CMT = c(6,0,5,1,2,3),
    TPTC = c("Baseline", "Week 1", "Week 2", "Week 3", "Week 4", "Week 5"),
    DVID = c("BP", "BP", "WT", "CHOL", "WT", "BP"),
    DVIDU = "mg/dl",
    ROUTE = rep("oral", 6),
    FRQ = rep("QD", 6),
    BAGE = c(47,29,31,28,29,21)
)

PC2 <- data.frame(
    USUBJID = c("D1", "D1", "E2", "E2", "F3", "F3"),
    DTIM = second_iso_dates[1:6],
    NDAY = c(2,3,4,2,3,8),
    DOMAIN = rep("PC", 6),
    TPT = c(2,1,2,1,3,1),
    ODV = rep(1, 6),
    LLOQ = rep(0.05, 6),
    CMT = c(6,0,5,1,2,3),
    VISIT = c("2023-03-05", "2023-03-03","2023-03-04","2023-03-05","2023-03-01","2023-03-10"),
    TPTC = c("Baseline", "Week 1", "Week 2", "Week 3", "Week 4", "Week 5"),
    DVID = c("BP", "BP", "WT", "CHOL", "WT", "BP"),
    DVIDU = c("mg/dL", "mg/dL", "kg", "mmHg", "kg", "mg/dL")
)


#################### END: DUMMY DATA ####################

test_that("pk_define QC Checks", {
    pkdf <- pk_build(ex = EX, pc = PC)
    # Use line below if using testthat.
    pk_write(pkdf, file = "test-pk-define-files/pkdf.csv")

    # Use line below if using R console. (Using devtools::test() uses a different directory.)
    # pk_write(pkdf, file = "tests/testthat/test-pk-define-files/pkdf.csv")
    # unlink("tests/testthat/test-pk-define-files/pkdf.csv")

    expect_error(pk_define(file = "not-valid-file"), "not-valid-file is not a valid filepath.")
    expect_error(pk_define(file = "tests/testthat/test-pk-define-files/pkdf"),
                         "filepath must include document name and .csv suffix.")
    expect_error(pk_define(file = "test-pk-define-files/pkdf.csv", size = "NA"), 
                                                "size parameter must be numeric.")
    expect_error(pk_define(file = "test-pk-define-files/pkdf.csv", size = 0), 
                            "size parameter must be greater than or equal to 0.")
    expect_error(pk_define(file = "test-pk-define-files/pkdf.csv", font = 0), 
                            "font parameter must be character font description.")
    expect_error(pk_define(file = "test-pk-define-files/pkdf.csv", na = "NA"),
                             "na must be numeric type.")
})

test_that("Checking to see if define file was created", {
    pk_define(file = "test-pk-define-files/pkdf.csv",
          project = "Testings",
          na = -999,
          variable.list = "test-pk-define-files/test-variable-list.csv",
          template      = "test-pk-define-files/template_define.docx")
        # This writes in place.
        expect_true(file.exists("test-pk-define-files\\DEFINE_pkdf.docx"))
        unlink("test-pk-define-files\\DEFINE_pkdf.docx")
})
