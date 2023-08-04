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
    DVIDU = c("mg/dL", "mg/dL", "kg", "mmHg", "kg", "mg/dL"),
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

# test_that("PK Combine QC Triggering Events", {
#     # source("R//PK_ASSEMBLY.R")
#     EX$FAILURE <- 22
#     pkdf <- pk_build(ex = EX, pc = PC)
#     pkdf2 <- pk_build(ex = EX2, pc = PC2)
#     # combined_df <- pk_combine(pkdf, pkdf2)
#     expect_warning(pk_combine(pkdf, pkdf2), "Column names do not match between both datasets")
#     # Removing the FAILURE column.
#     EX <- EX %>% select(-FAILURE)
#     # Going to add a USUBJID into the data set so that not all values are unique.
#     EX2$USUBJID[1] <- "A1"
#     pkdf2 <- pk_build(ex = EX2, pc = PC2)
#     expect_error(pk_combine(pkdf, pkdf2), "At least one USUBJID exists in both datasets. Please ensure all USUBJID values are unique.")

#     # Now we are going to trigger "At least one NSTUDYC exists in both datasets."
#     EX2$USUBJID[1] <- "D1"
#     EX2$STUDYID[1] <- "STUDYID"
#     pkdf2 <- pk_build(ex = EX2, pc = PC2)
#     expect_warning(pk_combine(pkdf, pkdf2), "At least one NSTUDYC exists in both datasets")

#     # Changing time units so that they are not equal in both datasets.
#     EX2$STUDYID[1] <- "STUDY1"
#     pkdf2 <- pk_build(ex = EX2, pc = PC2, time.units = "hours")
#     expect_error(pk_combine(pkdf, pkdf2), "Time units must be equal between both datasets.")
  
# })

# test_that("PK Combine, checking DVID and DVIDC are the same.", {
#     # source("R//PK_ASSEMBLY.R")
#     # Making a DVID not the same for EX to EX2.
#     EX$DVID[1] = "FAILURE"
#     pkdf <- pk_build(ex = EX, pc = PC)
#     pkdf2 <- pk_build(ex = EX2, pc = PC2)
#     expect_error(pk_combine(pkdf, pkdf2, "DVID and DVIDC observation assignments are not the same bewteen both datasets"))

#     # Testing Compartments.
#     EX$DVID[1] = "BP"
#     EX$CMT[1] = 1293842
#     pkdf <- pk_build(ex = EX, pc = PC)
#     pkdf2 <- pk_build(ex = EX2, pc = PC2)
#     expect_warning(pk_combine(pkdf, pkdf2), "CMT = 1293842 not included in df2")

#     # Triggering "DVID and CMT assignments are not the same between both datasets"
#     EX$CMT[1] = 0
#     EX$CMT[3] = 6
#     pkdf <- pk_build(ex = EX, pc = PC)
#     pkdf2 <- pk_build(ex = EX2, pc = PC2)
#     expect_error(pk_combine(pkdf, pkdf2), "DVID and CMT assignments are not the same between both datasets") 
# })

# test_that("Combining datasets", {
#     # source("R//PK_ASSEMBLY.R")
#     pkdf <- pk_build(ex = EX, pc = PC)
#     pkdf2 <- pk_build(ex = EX2, pc = PC2)
#     comb <- pk_combine(pkdf, pkdf2)
#     # Checking the location of the columns after reordering.
#     expect_equal(names(comb[2]), "NSTUDY")
#     expect_equal(names(comb[23]), "NROUTE")
#     expect_equal(names(comb[25]), "BAGE")
#     expect_equal(names(comb[52]), "NROUTEC")
#     expect_equal(names(comb[53]), "NFRQC")

#     # Checking random values.
#     expect_equal(comb$BAGE[6], 42)
#     expect_equal(comb$DOSEA[20], 62)
#     expect_equal(comb$USUBJID[16], "D1")
#     expect_equal(comb$DVIDU[22], "mg/dL")
#     expect_equal(comb$DOSEA[6], 13)

#     # Making Sure that the covariates are filled in with the na value.
#     expect_equal(comb$BAGE[1], -999)

# })
