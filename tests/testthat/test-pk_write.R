# Define some tests
# library(tidyr)
test_that("apmx::pk_write writes a CSV file", {

  # Create a temporary file
  temp_file <- tempfile(fileext = ".csv")
  df <- data.frame(a = 1:5, b = 6:10)

  # Test that apmx::pk_write does not throw an error when used correctly
  expect_error(apmx::pk_write(df, temp_file), NA)

  # Check that the file exists after calling apmx::pk_write
  expect_true(file.exists(temp_file))

  # Cleanup: delete the temporary file
  file.remove(temp_file)
})

test_that("Checking to see if file format is CSV (apmx::pk_write())", {
  df <- data.frame(a = 1:5, b = 6:10, c = 11:15)
  temp_file <- tempfile(fileext = "temp.pdf")
  testthat::expect_error(apmx::pk_write(df, temp_file), "filepath must include document name and .csv suffix.")

  temp_file <- tempfile(fileext = "temp.docx")
  testthat::expect_error(apmx::pk_write(df, temp_file), "filepath must include document name and .csv suffix.")

  temp_file <- tempfile(fileext = "temp.csv")
  testthat::expect_no_error(apmx::pk_write(df, temp_file))
})

# Testing the file path.

test_that("apmx::pk_write() - testing path", {
  df <- data.frame(a = 1:10, b = 11:20, c = 21:30)
  temp_file <- tempfile(fileext = "temp.csv")
  expect_error(apmx::pk_write(df, "."), "is not a valid filepath.")
  expect_error(apmx::pk_write(df, "notValidFile"), "is not a valid filepath.")
  expect_no_error(apmx::pk_write(df, temp_file))
})


