library(testthat)

# This test file is for R/utils.r

test_that("normalize_bma_path returns existing path as is", {
  # Create a temporary file that is guaranteed to exist
  temp_file <- tempfile()
  file.create(temp_file)
  on.exit(unlink(temp_file, TRUE, TRUE))

  # The path should be returned unchanged
  expect_equal(normalize_bma_path(temp_file), temp_file)
})

test_that("normalize_bma_path correctly converts old Windows-style paths", {
  # Define old and new path styles
  # This one tests backslash to forward-slash conversion
  old_path_slashes <- "C:\\path\\to\\bma.exe"
  new_path_slashes <- "C:/path/to/bma.exe"

  # This one tests removal of escaped quotes and backslash conversion
  old_path_quotes <- "\\\"C:\\Program Files\\bma.exe\\\""
  new_path_quotes <- "\"C:/Program Files/bma.exe\""

  # Since we can't easily mock file.exists without an extra dependency, we test
  # the conversion by checking the error message when paths don't exist.
  err_slashes <- expect_error(normalize_bma_path(old_path_slashes))
  expect_true(grepl(new_path_slashes, err_slashes$message, fixed = TRUE))

  err_quotes <- expect_error(normalize_bma_path(old_path_quotes))
  expect_true(grepl(new_path_quotes, err_quotes$message, fixed = TRUE))
})

test_that("normalize_bma_path throws an error if file does not exist", {
  # A path that is unlikely to exist on any system
  non_existent_path <- "/a/path/that/does/not/exist/for/real"

  # The error message should contain the path.
  expect_error(normalize_bma_path(non_existent_path),
    regexp = "BMA executable not found at:"
  )

  # Test with a windows-style path that doesn't exist, and its conversion
  non_existent_win_path <- "C:\\a\\non\\existent\\path"
  new_non_existent_win_path <- "C:/a/non/existent/path"

  # The error message should contain both the original and converted paths.
  err <- expect_error(normalize_bma_path(non_existent_win_path))
  expect_true(grepl(non_existent_win_path, err$message, fixed = TRUE))
  expect_true(grepl(new_non_existent_win_path, err$message, fixed = TRUE))
})
