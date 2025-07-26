source("testing_utils.r")

test_that("import_drugs_clean imports and cleans drug names correctly", {

  # Create a temporary CSV file with test drug data
  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  test_drugs <- tibble::tibble(
    drug = c("Drug Name 1", "drug-name-2", "Drug_Name_3"),
    node = c("a", "b", "c"),
    activity = c(0, 1, 2)
  )

  readr::write_csv(test_drugs, temp_file)

  result <- import_drugs_clean(temp_file, show_col_types = FALSE)

  # Check that structure is preserved
  expect_equal(ncol(result), 3)
  expect_equal(nrow(result), 3)
  expect_true(all(c("drug", "node", "activity") %in% colnames(result)))

  # Check that drug names are cleaned using janitor::make_clean_names
  expected_clean_names <- c("drug_name_1", "drug_name_2", "drug_name_3")
  expect_equal(result$drug, expected_clean_names)

  # Check that other columns are unchanged
  expect_equal(result$node, test_drugs$node)
  expect_equal(result$activity, test_drugs$activity)
})

test_that("import_drugs_clean handles various special characters in drug names", {

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  test_drugs <- tibble::tibble(
    drug = c("Drug With Spaces", "Drug-With-Dashes", "Drug.With.Dots",
             "Drug123", "UPPERCASE", "lowercase", "MiXeD cAsE"),
    node = c("a", "b", "c", "d", "e", "f", "g"),
    activity = c(0, 1, 2, 0, 1, 2, 0)
  )

  readr::write_csv(test_drugs, temp_file)

  result <- import_drugs_clean(temp_file, show_col_types = FALSE)

  # Check that all names are properly cleaned
  # janitor::make_clean_names converts to snake_case and removes special chars
  expected_names <- c("drug_with_spaces", "drug_with_dashes", "drug_with_dots",
                      "drug123", "uppercase", "lowercase", "mi_xe_d_c_as_e")

  expect_equal(result$drug, expected_names)
})

test_that("import_drugs_clean preserves all original data types", {

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  test_drugs <- tibble::tibble(
    drug = c("Test Drug"),
    node = c("test_node"),
    activity = c(1.5),
    extra_col = c("extra_data")
  )

  readr::write_csv(test_drugs, temp_file)

  result <- import_drugs_clean(temp_file, show_col_types = FALSE)

  # Should preserve all columns
  expect_equal(ncol(result), 4)
  expect_true("extra_col" %in% colnames(result))
  expect_equal(result$extra_col, "extra_data")

  # Should preserve numeric types
  expect_equal(result$activity, 1.5)
  expect_true(is.numeric(result$activity))
})

test_that("import_drugs_clean handles empty CSV file", {

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  # Create empty CSV with just headers
  empty_drugs <- tibble::tibble(
    drug = character(),
    node = character(),
    activity = numeric()
  )

  readr::write_csv(empty_drugs, temp_file)

  result <- import_drugs_clean(temp_file, show_col_types = FALSE)

  expect_equal(nrow(result), 0)
  expect_equal(ncol(result), 3)
  expect_true(all(c("drug", "node", "activity") %in% colnames(result)))
})

test_that("import_drugs_clean handles duplicate drug names correctly", {

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  # Test with drug names that would create duplicates after cleaning
  test_drugs <- tibble::tibble(
    drug = c("Drug Name", "Drug.Name", "Drug_Name"),
    node = c("a", "b", "c"),
    activity = c(0, 1, 2)
  )

  readr::write_csv(test_drugs, temp_file)

  result <- import_drugs_clean(temp_file, show_col_types = FALSE)

  # Note: import_drugs_clean applies make_clean_names individually to each name
  # so it doesn't handle duplicates - all become "drug_name"
  expect_equal(nrow(result), 3)
  expect_equal(length(unique(result$drug)), 1)  # All become the same name

  # All should become "drug_name"
  expect_equal(unique(result$drug), "drug_name")
  expect_true(all(result$drug == "drug_name"))
})

test_that("import_drugs_clean works with show_col_types parameter", {

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  test_drugs <- tibble::tibble(
    drug = c("Test"),
    node = c("a"),
    activity = c(1)
  )

  readr::write_csv(test_drugs, temp_file)

  # Test with show_col_types = TRUE (default)
  suppressMessages(expect_message(
    result1 <- import_drugs_clean(temp_file, show_col_types = TRUE),
    "Rows|Columns|Delimiter"  # readr typically shows column info
  ))

  # Test with show_col_types = FALSE
  expect_no_message(
    result2 <- import_drugs_clean(temp_file, show_col_types = FALSE)
  )

  # Results should be identical regardless of show_col_types
  expect_equal(result1, result2)
})

test_that("import_drugs_clean errors with non-existent file", {

  expect_error(
    import_drugs_clean("non_existent_file.csv"),
    "does not exist"
  )
})

test_that("import_drugs_clean preserves tibble class", {

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  test_drugs <- tibble::tibble(
    drug = c("Test"),
    node = c("a"),
    activity = c(1)
  )

  readr::write_csv(test_drugs, temp_file)

  result <- import_drugs_clean(temp_file, show_col_types = FALSE)

  # Should return a tibble
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
})

test_that("import_drugs_clean handles missing drug column", {

  temp_file <- tempfile(fileext = ".csv")
  on.exit(unlink(temp_file))

  # CSV without drug column
  test_data <- tibble::tibble(
    compound = c("test"),
    node = c("a"),
    activity = c(1)
  )

  readr::write_csv(test_data, temp_file)

  expect_error(
    import_drugs_clean(temp_file, show_col_types = FALSE),
    "drug"
  )
})