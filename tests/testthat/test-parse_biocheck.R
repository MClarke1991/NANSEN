test_that("parse_biocheck_json works with valid JSON", {
  # Use existing helper result file
  json_file <- here::here("tests", "testthat", "helper_results_json", "growth_factor__1__b__0.json")

  result <- parse_biocheck_json(json_file)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("Time", "Id", "Lo", "Hi") %in% names(result)))
  expect_true(nrow(result) > 0)

  # Should filter to maximum time only
  expect_true(all(result$Time == max(result$Time)))
})

test_that("parse_biocheck_json handles missing file", {
  expect_snapshot(
    parse_biocheck_json("nonexistent_file.json"),
    error = TRUE
  )
})

test_that("parse_biocheck_json handles malformed JSON", {
  # Create temporary malformed JSON
  temp_json <- tempfile(fileext = ".json")
  writeLines('{"Status":"Stabilizing","Ticks":[{malformed}]}', temp_json)

  expect_snapshot(
    parse_biocheck_json(temp_json),
    error = TRUE
  )

  unlink(temp_json)
})

test_that("parse_biocheck_json extracts maximum time correctly", {
  json_file <- here::here("tests", "testthat", "helper_results_json", "growth_factor__1__b__0.json")

  result <- parse_biocheck_json(json_file)

  # All returned rows should have the same (maximum) time
  expect_true(length(unique(result$Time)) == 1)
  expect_equal(unique(result$Time), 12)  # Based on the example file
})

test_that("parse_biocheck_dir works with directory", {
  # Create temporary network variables for testing
  netw_variables <- data.frame(
    id = c(3, 4, 5, 6, 7, 9, 13, 19),
    name = c("a", "b", "c", "d", "e", "f", "g", "h"),
    range_from = rep(0, 8),
    range_to = rep(2, 8),
    formula = rep("", 8),
    stringsAsFactors = FALSE
  )

  results_dir <- here::here("tests", "testthat", "helper_results_json")

  result <- parse_biocheck_dir(results_dir, netw_variables)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("filename", "time", "id", "lo", "hi", "name", "range_from", "range_to", "formula") %in% names(result)))
  expect_true(nrow(result) > 0)
  expect_true("growth_factor__1__b__0.json" %in% result$filename)
})

test_that("parse_biocheck_dir excludes CEX files", {
  # Create temporary directory with CEX file
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "test_biocheck")
  dir.create(test_dir, showWarnings = FALSE)

  # Create a normal JSON file
  normal_json <- file.path(test_dir, "normal.json")
  json_content <- '{"Status":"Stabilizing","Error":"","Ticks":[{"Time":1,"Variables":[{"Id":1,"Lo":0,"Hi":1}]}]}'
  writeLines(json_content, normal_json)

  # Create a CEX file that should be excluded
  cex_json <- file.path(test_dir, "test_cex.json")
  writeLines(json_content, cex_json)

  netw_variables <- data.frame(
    id = 1,
    name = "test",
    range_from = 0,
    range_to = 1,
    formula = "",
    stringsAsFactors = FALSE
  )

  result <- parse_biocheck_dir(test_dir, netw_variables)

  # Should only include normal.json, not test_cex.json
  expect_true("normal.json" %in% result$filename)
  expect_false("test_cex.json" %in% result$filename)

  unlink(test_dir, recursive = TRUE)
})

test_that("parse_biocheck_dir handles empty directory", {
  # Create temporary empty directory
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "empty_biocheck")
  dir.create(test_dir, showWarnings = FALSE)

  netw_variables <- data.frame(
    id = integer(0),
    name = character(0),
    range_from = integer(0),
    range_to = integer(0),
    formula = character(0),
    stringsAsFactors = FALSE
  )

  expect_error(
    parse_biocheck_dir(test_dir, netw_variables),
    "No JSON files found in the specified directory"
  )})

test_that("parse_biocheck_dir_apend works with existing file", {
  # Create temporary existing results file
  temp_dir <- tempdir()
  existing_file <- file.path(temp_dir, "existing_results.csv")

  # Create existing results
  existing_data <- data.frame(
    filename = "old_file.json",
    time = 10,
    id = 1,
    lo = 0,
    hi = 1,
    name = "old_node",
    range_from = 0,
    range_to = 1,
    formula = "",
    stringsAsFactors = FALSE
  )
  readr::write_csv(existing_data, existing_file)

  # Create directory with new JSON file
  test_dir <- file.path(temp_dir, "new_results")
  dir.create(test_dir, showWarnings = FALSE)

  new_json <- file.path(test_dir, "new_file.json")
  json_content <- '{"Status":"Stabilizing","Error":"","Ticks":[{"Time":5,"Variables":[{"Id":2,"Lo":1,"Hi":2}]}]}'
  writeLines(json_content, new_json)

  netw_variables <- data.frame(
    id = c(1, 2),
    name = c("old_node", "new_node"),
    range_from = c(0, 1),
    range_to = c(1, 2),
    formula = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- parse_biocheck_dir_apend(existing_file, test_dir, netw_variables)

  expect_s3_class(result, "tbl_df")
  expect_true(nrow(result) > nrow(existing_data))
  expect_true("old_file.json" %in% result$filename)
  expect_true("new_file.json" %in% result$filename)

  unlink(temp_dir, recursive = TRUE)
})

test_that("parse_biocheck_dir_apend handles no new files", {
  # Create temporary existing results file
  temp_dir <- tempdir()
  existing_file <- file.path(temp_dir, "existing_results.csv")

  # Create existing results
  existing_data <- data.frame(
    filename = "growth_factor__1__b__0.json",
    time = 12,
    id = 3,
    lo = 1,
    hi = 1,
    name = "a",
    range_from = 0,
    range_to = 1,
    formula = "",
    stringsAsFactors = FALSE
  )
  readr::write_csv(existing_data, existing_file)

  netw_variables <- data.frame(
    id = 3,
    name = "a",
    range_from = 0,
    range_to = 1,
    formula = "",
    stringsAsFactors = FALSE
  )

  results_dir <- here::here("tests", "testthat", "helper_results_json")

  expect_output(
    result <- parse_biocheck_dir_apend(existing_file, results_dir, netw_variables),
    "All files already parsed"
  )

  expect_equal(nrow(result), nrow(existing_data))

  unlink(existing_file)
})

test_that("parse_biocheck_dir handles recursive option", {
  # Create nested directory structure
  temp_dir <- tempdir()
  test_dir <- file.path(temp_dir, "nested_test")
  sub_dir <- file.path(test_dir, "subdir")
  dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)

  # Create JSON in subdirectory
  sub_json <- file.path(sub_dir, "nested.json")
  json_content <- '{"Status":"Stabilizing","Error":"","Ticks":[{"Time":1,"Variables":[{"Id":1,"Lo":0,"Hi":1}]}]}'
  writeLines(json_content, sub_json)

  netw_variables <- data.frame(
    id = 1,
    name = "test",
    range_from = 0,
    range_to = 1,
    formula = "",
    stringsAsFactors = FALSE
  )

  # Test with rec = TRUE
  result_rec <- parse_biocheck_dir(test_dir, netw_variables, rec = TRUE)
  expect_true(any(grepl("subdir", result_rec$filename)))

  # Test with rec = FALSE (default)
  expect_error(
    parse_biocheck_dir(test_dir, netw_variables, rec = FALSE),
    "No JSON files found in the specified directory."
  )

  unlink(test_dir, recursive = TRUE)
})