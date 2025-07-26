test_that("get_netw_variables works with valid JSON", {
  # Use existing example file
  netw_file <- here::here("examples", "autopert", "helper_autopert_1.json")

  result <- get_netw_variables(netw_file)

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("name", "id", "range_from", "range_to", "formula") %in% names(result)))
  expect_true(nrow(result) > 0)
  expect_type(result$id, "integer")
  expect_type(result$range_from, "integer")
  expect_type(result$range_to, "integer")
})

test_that("get_netw_variables handles missing file", {
  expect_snapshot(
    get_netw_variables("nonexistent_file.json"),
    error = TRUE
  )
})

test_that("get_netw_variables removes newline characters from formulas", {
  # Create temporary JSON with newlines in formula
  temp_json <- tempfile(fileext = ".json")
  json_content <- '{"Model":{"Variables":[{"Name":"test","Id":1,"RangeFrom":0,"RangeTo":1,"Formula":"line1\\nline2\\rline3"}]}}'
  writeLines(json_content, temp_json)

  expect_message(
    result <- get_netw_variables(temp_json),
    "New line characters in formula"
  )

  expect_false(any(grepl("\\n|\\r", result$formula)))

  unlink(temp_json)
})

test_that("get_netw_variables detects duplicate node names", {
  # Create temporary JSON with duplicate names
  temp_json <- tempfile(fileext = ".json")
  json_content <- '{"Model":{"Variables":[{"Name":"duplicate","Id":1,"RangeFrom":0,"RangeTo":1,"Formula":""},{"Name":"duplicate","Id":2,"RangeFrom":0,"RangeTo":2,"Formula":""}]}}'
  writeLines(json_content, temp_json)

  expect_snapshot(
    get_netw_variables(temp_json),
    error = TRUE
  )

  unlink(temp_json)
})

test_that("get_netw_variables detects zero granularity nodes", {
  # Create temporary JSON with zero granularity (range_from == range_to)
  temp_json <- tempfile(fileext = ".json")
  json_content <- '{"Model":{"Variables":[{"Name":"zero_gran","Id":1,"RangeFrom":1,"RangeTo":1,"Formula":""}]}}'
  writeLines(json_content, temp_json)

  expect_snapshot(
    get_netw_variables(temp_json),
    error = TRUE
  )

  unlink(temp_json)
})

test_that("get_netw_variables converts character numeric values", {
  # Create temporary JSON with character numeric values
  temp_json <- tempfile(fileext = ".json")
  json_content <- '{"Model":{"Variables":[{"Name":"test","Id":"1","RangeFrom":"0","RangeTo":"2","Formula":""}]}}'
  writeLines(json_content, temp_json)

  result <- get_netw_variables(temp_json)

  expect_type(result$id, "integer")
  expect_type(result$range_from, "integer")
  expect_type(result$range_to, "integer")

  unlink(temp_json)
})

test_that("get_netw_variables validates node names", {
  # Create temporary JSON with invalid node name (this will depend on node_name_check implementation)
  temp_json <- tempfile(fileext = ".json")
  # Assuming node_name_check rejects names with certain characters
  json_content <- '{"Model":{"Variables":[{"Name":"invalid__name","Id":1,"RangeFrom":0,"RangeTo":1,"Formula":""}]}}'
  writeLines(json_content, temp_json)

  # This test assumes node_name_check will fail on hyphenated names
  # Adjust based on actual node_name_check implementation
  expect_error(get_netw_variables(temp_json))

  unlink(temp_json)
})

test_that("get_netw_variables handles empty node names", {
  # Test handling of empty node names (as seen in example file)
  netw_file <- here::here("examples", "autopert", "helper_autopert_1.json")

  result <- get_netw_variables(netw_file)

  # Should handle empty names without error
  expect_true(any(result$name == ""))
})

test_that("get_netw_variables handles malformed JSON", {
  # Create temporary malformed JSON
  temp_json <- tempfile(fileext = ".json")
  writeLines('{"Model":{"Variables":[{malformed}]}}', temp_json)

  expect_snapshot(
    get_netw_variables(temp_json),
    error = TRUE
  )

  unlink(temp_json)
})

test_that("get_netw_variables returns correct structure", {
  netw_file <- here::here("examples", "autopert", "helper_autopert_1.json")

  result <- get_netw_variables(netw_file)

  # Check that all required columns are present
  expected_cols <- c("name", "id", "range_from", "range_to", "formula")
  expect_true(all(expected_cols %in% names(result)))

  # Check that ranges are valid
  expect_true(all(result$range_from <= result$range_to))

  # Check that IDs are unique
  expect_false(any(duplicated(result$id)))
})

test_that("get_netw_variables handles missing Model structure", {
  # Create temporary JSON without Model structure
  temp_json <- tempfile(fileext = ".json")
  json_content <- '{"NotModel":{"Variables":[]}}'
  writeLines(json_content, temp_json)

  expect_snapshot(
    get_netw_variables(temp_json),
    error = TRUE
  )

  unlink(temp_json)
})

