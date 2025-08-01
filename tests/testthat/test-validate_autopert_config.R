source(here::here("tests", "testthat", "testing_utils.r"))

temp_dir <- here::here("tests/testthat/temp_test_outputs")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

test_that("validate_autopert_config handles valid configuration", {
  # Create temporary valid config
  valid_config <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output",
    nosat = FALSE,
    loserum = TRUE
  )
  
  config_file <- file.path(temp_dir, "valid_config.json")
  jsonlite::write_json(valid_config, config_file, auto_unbox = TRUE)
  
  on.exit(unlink(config_file))
  
  result <- validate_autopert_config(config_file)
  
  expect_equal(result$netw_file_path, valid_config$netw_file_path)
  expect_equal(result$spec_path, valid_config$spec_path)
  expect_equal(result$out_dir, valid_config$out_dir)
  expect_equal(result$nosat, FALSE)
  expect_equal(result$loserum, TRUE)
  # Check defaults are applied
  expect_equal(result$missing_nodes_perturbed_overide, FALSE)
  expect_equal(result$missing_nodes_expected_overide, FALSE)
  expect_true(is.na(result$project_path))
  expect_equal(result$group_vars, c("source", "cell_line", "experiment_particular"))
})

test_that("validate_autopert_config applies defaults correctly", {
  # Create minimal config with only required fields
  minimal_config <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "minimal_config.json")
  jsonlite::write_json(minimal_config, config_file, auto_unbox = TRUE)
  
  on.exit(unlink(config_file))
  
  result <- validate_autopert_config(config_file)
  
  # Check that defaults are applied
  expect_equal(result$nosat, TRUE)
  expect_equal(result$loserum, FALSE)
  expect_equal(result$missing_nodes_perturbed_overide, FALSE)
  expect_equal(result$missing_nodes_expected_overide, FALSE)
  expect_true(is.na(result$project_path))
  expect_equal(result$group_vars, c("source", "cell_line", "experiment_particular"))
})

test_that("validate_autopert_config errors on missing config file", {
  expect_snapshot(
    validate_autopert_config("nonexistent_config.json"),
    error = TRUE
  )
})

test_that("validate_autopert_config errors on invalid JSON", {
  invalid_json_file <- file.path(temp_dir, "invalid.json")
  writeLines("{ invalid json", invalid_json_file)
  
  on.exit(unlink(invalid_json_file))
  
  expect_snapshot(
    validate_autopert_config(invalid_json_file),
    error = TRUE
  )
})

test_that("validate_autopert_config errors on missing required fields", {
  # Test missing netw_file_path
  config_missing_netw <- list(
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_netw.json")
  jsonlite::write_json(config_missing_netw, config_file, auto_unbox = TRUE)
  
  expect_snapshot(
    validate_autopert_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
  
  # Test missing spec_path
  config_missing_spec <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_spec.json")
  jsonlite::write_json(config_missing_spec, config_file, auto_unbox = TRUE)
  
  expect_snapshot(
    validate_autopert_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
  
  # Test missing out_dir
  config_missing_out <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv")
  )
  
  config_file <- file.path(temp_dir, "missing_out.json")
  jsonlite::write_json(config_missing_out, config_file, auto_unbox = TRUE)
  
  expect_snapshot(
    validate_autopert_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
})

test_that("validate_autopert_config errors on missing referenced files", {
  # Test missing network file
  config_missing_netw_file <- list(
    netw_file_path = "nonexistent_network.json",
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_netw_file.json")
  jsonlite::write_json(config_missing_netw_file, config_file, auto_unbox = TRUE)
  
  expect_snapshot(
    validate_autopert_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
  
  # Test missing spec file
  config_missing_spec_file <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = "nonexistent_spec.csv",
    out_dir = "test_output"
  )
  
  config_file <- file.path(temp_dir, "missing_spec_file.json")
  jsonlite::write_json(config_missing_spec_file, config_file, auto_unbox = TRUE)
  
  expect_snapshot(
    validate_autopert_config(config_file),
    error = TRUE
  )
  
  unlink(config_file)
})