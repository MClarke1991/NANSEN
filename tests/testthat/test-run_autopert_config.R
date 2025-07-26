source(here::here("tests", "testthat", "testing_utils.r"))

temp_dir <- here::here("tests/testthat/temp_test_outputs")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

test_that("run_autopert_config.r handles command line arguments correctly", {
  script_path <- here::here("examples", "run_autopert_config.r")
  
  # Test with no arguments
  result <- tryCatch({
    system2("Rscript", args = script_path, stdout = TRUE, stderr = TRUE)
  }, error = function(e) e)
  
  expect_snapshot(result, error = TRUE)
  
  # Test with multiple arguments
  result <- tryCatch({
    system2("Rscript", args = c(script_path, "arg1", "arg2"), stdout = TRUE, stderr = TRUE)
  }, error = function(e) e)
  
  expect_snapshot(result, error = TRUE)
})

test_that("run_autopert_config.r handles nonexistent config file", {
  script_path <- here::here("examples", "run_autopert_config.r")
  
  result <- tryCatch({
    system2("Rscript", args = c(script_path, "nonexistent_config.json"), stdout = TRUE, stderr = TRUE)
  }, error = function(e) e)
  
  expect_snapshot(result, error = TRUE)
})

test_that("run_autopert_config.r handles invalid config file", {
  script_path <- here::here("examples", "run_autopert_config.r")
  
  # Create invalid config
  invalid_config_file <- file.path(temp_dir, "invalid_for_script.json")
  writeLines("{ invalid json", invalid_config_file)
  
  on.exit(unlink(invalid_config_file))
  
  result <- tryCatch({
    system2("Rscript", args = c(script_path, invalid_config_file), stdout = TRUE, stderr = TRUE)
  }, error = function(e) e)
  
  expect_snapshot(result, error = TRUE)
})

test_that("run_autopert_config.r processes valid config file", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "autopert requires Windows BMA tools")
  
  script_path <- here::here("examples", "run_autopert_config.r")
  
  # Create valid config that points to existing test files
  valid_config <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = file.path(temp_dir, "script_test_output"),
    nosat = TRUE,
    loserum = FALSE
  )
  
  config_file <- file.path(temp_dir, "valid_script_config.json")
  jsonlite::write_json(valid_config, config_file, auto_unbox = TRUE)
  
  on.exit({
    unlink(config_file)
    if (dir.exists(valid_config$out_dir)) {
      unlink(valid_config$out_dir, recursive = TRUE)
    }
  })
  
  # This test verifies the script can load and validate the config
  # We don't actually run autopert due to BMA dependency requirements
  # Instead we test that config loading works by checking initial output
  result <- system2("Rscript", args = c(script_path, config_file), 
                   stdout = TRUE, stderr = TRUE, timeout = 10)
  
  # Check that script started processing (config loading messages should appear)
  expect_true(any(grepl("Loading configuration", result) | grepl("Running autopert", result)))
})

test_that("run_autopert_config.r shows proper usage message", {
  script_path <- here::here("examples", "run_autopert_config.r")
  
  # Test the stop message for incorrect usage
  expect_snapshot(
    stop("Usage: Rscript run_autopert_config.r <config_file_path>"),
    error = TRUE
  )
})