source(here::here("tests", "testthat", "testing_utils.r"))

temp_dir <- here::here("tests/testthat/temp_test_outputs")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

# Helper function to mock commandArgs and source the script
mock_script_with_args <- function(args) {
  script_path <- here::here("examples", "run_autopert_config.r")
  
  # Save original commandArgs function
  old_commandArgs <- commandArgs
  
  # Create mock commandArgs function
  mock_commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) {
      return(args)
    } else {
      return(c("R", "--slave", "--no-restore", "--file=run_autopert_config.r", "--args", args))
    }
  }
  
  # Temporarily replace commandArgs and source the script
  on.exit(assign("commandArgs", old_commandArgs, envir = .GlobalEnv))
  assign("commandArgs", mock_commandArgs, envir = .GlobalEnv)
  
  source(script_path, local = TRUE)
}

test_that("run_autopert_config.r handles no command line arguments", {
  expect_snapshot(
    mock_script_with_args(character(0)),
    error = TRUE
  )
})

test_that("run_autopert_config.r handles multiple command line arguments", {
  expect_snapshot(
    mock_script_with_args(c("arg1", "arg2")),
    error = TRUE
  )
})

test_that("run_autopert_config.r handles nonexistent config file", {
  expect_snapshot(
    mock_script_with_args("nonexistent_config.json"),
    error = TRUE
  )
})

test_that("run_autopert_config.r handles invalid config file", {
  # Create invalid config
  invalid_config_file <- file.path(temp_dir, "invalid_for_script.json")
  writeLines("{ invalid json", invalid_config_file)
  
  on.exit(unlink(invalid_config_file))
  
  expect_snapshot(
    mock_script_with_args(invalid_config_file),
    error = TRUE
  )
})

test_that("run_autopert_config.r processes valid config file", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "autopert requires Windows BMA tools")
  
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
  
  # This test should not error but will be skipped on non-Windows due to autopert BMA dependencies
  # We test that the script loads config successfully before trying to run autopert
  expect_no_error(
    mock_script_with_args(config_file)
  )
})