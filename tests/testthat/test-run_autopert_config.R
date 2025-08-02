source(here::here("tests", "testthat", "testing_utils.r"))

library(mockery)

temp_dir <- here::here("tests/testthat/temp_test_outputs")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

test_that("run_autopert_config.r works with valid config", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "autopert requires Windows BMA tools")

  # Create temporary valid config with absolute paths
  valid_config <- list(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = "auto_pert_results",
    nosat = TRUE,
    loserum = FALSE,
    missing_nodes_perturbed_overide = FALSE,
    missing_nodes_expected_overide = FALSE,
    project_path = NULL,
    group_vars = c("source", "cell_line", "experiment_particular")
  )

  config_file <- file.path(temp_dir, "test_config.toml")
  configr::write.config(valid_config, config_file, file.type = "toml")

  # Clean up on exit
  on.exit(if (file.exists(config_file)) file.remove(config_file))

  args <- c(here::here("examples/run_autopert_config.r"), config_file)

  with_mocked_bindings(
    commandArgs = function(trailingOnly = FALSE) {
      if (trailingOnly) {
        return(args[-1])
      } else {
        return(c("R", "--slave", "--no-restore", "--file=script.R", "--args", args[-1]))
      }
    },
    .package = "base",
    {
      suppressMessages(expect_no_error(source(here::here("examples/run_autopert_config.r"))))
    }
  )
})

test_that("run_autopert_config.r handles no arguments", {
  # Save original commandArgs
  old_commandArgs <- commandArgs

  # Mock commandArgs to return no arguments
  commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) {
      return(character(0))
    } else {
      return(c("R", "--slave", "--no-restore"))
    }
  }

  # Restore on exit
  on.exit(commandArgs <- old_commandArgs)

  # Test that script errors with usage message
  expect_snapshot(source(here::here("examples/run_autopert_config.r")), error = TRUE)
})

test_that("run_autopert_config.r handles multiple arguments", {
  # Save original commandArgs
  old_commandArgs <- commandArgs

  # Mock commandArgs to return multiple arguments
  commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) {
      return(c("config1.toml", "config2.toml"))
    } else {
      return(c("R", "--slave", "--no-restore", "--file=script.R", "--args", "config1.toml", "config2.toml"))
    }
  }

  # Restore on exit
  on.exit(commandArgs <- old_commandArgs)

  # Test that script errors with usage message
  expect_snapshot(source(here::here("examples/run_autopert_config.r")), error = TRUE)
})

test_that("run_autopert_config.r handles nonexistent config file", {
  # Save original commandArgs
  old_commandArgs <- commandArgs

  # Mock commandArgs to return nonexistent file
  commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) {
      return("nonexistent_config.toml")
    } else {
      return(c("R", "--slave", "--no-restore", "--file=script.R", "--args", "nonexistent_config.toml"))
    }
  }

  # Restore on exit
  on.exit(commandArgs <- old_commandArgs)

  # Test that script errors when config file doesn't exist
  expect_snapshot(source(here::here("examples/run_autopert_config.r")), error = TRUE)
})