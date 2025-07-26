source(here::here("tests", "testthat", "testing_utils.r"))

test_that("run_autopert_config.r works with valid config", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "autopert requires Windows BMA tools")

  # Save original commandArgs
  old_commandArgs <- commandArgs

  # Set up mock commandArgs like temp_test_command_line.r
  args <- c(here::here("examples/run_autopert_config.r"), here::here("examples/autopert_config_example.json"))
  commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) {
      return(args[-1])  # Remove script name
    } else {
      return(c("R", "--slave", "--no-restore", "--file=script.R", "--args", args[-1]))
    }
  }

  # Restore on exit
  on.exit(commandArgs <- old_commandArgs)

  # Test that script runs without error
  expect_no_error(source(here::here("examples/run_autopert_config.r")))
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
  expect_snapshot(source("examples/run_autopert_config.r"), error = TRUE)
})

test_that("run_autopert_config.r handles multiple arguments", {
  # Save original commandArgs
  old_commandArgs <- commandArgs

  # Mock commandArgs to return multiple arguments
  commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) {
      return(c("config1.json", "config2.json"))
    } else {
      return(c("R", "--slave", "--no-restore", "--file=script.R", "--args", "config1.json", "config2.json"))
    }
  }

  # Restore on exit
  on.exit(commandArgs <- old_commandArgs)

  # Test that script errors with usage message
  expect_snapshot(source("examples/run_autopert_config.r"), error = TRUE)
})

test_that("run_autopert_config.r handles nonexistent config file", {
  # Save original commandArgs
  old_commandArgs <- commandArgs

  # Mock commandArgs to return nonexistent file
  commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) {
      return("nonexistent_config.json")
    } else {
      return(c("R", "--slave", "--no-restore", "--file=script.R", "--args", "nonexistent_config.json"))
    }
  }

  # Restore on exit
  on.exit(commandArgs <- old_commandArgs)

  # Test that script errors when config file doesn't exist
  expect_snapshot(source("examples/run_autopert_config.r"), error = TRUE)
})