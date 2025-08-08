source(here::here("tests", "testthat", "testing_utils.r"))

library(mockery)

temp_dir <- here::here("tests/testthat/temp_test_outputs")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

test_that("run_autopert_config.r works with valid config", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "autopert requires Windows BMA command line tools (BioCheckConsole.exe)")

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
  
  # Write TOML directly to avoid configr NULL handling issues
  toml_content <- sprintf('
netw_file_path = "%s"
spec_path = "%s" 
out_dir = "%s"
nosat = %s
loserum = %s
missing_nodes_perturbed_overide = %s
missing_nodes_expected_overide = %s
project_path = ""
group_vars = [%s]
',
    gsub("\\\\", "/", valid_config$netw_file_path),
    gsub("\\\\", "/", valid_config$spec_path),
    valid_config$out_dir,
    tolower(valid_config$nosat),
    tolower(valid_config$loserum), 
    tolower(valid_config$missing_nodes_perturbed_overide),
    tolower(valid_config$missing_nodes_expected_overide),
    paste0('"', valid_config$group_vars, '"', collapse = ", ")
  )
  
  writeLines(toml_content, config_file)

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

test_that("run_autopert_config.r works with short_filenames configuration - Windows only", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "autopert requires Windows BMA command line tools")

  # Create test config with short_filenames = true
  config_file <- file.path(temp_dir, "short_filenames_config.toml")
  toml_content <- sprintf("
netw_file_path = \"%s\"
spec_path = \"%s\" 
out_dir = \"%s\"
nosat = %s
loserum = %s
missing_nodes_perturbed_overide = %s
missing_nodes_expected_overide = %s
project_path = \"\"
group_vars = [%s]
short_filenames = %s
",
    gsub("\\\\", "/", here::here("examples", "autopert", "helper_autopert_1.json")),
    gsub("\\\\", "/", here::here("examples", "autopert", "helper_spec_1.csv")),
    "short_filenames_autopert_test",
    "true",
    "false",
    "false",
    "false",
    "\"source\", \"cell_line\", \"experiment_particular\"",
    "true"
  )
  writeLines(toml_content, config_file)
  
  on.exit({
    unlink(config_file)
    if (dir.exists(file.path(temp_dir, "short_filenames_autopert_test"))) {
      unlink(file.path(temp_dir, "short_filenames_autopert_test"), recursive = TRUE)
    }
  })

  # Mock commandArgs to return our config file
  old_commandArgs <- commandArgs
  commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) {
      return(config_file)
    } else {
      return(c("R", "--slave", "--no-restore", "--file=script.R", "--args", config_file))
    }
  }

  # Restore on exit
  on.exit({
    commandArgs <- old_commandArgs
    unlink(config_file)
    if (dir.exists(file.path(temp_dir, "short_filenames_autopert_test"))) {
      unlink(file.path(temp_dir, "short_filenames_autopert_test"), recursive = TRUE)
    }
  }, add = TRUE)

  # Run the script and expect it to work
  expect_no_error({
    suppressMessages(source(here::here("examples/run_autopert_config.r")))
  })

  # Verify that the output directory contains results with hashed filenames
  out_dir <- file.path(temp_dir, "short_filenames_autopert_test")
  expect_true(dir.exists(out_dir))
  
  # Find AP_RUN directory
  ap_dirs <- list.dirs(out_dir, full.names = FALSE, recursive = FALSE)
  run_dir_name <- ap_dirs[grepl("^AP_RUN_", ap_dirs)]
  expect_true(length(run_dir_name) == 1)
  run_dir <- file.path(out_dir, run_dir_name)
  
  # Verify hashed files exist
  biocheck_files <- list.files(file.path(run_dir, "BioCheck_output"), pattern = "\.json$")
  expect_true(length(biocheck_files) > 0)
  
  # Verify files are hashed
  for (json_file in biocheck_files) {
    filename_without_ext <- tools::file_path_sans_ext(json_file)
    expect_true(nchar(filename_without_ext) == 32, 
               info = paste("Expected 32-char hash, got:", nchar(filename_without_ext)))
  }
})
