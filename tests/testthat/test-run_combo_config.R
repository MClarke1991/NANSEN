source(here::here("tests", "testthat", "testing_utils.r"))

library(mockery)

temp_dir <- here::here("tests/testthat/temp_test_outputs")
setup_temp_dir(temp_dir)

# Clean up temp directory after all tests complete
on.exit(cleanup_temp_dir(temp_dir), add = TRUE)

test_that("run_combo_config.r works with valid config", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools (BioCheckConsole.exe)")

  # Create temporary valid config with absolute paths
  valid_config <- list(
    netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
    backgrounds_path = here::here("examples", "combo", "helper_combo_bkg_1.csv"),
    out_dir = temp_dir,
    skip_autopert = TRUE,
    skip_combo_sim = FALSE,
    skip_heatmaps = TRUE,
    skip_heatmaps_uc = TRUE,
    pheno_only = TRUE,
    phenotypes = c("output_a", "output_b"),
    project_path = "",
    node_col_name = "node",
    use_vmcai = TRUE
  )

  config_file <- file.path(temp_dir, "test_combo_config.toml")

  # Write TOML directly to avoid configr issues with complex lists
  toml_content <- sprintf('
netw_file_path = "%s"
backgrounds_path = "%s"
out_dir = "%s"
skip_autopert = %s
skip_combo_sim = %s
skip_heatmaps = %s
skip_heatmaps_uc = %s
pheno_only = %s
phenotypes = [%s]
project_path = "%s"
node_col_name = "%s"
use_vmcai = %s
',
    gsub("\\\\", "/", valid_config$netw_file_path),
    gsub("\\\\", "/", valid_config$backgrounds_path),
    valid_config$out_dir,
    tolower(valid_config$skip_autopert),
    tolower(valid_config$skip_combo_sim),
    tolower(valid_config$skip_heatmaps),
    tolower(valid_config$skip_heatmaps_uc),
    tolower(valid_config$pheno_only),
    paste0('"', valid_config$phenotypes, '"', collapse = ", "),
    valid_config$project_path,
    valid_config$node_col_name,
    tolower(valid_config$use_vmcai)
  )

  writeLines(toml_content, config_file)

  # Clean up on exit
  on.exit({
    if (file.exists(config_file)) file.remove(config_file)
    # Clean up any output files created in temp_dir during the test
    output_files <- list.files(temp_dir, full.names = TRUE, recursive = FALSE)
    output_files <- output_files[!grepl("test_combo_config\\.toml$", output_files)]
    if (length(output_files) > 0) {
      unlink(output_files, recursive = TRUE)
    }
  })

  args <- c(here::here("examples/run_combo_config.r"), config_file)

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
      suppressWarnings(suppressMessages(expect_no_error(source(here::here("examples/run_combo_config.r")))))
    }
  )
})

test_that("run_combo_config.r handles no arguments", {
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
  expect_snapshot(source(here::here("examples/run_combo_config.r")), error = TRUE)
})

test_that("run_combo_config.r handles multiple arguments", {
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
  expect_snapshot(source(here::here("examples/run_combo_config.r")), error = TRUE)
})

test_that("run_combo_config.r handles nonexistent config file", {
  # Save original commandArgs
  old_commandArgs <- commandArgs

  # Mock commandArgs to return nonexistent file
  commandArgs <- function(trailingOnly = FALSE) {
    if (trailingOnly) {
      return("nonexistent_combo_config.toml")
    } else {
      return(c("R", "--slave", "--no-restore", "--file=script.R", "--args", "nonexistent_combo_config.toml"))
    }
  }

  # Restore on exit
  on.exit(commandArgs <- old_commandArgs)

  # Test that script errors when config file doesn't exist
  expect_snapshot(source(here::here("examples/run_combo_config.r")), error = TRUE)
})

test_that("run_combo_config.r works with short_filenames configuration - Windows only", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "combo requires Windows BMA command line tools")

  # Create test config with short_filenames = true
  config_file <- file.path(temp_dir, "short_filenames_combo_config.toml")
  toml_content <- sprintf("
netw_file_path = \"%s\"
backgrounds_path = \"%s\"
out_dir = \"%s\"
skip_autopert = %s
skip_combo_sim = %s
skip_heatmaps = %s
skip_heatmaps_uc = %s
pheno_only = %s
phenotypes = [%s]
project_path = \"\"
node_col_name = \"%s\"
use_vmcai = %s
short_filenames = %s
",
    gsub("\\\\", "/", here::here("examples", "combo", "helper_combo_1.json")),
    gsub("\\\\", "/", here::here("examples", "combo", "helper_combo_bkg_1.csv")),
    file.path(temp_dir, "short_filenames_combo_test"),
    "true",
    "false",
    "true",
    "true",
    "true",
    "\"output_a\", \"output_b\"",
    "node",
    "true",
    "true"
  )
  writeLines(toml_content, config_file)

  on.exit({
    unlink(config_file)
    out_dir <- file.path(temp_dir, "short_filenames_combo_test")
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  args <- c(here::here("examples/run_combo_config.r"), config_file)

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
      suppressMessages(suppressWarnings(expect_no_error(source(here::here("examples/run_combo_config.r")))))
    }
  )

  # Verify that the output directory contains results with hashed functionality
  out_dir <- file.path(temp_dir, "short_filenames_combo_test")
  expect_true(dir.exists(out_dir))

  # Find COMBO_RUN directory
  combo_dirs <- list.dirs(out_dir, full.names = FALSE, recursive = FALSE)
  run_dir_name <- combo_dirs[grepl("^COMBO_RUN_", combo_dirs)]
  expect_true(length(run_dir_name) == 1)
  run_dir <- file.path(out_dir, run_dir_name)

  # Verify file_hashtables directory exists
  hashtable_dir <- file.path(run_dir, "file_hashtables")
  expect_true(dir.exists(hashtable_dir))

  # Verify hashtable files exist
  hashtable_files <- list.files(hashtable_dir, pattern = "^file_hashtable_.*\\.csv$")
  expect_true(length(hashtable_files) > 0)

  # Verify results files exist
  expect_true(file.exists(file.path(run_dir, "parsed_results.csv")))
  expect_true(file.exists(file.path(run_dir, "processed_results.csv")))
})
