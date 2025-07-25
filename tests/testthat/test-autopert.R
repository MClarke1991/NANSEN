source(here::here("tests", "testthat", "testing_utils.r"))

bma_path = 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'
test_that("autopert integration test - Windows only", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "autopert requires Windows BMA tools")

  # Check if BMA executable exists
  # bma_path <- 'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe'
  # skip_if_not(file.exists(bma_path), paste("BMA executable not found at:", bma_path))

  # Create temporary directory for test outputs
  test_dir <- tempdir()
  out_dir <- file.path(test_dir, "autopert_test_output")

  # Set up test logging
  setup_log_file(futile.logger::INFO)
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  # Run autopert function with example data
  expect_no_error(
    autopert(
      netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
      spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
      out_dir = out_dir,
      bma_path = bma_path
    )
  )

  # Verify output directory structure
  run_dir <- file.path(out_dir, "AP_RUN_helper_autopert_1")
  expect_true(dir.exists(run_dir))
  expect_true(dir.exists(file.path(run_dir, "BioCheck_output")))
  expect_true(dir.exists(file.path(run_dir, "results")))

  # Verify key files exist
  expect_true(file.exists(file.path(run_dir, "results", "results.csv")))
  expect_true(file.exists(file.path(run_dir, "results", "results_score.csv")))
  expect_true(file.exists(file.path(run_dir, "results", "results_short.csv")))
  expect_true(file.exists(file.path(run_dir, "results", "results_mismatch.csv")))
  expect_true(file.exists(file.path(run_dir, "results", "results_short_node_summary.csv")))
  expect_true(file.exists(file.path(run_dir, "results", "parse_results.csv")))

  # Verify PNG plots exist
  expect_true(file.exists(file.path(run_dir, "results", "results_plot.png")))
  expect_true(file.exists(file.path(run_dir, "results", "results_short_node_summary.png")))
  expect_true(file.exists(file.path(run_dir, "results", "results_short_node_summary_abs_and_diff.png")))
  expect_true(file.exists(file.path(run_dir, "results", "results_per_pert_per_gene.png")))

  # Verify CSV structure and content
  results <- readr::read_csv(file.path(run_dir, "results", "results.csv"), show_col_types = FALSE, col_types = cols(formula = "c"))
  expect_true(all(c("gene", "perturbation", "expectation_bma", "lo", "hi", "mean_result", "diff") %in% colnames(results)))

  parse_results <- readr::read_csv(file.path(run_dir, "results", "parse_results.csv"), show_col_types = FALSE, col_types = cols(formula = "c"))
  expect_true(all(c("filename", "time", "id", "lo", "hi", "name", "range_from", "range_to", "formula") %in% colnames(parse_results)))

  results_score <- readr::read_csv(file.path(run_dir, "results", "results_score.csv"), show_col_types = FALSE, col_types = cols(formula = "c"))
  expect_true("score" %in% colnames(results_score))
  expect_true(nrow(results_score) == 1)
  expect_true(is.numeric(results_score$score))

  results_short_node_summary <- readr::read_csv(file.path(run_dir, "results", "results_short_node_summary.csv"), show_col_types = FALSE, col_types = cols(formula = "c"))
  expect_true(all(c("gene", "diff_per_gene", "abs_diff_per_gene") %in% colnames(results_short_node_summary)))

  # Verify BioCheck output files exist
  biocheck_files <- list.files(file.path(run_dir, "BioCheck_output"), pattern = "\\.json$")
  expect_true(length(biocheck_files) > 0)

  # Verify each BioCheck file is valid JSON
  for (json_file in biocheck_files) {
    json_path <- file.path(run_dir, "BioCheck_output", json_file)
    expect_no_error(jsonlite::fromJSON(json_path))
  }

  # Snapshot test for results.csv to ensure output doesn't change
  results_csv_path <- file.path(run_dir, "results", "results.csv")
  results_data <- readr::read_csv(results_csv_path, show_col_types = FALSE, col_types = cols(formula = "c"))
  expect_snapshot(results_data)
})

test_that("autopert handles missing network file", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "autopert requires Windows BMA tools")

  test_dir <- tempdir()
  out_dir <- file.path(test_dir, "autopert_error_test")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  expect_error(
    autopert(
      netw_file_path = "nonexistent_file.json",
      spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
      out_dir = out_dir
    )
  )
})

test_that("autopert handles missing specification file", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "autopert requires Windows BMA tools")

  test_dir <- tempdir()
  out_dir <- file.path(test_dir, "autopert_error_test")

  setup_log_file()
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  expect_error(
    autopert(
      netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
      spec_path = "nonexistent_spec.csv",
      out_dir = out_dir
    )
  )
})

test_that("autopert creates expected directory structure", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "autopert requires Windows BMA tools")

  test_dir <- tempdir()
  out_dir <- file.path(test_dir, "autopert_structure_test")

  setup_log_file(futile.logger::INFO)
  on.exit({
    cleanup_log_file()
    if (dir.exists(out_dir)) {
      unlink(out_dir, recursive = TRUE)
    }
  })

  autopert(
    netw_file_path = here::here("examples", "autopert", "helper_autopert_1.json"),
    spec_path = here::here("examples", "autopert", "helper_spec_1.csv"),
    out_dir = out_dir
  )

  # Test the specific directory structure from autopert_example_structure.md
  run_dir <- file.path(out_dir, "AP_RUN_helper_autopert_1")

  # Check main directories
  expect_true(dir.exists(run_dir))
  expect_true(dir.exists(file.path(run_dir, "BioCheck_output")))
  expect_true(dir.exists(file.path(run_dir, "results")))

  # Check specific files match the documented structure
  results_files <- list.files(file.path(run_dir, "results"))
  expected_csv_files <- c("parse_results.csv", "results.csv", "results_short.csv",
                         "results_mismatch.csv", "results_short_node_summary.csv",
                         "results_score.csv")
  expected_png_files <- c("results_plot.png", "results_short_node_summary.png",
                         "results_short_node_summary_abs_and_diff.png",
                         "results_per_pert_per_gene.png")

  for (file in expected_csv_files) {
    expect_true(file %in% results_files, info = paste("Missing CSV file:", file))
  }

  for (file in expected_png_files) {
    expect_true(file %in% results_files, info = paste("Missing PNG file:", file))
  }
})