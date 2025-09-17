source(here::here("tests", "testthat", "testing_utils.r"))

test_that("example_parallel_combo.r runs without errors", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", 
              "combo requires Windows BMA command line tools (BioCheckConsole.exe)")
  
  # Set up test logging
  setup_log_file(futile.logger::INFO)
  
  # clean up on exit
  out_dir <- file.path("combo_results", "parallel_combo_results")
  
  on.exit({
    cleanup_log_file()
    if(dir.exists(out_dir)){
      unlink(out_dir, recursive = TRUE)
    }
  })
  
  # Parallel combo runs without errors
  suppressMessages(expect_no_error(source(
    here::here("examples/example_parallel_combo.r"))))
  
  # A separate result directory is created per background, with expected files
  backgrounds <- c("cancer", "wt")
  
  for(background in backgrounds){
    results_dir <- file.path(out_dir, paste0("results_", background), "COMBO_RUN_helper_combo_1")
    
    expect_true(dir.exists(results_dir))
    expect_true(dir.exists(file.path(results_dir, paste0("RAW__single__", background))))
    expect_true(dir.exists(file.path(results_dir, paste0("RAW__double__", background))))
    
    
    expected_files <- c("parsed_results.csv", "processed_results.csv", "node_results.csv")
    
    for (file in expected_files) {
      expect_true(file.exists(file.path(results_dir, file)), info = paste("Missing file:", file))
    }
    
    # Check that each RAW directory contains JSON files
    raw_dirs <- c(paste0("RAW__single__", background), paste0("RAW__double__", background))
    
    for (raw_dir in raw_dirs) {
      json_files <- list.files(file.path(results_dir, raw_dir), pattern = "\\.json$")
      expect_true(length(json_files) > 0, info = paste("No JSON files in", raw_dir))
    }
  }
  
  # Integrated files are correctly created
  expected_integrated_files <- c("parsed_integrated_results.csv", 
                                 "processed_integrated_results.csv",
                                 "node_integrated_results.csv")
  
  for (file in expected_integrated_files) {
    expect_true(file.exists(file.path(pipe_dir, file)), info = paste("Missing file:", file))
  }
  
  # Snapshot testing
  for (file in expected_integrated_files) {
    data <- readr::read_csv(file.path(pipe_dir, file), show_col_types = FALSE)
    expect_snapshot(data)
  }
  
  # Temporary files are cleaned up
  tmp_files <- list.files(pattern = ".+_tmp_background.csv$")
  expect_true(length(tmp_files) == 0, info = "Temporary background files have not been deleted")
})
