# Testing utilities for NANSEN package tests

# Helper function to set up log_file environment for functions that require it
setup_log_file <- function(level = futile.logger::ERROR) {
  assign("log_file", "test.log", envir = .GlobalEnv)
  # Suppress futile.logger output during testing
  futile.logger::flog.threshold(level)
}

# Helper function to clean up log_file environment after tests
cleanup_log_file <- function() {
  if (exists("log_file", envir = .GlobalEnv)) {
    rm("log_file", envir = .GlobalEnv)
  }
  # Reset futile.logger threshold to default
  futile.logger::flog.threshold(futile.logger::INFO)
}

# Helper function to set up temporary directory for tests
setup_temp_dir <- function(temp_dir) {
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
  dir.create(temp_dir, recursive = TRUE)
}

# Helper function to clean up temporary directory after tests
cleanup_temp_dir <- function(temp_dir) {
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
}