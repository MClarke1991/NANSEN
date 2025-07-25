# Debug CSV Reader Utility
# This script analyzes CSV files with parsing issues to identify the root cause

library(readr)
library(vroom) 
library(dplyr)
library(here)

debug_csv_files <- function(base_dir = here::here("debug_csv_output", "COMBO_RUN_helper_combo_1")) {
  
  cat("=== CSV PARSING DEBUG ANALYSIS ===\n\n")
  
  if (!dir.exists(base_dir)) {
    cat("Debug directory doesn't exist:", base_dir, "\n")
    cat("Run the test-debug-csv-parsing.R test first to generate files.\n")
    return()
  }
  
  csv_files <- c(
    "parsed_results.csv",
    "processed_results.csv", 
    "conflicts.csv"
  )
  
  for (csv_file in csv_files) {
    csv_path <- file.path(base_dir, csv_file)
    
    cat("----------------------------------------\n")
    cat("ANALYZING:", csv_file, "\n")
    cat("Path:", csv_path, "\n")
    
    if (!file.exists(csv_path)) {
      cat("FILE NOT FOUND!\n\n")
      next
    }
    
    # Get file info
    file_info <- file.info(csv_path)
    cat("Size:", file_info$size, "bytes\n")
    
    # Try to read and check for problems
    cat("Reading with readr::read_csv...\n")
    tryCatch({
      data <- readr::read_csv(csv_path, show_col_types = FALSE)
      problems_df <- vroom::problems(data)
      
      cat("Rows read:", nrow(data), "\n")
      cat("Columns:", ncol(data), "\n")
      cat("Column names:", paste(colnames(data), collapse = ", "), "\n")
      
      if (nrow(problems_df) > 0) {
        cat("\nPROBLEMS FOUND:\n")
        print(problems_df)
        
        # Show specific problem rows
        if (nrow(problems_df) <= 10) {
          cat("\nDETAILED PROBLEM ANALYSIS:\n")
          for (i in 1:nrow(problems_df)) {
            problem <- problems_df[i, ]
            cat("Row", problem$row, "Col", problem$col, ":", problem$expected, "!=", problem$actual, "\n")
          }
        }
      } else {
        cat("No vroom problems detected.\n")
      }
      
    }, error = function(e) {
      cat("ERROR reading file:", e$message, "\n")
    })
    
    # Try alternative reading methods
    cat("\nTrying alternative read methods...\n")
    
    # Method 1: read.csv (base R)
    tryCatch({
      data_base <- read.csv(csv_path)
      cat("Base R read.csv: SUCCESS -", nrow(data_base), "rows\n")
    }, error = function(e) {
      cat("Base R read.csv: FAILED -", e$message, "\n")
    })
    
    # Method 2: vroom directly
    tryCatch({
      data_vroom <- vroom::vroom(csv_path, show_col_types = FALSE)
      problems_vroom <- vroom::problems(data_vroom)
      cat("Direct vroom: SUCCESS -", nrow(data_vroom), "rows,", nrow(problems_vroom), "problems\n")
    }, error = function(e) {
      cat("Direct vroom: FAILED -", e$message, "\n")
    })
    
    # Method 3: Check raw file content
    cat("\nChecking raw file content (first 5 lines):\n")
    tryCatch({
      lines <- readLines(csv_path, n = 5)
      for (i in 1:length(lines)) {
        cat("Line", i, ":", substr(lines[i], 1, 100), "\n")
        if (nchar(lines[i]) > 100) {
          cat("  ... (line truncated, total length:", nchar(lines[i]), ")\n")
        }
      }
    }, error = function(e) {
      cat("Error reading raw lines:", e$message, "\n")
    })
    
    cat("\n")
  }
  
  cat("=== DEBUG ANALYSIS COMPLETE ===\n")
}

# Function to examine specific problematic rows/columns
examine_csv_problems <- function(csv_path, max_problems = 5) {
  cat("EXAMINING PROBLEMS IN:", basename(csv_path), "\n")
  
  data <- readr::read_csv(csv_path, show_col_types = FALSE)
  problems_df <- vroom::problems(data)
  
  if (nrow(problems_df) == 0) {
    cat("No problems found.\n")
    return()
  }
  
  cat("Total problems:", nrow(problems_df), "\n")
  
  # Show first few problems in detail
  n_show <- min(max_problems, nrow(problems_df))
  for (i in 1:n_show) {
    problem <- problems_df[i, ]
    cat("\nProblem", i, ":\n")
    cat("  Row:", problem$row, "\n")
    cat("  Column:", problem$col, "\n") 
    cat("  Expected:", problem$expected, "\n")
    cat("  Actual:", problem$actual, "\n")
    
    # Try to show the actual problematic line
    tryCatch({
      lines <- readLines(csv_path)
      if (problem$row <= length(lines)) {
        cat("  Full line:", lines[problem$row], "\n")
      }
    }, error = function(e) {
      cat("  Could not read line\n")
    })
  }
}

# Run the analysis if sourced directly
if (interactive()) {
  debug_csv_files()
}