source(here::here("tests", "testthat", "testing_utils.r"))

temp_dir <- here::here("tests/testthat/temp_test_outputs")
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

test_that("split_combo_results works without drugs (drug_path = NA)", {
  # Set up test directory structure
  test_dir <- file.path(temp_dir, "split_combo_test_no_drugs")
  results_dir <- file.path(test_dir, "COMBO_RUN_helper_combo_1")
  
  on.exit({
    if (dir.exists(test_dir)) {
      unlink(test_dir, recursive = TRUE)
    }
  })
  
  # Create directory structure
  dir.create(results_dir, recursive = TRUE)
  
  # Copy test data
  file.copy(
    here::here("tests", "testthat", "helper_results_json", "processed_results.csv"),
    file.path(results_dir, "processed_results.csv")
  )
  
  # Run split_combo_results without drugs
  expect_no_error(
    split_combo_results(
      results_prefix = "COMBO_RUN",
      project_path = "",
      out_dir = test_dir,
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      drug_path = NA,
      node_col_name = "node"
    )
  )
  
  # Check that node_results.csv was created
  node_results_path <- file.path(results_dir, "node_results.csv")
  expect_true(file.exists(node_results_path))
  
  # Check that drug-related files were NOT created
  expect_false(file.exists(file.path(results_dir, "druggable_results.csv")))
  expect_false(file.exists(file.path(results_dir, "drug_results.csv")))
  
  # Read and verify node_results.csv content
  node_results <- readr::read_csv(node_results_path, show_col_types = FALSE)
  
  # Should only contain perturbations of nodes in the network (plus baseline)
  netw_variables <- get_netw_variables(here::here("examples", "combo", "helper_combo_1.json"))
  all_nodes <- c(unique(netw_variables$name), "baseline")
  
  # Check that all muta values are in the node list
  expect_true(all(node_results$muta %in% all_nodes))
  
  # Check that all mutb values are either in the node list or NA
  expect_true(all(is.na(node_results$mutb) | node_results$mutb %in% all_nodes))
})

test_that("split_combo_results works with drugs", {
  # Set up test directory structure
  test_dir <- file.path(temp_dir, "split_combo_test_with_drugs")
  results_dir <- file.path(test_dir, "COMBO_RUN_helper_combo_1")
  
  on.exit({
    if (dir.exists(test_dir)) {
      unlink(test_dir, recursive = TRUE)
    }
  })
  
  # Create directory structure
  dir.create(results_dir, recursive = TRUE)
  
  # Copy test data
  file.copy(
    here::here("tests", "testthat", "helper_results_json", "processed_results.csv"),
    file.path(results_dir, "processed_results.csv")
  )
  
  # Run split_combo_results with drugs
  expect_no_error(
    split_combo_results(
      results_prefix = "COMBO_RUN",
      project_path = "",
      out_dir = test_dir,
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      drug_path = here::here("examples", "combo", "helper_combo_drugs_1.csv"),
      node_col_name = "node"
    )
  )
  
  # Check that all result files were created
  expect_true(file.exists(file.path(results_dir, "node_results.csv")))
  expect_true(file.exists(file.path(results_dir, "druggable_results.csv")))
  expect_true(file.exists(file.path(results_dir, "drug_results.csv")))
  
  # Read and verify content
  node_results <- readr::read_csv(file.path(results_dir, "node_results.csv"), show_col_types = FALSE)
  druggable_results <- readr::read_csv(file.path(results_dir, "druggable_results.csv"), show_col_types = FALSE)
  drug_results <- readr::read_csv(file.path(results_dir, "drug_results.csv"), show_col_types = FALSE)
  
  # Get network nodes and drug info
  netw_variables <- get_netw_variables(here::here("examples", "combo", "helper_combo_1.json"))
  all_nodes <- c(unique(netw_variables$name), "baseline")
  
  drugs <- import_drugs_clean(here::here("examples", "combo", "helper_combo_drugs_1.csv"))
  druggable_nodes <- c(unique(drugs$node), "baseline")
  drug_names <- c(unique(drugs$drug), "baseline")
  
  # Verify node_results filtering
  expect_true(all(node_results$muta %in% all_nodes))
  expect_true(all(is.na(node_results$mutb) | node_results$mutb %in% all_nodes))
  
  # Verify druggable_results filtering
  expect_true(all(druggable_results$muta %in% druggable_nodes))
  expect_true(all(is.na(druggable_results$mutb) | druggable_results$mutb %in% druggable_nodes))
  
  # Verify drug_results filtering and structure
  # Note: drug_results may be empty if no drug names match the test data
  if (nrow(drug_results) > 0) {
    expect_true(all(drug_results$muta %in% drug_names))
    expect_true(all(is.na(drug_results$mutb) | drug_results$mutb %in% drug_names))
    expect_true(all(drug_results$leva == "" | is.na(drug_results$leva)))
    expect_true(all(drug_results$levb == "" | is.na(drug_results$levb)))
  } else {
    # If no drug matches, that's also valid behavior
    expect_true(nrow(drug_results) == 0)
  }
})

test_that("split_combo_results handles missing files", {
  expect_snapshot(
    split_combo_results(
      results_prefix = "COMBO_RUN",
      project_path = "",
      out_dir = "nonexistent_dir",
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      drug_path = NA,
      node_col_name = "node"
    ),
    error = TRUE
  )
})

test_that("split_combo_results handles invalid network file", {
  # Set up test directory with processed_results.csv
  test_dir <- file.path(temp_dir, "split_combo_test_invalid_network")
  results_dir <- file.path(test_dir, "COMBO_RUN_nonexistent")
  
  on.exit({
    if (dir.exists(test_dir)) {
      unlink(test_dir, recursive = TRUE)
    }
  })
  
  dir.create(results_dir, recursive = TRUE)
  file.copy(
    here::here("tests", "testthat", "helper_results_json", "processed_results.csv"),
    file.path(results_dir, "processed_results.csv")
  )
  
  expect_snapshot(
    split_combo_results(
      results_prefix = "COMBO_RUN",
      project_path = "",
      out_dir = test_dir,
      netw_file_path = "nonexistent_network.json",
      drug_path = NA,
      node_col_name = "node"
    ),
    error = TRUE
  )
})

test_that("split_combo_results handles invalid drug file", {
  # Set up test directory
  test_dir <- file.path(temp_dir, "split_combo_test_invalid_drugs")
  results_dir <- file.path(test_dir, "COMBO_RUN_helper_combo_1")
  
  on.exit({
    if (dir.exists(test_dir)) {
      unlink(test_dir, recursive = TRUE)
    }
  })
  
  dir.create(results_dir, recursive = TRUE)
  file.copy(
    here::here("tests", "testthat", "helper_results_json", "processed_results.csv"),
    file.path(results_dir, "processed_results.csv")
  )
  
  expect_snapshot(
    split_combo_results(
      results_prefix = "COMBO_RUN",
      project_path = "",
      out_dir = test_dir,
      netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
      drug_path = "nonexistent_drugs.csv",
      node_col_name = "node"
    ),
    error = TRUE
  )
})