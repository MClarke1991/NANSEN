test_that("get_ap_results_dir constructs correct path with valid inputs", {
  results_prefix <- "test_results"
  project_path <- "test_project"
  out_dir <- "output"
  netw_file_path <- "network_file.json"
  
  result <- get_ap_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  expect_type(result, "character")
  expect_true(grepl("test_results_network_file", result))
  expect_true(grepl("test_project", result))
  expect_true(grepl("output", result))
})

test_that("get_ap_results_dir removes .json extension from network file", {
  results_prefix <- "prefix"
  project_path <- "project"
  out_dir <- "out"
  netw_file_path <- "path/to/my_network.json"
  
  result <- get_ap_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  expect_true(grepl("prefix_my_network", result))
  expect_false(grepl("\\.json", result))
})

test_that("get_ap_results_dir handles network file without .json extension", {
  results_prefix <- "prefix"
  project_path <- "project"
  out_dir <- "out"
  netw_file_path <- "path/to/my_network"
  
  result <- get_ap_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  expect_true(grepl("prefix_my_network", result))
})

test_that("get_ap_results_dir handles complex file names", {
  results_prefix <- "test"
  project_path <- "proj"
  out_dir <- "output"
  netw_file_path <- "path/to/complex_network_name-v2.json"
  
  result <- get_ap_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  expect_true(grepl("test_complex_network_name-v2", result))
})

test_that("get_ap_results_dir extracts basename from nested path", {
  results_prefix <- "analysis"
  project_path <- "my_project"
  out_dir <- "results"
  netw_file_path <- "deep/nested/path/to/final_network.json"
  
  result <- get_ap_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  expect_true(grepl("analysis_final_network", result))
  expect_false(grepl("deep/nested/path", result))
})

test_that("get_ap_results_dir handles empty prefix", {
  results_prefix <- ""
  project_path <- "project"
  out_dir <- "out"
  netw_file_path <- "network.json"
  
  result <- get_ap_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  expect_true(grepl("_network", result))
  expect_type(result, "character")
})

test_that("get_ap_results_dir handles special characters in paths", {
  results_prefix <- "test-prefix"
  project_path <- "project_path"
  out_dir <- "out-dir"
  netw_file_path <- "path/network_file-v1.json"
  
  result <- get_ap_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  expect_true(grepl("test-prefix_network_file-v1", result))
  expect_type(result, "character")
})

test_that("get_ap_results_dir uses here::here for path construction", {
  results_prefix <- "test"
  project_path <- "project"
  out_dir <- "output"
  netw_file_path <- "network.json"
  
  result <- get_ap_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  expected_end <- file.path("project", "output", "test_network")
  expect_true(grepl(gsub("\\\\", "/", expected_end), gsub("\\\\", "/", result)))
})