source("testing_utils.r")

test_that("get_combo_results_dir creates correct directory path", {
  
  # Test basic directory path construction
  results_prefix <- "COMBO_RUN"
  project_path <- "/test/project"
  out_dir <- "output"
  netw_file_path <- "/path/to/network.json"
  
  result <- get_combo_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  # Should combine here() with project_path, out_dir, and prefix_network
  # Expected format: here(project_path, out_dir, "prefix_network")
  expected_end <- file.path("output", "COMBO_RUN_network")
  
  # Result should end with the expected path
  expect_true(grepl("COMBO_RUN_network$", result))
  expect_true(grepl("output", result))
})

test_that("get_combo_results_dir removes .json extension from network file", {
  
  results_prefix <- "TEST_RUN"
  project_path <- ""
  out_dir <- "results"
  netw_file_path <- "/path/to/my_network.json"
  
  result <- get_combo_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  # Should remove .json extension
  expect_true(grepl("TEST_RUN_my_network$", result))
  expect_false(grepl("\\.json", result))
})

test_that("get_combo_results_dir handles network files without .json extension", {
  
  results_prefix <- "RUN"
  project_path <- ""
  out_dir <- "output"
  netw_file_path <- "/path/to/network_file"  # No .json extension
  
  result <- get_combo_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  # Should work fine without .json extension
  expect_true(grepl("RUN_network_file$", result))
})

test_that("get_combo_results_dir handles different file extensions", {
  
  results_prefix <- "TEST"
  project_path <- ""
  out_dir <- "out"
  
  # Test with .json
  result_json <- get_combo_results_dir(results_prefix, "", out_dir, "network.json")
  expect_true(grepl("TEST_network$", result_json))
  
  # Test with other extension
  result_txt <- get_combo_results_dir(results_prefix, "", out_dir, "network.txt")
  expect_true(grepl("TEST_network.txt$", result_txt))
  
  # Test with no extension
  result_none <- get_combo_results_dir(results_prefix, "", out_dir, "network")
  expect_true(grepl("TEST_network$", result_none))
})

test_that("get_combo_results_dir handles complex file paths", {
  
  results_prefix <- "COMPLEX"
  project_path <- "/very/long/project/path"
  out_dir <- "deep/output/directory"
  netw_file_path <- "/some/deeply/nested/path/to/complex_network_name.json"
  
  result <- get_combo_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  # Should extract just the basename without extension
  expect_true(grepl("COMPLEX_complex_network_name$", result))
  expect_true(grepl("deep/output/directory", result))
})

test_that("get_combo_results_dir handles special characters in filenames", {
  
  results_prefix <- "TEST"
  project_path <- ""
  out_dir <- "output"
  
  # Test with underscores and dashes
  netw_file_path <- "/path/network-with_special-chars.json"
  result <- get_combo_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  expect_true(grepl("TEST_network-with_special-chars$", result))
  
  # Test with numbers
  netw_file_path2 <- "/path/network123.json"
  result2 <- get_combo_results_dir(results_prefix, project_path, out_dir, netw_file_path2)
  expect_true(grepl("TEST_network123$", result2))
})

test_that("get_combo_results_dir handles empty or minimal inputs", {
  
  # Test with minimal inputs
  result <- get_combo_results_dir("RUN", "", "out", "net.json")
  expect_true(grepl("RUN_net$", result))
  expect_true(grepl("out", result))
  
  # Test with empty prefix
  result2 <- get_combo_results_dir("", "", "out", "net.json")
  expect_true(grepl("_net$", result2))
})

test_that("get_combo_results_dir creates cross-platform compatible paths", {
  
  results_prefix <- "TEST"
  project_path <- "project"
  out_dir <- "output"
  netw_file_path <- "network.json"
  
  result <- get_combo_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  # Result should be a valid path string
  expect_type(result, "character")
  expect_true(nchar(result) > 0)
  
  # Should contain the expected components
  expect_true(grepl("project", result))
  expect_true(grepl("output", result))
  expect_true(grepl("TEST_network", result))
})

test_that("get_combo_results_dir uses here() function correctly", {
  
  results_prefix <- "HERE_TEST"
  project_path <- "test_project"
  out_dir <- "test_output"
  netw_file_path <- "test_network.json"
  
  result <- get_combo_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  # The function uses here() which should create an absolute path
  # We can't test the exact path since it depends on the current working directory
  # But we can test that it's not empty and contains our expected components
  expect_true(nchar(result) > 0)
  expect_true(grepl("test_project", result))
  expect_true(grepl("test_output", result))
  expect_true(grepl("HERE_TEST_test_network", result))
})

test_that("get_combo_results_dir handles relative project paths", {
  
  results_prefix <- "REL"
  project_path <- "./relative/path"
  out_dir <- "output"
  netw_file_path <- "network.json"
  
  result <- get_combo_results_dir(results_prefix, project_path, out_dir, netw_file_path)
  
  # Should handle relative paths
  expect_true(grepl("relative/path", result))
  expect_true(grepl("output", result))
  expect_true(grepl("REL_network", result))
})

test_that("get_combo_results_dir separates prefix and network name correctly", {
  
  # Test various prefix and network combinations
  test_cases <- list(
    list(prefix = "RUN", network = "net.json", expected = "RUN_net"),
    list(prefix = "EXPERIMENT_1", network = "my_network.json", expected = "EXPERIMENT_1_my_network"),
    list(prefix = "test", network = "complex-name_123.json", expected = "test_complex-name_123")
  )
  
  for (case in test_cases) {
    result <- get_combo_results_dir(case$prefix, "", "out", case$network)
    expect_true(grepl(case$expected, result),
                info = paste("Failed for prefix:", case$prefix, "network:", case$network))
  }
})

test_that("get_combo_results_dir returns character string", {
  
  result <- get_combo_results_dir("TEST", "", "out", "net.json")
  
  expect_type(result, "character")
  expect_length(result, 1)
  expect_false(is.na(result))
  expect_true(nchar(result) > 0)
})