test_that("get_combo_results_dir works with normal project path", {
  result <- get_combo_results_dir(
    results_prefix = "TEST_RUN",
    project_path = "my_project", 
    out_dir = "output",
    netw_file_path = "network.json"
  )
  
  expect_true(stringr::str_detect(result, "TEST_RUN_network"))
  expect_true(stringr::str_detect(result, "my_project"))
  expect_true(stringr::str_detect(result, "output"))
})

test_that("get_combo_results_dir works with empty project path", {
  result <- get_combo_results_dir(
    results_prefix = "TEST_RUN",
    project_path = "",
    out_dir = "output", 
    netw_file_path = "network.json"
  )
  
  expect_true(stringr::str_detect(result, "TEST_RUN_network"))
  expect_true(stringr::str_detect(result, "output"))
  expect_false(stringr::str_detect(result, "my_project"))
})

test_that("get_combo_results_dir works with NULL project path", {
  result <- get_combo_results_dir(
    results_prefix = "TEST_RUN", 
    project_path = NULL,
    out_dir = "output",
    netw_file_path = "network.json"
  )
  
  expect_true(stringr::str_detect(result, "TEST_RUN_network"))
  expect_true(stringr::str_detect(result, "output"))
})

test_that("get_combo_results_dir works with Windows temp directory", {
  skip_if_not(Sys.info()[["sysname"]] == "Windows", "Windows-specific test")
  
  # Simulate Windows temp directory path
  temp_dir <- "C:\\Users\\RUNNER~1\\AppData\\Local\\Temp\\RtmpAbc123\\combo_test"
  
  result <- get_combo_results_dir(
    results_prefix = "COMBO_RUN",
    project_path = "",
    out_dir = temp_dir,
    netw_file_path = "helper_combo_1.json"
  )
  
  expect_true(stringr::str_detect(result, "COMBO_RUN_helper_combo_1"))
  expect_true(stringr::str_detect(result, "combo_test"))
  # Should not have double C:\ in the path
  expect_false(stringr::str_detect(result, "C:.*C:"))
})

test_that("get_combo_results_dir removes .json extension properly", {
  result <- get_combo_results_dir(
    results_prefix = "TEST",
    project_path = "",
    out_dir = "output",
    netw_file_path = "/path/to/complex_network_name.json"
  )
  
  expect_true(stringr::str_detect(result, "TEST_complex_network_name"))
  expect_false(stringr::str_detect(result, "\\.json"))
})

test_that("get_combo_results_dir handles nested directory paths", {
  result <- get_combo_results_dir(
    results_prefix = "COMBO_RUN",
    project_path = "/home/user/projects/myproject",
    out_dir = "results/combo_output",
    netw_file_path = "networks/test.json"
  )
  
  expect_true(stringr::str_detect(result, "COMBO_RUN_test"))
  expect_true(stringr::str_detect(result, "myproject"))
  expect_true(stringr::str_detect(result, "combo_output"))
})