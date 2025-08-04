source("testing_utils.r")

test_that("process_results works with real parsed data", {
  setup_log_file()

  # Read the example parsed results
  parsed_results <- readr::read_csv(
    here::here("tests", "testthat", "helper_results_json", "parsed_results.csv"),
    show_col_types = FALSE, col_types = readr::cols(formula = "c")
  )

  phenotypes <- c("growth_factor", "output_a")

  result <- process_results(
    parsed_results = parsed_results,
    phenotypes = phenotypes,
    log_file = log_file,
    node_col = "node",
    pheno_only = FALSE
  )

  expect_s3_class(result, "tbl_df")
  expect_true(all(c("case",
                    "background",
                    "bkg_pert",
                    "muta",
                    "leva",
                    "mutb",
                    "levb",
                    "time",
                    "id",
                    "lo",
                    "hi",
                    "range_from",
                    "range_to",
                    "formula",
                    "mean") %in% names(result)))
  expect_true(nrow(result) > 0)

  # Check that mean is calculated correctly
  expect_true(all(!is.na(result$mean)))
  expect_true(all(result$mean == (result$hi + result$lo) / 2))

  # Check that uncertainty is calculated correctly
  expect_true(all(!is.na(result$uncertainty)))
  expected_uncertainty <- (result$hi - result$lo) / (result$range_to - result$range_from)
  expect_equal(result$uncertainty, expected_uncertainty)

  cleanup_log_file()
})

test_that("process_results handles pheno_only filtering", {
  setup_log_file()

  parsed_results <- readr::read_csv(
    here::here("tests", "testthat", "helper_results_json", "parsed_results.csv"),
    show_col_types = FALSE, col_types = readr::cols(formula = "c"),
  )

  phenotypes <- c("growth_factor")

  # Test with pheno_only = TRUE
  result_filtered <- process_results(
    parsed_results = parsed_results,
    phenotypes = phenotypes,
    log_file = log_file,
    node_col = "node",
    pheno_only = TRUE
  )

  # Should only contain growth_factor nodes
  expect_true(all(result_filtered$node %in% phenotypes))

  # Test with pheno_only = FALSE
  result_all <- process_results(
    parsed_results = parsed_results,
    phenotypes = phenotypes,
    log_file = log_file,
    node_col = "node",
    pheno_only = FALSE
  )

  # Should contain all nodes
  expect_true(nrow(result_all) >= nrow(result_filtered))
  expect_true(any(!result_all$node %in% phenotypes))

  cleanup_log_file()
})

test_that("process_results parses filename structure correctly", {
  setup_log_file()

  # Create test data with known filename structure
  test_data <- data.frame(
    filename = c(
      "RAW__single__background1/a__1__PERT__gene1__2.json",
      "RAW__double__background2/b__0__c__1__PERT__gene2__0__gene3__1.json"
    ),
    time = c(5, 5),
    id = c(1, 2),
    lo = c(0, 1),
    hi = c(1, 2),
    node = c("test1", "test2"),
    range_from = c(0, 0),
    range_to = c(2, 2),
    formula = c("", ""),
    stringsAsFactors = FALSE
  )

  result <- process_results(
    parsed_results = test_data,
    phenotypes = c("test1", "test2"),
    log_file = log_file,
    node_col = "node",
    pheno_only = FALSE
  )

  # Check case parsing
  expect_true("single" %in% result$case)
  expect_true("double" %in% result$case)

  # Check background parsing
  expect_true("background1" %in% result$background)
  expect_true("background2" %in% result$background)

  # Check perturbation parsing for single case
  single_row <- result[result$case == "single", ]
  expect_equal(single_row$muta, "gene1")
  expect_equal(single_row$leva, "2")
  expect_true(is.na(single_row$mutb))
  expect_true(is.na(single_row$levb))

  # Check perturbation parsing for double case
  double_row <- result[result$case == "double", ]
  expect_equal(double_row$muta, "gene2")
  expect_equal(double_row$leva, "0")
  expect_equal(double_row$mutb, "gene3")
  expect_equal(double_row$levb, "1")

  cleanup_log_file()
})

test_that("process_results calculates mean and uncertainty correctly", {
  setup_log_file()

  test_data <- data.frame(
    filename = c("RAW__single__bg/test__PERT__a__1.json"),
    time = c(5),
    id = c(1),
    lo = c(2),
    hi = c(8),
    node = c("test_node"),
    range_from = c(0),
    range_to = c(10),
    formula = c(""),
    stringsAsFactors = FALSE
  )

  result <- process_results(
    parsed_results = test_data,
    phenotypes = c("test_node"),
    log_file = log_file,
    node_col = "node"
  )

  # Mean should be (lo + hi) / 2 = (2 + 8) / 2 = 5
  expect_equal(result$mean, 5)

  # Uncertainty should be (hi - lo) / (range_to - range_from) = (8 - 2) / (10 - 0) = 0.6
  expect_equal(result$uncertainty, 0.6)

  cleanup_log_file()
})

test_that("process_results handles edge cases in filename parsing", {
  setup_log_file()

  # Test baseline case
  test_data <- data.frame(
    filename = c("RAW__single__bg/test__PERT__baseline.json"),
    time = c(5),
    id = c(1),
    lo = c(1),
    hi = c(1),
    node = c("test"),
    range_from = c(0),
    range_to = c(2),
    formula = c(""),
    stringsAsFactors = FALSE
  )

  result <- process_results(
    parsed_results = test_data,
    phenotypes = c("test"),
    log_file = log_file,
    node_col = "node"
  )

  expect_equal(result$muta, "baseline")
  expect_true(is.na(result$leva))
  expect_true(is.na(result$mutb))
  expect_true(is.na(result$levb))

  cleanup_log_file()
})

test_that("process_results works with different node_col", {
  setup_log_file()

  test_data <- data.frame(
    filename = c("RAW__single__bg/test__PERT__a__1.json"),
    time = c(5),
    id = c(1),
    lo = c(1),
    hi = c(2),
    gene_name = c("custom_gene"),  # Different column name
    range_from = c(0),
    range_to = c(2),
    formula = c(""),
    stringsAsFactors = FALSE
  )

  result <- process_results(
    parsed_results = test_data,
    phenotypes = c("custom_gene"),
    log_file = log_file,
    node_col = "gene_name"
  )

  expect_true("gene_name" %in% names(result))
  expect_equal(result$gene_name, "custom_gene")

  cleanup_log_file()
})

test_that("process_results preserves all original columns", {
  setup_log_file()

  test_data <- data.frame(
    filename = c("RAW__single__bg/test__PERT__a__1.json"),
    time = c(5),
    id = c(1),
    lo = c(1),
    hi = c(2),
    node = c("test"),
    range_from = c(0),
    range_to = c(2),
    formula = c("test_formula"),
    extra_column = c("extra_value"),
    stringsAsFactors = FALSE
  )

  result <- process_results(
    parsed_results = test_data,
    phenotypes = c("test"),
    log_file = log_file,
    node_col = "node"
  )

  # Should preserve original columns
  expect_true("extra_column" %in% names(result))
  expect_equal(result$extra_column, "extra_value")
  expect_true("formula" %in% names(result))
  expect_equal(result$formula, "test_formula")

  cleanup_log_file()
})

test_that("process_results handles zero range scenarios", {
  setup_log_file()

  # Test case where range_to == range_from (zero range)
  test_data <- data.frame(
    filename = c("RAW__single__bg/test__PERT__a__1.json"),
    time = c(5),
    id = c(1),
    lo = c(1),
    hi = c(1),
    node = c("test"),
    range_from = c(1),
    range_to = c(1),  # Same as range_from
    formula = c(""),
    stringsAsFactors = FALSE
  )

  result <- process_results(
    parsed_results = test_data,
    phenotypes = c("test"),
    log_file = log_file,
    node_col = "node"
  )

  expect_equal(result$mean, 1)
  # Uncertainty should be 0/0 = NaN or Inf
  expect_true(is.na(result$uncertainty) || is.infinite(result$uncertainty))

  cleanup_log_file()
})