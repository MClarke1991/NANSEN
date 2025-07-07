source("testing_utils.r")

test_that("convert_spec_levels converts min/mid/max to numeric values", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB", "geneC"),
    perturbation = c("min", "mid", "max"),
    expected_result_bma = c("min", "mid", "max"),
    range_from = c(0, 1, 2),
    range_to = c(10, 11, 12)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(0, 6, 12))
  expect_equal(result$expectation_bma, c(0, 6, 12))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles numeric strings", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB"),
    perturbation = c("5", "7"),
    expected_result_bma = c("3", "9"),
    range_from = c(0, 1),
    range_to = c(10, 11)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(5, 7))
  expect_equal(result$expectation_bma, c(3, 9))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles NA values", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB", "geneC"),
    perturbation = c("min", NA, "max"),
    expected_result_bma = c(NA, "mid", "max"),
    range_from = c(0, 1, 2),
    range_to = c(10, 11, 12)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(0, NA, 12))
  expect_equal(result$expectation_bma, c(NA, 6, 12))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles mixed relative and numeric values", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB", "geneC", "geneD"),
    perturbation = c("min", "5", "mid", "max"),
    expected_result_bma = c("3", "mid", "max", "min"),
    range_from = c(0, 1, 2, 3),
    range_to = c(10, 11, 12, 13)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(0, 5, 7, 13))
  expect_equal(result$expectation_bma, c(3, 6, 12, 3))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles zero-width ranges", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB"),
    perturbation = c("min", "mid"),
    expected_result_bma = c("max", "mid"),
    range_from = c(5, 5),
    range_to = c(5, 5)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(5, 5))
  expect_equal(result$expectation_bma, c(5, 5))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles negative ranges", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB"),
    perturbation = c("min", "max"),
    expected_result_bma = c("mid", "min"),
    range_from = c(-10, -5),
    range_to = c(-1, 5)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(-10, 5))
  expect_equal(result$expectation_bma, c(-5.5, -5))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels uses direct assignment for numeric columns", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB"),
    perturbation = c(5, 7),
    expected_result_bma = c(3, 9),
    range_from = c(0, 1),
    range_to = c(10, 11)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(5, 7))
  expect_equal(result$expectation_bma, c(3, 9))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles mixed column types", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB"),
    perturbation = c("min", "max"),
    expected_result_bma = c(3, 9),
    range_from = c(0, 1),
    range_to = c(10, 11)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c("min", "max"))
  expect_equal(result$expectation_bma, c(3, 9))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles empty dataframe", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = character(0),
    perturbation = character(0),
    expected_result_bma = character(0),
    range_from = numeric(0),
    range_to = numeric(0)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(nrow(result), 0)
  expect_true("perturbation_bma" %in% names(result))
  expect_true("expectation_bma" %in% names(result))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles single row dataframe", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = "geneA",
    perturbation = "mid",
    expected_result_bma = "max",
    range_from = 0,
    range_to = 10
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, 5)
  expect_equal(result$expectation_bma, 10)
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles large ranges", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB"),
    perturbation = c("min", "mid"),
    expected_result_bma = c("max", "mid"),
    range_from = c(0, 1000),
    range_to = c(1000000, 2000000)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(0, 1000500))
  expect_equal(result$expectation_bma, c(1000000, 1000500))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles decimal ranges", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB"),
    perturbation = c("min", "mid"),
    expected_result_bma = c("max", "mid"),
    range_from = c(0.5, 1.5),
    range_to = c(10.5, 11.5)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(0.5, 6.5))
  expect_equal(result$expectation_bma, c(10.5, 6.5))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels preserves original columns", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB"),
    source = c("source1", "source2"),
    perturbation = c("min", "max"),
    expected_result_bma = c("min", "max"),
    range_from = c(0, 1),
    range_to = c(10, 11),
    experiment_particular = c("exp1", "exp2")
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_true(all(c("gene", "source", "perturbation", "expected_result_bma", 
                   "range_from", "range_to", "experiment_particular") %in% names(result)))
  expect_equal(result$gene, c("geneA", "geneB"))
  expect_equal(result$source, c("source1", "source2"))
  expect_equal(result$experiment_particular, c("exp1", "exp2"))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles case sensitivity", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB", "geneC"),
    perturbation = c("MIN", "Mid", "MAX"),
    expected_result_bma = c("min", "MID", "max"),
    range_from = c(0, 1, 2),
    range_to = c(10, 11, 12)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(0, 6, 12))
  expect_equal(result$expectation_bma, c(0, 6, 12))
  expect_snapshot()
  
  cleanup_log_file()
})

test_that("convert_spec_levels handles partial matches", {
  setup_log_file()
  
  spec <- tibble::tibble(
    gene = c("geneA", "geneB", "geneC"),
    perturbation = c("minimum", "midrange", "maximum"),
    expected_result_bma = c("min_val", "mid_val", "max_val"),
    range_from = c(0, 1, 2),
    range_to = c(10, 11, 12)
  )
  
  result <- suppressMessages(suppressWarnings(convert_spec_levels(spec, log_file)))
  
  expect_equal(result$perturbation_bma, c(0, 6, 12))
  expect_equal(result$expectation_bma, c(0, 6, 12))
  expect_snapshot()
  
  cleanup_log_file()
})