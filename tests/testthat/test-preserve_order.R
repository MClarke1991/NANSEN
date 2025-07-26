test_that("preserve_order converts column to factor with correct levels", {
  # Create test dataframe with string column
  test_df <- tibble::tibble(
    category = c("gamma", "alpha", "beta", "gamma", "alpha"),
    value = c(10, 20, 30, 40, 50)
  )
  
  result <- preserve_order(test_df, "category")
  
  # Check that category is now a factor
  expect_true(is.factor(result$category))
  
  # Check that levels are in order of first appearance
  expected_levels <- c("gamma", "alpha", "beta")
  expect_equal(levels(result$category), expected_levels)
  
  # Check that values are preserved
  expect_equal(as.character(result$category), c("gamma", "alpha", "beta", "gamma", "alpha"))
  expect_equal(result$value, c(10, 20, 30, 40, 50))
})

test_that("preserve_order works with numeric columns", {
  test_df <- tibble::tibble(
    numbers = c(3.5, 1.2, 2.8, 1.2, 3.5),
    labels = c("a", "b", "c", "d", "e")
  )
  
  result <- preserve_order(test_df, "numbers")
  
  expect_true(is.factor(result$numbers))
  expect_equal(levels(result$numbers), c("3.5", "1.2", "2.8"))
  expect_equal(result$labels, c("a", "b", "c", "d", "e"))
})

test_that("preserve_order works with integer columns", {
  test_df <- tibble::tibble(
    ints = c(30L, 10L, 20L, 10L, 30L),
    data = c("x", "y", "z", "w", "v")
  )
  
  result <- preserve_order(test_df, "ints")
  
  expect_true(is.factor(result$ints))
  expect_equal(levels(result$ints), c("30", "10", "20"))
  expect_equal(result$data, c("x", "y", "z", "w", "v"))
})

test_that("preserve_order handles duplicate values correctly", {
  test_df <- tibble::tibble(
    duplicated = c("x", "x", "y", "z", "y", "x"),
    id = 1:6
  )
  
  result <- preserve_order(test_df, "duplicated")
  
  expect_true(is.factor(result$duplicated))
  expect_equal(levels(result$duplicated), c("x", "y", "z"))
  expect_equal(as.character(result$duplicated), c("x", "x", "y", "z", "y", "x"))
})

test_that("preserve_order handles NA values correctly", {
  test_df <- tibble::tibble(
    with_na = c("a", NA, "b", "a", NA, "c"),
    index = 1:6
  )
  
  result <- preserve_order(test_df, "with_na")
  
  expect_true(is.factor(result$with_na))
  # NA should be included in levels
  expect_equal(levels(result$with_na), c("a", "b", "c"))
  expect_equal(sum(is.na(result$with_na)), 2)
})

test_that("preserve_order handles column with all NA values", {
  test_df <- tibble::tibble(
    all_na = c(NA, NA, NA),
    other = c(1, 2, 3)
  )
  
  result <- preserve_order(test_df, "all_na")
  
  expect_true(is.factor(result$all_na))
  expect_equal(length(levels(result$all_na)), 0)  # No levels for all NA
  expect_equal(sum(is.na(result$all_na)), 3)
})

test_that("preserve_order works with single row dataframe", {
  test_df <- tibble::tibble(
    single = "only_value",
    other = 42
  )
  
  result <- preserve_order(test_df, "single")
  
  expect_true(is.factor(result$single))
  expect_equal(levels(result$single), "only_value")
  expect_equal(result$other, 42)
})

test_that("preserve_order handles empty dataframe", {
  test_df <- tibble::tibble(
    empty_col = character(0)
  )
  
  result <- preserve_order(test_df, "empty_col")
  
  expect_true(is.factor(result$empty_col))
  expect_equal(length(levels(result$empty_col)), 0)
  expect_equal(length(result$empty_col), 0)
})

test_that("preserve_order handles already factorized column", {
  test_df <- tibble::tibble(
    already_factor = factor(c("z", "a", "m"), levels = c("a", "m", "z")),
    other = c(1, 2, 3)
  )
  
  result <- preserve_order(test_df, "already_factor")
  
  expect_true(is.factor(result$already_factor))
  # Should reorder levels based on current appearance order
  expect_equal(levels(result$already_factor), c("z", "a", "m"))
  expect_equal(result$other, c(1, 2, 3))
})

test_that("preserve_order preserves other columns unchanged", {
  test_df <- tibble::tibble(
    to_factor = c("c", "a", "b"),
    character_col = c("x", "y", "z"),
    numeric_col = c(1.1, 2.2, 3.3),
    integer_col = c(10L, 20L, 30L),
    logical_col = c(TRUE, FALSE, TRUE),
    date_col = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03"))
  )
  
  result <- preserve_order(test_df, "to_factor")
  
  # Only to_factor should be changed to factor
  expect_true(is.factor(result$to_factor))
  expect_true(is.character(result$character_col))
  expect_true(is.numeric(result$numeric_col))
  expect_true(is.integer(result$integer_col))
  expect_true(is.logical(result$logical_col))
  expect_true(inherits(result$date_col, "Date"))
  
  # Values should be unchanged
  expect_equal(result$character_col, c("x", "y", "z"))
  expect_equal(result$numeric_col, c(1.1, 2.2, 3.3))
  expect_equal(result$integer_col, c(10L, 20L, 30L))
  expect_equal(result$logical_col, c(TRUE, FALSE, TRUE))
})

test_that("preserve_order throws error for non-existent column", {
  test_df <- tibble::tibble(
    existing_col = c("a", "b", "c")
  )
  
  expect_error(
    preserve_order(test_df, "non_existent_col")
  )
})

test_that("preserve_order handles NULL dataframe gracefully", {
  # The function doesn't validate inputs, so it processes NULL as expected
  result <- preserve_order(NULL, "col")
  expect_true(is.list(result))
  expect_true(is.factor(result$col))
  expect_equal(length(result$col), 0)
})

test_that("preserve_order throws error for NULL column name", {
  test_df <- tibble::tibble(
    col1 = c("a", "b", "c")
  )
  
  expect_error(
    preserve_order(test_df, NULL)
  )
})

test_that("preserve_order works with logical columns", {
  test_df <- tibble::tibble(
    logical_col = c(TRUE, FALSE, TRUE, FALSE, TRUE),
    index = 1:5
  )
  
  result <- preserve_order(test_df, "logical_col")
  
  expect_true(is.factor(result$logical_col))
  expect_equal(levels(result$logical_col), c("TRUE", "FALSE"))
  expect_equal(result$index, 1:5)
})

test_that("preserve_order preserves dataframe class and attributes", {
  test_df <- tibble::tibble(
    test_col = c("x", "y", "z"),
    other = c(1, 2, 3)
  )
  
  result <- preserve_order(test_df, "test_col")
  
  # Should still be a tibble
  expect_true(inherits(result, "tbl_df"))
  expect_true(inherits(result, "data.frame"))
  
  # Should have same number of rows and columns
  expect_equal(nrow(result), nrow(test_df))
  expect_equal(ncol(result), ncol(test_df))
  expect_equal(colnames(result), colnames(test_df))
})

test_that("preserve_order maintains order for ggplot compatibility", {
  # Test case that simulates common ggplot use case
  test_df <- tibble::tibble(
    treatment = c("Control", "High", "Medium", "Low", "Medium", "High"),
    response = c(10, 50, 30, 20, 35, 45)
  )
  
  result <- preserve_order(test_df, "treatment")
  
  expect_true(is.factor(result$treatment))
  # Levels should be in order of first appearance: Control, High, Medium, Low
  expect_equal(levels(result$treatment), c("Control", "High", "Medium", "Low"))
  
  # This would preserve the intended order in ggplot
  # (as opposed to alphabetical: Control, High, Low, Medium)
})