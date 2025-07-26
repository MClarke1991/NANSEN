source("testing_utils.r")

test_that("check_drugs_in_range passes when all activities are in range", {
  
  drugs_commands <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c"),
    node = c("a", "b", "c"),
    activity = c(0, 1, 2),
    range_from = c(0, 0, 0),
    range_to = c(2, 3, 2),
    command_arg = c("-ko 1 0", "-ko 2 1", "-ko 3 2")
  )
  
  expect_message(
    check_drugs_in_range(drugs_commands),
    "CHECK PASSED: All drug perturbations are within node ranges"
  )
})

test_that("check_drugs_in_range passes when activities are at range boundaries", {
  
  # Test activities exactly at range_from and range_to
  drugs_commands <- tibble::tibble(
    drug = c("drug_min", "drug_max"),
    node = c("a", "b"),
    activity = c(0, 5),     # Exactly at boundaries
    range_from = c(0, 1),
    range_to = c(3, 5),
    command_arg = c("-ko 1 0", "-ko 2 5")
  )
  
  expect_message(
    check_drugs_in_range(drugs_commands),
    "CHECK PASSED: All drug perturbations are within node ranges"
  )
})

test_that("check_drugs_in_range errors when activity below range_from", {
  
  drugs_commands <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("a", "b"),
    activity = c(-1, 1),    # -1 is below range_from of 0
    range_from = c(0, 0),
    range_to = c(2, 2),
    command_arg = c("-ko 1 -1", "-ko 2 1")
  )
  
  expect_snapshot(
    check_drugs_in_range(drugs_commands),
    error = TRUE
  )
})

test_that("check_drugs_in_range errors when activity above range_to", {
  
  drugs_commands <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("a", "b"),
    activity = c(1, 5),     # 5 is above range_to of 2
    range_from = c(0, 0),
    range_to = c(2, 2),
    command_arg = c("-ko 1 1", "-ko 2 5")
  )
  
  expect_snapshot(
    check_drugs_in_range(drugs_commands),
    error = TRUE
  )
})

test_that("check_drugs_in_range errors with multiple out-of-range activities", {
  
  drugs_commands <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c"),
    node = c("a", "b", "c"),
    activity = c(-1, 1, 10),  # -1 too low, 10 too high
    range_from = c(0, 0, 0),
    range_to = c(2, 2, 5),
    command_arg = c("-ko 1 -1", "-ko 2 1", "-ko 3 10")
  )
  
  expect_snapshot(
    check_drugs_in_range(drugs_commands),
    error = TRUE
  )
})

test_that("check_drugs_in_range handles empty drugs_commands", {
  
  drugs_commands <- tibble::tibble(
    drug = character(),
    node = character(),
    activity = numeric(),
    range_from = numeric(),
    range_to = numeric(),
    command_arg = character()
  )
  
  expect_message(
    check_drugs_in_range(drugs_commands),
    "CHECK PASSED: All drug perturbations are within node ranges"
  )
})

test_that("check_drugs_in_range handles different range values correctly", {
  
  # Test with non-zero lower bounds and different ranges
  drugs_commands <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c"),
    node = c("a", "b", "c"),
    activity = c(2, 5, 10),
    range_from = c(1, 3, 8),    # Non-zero lower bounds
    range_to = c(5, 7, 12),     # Different upper bounds
    command_arg = c("-ko 1 2", "-ko 2 5", "-ko 3 10")
  )
  
  expect_message(
    check_drugs_in_range(drugs_commands),
    "CHECK PASSED: All drug perturbations are within node ranges"
  )
})

test_that("check_drugs_in_range handles decimal activity values", {
  
  # Test with decimal values
  drugs_commands <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("a", "b"),
    activity = c(1.5, 2.7),
    range_from = c(0, 2),
    range_to = c(2, 3),
    command_arg = c("-ko 1 1.5", "-ko 2 2.7")
  )
  
  expect_message(
    check_drugs_in_range(drugs_commands),
    "CHECK PASSED: All drug perturbations are within node ranges"
  )
  
  # Test decimal value out of range
  drugs_commands_bad <- tibble::tibble(
    drug = c("drug_a"),
    node = c("a"),
    activity = c(2.1),    # Just above range_to of 2
    range_from = c(0),
    range_to = c(2),
    command_arg = c("-ko 1 2.1")
  )
  
  expect_snapshot(
    check_drugs_in_range(drugs_commands_bad),
    error = TRUE
  )
})

test_that("check_drugs_in_range error includes problematic rows", {
  
  drugs_commands <- tibble::tibble(
    drug = c("problem_drug"),
    node = c("problem_node"),
    activity = c(10),
    range_from = c(0),
    range_to = c(5),
    command_arg = c("-ko 1 10"),
    extra_info = c("extra")
  )
  
  # Capture error to check it includes the problematic row information
  error_output <- tryCatch({
    check_drugs_in_range(drugs_commands)
  }, error = function(e) e$message)
  
  expect_true(grepl("problem_drug", error_output))
  expect_true(grepl("problem_node", error_output))
  expect_true(grepl("10", error_output))
})

test_that("check_drugs_in_range preserves error call behavior", {
  
  drugs_commands <- tibble::tibble(
    drug = c("bad_drug"),
    node = c("a"),
    activity = c(10),
    range_from = c(0),
    range_to = c(2),
    command_arg = c("-ko 1 10")
  )
  
  # The function uses call. = FALSE, so error should not show the function call
  # Test that error doesn't contain function name (due to call. = FALSE)
  error_result <- tryCatch(
    check_drugs_in_range(drugs_commands),
    error = function(e) e$message
  )
  expect_false(grepl("check_drugs_in_range", error_result, fixed = TRUE))
})

test_that("check_drugs_in_range handles mixed valid and invalid activities", {
  
  drugs_commands <- tibble::tibble(
    drug = c("good_drug", "bad_drug_low", "bad_drug_high"),
    node = c("a", "b", "c"),
    activity = c(1, -1, 10),     # One good, two bad
    range_from = c(0, 0, 0),
    range_to = c(2, 2, 5),
    command_arg = c("-ko 1 1", "-ko 2 -1", "-ko 3 10")
  )
  
  expect_snapshot(
    check_drugs_in_range(drugs_commands),
    error = TRUE
  )
})

test_that("check_drugs_in_range works with identical ranges", {
  
  # Test when range_from equals range_to (zero granularity)
  drugs_commands <- tibble::tibble(
    drug = c("fixed_drug"),
    node = c("fixed_node"),
    activity = c(1),
    range_from = c(1),
    range_to = c(1),      # Same as range_from
    command_arg = c("-ko 1 1")
  )
  
  expect_message(
    check_drugs_in_range(drugs_commands),
    "CHECK PASSED: All drug perturbations are within node ranges"
  )
  
  # Activity outside this fixed range should error
  drugs_commands_bad <- tibble::tibble(
    drug = c("fixed_drug"),
    node = c("fixed_node"),
    activity = c(2),      # Different from fixed value
    range_from = c(1),
    range_to = c(1),
    command_arg = c("-ko 1 2")
  )
  
  expect_snapshot(
    check_drugs_in_range(drugs_commands_bad),
    error = TRUE
  )
})