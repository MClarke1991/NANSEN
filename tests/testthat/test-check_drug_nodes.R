source("testing_utils.r")

test_that("check_drug_nodes passes when all drug nodes exist in network", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c"),
    node = c("a", "b", "a"),  # drug_c also affects node a
    activity = c(0, 1, 2)
  )
  
  netw_variables <- tibble::tibble(
    node = c("a", "b", "c", "d"),
    id = c(1, 2, 3, 4),
    range_from = c(0, 0, 0, 0),
    range_to = c(2, 2, 2, 2)
  )
  
  # Should not error when all nodes exist
  expect_no_error(
    check_drug_nodes(drugs, netw_variables, "node")
  )
})

test_that("check_drug_nodes errors when drug nodes missing from network", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("existing_node", "missing_node"),
    activity = c(0, 1)
  )
  
  netw_variables <- tibble::tibble(
    node = c("existing_node", "other_node"),
    id = c(1, 2),
    range_from = c(0, 0),
    range_to = c(2, 2)
  )
  
  expect_snapshot(
    check_drug_nodes(drugs, netw_variables, "node"),
    error = TRUE
  )
})

test_that("check_drug_nodes errors with multiple missing nodes", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c"),
    node = c("missing1", "existing", "missing2"),
    activity = c(0, 1, 2)
  )
  
  netw_variables <- tibble::tibble(
    node = c("existing", "other"),
    id = c(1, 2),
    range_from = c(0, 0),
    range_to = c(2, 2)
  )
  
  expect_snapshot(
    check_drug_nodes(drugs, netw_variables, "node"),
    error = TRUE
  )
})

test_that("check_drug_nodes works with custom node column names", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a"),
    gene = c("test_gene"),
    activity = c(1)
  )
  
  netw_variables <- tibble::tibble(
    gene = c("test_gene", "other_gene"),
    id = c(1, 2),
    range_from = c(0, 0),
    range_to = c(2, 2)
  )
  
  # Should work with custom column name
  expect_no_error(
    check_drug_nodes(drugs, netw_variables, "gene")
  )
  
  # Should error if using wrong column name
  expect_error(
    check_drug_nodes(drugs, netw_variables, "node")
  )
})

test_that("check_drug_nodes handles empty drugs data frame", {
  
  drugs <- tibble::tibble(
    drug = character(),
    node = character(),
    activity = numeric()
  )
  
  netw_variables <- tibble::tibble(
    node = c("a", "b"),
    id = c(1, 2),
    range_from = c(0, 0),
    range_to = c(2, 2)
  )
  
  # Should not error with empty drugs
  expect_no_error(
    check_drug_nodes(drugs, netw_variables, "node")
  )
})

test_that("check_drug_nodes handles empty network variables", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a"),
    node = c("a"),
    activity = c(1)
  )
  
  netw_variables <- tibble::tibble(
    node = character(),
    id = integer(),
    range_from = numeric(),
    range_to = numeric()
  )
  
  # Should error since no nodes exist in network
  expect_snapshot(
    check_drug_nodes(drugs, netw_variables, "node"),
    error = TRUE
  )
})

test_that("check_drug_nodes handles duplicate node names correctly", {
  
  # Same drug affects same node multiple times (shouldn't matter for this check)
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_a", "drug_b"),
    node = c("a", "a", "b"),
    activity = c(0, 1, 2)
  )
  
  netw_variables <- tibble::tibble(
    node = c("a", "b", "c"),
    id = c(1, 2, 3),
    range_from = c(0, 0, 0),
    range_to = c(2, 2, 2)
  )
  
  # Should pass - all drug nodes exist in network
  expect_no_error(
    check_drug_nodes(drugs, netw_variables, "node")
  )
})

test_that("check_drug_nodes error message includes missing node names", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("missing_alpha", "missing_beta"),
    activity = c(0, 1)
  )
  
  netw_variables <- tibble::tibble(
    node = c("existing"),
    id = c(1),
    range_from = c(0),
    range_to = c(2)
  )
  
  # Capture the error to check it contains the missing node names
  error_output <- tryCatch({
    check_drug_nodes(drugs, netw_variables, "node")
  }, error = function(e) e$message)
  
  expect_true(grepl("missing_alpha", error_output))
  expect_true(grepl("missing_beta", error_output))
})

test_that("check_drug_nodes handles case sensitivity", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a"),
    node = c("NodeA"),  # Different case
    activity = c(1)
  )
  
  netw_variables <- tibble::tibble(
    node = c("nodea"),  # Lower case
    id = c(1),
    range_from = c(0),
    range_to = c(2)
  )
  
  # Should treat as different (case sensitive)
  expect_snapshot(
    check_drug_nodes(drugs, netw_variables, "node"),
    error = TRUE
  )
})

test_that("check_drug_nodes uses unique values for comparison", {
  
  # Test that function properly uses unique() for comparison
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_a", "drug_a"),
    node = c("a", "a", "a"),  # Same node repeated
    activity = c(0, 1, 2)
  )
  
  netw_variables <- tibble::tibble(
    node = c("a", "a", "b"),  # Network also has duplicates
    id = c(1, 1, 2),
    range_from = c(0, 0, 0),
    range_to = c(2, 2, 2)
  )
  
  # Should work fine - function should use unique values
  expect_no_error(
    check_drug_nodes(drugs, netw_variables, "node")
  )
})