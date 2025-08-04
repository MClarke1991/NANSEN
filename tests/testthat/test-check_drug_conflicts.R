source("testing_utils.r")

test_that("check_drug_conflicts passes when no conflicts exist", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c"),
    node = c("a", "b", "c"),
    activity = c(0, 1, 2)
  )
  
  result <- check_drug_conflicts(drugs, "node")
  expect_equal(result, "Drug conflicts check: passed")
})

test_that("check_drug_conflicts passes when same node has same activity", {
  
  # Multiple drugs affecting same node with same activity
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c"),
    node = c("a", "a", "b"),  # drug_a and drug_b both affect node "a"
    activity = c(1, 1, 2)     # with same activity level
  )
  
  result <- check_drug_conflicts(drugs, "node")
  expect_equal(result, "Drug conflicts check: passed")
})

test_that("check_drug_conflicts errors when same node has different activities", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("target_node", "target_node"),  # Same node
    activity = c(0, 1)                       # Different activities
  )
  
  expect_snapshot(
    check_drug_conflicts(drugs, "node"),
    error = TRUE
  )
})

test_that("check_drug_conflicts errors with multiple conflicting nodes", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c", "drug_d"),
    node = c("node1", "node1", "node2", "node2"),  # Two nodes with conflicts
    activity = c(0, 1, 2, 0)                       # Different activities for each
  )
  
  expect_snapshot(
    check_drug_conflicts(drugs, "node"),
    error = TRUE
  )
})

test_that("check_drug_conflicts works with custom node column names", {
  
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    gene = c("target_gene", "target_gene"),
    activity = c(0, 1)
  )
  
  expect_snapshot(
    check_drug_conflicts(drugs, "gene"),
    error = TRUE
  )
  
  # Test with no conflicts using custom column
  drugs_no_conflict <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    gene = c("gene1", "gene2"),
    activity = c(0, 1)
  )
  
  result <- check_drug_conflicts(drugs_no_conflict, "gene")
  expect_equal(result, "Drug conflicts check: passed")
})

test_that("check_drug_conflicts handles empty drugs data frame", {
  
  drugs <- tibble::tibble(
    drug = character(),
    node = character(),
    activity = numeric()
  )
  
  result <- check_drug_conflicts(drugs, "node")
  expect_equal(result, "Drug conflicts check: passed")
})

test_that("check_drug_conflicts handles single drug", {
  
  drugs <- tibble::tibble(
    drug = c("single_drug"),
    node = c("single_node"),
    activity = c(1)
  )
  
  result <- check_drug_conflicts(drugs, "node")
  expect_equal(result, "Drug conflicts check: passed")
})

test_that("check_drug_conflicts handles complex conflict scenarios", {
  
  # Mix of conflicts and non-conflicts
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c", "drug_d", "drug_e"),
    node = c("node1", "node1", "node2", "node3", "node3"),
    activity = c(0, 1, 2, 0, 0)  # node1 has conflict, node2 and node3 don't
  )
  
  expect_snapshot(
    check_drug_conflicts(drugs, "node"),
    error = TRUE
  )
})

test_that("check_drug_conflicts filters distinct combinations correctly", {
  
  # Test that function properly handles duplicate drug-node-activity combinations
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_a", "drug_b", "drug_b"),
    node = c("node1", "node1", "node1", "node1"),
    activity = c(1, 1, 2, 2)  # Duplicates but conflict between activities 1 and 2
  )
  
  expect_snapshot(
    check_drug_conflicts(drugs, "node"),
    error = TRUE
  )
})

test_that("check_drug_conflicts handles multiple drugs on different nodes", {
  
  # No conflicts - each drug affects different nodes
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_a", "drug_b", "drug_b"),
    node = c("node1", "node2", "node3", "node4"),
    activity = c(0, 1, 2, 0)
  )
  
  result <- check_drug_conflicts(drugs, "node")
  expect_equal(result, "Drug conflicts check: passed")
})

test_that("check_drug_conflicts handles numeric activity edge cases", {
  
  # Test with very similar but different activity values
  drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("precise_node", "precise_node"),
    activity = c(1.0, 1.1)  # Small difference
  )
  
  expect_snapshot(
    check_drug_conflicts(drugs, "node"),
    error = TRUE
  )
  
  # Test with identical decimal values
  drugs_identical <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("precise_node", "precise_node"),
    activity = c(1.5, 1.5)  # Identical decimals
  )
  
  result <- check_drug_conflicts(drugs_identical, "node")
  expect_equal(result, "Drug conflicts check: passed")
})

test_that("check_drug_conflicts preserves original data frame", {
  
  # Ensure function doesn't modify the input data frame
  original_drugs <- tibble::tibble(
    drug = c("drug_a", "drug_b"),
    node = c("node1", "node2"),
    activity = c(0, 1),
    extra_col = c("extra1", "extra2")
  )
  
  drugs_copy <- original_drugs
  result <- check_drug_conflicts(drugs_copy, "node")
  
  expect_equal(drugs_copy, original_drugs)
  expect_equal(result, "Drug conflicts check: passed")
})