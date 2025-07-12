source("testing_utils.r")

test_that("make_single_drugs combines commands for single drugs correctly", {
  
  # Test with single-node drugs
  drugs_commands <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c"),
    node = c("a", "b", "c"),
    command_arg = c("-ko 1 0", "-ko 2 1", "-ko 3 2"),
    alt_filename_part = c("a__0", "b__1", "c__2")
  )
  
  result <- make_single_drugs(drugs_commands)
  
  # Should have one row per unique drug
  expect_equal(nrow(result), 3)
  expect_equal(result$drug, c("drug_a", "drug_b", "drug_c"))
  
  # Check command arguments (should be unchanged for single-node drugs)
  expect_equal(result$command_arg, c("-ko 1 0", "-ko 2 1", "-ko 3 2"))
  
  # Check alt_filename_part (should be unchanged for single-node drugs)
  expect_equal(result$alt_filename_part, c("a__0", "b__1", "c__2"))
  
  # Check filename_part format: "{drug}__NA"
  expect_equal(result$filename_part, c("drug_a__NA", "drug_b__NA", "drug_c__NA"))
})

test_that("make_single_drugs combines commands for multi-node drugs", {
  
  # Test with drug affecting multiple nodes
  drugs_commands <- tibble::tibble(
    drug = c("multi_drug", "multi_drug", "single_drug"),
    node = c("a", "b", "c"),
    command_arg = c("-ko 1 0", "-ko 2 1", "-ko 3 2"),
    alt_filename_part = c("a__0", "b__1", "c__2")
  )
  
  result <- make_single_drugs(drugs_commands)
  
  # Should have one row per unique drug
  expect_equal(nrow(result), 2)
  expect_true("multi_drug" %in% result$drug)
  expect_true("single_drug" %in% result$drug)
  
  # Check multi_drug row
  multi_row <- result[result$drug == "multi_drug", ]
  expect_equal(multi_row$command_arg, "-ko 1 0 -ko 2 1")  # Combined with space
  expect_equal(multi_row$alt_filename_part, "a__0__b__1")  # Combined with __
  expect_equal(multi_row$filename_part, "multi_drug__NA")
  
  # Check single_drug row
  single_row <- result[result$drug == "single_drug", ]
  expect_equal(single_row$command_arg, "-ko 3 2")
  expect_equal(single_row$alt_filename_part, "c__2")
  expect_equal(single_row$filename_part, "single_drug__NA")
})

test_that("make_single_drugs handles empty drugs_commands", {
  
  drugs_commands <- tibble::tibble(
    drug = character(),
    command_arg = character(),
    alt_filename_part = character()
  )
  
  result <- make_single_drugs(drugs_commands)
  
  expect_equal(nrow(result), 0)
  expect_true(all(c("drug", "command_arg", "alt_filename_part", "filename_part") %in% colnames(result)))
})

test_that("make_single_drugs preserves drug order in grouping", {
  
  # Test that drugs are processed in order encountered
  drugs_commands <- tibble::tibble(
    drug = c("zebra", "alpha", "zebra", "alpha"),
    command_arg = c("-ko 1 0", "-ko 2 1", "-ko 3 2", "-ko 4 3"),
    alt_filename_part = c("a__0", "b__1", "c__2", "d__3")
  )
  
  result <- make_single_drugs(drugs_commands)
  
  expect_equal(nrow(result), 2)
  
  # Check zebra combines correctly
  zebra_row <- result[result$drug == "zebra", ]
  expect_equal(zebra_row$command_arg, "-ko 1 0 -ko 3 2")
  expect_equal(zebra_row$alt_filename_part, "a__0__c__2")
  
  # Check alpha combines correctly
  alpha_row <- result[result$drug == "alpha", ]
  expect_equal(alpha_row$command_arg, "-ko 2 1 -ko 4 3")
  expect_equal(alpha_row$alt_filename_part, "b__1__d__3")
})

test_that("make_single_drugs handles complex multi-node scenarios", {
  
  # Test drug affecting 4 nodes
  drugs_commands <- tibble::tibble(
    drug = c(rep("complex_drug", 4), "simple_drug"),
    command_arg = c("-ko 1 0", "-ko 2 1", "-ko 3 2", "-ko 4 3", "-ko 5 4"),
    alt_filename_part = c("a__0", "b__1", "c__2", "d__3", "e__4")
  )
  
  result <- make_single_drugs(drugs_commands)
  
  expect_equal(nrow(result), 2)
  
  complex_row <- result[result$drug == "complex_drug", ]
  expect_equal(complex_row$command_arg, "-ko 1 0 -ko 2 1 -ko 3 2 -ko 4 3")
  expect_equal(complex_row$alt_filename_part, "a__0__b__1__c__2__d__3")
  expect_equal(complex_row$filename_part, "complex_drug__NA")
})

test_that("make_single_drugs handles drugs with identical names correctly", {
  
  # Test multiple rows with same drug name (normal scenario)
  drugs_commands <- tibble::tibble(
    drug = c("same_drug", "same_drug", "same_drug"),
    command_arg = c("-ko 1 0", "-ko 2 1", "-ko 3 2"),
    alt_filename_part = c("x__0", "y__1", "z__2")
  )
  
  result <- make_single_drugs(drugs_commands)
  
  expect_equal(nrow(result), 1)
  expect_equal(result$drug, "same_drug")
  expect_equal(result$command_arg, "-ko 1 0 -ko 2 1 -ko 3 2")
  expect_equal(result$alt_filename_part, "x__0__y__1__z__2")
  expect_equal(result$filename_part, "same_drug__NA")
})

test_that("make_single_drugs preserves additional columns during grouping", {
  
  # Test that other columns are handled appropriately
  drugs_commands <- tibble::tibble(
    drug = c("drug_a", "drug_a"),
    command_arg = c("-ko 1 0", "-ko 2 1"),
    alt_filename_part = c("a__0", "b__1"),
    extra_col = c("value1", "value2"),
    constant_col = c("same", "same")
  )
  
  result <- make_single_drugs(drugs_commands)
  
  expect_equal(nrow(result), 1)
  
  # Summarise should only keep grouped columns plus summarised ones
  expected_cols <- c("drug", "command_arg", "alt_filename_part", "filename_part")
  expect_true(all(expected_cols %in% colnames(result)))
  
  # Extra columns should not be in result due to group_by + summarise
  expect_false("extra_col" %in% colnames(result))
  expect_false("constant_col" %in% colnames(result))
})

test_that("make_single_drugs handles special characters in command args", {
  
  drugs_commands <- tibble::tibble(
    drug = c("special_drug", "special_drug"),
    command_arg = c("-ko 1 0.5", "-ko 2 -1"),  # Decimal and negative values
    alt_filename_part = c("node1__0.5", "node2__-1")
  )
  
  result <- make_single_drugs(drugs_commands)
  
  expect_equal(result$command_arg, "-ko 1 0.5 -ko 2 -1")
  expect_equal(result$alt_filename_part, "node1__0.5__node2__-1")
})

test_that("make_single_drugs maintains consistent output structure", {
  
  drugs_commands <- tibble::tibble(
    drug = c("test_drug"),
    command_arg = c("-ko 1 0"),
    alt_filename_part = c("a__0")
  )
  
  result <- make_single_drugs(drugs_commands)
  
  # Check that output has required columns with correct types
  expect_s3_class(result, "data.frame")
  expect_true("drug" %in% colnames(result))
  expect_true("command_arg" %in% colnames(result))
  expect_true("alt_filename_part" %in% colnames(result))
  expect_true("filename_part" %in% colnames(result))
  
  expect_type(result$drug, "character")
  expect_type(result$command_arg, "character")
  expect_type(result$alt_filename_part, "character")
  expect_type(result$filename_part, "character")
})

test_that("make_single_drugs handles whitespace in commands correctly", {
  
  drugs_commands <- tibble::tibble(
    drug = c("drug_a", "drug_a"),
    command_arg = c("-ko 1 0", " -ko 2 1 "),  # Extra whitespace
    alt_filename_part = c("a__0", "b__1")
  )
  
  result <- make_single_drugs(drugs_commands)
  
  # Commands should be combined with single space, preserving any existing whitespace
  expect_equal(result$command_arg, "-ko 1 0  -ko 2 1 ")
})