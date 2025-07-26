source("testing_utils.r")

test_that("make_pair_drugs generates correct pairs from drugs_single", {

  drugs_single <- tibble::tibble(
    drug = c("drug_a", "drug_b", "drug_c"),
    command_arg = c("-ko 1 0", "-ko 2 1", "-ko 3 2"),
    alt_filename_part = c("node_a__0", "node_b__1", "node_c__2"),
    filename_part = c("drug_a__NA", "drug_b__NA", "drug_c__NA")
  )

  result <- make_pair_drugs(drugs_single)

  # Should generate 3 pairs: (a,b), (a,c), (b,c)
  expect_equal(nrow(result), 3)

  # Check pair combinations
  expect_true(all(c("drug_a", "drug_b") %in% c(result$a[1], result$b[1])))
  expect_true(all(c("drug_a", "drug_c") %in% c(result$a[2], result$b[2])))
  expect_true(all(c("drug_b", "drug_c") %in% c(result$a[3], result$b[3])))
})

test_that("make_pair_drugs combines command arguments correctly", {

  drugs_single <- tibble::tibble(
    drug = c("drug_x", "drug_y"),
    command_arg = c("-ko 10 0", "-ko 20 1"),
    alt_filename_part = c("node_x__0", "node_y__1"),
    filename_part = c("drug_x__NA", "drug_y__NA")
  )

  result <- make_pair_drugs(drugs_single)

  # Should combine both command arguments
  expect_equal(result$command_arg[1], "-ko 10 0 -ko 20 1")
})

test_that("make_pair_drugs combines filename parts correctly", {

  drugs_single <- tibble::tibble(
    drug = c("drug_alpha", "drug_beta"),
    command_arg = c("-ko 1 0", "-ko 2 1"),
    alt_filename_part = c("alpha__0", "beta__1"),
    filename_part = c("drug_alpha__NA", "drug_beta__NA")
  )

  result <- make_pair_drugs(drugs_single)

  # Check filename_part combination
  expect_equal(result$filename_part[1], "drug_alpha__NA__drug_beta__NA")

  # Check alt_filename_part combination
  expect_equal(result$alt_filename_part[1], "alpha__0__beta__1")
})

test_that("make_pair_drugs selects correct columns", {

  drugs_single <- tibble::tibble(
    drug = c("drug_1", "drug_2"),
    command_arg = c("-ko 1 0", "-ko 2 1"),
    alt_filename_part = c("node1__0", "node2__1"),
    filename_part = c("drug_1__NA", "drug_2__NA"),
    extra_col = c("extra1", "extra2")  # This should not appear in result
  )

  result <- make_pair_drugs(drugs_single)

  expected_cols <- c("a", "b", "filename_part", "alt_filename_part", "command_arg")
  expect_equal(sort(colnames(result)), sort(expected_cols))
  expect_false("extra_col" %in% colnames(result))
})

test_that("make_pair_drugs handles minimum number of drugs", {

  # Exactly 2 drugs should produce 1 pair
  drugs_single <- tibble::tibble(
    drug = c("drug_one", "drug_two"),
    command_arg = c("-ko 1 0", "-ko 2 1"),
    alt_filename_part = c("one__0", "two__1"),
    filename_part = c("drug_one__NA", "drug_two__NA")
  )

  result <- make_pair_drugs(drugs_single)

  expect_equal(nrow(result), 1)
  expect_equal(result$a[1], "drug_one")
  expect_equal(result$b[1], "drug_two")
})

test_that("make_pair_drugs handles larger number of drugs", {

  # 4 drugs should produce 6 pairs: C(4,2) = 6
  drugs_single <- tibble::tibble(
    drug = c("drug_1", "drug_2", "drug_3", "drug_4"),
    command_arg = c("-ko 1 0", "-ko 2 1", "-ko 3 2", "-ko 4 0"),
    alt_filename_part = c("n1__0", "n2__1", "n3__2", "n4__0"),
    filename_part = c("drug_1__NA", "drug_2__NA", "drug_3__NA", "drug_4__NA")
  )

  result <- make_pair_drugs(drugs_single)

  expect_equal(nrow(result), 6)

  # Check that all combinations are present
  pairs <- paste(result$a, result$b, sep = "-")
  expected_pairs <- c("drug_1-drug_2", "drug_1-drug_3", "drug_1-drug_4",
                      "drug_2-drug_3", "drug_2-drug_4", "drug_3-drug_4")
  expect_equal(sort(pairs), sort(expected_pairs))
})

test_that("make_pair_drugs handles single drug input", {

  # Should error or return empty for single drug (can't make pairs)
  drugs_single <- tibble::tibble(
    drug = c("only_drug"),
    command_arg = c("-ko 1 0"),
    alt_filename_part = c("only__0"),
    filename_part = c("only_drug__NA")
  )

  # combn(1, 2) should error - expect this to propagate
  expect_error(make_pair_drugs(drugs_single))
})

test_that("make_pair_drugs handles empty input", {

  drugs_single <- tibble::tibble(
    drug = character(),
    command_arg = character(),
    alt_filename_part = character(),
    filename_part = character()
  )

  # combn with empty vector should error - expect this to propagate
  expect_error(make_pair_drugs(drugs_single))
})

test_that("make_pair_drugs preserves drug name order in pairs", {

  # Test that function consistently orders pairs
  drugs_single <- tibble::tibble(
    drug = c("zzz_drug", "aaa_drug"),  # Reverse alphabetical
    command_arg = c("-ko 1 0", "-ko 2 1"),
    alt_filename_part = c("zzz__0", "aaa__1"),
    filename_part = c("zzz_drug__NA", "aaa_drug__NA")
  )

  result <- make_pair_drugs(drugs_single)

  # combn should maintain order from input vector
  expect_equal(result$a[1], "zzz_drug")
  expect_equal(result$b[1], "aaa_drug")
})

test_that("make_pair_drugs handles complex command arguments", {

  # Test with multi-part command arguments
  drugs_single <- tibble::tibble(
    drug = c("multi_drug_a", "multi_drug_b"),
    command_arg = c("-ko 1 0 -ko 2 1", "-ko 3 2 -ko 4 0"),
    alt_filename_part = c("complex_a", "complex_b"),
    filename_part = c("multi_drug_a__NA", "multi_drug_b__NA")
  )

  result <- make_pair_drugs(drugs_single)

  expected_command <- "-ko 1 0 -ko 2 1 -ko 3 2 -ko 4 0"
  expect_equal(result$command_arg[1], expected_command)
})

test_that("make_pair_drugs handles special characters in names", {

  drugs_single <- tibble::tibble(
    drug = c("drug_with_underscore", "drug.with.dots"),
    command_arg = c("-ko 1 0", "-ko 2 1"),
    alt_filename_part = c("under__0", "dots__1"),
    filename_part = c("drug_with_underscore__NA", "drug.with.dots__NA")
  )

  result <- make_pair_drugs(drugs_single)

  # Should handle special characters in drug names
  expect_equal(result$a[1], "drug_with_underscore")
  expect_equal(result$b[1], "drug.with.dots")

  # Check filename combination preserves special characters
  expected_filename <- "drug_with_underscore__NA__drug.with.dots__NA"
  expect_equal(result$filename_part[1], expected_filename)
})

test_that("make_pair_drugs handles identical drug components", {

  # Edge case: drugs with identical command arguments but different names
  drugs_single <- tibble::tibble(
    drug = c("drug_same_a", "drug_same_b"),
    command_arg = c("-ko 1 0", "-ko 1 0"),  # Same command
    alt_filename_part = c("same__0", "same__0"),  # Same alt filename
    filename_part = c("drug_same_a__NA", "drug_same_b__NA")
  )

  result <- make_pair_drugs(drugs_single)

  # Should still create pair even with identical commands
  expect_equal(result$command_arg[1], "-ko 1 0 -ko 1 0")
  expect_equal(result$alt_filename_part[1], "same__0__same__0")
})

test_that("make_pair_drugs maintains tibble structure", {

  drugs_single <- tibble::tibble(
    drug = c("test_1", "test_2", "test_3"),
    command_arg = c("-ko 1 0", "-ko 2 1", "-ko 3 2"),
    alt_filename_part = c("t1__0", "t2__1", "t3__2"),
    filename_part = c("test_1__NA", "test_2__NA", "test_3__NA")
  )

  result <- make_pair_drugs(drugs_single)

  # Result should be a tibble
  expect_s3_class(result, "tbl_df")
  expect_s3_class(result, "data.frame")
})