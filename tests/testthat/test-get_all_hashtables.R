test_that("get_all_hashtables reads and combines CSV files correctly", {
  # Create temporary directory with test CSV files
  temp_dir <- tempdir()
  hashtable_dir <- file.path(temp_dir, "hashtables")
  dir.create(hashtable_dir, showWarnings = FALSE)

  # Create test CSV files
  csv1_data <- data.frame(
    unhash_full_filename = c("file1.json", "file2.json"),
    full_filename = c("hash1.json", "hash2.json"),
    stringsAsFactors = FALSE
  )

  csv2_data <- data.frame(
    unhash_full_filename = c("file3.json", "file4.json"),
    full_filename = c("hash3.json", "hash4.json"),
    stringsAsFactors = FALSE
  )

  readr::write_csv(csv1_data, file.path(hashtable_dir, "table1.csv"))
  readr::write_csv(csv2_data, file.path(hashtable_dir, "table2.csv"))

  # Test the function
  result <- get_all_hashtables(hashtable_dir, "test.log")

  # Should combine all rows from both CSV files
  expect_equal(nrow(result), 4)
  expect_true(all(c("file1.json", "file2.json", "file3.json", "file4.json") %in% result$unhash_full_filename))
  expect_true(all(c("hash1.json", "hash2.json", "hash3.json", "hash4.json") %in% result$full_filename))

  # Should have file column indicating source CSV
  expect_true("file" %in% colnames(result))

  # Clean up
  unlink(hashtable_dir, recursive = TRUE)
})

test_that("get_all_hashtables stops when hash collisions detected", {
  # Create temporary directory with test CSV files
  temp_dir <- tempdir()
  hashtable_dir <- file.path(temp_dir, "hashtables_collision")
  dir.create(hashtable_dir, showWarnings = FALSE)

  # Create CSV with hash collision (same hash for different filenames)
  csv_collision <- data.frame(
    unhash_full_filename = c("file1.json", "file2.json"),
    full_filename = c("samehash.json", "samehash.json"),  # Collision!
    stringsAsFactors = FALSE
  )

  readr::write_csv(csv_collision, file.path(hashtable_dir, "collision.csv"))

  # Should stop with error message
  expect_error(
    get_all_hashtables(hashtable_dir, "test.log"),
    "Not all perturbations have been assigned a unique hash, please use the full length filenames."
  )

  # Clean up
  unlink(hashtable_dir, recursive = TRUE)
})

test_that("get_all_hashtables handles empty directory", {
  # Create empty directory
  temp_dir <- tempdir()
  empty_dir <- file.path(temp_dir, "empty_hashtables")
  dir.create(empty_dir, showWarnings = FALSE)

  # Should error when directory is empty
  expect_error(
    get_all_hashtables(empty_dir, "test.log"),
    "Expected location for hashtables .* is empty"
  )

  # Clean up
  unlink(empty_dir, recursive = TRUE)
})

test_that("get_all_hashtables handles unique hashes correctly", {
  # Create temporary directory with test CSV files
  temp_dir <- tempdir()
  hashtable_dir <- file.path(temp_dir, "hashtables_unique")
  dir.create(hashtable_dir, showWarnings = FALSE)

  # Create CSV with unique hashes
  csv_unique <- data.frame(
    unhash_full_filename = c("file1.json", "file2.json", "file3.json"),
    full_filename = c("hash1.json", "hash2.json", "hash3.json"),
    stringsAsFactors = FALSE
  )

  readr::write_csv(csv_unique, file.path(hashtable_dir, "unique.csv"))

  # Should not error and return the data
  result <- get_all_hashtables(hashtable_dir, "test.log")
  expect_equal(nrow(result), 3)
  expect_equal(length(unique(result$unhash_full_filename)), 3)
  expect_equal(length(unique(result$full_filename)), 3)

  # Clean up
  unlink(hashtable_dir, recursive = TRUE)
})

test_that("get_all_hashtables provides error when hashtables collide (are not unique)", {
  # Create temporary directory with collision scenario
  temp_dir <- tempdir()
  hashtable_dir <- file.path(temp_dir, "hashtables_snapshot")
  dir.create(hashtable_dir, showWarnings = FALSE)

  # Create CSV with hash collision for snapshot testing
  csv_collision <- data.frame(
    unhash_full_filename = c("different_file1.json", "different_file2.json"),
    full_filename = c("identical_hash.json", "identical_hash.json"),
    stringsAsFactors = FALSE
  )

  readr::write_csv(csv_collision, file.path(hashtable_dir, "collision_snapshot.csv"))

  # Test error message with snapshot
  expect_snapshot(
    get_all_hashtables(hashtable_dir, "test.log"),
    error = TRUE
  )

  # Clean up
  unlink(hashtable_dir, recursive = TRUE)
})