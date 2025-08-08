test_that("digest_filename creates MD5 hash from filename", {
  result <- digest_filename("test_filename.json")
  
  # Should return a 32-character MD5 hash
  expect_equal(nchar(result), 32)
  expect_match(result, "^[a-f0-9]{32}$")
  
  # Should be deterministic - same input gives same output
  expect_equal(result, digest_filename("test_filename.json"))
})

test_that("digest_filename with append_json = TRUE adds .json extension", {
  filename <- "test_filename"
  result_without_json <- digest_filename(filename, append_json = FALSE)
  result_with_json <- digest_filename(filename, append_json = TRUE)
  
  # Without append_json should be just the hash
  expect_equal(nchar(result_without_json), 32)
  expect_false(grepl("\\.json$", result_without_json))
  
  # With append_json should be hash + ".json"
  expect_equal(nchar(result_with_json), 37)  # 32 + 5 for ".json"
  expect_true(grepl("\\.json$", result_with_json))
  
  # The hash portion should be the same
  hash_portion <- substr(result_with_json, 1, 32)
  expect_equal(hash_portion, result_without_json)
})

test_that("digest_filename produces different hashes for different inputs", {
  hash1 <- digest_filename("filename1")
  hash2 <- digest_filename("filename2")
  hash3 <- digest_filename("very_long_filename_with_lots_of_characters")
  
  expect_false(hash1 == hash2)
  expect_false(hash1 == hash3)
  expect_false(hash2 == hash3)
})

test_that("digest_filename handles edge cases", {
  # Empty string
  empty_hash <- digest_filename("")
  expect_equal(nchar(empty_hash), 32)
  expect_match(empty_hash, "^[a-f0-9]{32}$")
  
  # Special characters
  special_hash <- digest_filename("file__with__special__chars__123.json")
  expect_equal(nchar(special_hash), 32)
  expect_match(special_hash, "^[a-f0-9]{32}$")
  
  # Very long filename (testing Windows path length issue scenario)
  long_filename <- paste0(rep("very_long_perturbation_name", 10), collapse = "__")
  long_hash <- digest_filename(long_filename)
  expect_equal(nchar(long_hash), 32)
  expect_match(long_hash, "^[a-f0-9]{32}$")
})

test_that("digest_filename produces known hash for specific input", {
  # Test with a known input to ensure consistency
  test_input <- "growth_factor__2__e__0__PERT__a__0__b__0.json"
  result <- digest_filename(test_input)
  
  # Should be deterministic and produce same result
  expected_hash <- digest::digest(test_input, algo = "md5")
  expect_equal(result, expected_hash)
})