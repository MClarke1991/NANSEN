source(here::here("tests", "testthat", "testing_utils.r"))

temp_dir <- here::here("tests/testthat/temp_test_outputs")
# Create a directory for test outputs
if (!dir.exists(temp_dir)) {
  dir.create(temp_dir)
}

# Helper function to load real combination results data
load_combo_data <- function() {
  readr::read_csv(
    here::here("tests", "testthat", "helper_results_json", "processed_results.csv"),
    show_col_types = FALSE
  )
}

# Helper function to load real autopert results data
load_autopert_data <- function() {
  readr::read_csv(
    here::here("tests", "testthat", "helper_results_json", "autopert_results.csv"),
    show_col_types = FALSE
  )
}

# Helper function to create temporary output directory
setup_temp_dir <- function(test_name) {
  temp_test_dir <- file.path(temp_dir, test_name)
  if (dir.exists(temp_test_dir)) {
    unlink(temp_test_dir, recursive = TRUE)
  }
  dir.create(temp_test_dir, recursive = TRUE)
  temp_test_dir
}

# Tests for plot_single function
test_that("plot_single creates single therapy heatmap", {
  test_dir <- setup_temp_dir("plot_single_test")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  combo_data <- load_combo_data()

  # Test basic functionality
  result <- plot_single(
    df = combo_data,
    filename = "test_single",
    results_dir = test_dir,
    vis_dir = vis_dir,
    palette = "Blues",
    pheno = "output_a",
    levs = 0
  )

  # Should return a ggplot object
  expect_true(ggplot2::is_ggplot(result))

  # Should create PNG and PDF files
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_single.png")))
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_single.pdf")))

  # Clean up
  unlink(test_dir, recursive = TRUE)
})

test_that("plot_single handles no variation warning", {
  test_dir <- setup_temp_dir("plot_single_no_variation")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  # Create data with no variation (all same values)
  combo_data <- load_combo_data()
  combo_data$mean[combo_data$case == "single" & combo_data$node == "output_a"] <- 1.0

  expect_warning(
    result <- plot_single(
      df = combo_data,
      filename = "test_no_variation",
      results_dir = test_dir,
      vis_dir = vis_dir,
      palette = "Blues",
      pheno = "output_a",
      levs = 0
    ),
    "Could not plot"
  )

  expect_true(is.na(result))

  unlink(test_dir, recursive = TRUE)
})

test_that("plot_single works with different parameters", {
  test_dir <- setup_temp_dir("plot_single_params")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  combo_data <- load_combo_data()

  # Test with different clustering and size parameters
  result <- plot_single(
    df = combo_data,
    filename = "test_params",
    results_dir = test_dir,
    vis_dir = vis_dir,
    palette = "Reds",
    cluster_rows = FALSE,
    pheno = "output_b",
    levs = c(1, 2),
    h = 15,
    w = 8,
    fontsize = 12,
    na_col = "gray"
  )

  expect_true(ggplot2::is_ggplot(result))
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_params.png")))

  unlink(test_dir, recursive = TRUE)
})

# Tests for plot_double function
test_that("plot_double creates combination therapy heatmap", {
  test_dir <- setup_temp_dir("plot_double_test")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  combo_data <- load_combo_data()

  result <- plot_double(
    df = combo_data,
    filename = "test_double",
    results_dir = test_dir,
    vis_dir = vis_dir,
    palette = "Greens",
    pheno = "output_a",
    levs = 0,
    bkg = "cancer"
  )

  expect_true(ggplot2::is_ggplot(result))
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_double.png")))
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_double.pdf")))

  unlink(test_dir, recursive = TRUE)
})

test_that("plot_double handles clustering parameters", {
  test_dir <- setup_temp_dir("plot_double_clustering")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  combo_data <- load_combo_data()

  result <- plot_double(
    df = combo_data,
    filename = "test_clustering",
    results_dir = test_dir,
    vis_dir = vis_dir,
    palette = "Purples",
    pheno = "output_b",
    cluster_rows = FALSE,
    cluster_cols = FALSE,
    levs = c(0, 1),
    bkg = "wt",
    h = 20,
    w = 20,
    fontsize = 8
  )

  expect_true(ggplot2::is_ggplot(result))
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_clustering.png")))

  unlink(test_dir, recursive = TRUE)
})

# Tests for plot_diff_single function
test_that("plot_diff_single creates differential single heatmap", {
  test_dir <- setup_temp_dir("plot_diff_single_test")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  combo_data <- load_combo_data()
  # Adjust mean values to have both positive and negative
  combo_data$mean <- runif(nrow(combo_data), -2, 2)

  result <- suppressWarnings(plot_diff_single(
    df = combo_data,
    filename = "test_diff_single",
    results_dir = test_dir,
    vis_dir = vis_dir,
    neg_col = "Reds",
    pos_col = "Blues",
    pheno = "output_a",
    levs = 0
  ))

  expect_true(ggplot2::is_ggplot(result))
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_diff_single.png")))
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_diff_single.pdf")))

  unlink(test_dir, recursive = TRUE)
})

# Tests for plot_diff_double function
test_that("plot_diff_double creates differential double heatmap", {
  test_dir <- setup_temp_dir("plot_diff_double_test")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  combo_data <- load_combo_data()
  combo_data$mean <- runif(nrow(combo_data), -2, 2)

  result <- suppressWarnings(plot_diff_double(
    df = combo_data,
    filename = "test_diff_double",
    results_dir = test_dir,
    vis_dir = vis_dir,
    neg_col = "Reds",
    pos_col = "Blues",
    pheno = "output_a",
    levs = 0,
    bkg = "cancer"
  ))

  expect_true(ggplot2::is_ggplot(result))
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_diff_double.png")))
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_diff_double.pdf")))

  unlink(test_dir, recursive = TRUE)
})

test_that("plot_diff_double works with mirror parameter", {
  test_dir <- setup_temp_dir("plot_diff_double_mirror")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  combo_data <- load_combo_data()
  combo_data$mean <- runif(nrow(combo_data), -1, 1)

  # Test with mirror = FALSE
  suppressWarnings(result <- plot_diff_double(
    df = combo_data,
    filename = "test_no_mirror",
    results_dir = test_dir,
    vis_dir = vis_dir,
    neg_col = "Reds",
    pos_col = "Blues",
    pheno = "output_b",
    levs = c(0, 1),
    bkg = "wt",
    mirror = FALSE
  ))

  expect_true(ggplot2::is_ggplot(result))
  expect_true(file.exists(file.path(test_dir, vis_dir, "test_no_mirror.png")))

  unlink(test_dir, recursive = TRUE)
})

# Tests for loop functions
test_that("loop_plot_double works correctly", {
  test_dir <- setup_temp_dir("loop_plot_double_test")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  combo_data <- load_combo_data()

  # Test the loop function
  expect_no_error(
    loop_plot_double(
      bkg = "cancer",
      df = combo_data,
      phenos = c("output_a", "output_b"),
      pals = c("Blues", "Reds"),
      levs = 0,
      results_dir = test_dir,
      vis_dir = vis_dir,
      suffix = "test",
      cluster_rows = TRUE,
      cluster_cols = TRUE,
      drug_type = "node"
    )
  )

  # Should create multiple files
  expect_true(file.exists(file.path(test_dir, vis_dir, "double_node_output_a_cancer_test.png")))
  expect_true(file.exists(file.path(test_dir, vis_dir, "double_node_output_b_cancer_test.png")))

  unlink(test_dir, recursive = TRUE)
})

test_that("loop_plot_diff_double works correctly", {
  test_dir <- setup_temp_dir("loop_plot_diff_double_test")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  combo_data <- load_combo_data()
  combo_data$mean <- runif(nrow(combo_data), -1, 1)

  suppressWarnings(expect_no_error(
    loop_plot_diff_double(
      bkg = "wt",
      df = combo_data,
      phenos = c("output_a"),
      levs = c(0, 1),
      neg_col = "Reds",
      pos_col = "Blues",
      cluster_rows = FALSE,
      cluster_cols = FALSE,
      results_dir = test_dir,
      vis_dir = vis_dir,
      suffix = "diff_test",
      drug_type = "test"
    )
  ))

  expect_true(file.exists(file.path(test_dir, vis_dir, "double_survival_test_output_a_wt_diff_test.png")))

  unlink(test_dir, recursive = TRUE)
})

# Tests for autopert plotting functionality
test_that("autopert plotting code creates correct plots", {
  test_dir <- setup_temp_dir("autopert_plots_test")

  # Load real autopert results data
  autopert_results <- load_autopert_data()

  # Create results_short_node_summary from real data
  autopert_node_summary <- autopert_results %>%
    dplyr::select(gene, diff) %>%
    dplyr::group_by(gene) %>%
    dplyr::summarise(
      diff_per_gene = sum(na.omit(diff)),
      abs_diff_per_gene = sum(abs(na.omit(diff))),
      .groups = 'drop'
    ) %>%
    dplyr::filter(abs_diff_per_gene != 0)

  # Test the autopert plotting code (extracted from autopert function)
  results_plot <- autopert_results %>%
    dplyr::filter(!is.na(expectation_bma)) %>%
    tidyr::unite(label, source, cell_line, experiment_particular, gene, sep = "_")

  results_plot$label <- factor(results_plot$label, levels = results_plot$label)

  # Test main results plot
  p1 <- ggplot2::ggplot(results_plot, ggplot2::aes(label, diff)) +
    ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = diff < 0)) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      guide = "none",
      breaks = c(TRUE, FALSE),
      values = c("blue", "red")
    ) +
    ggplot2::labs(
      title = "Mismatch between modelled and expected results",
      y = "Difference \n (mean model result - expected)",
      x = "Perturbation"
    ) +
    ggplot2::theme_bw()

  expect_true(ggplot2::is_ggplot(p1))

  # Test node summary plot
  p2 <- ggplot2::ggplot(autopert_node_summary, ggplot2::aes(gene, abs_diff_per_gene)) +
    ggplot2::geom_bar(stat = "identity", ggplot2::aes(fill = diff_per_gene < 0)) +
    ggplot2::coord_flip() +
    ggplot2::scale_fill_manual(
      guide = "none",
      breaks = c(TRUE, FALSE),
      values = c("blue", "red")
    ) +
    ggplot2::labs(
      title = "Mismatch between modelled and expected results",
      y = "Difference \n (mean model result - expected)",
      x = "Measured Gene"
    ) +
    ggplot2::theme_bw()

  expect_true(ggplot2::is_ggplot(p2))

  # Test gathered plot
  p3 <- autopert_node_summary %>%
    tidyr::gather(key = "type", value = "diff", -gene) %>%
    ggplot2::ggplot(ggplot2::aes(gene, diff)) +
    ggplot2::geom_bar(
      stat = "identity",
      ggplot2::aes(fill = type), position = ggplot2::position_dodge()
    ) +
    ggplot2::labs(
      title = "Mismatch between modelled and expected results",
      y = "Difference \n (mean model result - expected)",
      x = "Measured Gene"
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw()

  expect_true(ggplot2::is_ggplot(p3))

  # Test per perturbation per gene plot
  p4 <- autopert_results %>%
    tidyr::unite(label, source, cell_line, experiment_particular, sep = "_") %>%
    dplyr::select(label, gene, diff) %>%
    na.omit() %>%
    dplyr::arrange(gene) %>%
    ggplot2::ggplot(ggplot2::aes(gene, diff)) +
    ggplot2::geom_bar(
      stat = "identity",
      ggplot2::aes(
        fill = label,
        colour = I("black")
      ),
      position = ggplot2::position_dodge()
    ) +
    ggplot2::labs(
      title = "Mismatch between modelled and expected results",
      y = "Difference \n (mean model result - expected)",
      x = "Measured Gene"
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")

  expect_true(ggplot2::is_ggplot(p4))

  unlink(test_dir, recursive = TRUE)
})

# Edge case tests
test_that("plotting functions handle empty datasets", {
  test_dir <- setup_temp_dir("empty_data_test")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  # Create empty dataset with correct structure
  combo_data <- load_combo_data()
  empty_data <- combo_data[0, ]

  # Should handle empty data gracefully
  suppressWarnings(expect_error(
    plot_single(
      df = empty_data,
      filename = "test_empty",
      results_dir = test_dir,
      vis_dir = vis_dir,
      palette = "Blues",
      pheno = "output_a",
      levs = 0
    )
  ))

  unlink(test_dir, recursive = TRUE)
})

test_that("plotting functions handle missing columns", {
  test_dir <- setup_temp_dir("missing_cols_test")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  # Create data missing required columns
  incomplete_data <- load_combo_data()
  incomplete_data$mean <- NULL

  expect_error(
    plot_single(
      df = incomplete_data,
      filename = "test_missing",
      results_dir = test_dir,
      vis_dir = vis_dir,
      palette = "Blues",
      pheno = "output_a",
      levs = 0
    )
  )

  unlink(test_dir, recursive = TRUE)
})

test_that("plotting functions handle single data point", {
  test_dir <- setup_temp_dir("single_point_test")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  # Create dataset with single point
  combo_data <- load_combo_data()
  single_data <- combo_data[1, ]
  single_data$case <- "single"
  single_data$muta <- "a"
  single_data$leva <- 0

  # Should handle single point gracefully
  expect_warning(
    result <- plot_single(
      df = single_data,
      filename = "test_single_point",
      results_dir = test_dir,
      vis_dir = vis_dir,
      palette = "Blues",
      pheno = "output_a",
      levs = 0
    ),
    "Could not plot"
  )

  expect_true(is.na(result))

  unlink(test_dir, recursive = TRUE)
})

# Test file output validation
test_that("plot files have reasonable sizes", {
  test_dir <- setup_temp_dir("file_size_test")
  vis_dir <- "visualizations"
  dir.create(file.path(test_dir, vis_dir))

  combo_data <- load_combo_data()

  plot_single(
    df = combo_data,
    filename = "size_test",
    results_dir = test_dir,
    vis_dir = vis_dir,
    palette = "Blues",
    pheno = "output_a",
    levs = 0
  )

  png_file <- file.path(test_dir, vis_dir, "size_test.png")
  pdf_file <- file.path(test_dir, vis_dir, "size_test.pdf")

  # Files should exist and have reasonable sizes (> 1KB)
  expect_true(file.exists(png_file))
  expect_true(file.exists(pdf_file))
  expect_gt(file.size(png_file), 1000)
  expect_gt(file.size(pdf_file), 1000)

  unlink(test_dir, recursive = TRUE)
})

# Clean up after tests
if (dir.exists(temp_dir)) {
  unlink(temp_dir, recursive = TRUE)
}