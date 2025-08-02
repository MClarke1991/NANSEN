## Copyright 2021 Matthew A. Clarke, Fisher Lab <matthewaclarke1991@gmail.com>

## Functions to plot heatmaps for combinations

#' Plot monotherapy heatmap
#'
#' @title plot_single
#' @param df dataframe of processed combination results
#' @param filename filename to save heatmap
#' @param results_dir results directory
#' @param vis_dir subdirectory for heatmaps
#' @param palette colour palette for heatmaps to be provided to
#'     RColorBrewer::brewer.pal
#' @param cluster_rows cluster_rows TRUE/FALSE
#' @param pheno phenotype to visualise. Selects from `node` column of
#'     processed results
#' @param levs level of the mutations to visualise e.g. 0,
#'     c(1:max_level) or "" for drugs
#' @param h height of heatmap
#' @param w width of heatmap
#' @param fontsize fontsize of heatmap
#' @param node_col node column name in imported data
#' @param na_col set colour for na
#' @return heatmap saved to drive, and a ggplot object for further
#'     processing e.g. with patchwork
#' @export
plot_single <- function(df, filename, results_dir, vis_dir, palette,
                        cluster_rows = TRUE, pheno, levs, h = 20,
                        w = 6, fontsize = 10, node_col = "node", na_col = "black") {

    print(paste("Plotting:", filename))

    preprocess <- df %>%
        dplyr::filter(case == "single") %>%
        dplyr::select(-mutb, -levb) %>%
        dplyr::rename("mutation" = muta,
               "level" = leva) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        tidyr::unite(col = "pert",
                     "mutation",
                     "level",
                     sep = " ",
                     remove = FALSE)

    prep <- preprocess %>%
        dplyr::filter(level %in% levs) %>%
        dplyr::filter(mutation != "baseline")

    ## hclust fails if there is no variation, so filter these out and warn
    if (length(unique(dplyr::pull(prep, mean))) == 1) {
        warning(paste("Could not plot:",
                      pheno,
                      "with levels",
                      levs,
                      "as there is no variation"))
        return(NA)
        }

    ## Need to generate a colour scale that works for all heatmaps
    ## based on the full range of the phenotype, and then subset that
    ## to only the colours needed for the actual values seen in this
    ## heatmap, and only ass that to pheatmap. Otherwise pheatmap
    ## renormalises, making comparisons between heatmaps more complex.
    backgrounds <- unique(dplyr::pull(prep, "background"))

    ## min and max of the possible range of the pheno node
    min_range <- prep %>%
        dplyr::filter(!is.na(mean)) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        dplyr::pull("range_from") %>%
        unique() %>%
        min()
    max_range <- prep %>%
        dplyr::filter(!is.na(mean)) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        dplyr::pull("range_to") %>%
        unique() %>%
        max()

    colour_range_full <- (max_range - min_range) + 1
    colours_full <- grDevices::colorRampPalette(
                                      RColorBrewer::brewer.pal(
                                                        colour_range_full,
                                                        palette))(2 * (max_range - min_range) + 1)
    ## use colorRampPalette to extend number of colours to handle .5 gradation in phenotype

    ## min and max /observed/ in /this/ heatmap
    min_pheno <- min(dplyr::pull(prep, mean))
    max_pheno <- max(dplyr::pull(prep, mean))
    colours_actual <- colours_full[(2 * min_pheno + 1):(2 * max_pheno + 1)]
    ## multiply by two to allow different colours for .5 change in pheno

    ## force heatmap to show whole range to allow easier comparison
    ## between heatmaps
    ## breakslist <- seq(min_pheno, max_pheno, by = 0.5)

    ## make an annotation row for the baseline results
    baseline_annotation <- preprocess %>%
        dplyr::filter(mutation == "baseline") %>%
        dplyr::mutate(pert = "baseline") %>%
        dplyr::select("background", "mean") %>%
        dplyr::rename("baseline" = "mean") %>%
        ## dplyr::mutate("colour" =
        ##                   colours_full[baseline*2 + 1 - min_pheno]) %>%
        ## minus min_pheno corrects for case where lowest value of
        ## phenotype range is non-zero. Multiply by two to allow for .5 to have own colour
        tibble::column_to_rownames("background")

    ## Need to feed only the range of colours needed by baseline or
    ## else pheatmap rescales, factor of 2 is because we double the
    ## number of colours to allow difference in shade for a .5 change
    ## in phenotype level
    colour_range_baseline <- colours_full[(2 * min(dplyr::pull(baseline_annotation, baseline)) + 1):
                                          (2 * max(dplyr::pull(baseline_annotation, baseline)) + 1)]

    wide <- prep %>%
        dplyr::select(background, pert, mean) %>%
        tidyr::pivot_wider(names_from = background, values_from = mean) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(sd = sd(dplyr::c_across(
                                         dplyr::all_of(backgrounds))))

    mat <- wide %>%
        dplyr::select(-sd) %>%
        tibble::column_to_rownames("pert") %>%
        as.matrix()

    p <- pheatmap::pheatmap(mat,
                       na_col = na_col,
                       color = colours_actual,
                       border_color = "white",
                       ## angle_col = 45,
                       #breaks = breakslist,
                       ## annotation_row =
                       ##     dplyr::select(
                       ##                tibble::column_to_rownames(wide, "pert"),
                       ##                sd),
                       annotation_col = baseline_annotation,
                       annotation_colors = list(
                           baseline =
                               colour_range_baseline),
                       filename = file.path(results_dir, vis_dir,
                                            paste0(filename, ".png")),
                       display_numbers = FALSE,
                       cluster_cols = FALSE,
                       cluster_rows = cluster_rows,
                       fontsize = fontsize,
                       width = w,
                       height = h
                       )
    pheatmap::pheatmap(mat,
                       na_col = na_col,
                       color = colours_actual,
                       border_color = "white",
                       ## angle_col = 45,
                       #breaks = breakslist,
                       ## annotation_row =
                       ##     dplyr::select(
                       ##                tibble::column_to_rownames(wide, "pert"),
                       ##                sd),
                       annotation_col = baseline_annotation,
                       annotation_colors = list(
                           baseline =
                               colour_range_baseline),
                       filename = file.path(results_dir, vis_dir,
                                            paste0(filename, ".pdf")),
                       display_numbers = FALSE,
                       cluster_cols = FALSE,
                       cluster_rows = cluster_rows,
                       fontsize = fontsize,
                       width = w,
                       height = h
                       )
    p
    return(ggplotify::as.ggplot(p))
}
#' Plot pair therapy heatmap
#'
#' @title plot_double
#' @inheritParams plot_single
#' @param cluster_cols cluster columns (TRUE/FALSE)
#' @param bkg background to plot
#' @return heatmap saved to drive, and a ggplot object for further
#'     processing e.g. with patchwork
#' @export
plot_double <- function(df, filename, results_dir, vis_dir, palette,
                        pheno, cluster_rows = TRUE,
                        cluster_cols = TRUE, levs, bkg, h = 25,
                        w = 25, fontsize = 10, node_col = "node", na_col = "black") {

    print(paste("Plotting:", filename))

    prep <- df %>%
        dplyr::filter(case == "double") %>%
        dplyr::filter(background == bkg) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        dplyr::filter(leva %in% levs) %>%
        dplyr::filter(levb %in% levs) %>%
        tidyr::unite(col = "first",
                     "muta", "leva",
                     sep = " ",
                     remove = FALSE) %>%
        tidyr::unite(col = "second",
                     "mutb", "levb",
                     sep = " ",
                     remove = FALSE)

    ## hclust fails if there is no variation, so filter these out and warn
    if (length(unique(dplyr::pull(prep, mean))) == 1) {
        warning(paste("Could not plot:",
                      pheno,
                      "with levels",
                      levs,
                      "on background",
                      bkg,
                      "as there is no variation"))
        return(NA)
        }

    ## Need to generate a colour scale that works for all heatmaps
    ## based on the full range of the phenotype, and then subset that
    ## to only the colours needed for the actual values seen in this
    ## heatmap, and only ass that to pheatmap. Otherwise pheatmap
    ## renormalises, making comparisons between heatmaps more complex.

    ## min and max of the possible range of the pheno node
    min_range <- prep %>%
        dplyr::filter(!is.na(mean)) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        dplyr::pull("range_from") %>%
        unique() %>%
        min()
    max_range <- prep %>%
        dplyr::filter(!is.na(mean)) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        dplyr::pull("range_to") %>%
        unique() %>%
        max()

    colour_range_full <- (max_range - min_range) + 1
    colours_full <- grDevices::colorRampPalette(
                                      RColorBrewer::brewer.pal(
                                                        colour_range_full,
                                                        palette))(2 * (max_range - min_range) + 1)
    ## use colorRampPalette to extend number of colours to handle .5 gradation in phenotype

    ## min and max /observed/ in /this/ heatmap
    min_pheno <- min(dplyr::pull(prep, mean))
    max_pheno <- max(dplyr::pull(prep, mean))
    colours_actual <- colours_full[(2 * min_pheno + 1):(2 * max_pheno + 1)]

        ## force heatmap to show whole range to allow easier comparison
    ## between heatmaps
    ## breakslist <- seq(min_pheno, max_pheno, by = 0.5)

    prep_short <- prep %>%
        dplyr::select(first, second, mean)

    prep_flip <- prep_short %>%
        dplyr::rename("second" = first,
               "first" = second)

    mat <- prep_short %>%
        dplyr::bind_rows(prep_flip) %>%
        dplyr::arrange(first) %>%
        tidyr::pivot_wider(names_from = second,
                    values_from = mean,
                    names_sort = TRUE) %>%
        tibble::column_to_rownames("first") %>%
        as.matrix()

    ## man_clust <- if (cluster) {
    ##                  hclust(dist(mat))
    ##              } else {
    ##                  FALSE
    ##              }

    p <- pheatmap::pheatmap(mat,
                       na_col = na_col,
                       color = colours_actual,
                       border_color = "white",
                       ## breaks = breakslist,
                       filename = file.path(results_dir, vis_dir,
                                            paste0(filename, ".png")),
                       cluster_rows = cluster_rows,
                       cluster_cols = cluster_cols,
                       fontsize = fontsize,
                       height = h,
                       width = w
                       )
    pheatmap::pheatmap(mat,
                       na_col = na_col,
                       color = colours_actual,
                       border_color = "white",
                       ## breaks = breakslist,
                       filename = file.path(results_dir, vis_dir,
                                           paste0(filename, ".pdf")),
                       cluster_rows = cluster_rows,
                       cluster_cols = cluster_cols,
                       fontsize = fontsize,
                       height = h,
                       width = w
                       )
    p
    return(ggplotify::as.ggplot(p))
}

#' Plot a monotherapy heatmap with a colourscheme optimised for
#' positive and negative data e.g. for relative levels rather than
#' absolute
#'
#' @title plot_diff_single
#' @inheritParams plot_single
#' @param neg_col negative colour
#' @param pos_col positive colour
#' @return heatmap saved to drive, and a ggplot object for further
#'     processing e.g. with patchwork
#' @export
plot_diff_single <- function(df, filename, results_dir, vis_dir,
                             neg_col = "Reds", pos_col = "Blues",
                             cluster_rows = TRUE, pheno, levs, h = 20,
                             w = 6, fontsize = 10, node_col = "node", na_col = "black") {

    print(paste("Plotting:", filename))

    preprocess <- df %>%
        dplyr::filter(case == "single") %>%
        dplyr::select(-mutb, -levb) %>%
        dplyr::rename("mutation" = muta,
               "level" = leva) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        tidyr::unite(col = "pert",
                     "mutation",
                     "level",
                     sep = " ",
                     remove = FALSE)

    prep <- preprocess %>%
        dplyr::filter(level %in% levs) %>%
        dplyr::filter(mutation != "baseline")

    ## hclust fails if there is no variation, so filter these out and warn
    if (length(unique(dplyr::pull(prep, mean))) == 1) {
        warning(paste("Could not plot:",
                      pheno,
                      "with levels",
                      levs,
                      "as there is no variation"))
        return(NA)
        }

    ## Need to generate a colour scale that works for all heatmaps
    ## based on the full range of the phenotype, and then subset that
    ## to only the colours needed for the actual values seen in this
    ## heatmap, and only ass that to pheatmap. Otherwise pheatmap
    ## renormalises, making comparisons between heatmaps more complex.
    backgrounds <- unique(dplyr::pull(prep, "background"))

    ## min and max of the possible range of the pheno node
    min_range <- prep %>%
        dplyr::filter(!is.na(mean)) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        dplyr::pull("mean") %>%
        unique() %>%
        min()
    max_range <- prep %>%
        dplyr::filter(!is.na(mean)) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        dplyr::pull("mean") %>%
        unique() %>%
        max()

    ## colour_range_full <- max(max_range, min_range)
    colours_neg <- grDevices::colorRampPalette(
                                      RColorBrewer::brewer.pal(
                                                        abs(min_range),
                                                        neg_col))(abs(min_range))
    colours_pos <- grDevices::colorRampPalette(
                                      RColorBrewer::brewer.pal(
                                                        max_range,
                                                        pos_col))(max_range)

    colours_actual <- c(rev(colours_neg), "white", colours_pos)
    ## multiply by two to allow different colours for .5 change in pheno

    ## force heatmap to show whole range to allow easier comparison
    ## between heatmaps
    ## breakslist <- seq(min_pheno, max_pheno, by = 0.5)

    ## make an annotation row for the baseline results
    ## baseline_annotation <- preprocess %>%
    ##     dplyr::filter(mutation == "baseline") %>%
    ##     dplyr::mutate(pert = "baseline") %>%
    ##     dplyr::select("background", "mean") %>%
    ##     dplyr::rename("baseline" = "mean") %>%
    ##     ## dplyr::mutate("colour" =
    ##     ##                   colours_full[baseline*2 + 1 - min_pheno]) %>%
    ##     ## minus min_pheno corrects for case where lowest value of
    ##     ## phenotype range is non-zero. Multiply by two to allow for .5 to have own colour
    ##     tibble::column_to_rownames("background")

    ## ## Need to feed only the range of colours needed by baseline or
    ## ## else pheatmap rescales, factor of 2 is because I double the
    ## ## number of colours to allow difference in shade for a .5 change
    ## ## in phenotype level
    ## colour_range_baseline <- colours_full[(2*min(dplyr::pull(baseline_annotation, baseline))+1):
    ##                                       (2*max(dplyr::pull(baseline_annotation, baseline))+1)]

    wide <- prep %>%
        dplyr::select(background, pert, mean) %>%
        tidyr::pivot_wider(names_from = background, values_from = mean) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(sd = sd(dplyr::c_across(
                                         dplyr::all_of(backgrounds))))

    mat <- wide %>%
        dplyr::select(-sd) %>%
        tibble::column_to_rownames("pert") %>%
        as.matrix()

    p <- pheatmap::pheatmap(mat,
                       na_col = na_col,
                       color = colours_actual,
                       border_color = "white",
                       ## angle_col = 45,
                       #breaks = breakslist,
                       ## annotation_row =
                       ##     dplyr::select(
                       ##                tibble::column_to_rownames(wide, "pert"),
                       ##                sd),
                       ## annotation_col = baseline_annotation,
                       ## annotation_colors = list(
                       ##     baseline =
                       ##         colour_range_baseline),
                       filename = file.path(results_dir, vis_dir,
                                            paste0(filename, ".png")),
                       display_numbers = FALSE,
                       cluster_cols = FALSE,
                       cluster_rows = cluster_rows,
                       fontsize = fontsize,
                       width = w,
                       height = h
                       )
    pheatmap::pheatmap(mat,
                       na_col = na_col,
                       color = colours_actual,
                       border_color = "white",
                       ## angle_col = 45,
                       #breaks = breakslist,
                       ## annotation_row =
                       ##     dplyr::select(
                       ##                tibble::column_to_rownames(wide, "pert"),
                       ##                sd),
                       ## annotation_col = baseline_annotation,
                       ## annotation_colors = list(
                       ##     baseline =
                       ##         colour_range_baseline),
                       filename = file.path(results_dir, vis_dir,
                                            paste0(filename, ".pdf")),
                       display_numbers = FALSE,
                       cluster_cols = FALSE,
                       cluster_rows = cluster_rows,
                       fontsize = fontsize,
                       width = w,
                       height = h
                       )
    p
    return(ggplotify::as.ggplot(p))
}

#' Plot a combination heatmap with a colourscheme optimised for
#' positive and negative data e.g. for relative levels rather than
#' absolute
#'
#' @title plot_diff_double
#' @inheritParams plot_double
#' @param cluster_rows cluster rows (TRUE/FALSE)
#' @param cluster_cols cluster columns (TRUE/FALSE)
#' @param neg_col negative colour
#' @param pos_col positive colour
#' @param mirror Usually data is not mirrored in muta and mutb, and
#'     so to plot a heatmap we do this within the function. For diff
#'     plots it is useful to sometimes feed pre-mirrored data, so
#'     this turns off this step
#' @return heatmap saved to drive, and a ggplot object for further
#'     processing e.g. with patchwork
#' @export
plot_diff_double <- function(df, filename, results_dir, vis_dir,
                             neg_col, pos_col,
                             pheno, cluster_rows = FALSE,
                             cluster_cols = FALSE, levs, bkg, h = 25,
                             w = 25, fontsize = 10, node_col = "node",
                             mirror = TRUE, na_col = "black") {

    print(paste("Plotting:", filename))

    prep <- df %>%
        dplyr::filter(case == "double") %>%
        dplyr::filter(background == bkg) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        dplyr::filter(leva %in% levs) %>%
        dplyr::filter(levb %in% levs) %>%
        tidyr::unite(col = "first",
                     "muta", "leva",
                     sep = " ",
                     remove = FALSE) %>%
        tidyr::unite(col = "second",
                     "mutb", "levb",
                     sep = " ",
                     remove = FALSE)

    ## hclust fails if there is no variation, so filter these out and warn
    if (length(unique(dplyr::pull(prep, mean))) == 1) {
        warning(paste("Could not plot:",
                      pheno,
                      "with levels",
                      levs,
                      "on background",
                      bkg,
                      "as there is no variation"))
        return(NA)
        }

    ## Need to generate a colour scale that works for all heatmaps
    ## based on the full range of the phenotype, and then subset that
    ## to only the colours needed for the actual values seen in this
    ## heatmap, and only ass that to pheatmap. Otherwise pheatmap
    ## renormalises, making comparisons between heatmaps more complex.

    ## min and max of the possible range of the pheno node
    min_range <- prep %>%
        dplyr::filter(!is.na(mean)) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        dplyr::pull("mean") %>%
        unique() %>%
        min()
    max_range <- prep %>%
        dplyr::filter(!is.na(mean)) %>%
        dplyr::filter(.data[[node_col]] == {{pheno}}) %>%
        dplyr::pull("mean") %>%
        unique() %>%
        max()

    ## colour_range_full <- max(max_range, min_range)
    colours_neg <- grDevices::colorRampPalette(
                                      RColorBrewer::brewer.pal(
                                                        abs(min_range),
                                                        neg_col))(abs(min_range))
    colours_pos <- grDevices::colorRampPalette(
                                      RColorBrewer::brewer.pal(
                                                        max_range,
                                                        pos_col))(max_range)

    colours_actual <- c(rev(colours_neg), "white", colours_pos)


    ## make colours from 0 to blue, colours from 0-red, then flip one and add together with white



    ## use colorRampPalette to extend number of colours to handle .5 gradation in phenotype

    ## min and max /observed/ in /this/ heatmap
    ## min_pheno <- min(dplyr::dplyr::pull(prep, mean))
    ## max_pheno <- max(dplyr::dplyr::pull(prep, mean))
    ## colours_actual <- colours_full[(2*min_pheno + 1):(2*max_pheno + 1)]

    ## force heatmap to show whole range to allow easier comparison
    ## between heatmaps
    ## breakslist <- seq(-max_range, max_range, by = 1)

    prep_short <- prep %>%
        dplyr::select(first, second, mean)

    if (mirror) {
        prep_flip <- prep_short %>%
            dplyr::rename("second" = first,
                          "first" = second)
        prep_mirror <- dplyr::bind_rows(prep_short, prep_flip)
    } else {
        prep_mirror <- prep_short
    }


    mat <- prep_mirror %>%
        dplyr::arrange(first) %>%
        tidyr::pivot_wider(names_from = second,
                    values_from = mean,
                    names_sort = TRUE) %>%
        tibble::column_to_rownames("first") %>%
        as.matrix()

    ## man_clust <- if(cluster) {
    ##                  hclust(dist(mat))
    ##              } else {
    ##                  FALSE
    ##              }

    p <- pheatmap::pheatmap(mat,
                       na_col = na_col,
                       ## color = colours_actual,
                       color = colours_actual,
                       border_color = "white",
                       ## breaks = breakslist,
                       filename = file.path(results_dir, vis_dir,
                                            paste0(filename, ".png")),
                       cluster_rows = cluster_rows,
                       cluster_cols = cluster_cols,
                       fontsize = fontsize,
                       height = h,
                       width = w
                       )
    pheatmap::pheatmap(mat,
                       na_col = na_col,
                       ## color = colours_actual,
                       color = colours_actual,
                       border_color = "white",
                       ## breaks = breakslist,
                       filename = file.path(results_dir, vis_dir,
                                            paste0(filename, ".pdf")),
                       cluster_rows = cluster_rows,
                       cluster_cols = cluster_cols,
                       fontsize = fontsize,
                       height = h,
                       width = w
                       )
    p
    return(ggplotify::as.ggplot(p))
}

#' Loop over plotting of double heatmaps
#' @title loop_plot_double
#' @inheritParams plot_double
#' @param phenos phenotypes to visualise. Selects from `node` column of
#'     processed results
#' @param pals colour palettes for heatmaps to be provided to
#'     RColorBrewer::brewer.pal
#' @param suffix suffix for filenames
#' @param drug_type encoded in filename
#' @param cluster_cols cluster columns (TRUE/FALSE)
#' @return write heatmaps to file
#' @export
loop_plot_double <- function(bkg,
                             df,
                             phenos,
                             pals,
                             levs,
                             results_dir,
                             vis_dir,
                             suffix,
                             cluster_rows,
                             cluster_cols,
                             h = 25,
                             w = 25,
                             fontsize = 10,
                             drug_type = "",
                             node_col = "node") {

    purrr::walk2(.x = phenos,
      .y = pals,
      ~plot_double(df = df,
                   bkg = bkg,
                 pheno = .x,
                 levs = levs,
                 palette = .y,
                 cluster_rows = cluster_rows,
                 cluster_cols = cluster_cols,
                 results_dir = results_dir,
                 vis_dir = vis_dir,
                 filename = paste("double", drug_type,
                                  .x, bkg, suffix, sep = "_"),
                 h = h,
                 w = w,
                 fontsize = fontsize,
                 node_col = node_col
                 ))
}

#' Loop over plotting of double diff heatmaps
#' @title loop_plot_diff_double
#' @inheritParams loop_plot_double
#' @param neg_col negative colour
#' @param pos_col positive colour
#' @param cluster_cols cluster columns (TRUE/FALSE)
#' @return write heatmaps to file
#' @export
loop_plot_diff_double <- function(bkg,
                           df,
                           phenos,
                           levs,
                           neg_col,
                           pos_col,
                           cluster_rows,
                           cluster_cols,
                           results_dir,
                           vis_dir,
                           suffix,
                           h = 25,
                           w = 25,
                           fontsize = 10,
                           drug_type = "",
                           node_col = "node") {

    purrr::walk(.x = phenos,
                 ~plot_diff_double(df = df,
                              bkg = bkg,
                              pheno = .x,
                              levs = levs,
                              ## palette = .y,
                              neg_col,
                              pos_col,
                              cluster_rows,
                              cluster_cols,
                              results_dir = results_dir,
                              vis_dir = vis_dir,
                              filename = paste("double_survival", drug_type,
                                               .x, bkg, suffix, sep = "_"),
                              h = h,
                              w = w,
                              fontsize = fontsize,
                              node_col = node_col
                              ))
}

#' plot all heatmaps of a particular type, node, druggable or drug
#'
#' @title plot_heatmaps
#' @inheritParams combo
#' @inheritParams plot_single
#' @inheritParams plot_double
#' @inheritParams combo
#' @inheritParams get_combo_results_dir
#' @inheritParams get_netw_variables
#' @param results_file filename of results file to read
#' @param cluster_double_cols cluster columns for double heatmaps (TRUE/FALSE)
#' @param type node, druggable or drug heatmaps
#' @param background_order order of backgrounds for plotting
#' @param background_order_neat neat order of backgrounds for plotting
#' @param neaten_background whether to neaten background names
#' @param single_fontsize fontsize for monotherapy node perturbation heatmaps
#' @param single_druggable_fontsize fontsize for monotherapy druggable perturbation heatmaps
#' @param single_drugs_fontsize fontsize for monotherapy drug perturbation heatmaps
#' @param double_fontsize fontsize for combination therapy node perturbation heatmaps
#' @param double_druggable_font_size fontsize for combination therapy druggable perturbation heatmaps
#' @param double_drugs_font_size fontsize for combination therapy drug perturbation heatmaps
#' @param h_s height of single heatmaps
#' @param h_d height of double heatmaps
#' @param w_s width of single heatmaps
#' @param w_d width of double heatmaps
#' @return plot heatmaps and save to directory
#' @export
plot_heatmaps <- function(results_file,
                          results_prefix,
                          project_path,
                          out_dir,
                          netw_file_path,
                          vis_dir,
                          cluster_rows = TRUE,
                          cluster_double_cols = TRUE,
                          type = "node",
                          background_order = NA,
                          background_order_neat = NA,
                          neaten_background = FALSE,
                          single_fontsize = 10,
                          single_druggable_fontsize = 25,
                          single_drugs_fontsize = 20,
                          double_fontsize = 10,
                          double_druggable_font_size = 20,
                          double_drugs_font_size = 20,
                          h_s = 20,
                          h_d = 25,
                          w_s = 6,
                          w_d = 25) {
    results_dir <- get_combo_results_dir(results_prefix = results_prefix,
                                         project_path = project_path,
                                         out_dir = out_dir,
                                         netw_file_path = netw_file_path)

    if (!dir.exists(file.path(results_dir, vis_dir))) {
        dir.create(file.path(results_dir, vis_dir))
    }

    results <- readr::read_csv(file.path(results_dir, results_file), lazy = FALSE, show_col_types = FALSE)

    if (neaten_background) {
        results <- results %>%
            dplyr::mutate(background = background_neat)
        background_order <- background_order_neat
    }

    if(!missing(background_order)){
        results <- results %>%
            dplyr::arrange(factor(background, levels = background_order)) %>%
            NANSEN::preserve_order("background")
    }

    max_level <- max(
        max(unique(dplyr::pull(results, mean)), na.rm = TRUE),
        max(unique(dplyr::pull(results, mean)), na.rm = TRUE))

    backgrounds <- results %>%
        dplyr::pull(background) %>%
        unique()

    switch(type,
           "node" = {
               print("Plotting single node heatmaps")
               ## All normal nodes
               purrr::walk2(.x = phenotypes,
                            .y = palettes,
                            ~plot_single(df = results,
                                         pheno = .x,
                                         levs = 0,
                                         fontsize = single_fontsize,
                                         cluster_rows,
                                         palette = .y,
                                         results_dir = results_dir,
                                         vis_dir = vis_dir,
                                         filename = paste("single", .x, "inhib", sep = "_"),
                                         node_col = node_col_name,
                                         h = h_s,
                                         w = w_s
                                      ))
               purrr::walk2(.x = phenotypes,
                            .y = palettes,
                            ~plot_single(df = results,
                                         pheno = .x,
                                         levs = c(1:max_level),
                                         fontsize = single_fontsize,
                                         palette = .y,
                                         cluster_rows,
                                         results_dir = results_dir,
                                         vis_dir = vis_dir,
                                         filename = paste("single", .x, "activ", sep = "_"),
                                         node_col = node_col_name,
                                         h = h_s,
                                         w = w_s
                                         ))
               print("Plotting double node heatmaps")
               purrr::walk(backgrounds,
                           ~loop_plot_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             pals = palettes,
                                             levs = 0,
                                             cluster_rows,
                                             cluster_double_cols,
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             fontsize = double_fontsize,
                                             suffix = "inhib",
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
               purrr::walk(backgrounds,
                           ~loop_plot_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             pals = palettes,
                                             cluster_rows,
                                             cluster_double_cols,
                                             levs = c(1:max_level),
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             suffix = "activ",
                                             fontsize = double_fontsize,
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
               purrr::walk(backgrounds,
                           ~loop_plot_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             pals = palettes,
                                             cluster_rows,
                                             cluster_double_cols,
                                             levs = c(0:max_level),
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             suffix = "all",
                                             fontsize = double_fontsize,
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
           } ,
           "druggable" = {
               print("Plotting single druggable heatmaps")
               purrr::walk2(.x = phenotypes,
                            .y = palettes,
                            ~plot_single(df = results,
                                         pheno = .x,
                                         levs = 0,
                                         palette = .y,
                                         cluster_rows,
                                         results_dir = results_dir,
                                         vis_dir = vis_dir,
                                         fontsize = single_druggable_fontsize,
                                         filename = paste("single_druggable", .x, "inhib", sep = "_"),
                                         node_col = node_col_name,
                                         h = h_s,
                                         w = w_s
                                         ))
               purrr::walk2(.x = phenotypes,
                            .y = palettes,
                            ~plot_single(df = results,
                                         pheno = .x,
                                         levs = c(1:max_level),
                                         palette = .y,
                                         cluster_rows,
                                         fontsize = single_druggable_fontsize,
                                         results_dir = results_dir,
                                         vis_dir = vis_dir,
                                         filename = paste("single_druggable", .x, "activ", sep = "_"),
                                         node_col = node_col_name,
                                         h = h_s,
                                         w = w_s
                                         ))
               print("Plotting double druggable heatmaps")
               purrr::walk(backgrounds,
                           ~loop_plot_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             pals = palettes,
                                             levs = 0,
                                             cluster_rows,
                                             cluster_double_cols,
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             fontsize = double_druggable_font_size,
                                             suffix = "inhib",
                                             drug_type = "druggable",
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
               purrr::walk(backgrounds,
                           ~loop_plot_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             pals = palettes,
                                             levs = c(1:max_level),
                                             cluster_rows,
                                             cluster_double_cols,
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             fontsize = double_druggable_font_size,
                                             suffix = "activ",
                                             drug_type = "druggable",
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
               purrr::walk(backgrounds,
                           ~loop_plot_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             pals = palettes,
                                             levs = c(0:max_level),
                                             cluster_rows,
                                             cluster_double_cols,
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             fontsize = double_druggable_font_size,
                                             suffix = "all",
                                             drug_type = "druggable",
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
           },
           "drug" = {
               print("Plotting single drug heatmaps")
               results <- results %>%
                   dplyr::mutate(leva = as.character(leva),
                          levb = as.character(levb),
                          leva = tidyr::replace_na(leva, ""),
                          levb = tidyr::replace_na(levb, ""),
                          muta = dplyr::case_when(
                              muta != "baseline" ~ stringr::str_to_title(muta),
                              TRUE ~ muta),
                          mutb = dplyr::case_when(
                              mutb != "baseline" ~ stringr::str_to_title(mutb),
                              TRUE ~ mutb))
               purrr::walk2(.x = phenotypes,
                            .y = palettes,
                            ~plot_single(df = results,
                                         pheno = .x,
                                         levs = "",
                                         palette = .y,
                                         cluster_rows,
                                         fontsize = single_drugs_fontsize,
                                         results_dir = results_dir,
                                         vis_dir = vis_dir,
                                         filename = paste("single_drugs", .x, sep = "_"),
                                         node_col = node_col_name,
                                         h = h_s,
                                         w = w_s
                                         ))
               print("Plotting double drug heatmaps")
               purrr::walk(backgrounds,
                           ~loop_plot_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             pals = palettes,
                                             cluster_rows,
                                             cluster_double_cols,
                                             fontsize = double_drugs_font_size,
                                             levs = "",
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             suffix = "",
                                             drug_type = "drugs",
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
           },
           stop("Unknown input, options are: node, druggable, drug, survival")
           )
}

#' plot all heatmaps of a particular type, node, druggable or drug
#'
#' @title plot_diff_heatmaps
#' @inheritParams combo
#' @inheritParams plot_single
#' @inheritParams plot_double
#' @param results_file filename of results file to read
#' @param neg_col negative colour
#' @param pos_col positive colour
#' @param cluster_double_cols cluster columns for double heatmaps (TRUE/FALSE)
#' @param phenotypes list of phenotypes in format `c("node_name_1", "node_name_2")` for use if pheno_only is TRUE
#' @param type node, druggable or drug heatmaps
#' @param background_order order of backgrounds for plotting
#' @param background_order_neat neat order of backgrounds for plotting
#' @param neaten_background whether to neaten background names
#' @param single_fontsize fontsize for monotherapy node perturbation heatmaps
#' @param single_druggable_fontsize fontsize for monotherapy druggable perturbation heatmaps
#' @param single_drugs_fontsize fontsize for monotherapy drug perturbation heatmaps
#' @param double_fontsize fontsize for combination therapy node perturbation heatmaps
#' @param double_druggable_font_size fontsize for combination therapy druggable perturbation heatmaps
#' @param double_drugs_font_size fontsize for combination therapy drug perturbation heatmaps
#' @param h_s height of single heatmaps
#' @param h_d height of double heatmaps
#' @param w_s width of single heatmaps
#' @param w_d width of double heatmaps
#' @return plot heatmaps and save to directory
#' @export
plot_diff_heatmaps <- function(results_file,
                               results_prefix,
                               project_path,
                               out_dir,
                               netw_file_path,
                               vis_dir,
                               phenotypes,
                               type = "node",
                               neg_col = "Reds",
                               pos_col = "Blues",
                               cluster_rows,
                               cluster_double_cols,
                               background_order = NA,
                               background_order_neat = NA,
                               neaten_background = FALSE,
                               single_fontsize = 10,
                               single_druggable_fontsize = 25,
                               single_drugs_fontsize = 20,
                               double_fontsize = 10,
                               double_druggable_font_size = 20,
                               double_drugs_font_size = 20,
                               h_s = 20,
                               h_d = 25,
                               w_s = 6,
                               w_d = 25) {
    results_dir <- get_combo_results_dir(results_prefix = results_prefix,
                                         project_path = project_path,
                                         out_dir = out_dir,
                                         netw_file_path = netw_file_path)

    if (!dir.exists(file.path(results_dir, vis_dir))) {
        dir.create(file.path(results_dir, vis_dir))
    }

    results <- readr::read_csv(file.path(results_dir, results_file), lazy = FALSE, show_col_types = FALSE)

    if (neaten_background) {
        results <- results %>%
            dplyr::mutate(background = background_neat)
        background_order <- background_order_neat
    }

    if(!missing(background_order)){
        results <- results %>%
            dplyr::arrange(factor(background, levels = background_order)) %>%
            NANSEN::preserve_order("background")
    }

    max_level <- max(
        max(unique(dplyr::pull(results, mean)), na.rm = TRUE),
        max(unique(dplyr::pull(results, mean)), na.rm = TRUE))

    backgrounds <- results %>%
        dplyr::pull(background) %>%
        unique()

    switch(type,
           "node" = {
               print("Plotting single node heatmaps")
               ## All normal nodes
               purrr::walk2(.x = phenotypes,
                            .y = palettes,
                            ~plot_diff_single(df = results,
                                              pheno = .x,
                                              levs = 0,
                                              fontsize = single_fontsize,
                                              neg_col = neg_col,
                                              pos_col = pos_col,
                                              cluster_rows = cluster_rows,
                                              results_dir = results_dir,
                                              vis_dir = vis_dir,
                                              filename = paste("single", .x, "inhib", sep = "_"),
                                              node_col = node_col_name,
                                              h = h_s,
                                              w = w_s
                                              ))
               purrr::walk2(.x = phenotypes,
                            .y = palettes,
                            ~plot_diff_single(df = results,
                                              pheno = .x,
                                              levs = c(1:max_level),
                                              fontsize = single_fontsize,
                                              neg_col = neg_col,
                                              pos_col = pos_col,
                                              cluster_rows = cluster_rows,
                                              results_dir = results_dir,
                                              vis_dir = vis_dir,
                                              filename = paste("single", .x, "activ", sep = "_"),
                                              node_col = node_col_name,
                                              h = h_s,
                                              w = w_s
                                              ))
               print("Plotting double node heatmaps")
               purrr::walk(backgrounds,
                           ~loop_plot_diff_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             neg_col = neg_col,
                                             pos_col = pos_col,
                                             levs = 0,
                                             cluster_rows = cluster_rows,
                                             cluster_cols = cluster_double_cols,
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             fontsize = double_fontsize,
                                             suffix = "inhib",
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
               purrr::walk(backgrounds,
                           ~loop_plot_diff_double(bkg = .x,
                                             df = results,
                                             neg_col = neg_col,
                                             pos_col = pos_col,
                                             phenos = phenotypes,
                                             cluster_rows = cluster_rows,
                                             cluster_cols = cluster_double_cols,
                                             levs = c(1:max_level),
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             suffix = "activ",
                                             fontsize = double_fontsize,
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
               purrr::walk(backgrounds,
                           ~loop_plot_diff_double(bkg = .x,
                                             df = results,
                                             neg_col = neg_col,
                                             pos_col = pos_col,
                                             phenos = phenotypes,
                                             cluster_rows = cluster_rows,
                                             cluster_cols = cluster_double_cols,
                                             levs = c(0:max_level),
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             suffix = "all",
                                             fontsize = double_fontsize,
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
           } ,
           "druggable" = {
               print("Plotting single druggable heatmaps")
               purrr::walk2(.x = phenotypes,
                            .y = palettes,
                            ~plot_diff_single(df = results,
                                         pheno = .x,
                                         levs = 0,
                                         neg_col = neg_col,
                                         pos_col = pos_col,
                                         cluster_rows = cluster_rows,
                                         results_dir = results_dir,
                                         vis_dir = vis_dir,
                                         fontsize = single_druggable_fontsize,
                                         filename = paste("single_druggable", .x, "inhib", sep = "_"),
                                         node_col = node_col_name,
                                         h = h_s,
                                         w = w_s
                                         ))
               purrr::walk2(.x = phenotypes,
                            .y = palettes,
                            ~plot_diff_single(df = results,
                                         pheno = .x,
                                         levs = c(1:max_level),
                                         neg_col = neg_col,
                                         pos_col = pos_col,
                                         cluster_rows = cluster_rows,
                                         fontsize = single_druggable_fontsize,
                                         results_dir = results_dir,
                                         vis_dir = vis_dir,
                                         filename = paste("single_druggable", .x, "activ", sep = "_"),
                                         node_col = node_col_name,
                                         h = h_s,
                                         w = w_s))
               print("Plotting double druggable heatmaps")
               purrr::walk(backgrounds,
                           ~loop_plot_diff_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             neg_col = neg_col,
                                             pos_col = pos_col,
                                             levs = 0,
                                             cluster_rows = cluster_rows,
                                             cluster_cols = cluster_double_cols,
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             fontsize = double_druggable_font_size,
                                             suffix = "inhib",
                                             drug_type = "druggable",
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
               purrr::walk(backgrounds,
                           ~loop_plot_diff_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             neg_col = neg_col,
                                             pos_col = pos_col,
                                             cluster_rows = cluster_rows,
                                             cluster_cols = cluster_double_cols,
                                             levs = c(1:max_level),
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             fontsize = double_druggable_font_size,
                                             suffix = "activ",
                                             drug_type = "druggable",
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
               purrr::walk(backgrounds,
                           ~loop_plot_diff_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             neg_col = neg_col,
                                             pos_col = pos_col,
                                             cluster_rows = cluster_rows,
                                             cluster_cols = cluster_double_cols,
                                             levs = c(0:max_level),
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             fontsize = double_druggable_font_size,
                                             suffix = "all",
                                             drug_type = "druggable",
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
           },
           "drug" = {
               print("Plotting single drug heatmaps")
               results <- results %>%
                   dplyr::mutate(leva = as.character(leva),
                          levb = as.character(levb),
                          leva = tidyr::replace_na(leva, ""),
                          levb = tidyr::replace_na(levb, ""),
                          muta = dplyr::case_when(
                              muta != "baseline" ~ stringr::str_to_title(muta),
                              TRUE ~ muta),
                          mutb = dplyr::case_when(
                              mutb != "baseline" ~ stringr::str_to_title(mutb),
                              TRUE ~ mutb))
               purrr::walk2(.x = phenotypes,
                            .y = palettes,
                            ~plot_diff_single(df = results,
                                         pheno = .x,
                                         levs = "",
                                         neg_col = neg_col,
                                         pos_col = pos_col,
                                         cluster_rows = cluster_rows,
                                         fontsize = single_drugs_fontsize,
                                         results_dir = results_dir,
                                         vis_dir = vis_dir,
                                         filename = paste("single_drugs", .x, sep = "_"),
                                         node_col = node_col_name,
                                         h = h_s,
                                         w = w_s
                                         ))
               print("Plotting double drug heatmaps")
               purrr::walk(backgrounds,
                           ~loop_plot_diff_double(bkg = .x,
                                             df = results,
                                             phenos = phenotypes,
                                             neg_col = neg_col,
                                             pos_col = pos_col,
                                             cluster_rows = cluster_rows,
                                             cluster_cols = cluster_double_cols,
                                             fontsize = double_drugs_font_size,
                                             levs = "",
                                             results_dir = results_dir,
                                             vis_dir = vis_dir,
                                             suffix = "",
                                             drug_type = "drugs",
                                             node_col = node_col_name,
                                             h = h_d,
                                             w = w_d))
           },
           stop("Unknown input, options are: node, druggable, drug, survival")
           )
}
