## Copyright 2022 Matthew A. Clarke, Fisher Lab <matthewaclarke1991@gmail.com>


##' Run autoperturbation test on a given network and specification
##' @title autopert
##' @param netw_file_path path to network JSON file
##' @param spec_path path to specification csv file
##' @param bma_path path to BMA command line installation, defaults to
##'     the path produced by the one click installer (.msi). The path
##'     is automatically normalized for cross-platform compatibility.
##' @param group_vars variables used to group rows of the
##'     specification into a single experiment. Defaults to "source"
##'     (citation key or other unique identifier of source of
##'     experimental data), "cell_line" (cell line or tissue used,
##'     which determines background mutations for the experiment),
##'     "experiment_particular" (details of the experiment
##'     e.g. "Application of cisplatin")
##' @param bma_tools_path path for BMATools development repo
##' @param out_dir path where all output files should be stored
##' @param nosat option to run without passing to SAT solver in case
##'     of VMCAI not finding a fixed-point attractor.
##' @param loserum EXPERIMENTAL option to set serum nodes to 1
##' @param missing_nodes_perturbed_overide option to override check
##'     for pertrubred nodes that are missing from network
##' @param missing_nodes_expected_overide option to override check for
##'     expected nodes that are missing from network
##' @param project_path project path for git SHA log, point to git
##'     repo of the network and specification being tested
##' @return Writes out results as JSON, CSV and PNG
##' @export
autopert <- function(netw_file_path,
                     spec_path,
                     bma_path =
                         'C:\\"Program Files (x86)"\\BMA\\BioCheckConsole.exe',
                     ## note exact format needed for BMA path
                     out_dir,
                     nosat = TRUE,
                     loserum = FALSE,
                     missing_nodes_perturbed_overide = FALSE,
                     missing_nodes_expected_overide = FALSE,
                     project_path = NA,
                     bma_tools_path = NA,
                     group_vars = c(
                         "source", "cell_line",
                         "experiment_particular"
                     )) {
    ## Normalize BMA path for cross-platform compatibility
    bma_path <- normalize_bma_path(bma_path)
    
    ## Output files

    autopert_dir <- here::here(out_dir, paste("AP_RUN",
        stringr::str_remove(
            basename(netw_file_path),
            ".json"
        ),
        sep = "_"
    ))
    parse_dir <- file.path(autopert_dir, "BioCheck_output")
    results_dir <- file.path(autopert_dir, "results")
    log_file <- file.path(autopert_dir, "AutoPertLogs.log")

    ## Create directories -----

    for (i in c(here::here(out_dir), autopert_dir, parse_dir, results_dir)) {
        if (!dir.exists(i)) {
            dir.create(i)
        }
    }

    ## Set up logs

    futile.logger::flog.appender(
        futile.logger::appender.file(log_file),
        name = log_file
    )
    ## Capture all unflogged warnings and errors in flogger
    ## https://github.com/zatonovo/futile.logger/issues/36
    options(error = function() {
        futile.logger::flog.warn(
            geterrmessage(),
            name = log_file
        )
    })
    # Starting timestamp
    futile.logger::flog.info("Start", name = log_file)


    futile.logger::flog.info(
        paste(
            "Running Autopert on model:",
            netw_file_path,
            "using specification",
            spec_path
        ),
        name = log_file
    )


    ## Functions ----

    ## TODO Better column name for expectation_bma as confusing with
    ## existing expected_result_bma

    ## TODO Improve this check, do I need to mke new columns (especially in
    ## else block)





    ## ----- Import Data -------

    netw_variables <- NANSEN::get_netw_variables(netw_file_path)
    ## TEMP get rid of weird character encoding of RangeTo in json
    netw_variables <- netw_variables %>%
        dplyr::mutate(range_to = as.integer(range_to))

    spec <- import_spec(
        spec_path = spec_path,
        loserum = loserum,
        clean_underscores = TRUE,
        netw_variables = netw_variables
    )


    ## ------ Checks -----

    check_spec_groups(spec, group_vars)

    stop_missing_nodes_perturbed(
        spec,
        missing_nodes_perturbed_overide,
        netw_variables,
        log_file
    )
    stop_missing_nodes_expected(
        spec,
        missing_nodes_expected_overide,
        netw_variables,
        log_file
    )
    stop_no_inputs(spec, group_vars, log_file = log_file)

    spec_levels <- convert_spec_levels(spec, log_file)



    check_perts_in_range(spec_levels)


    ## get command
    spec_commands <- spec_levels %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
        ## https://stackoverflow.com/a/66253244/10923234
        dplyr::filter(!is.na(perturbation_bma)) %>%
        dplyr::summarise(
            spec_command =
                paste0("-ko ",
                    id,
                    " ",
                    perturbation_bma,
                    collapse = " "
                ),
            file_name = paste0(gene,
                "__",
                perturbation_bma,
                collapse = "__"
            ),
            num_exp_perts = dplyr::n()
        )

    ## PoC get background
    ## TODO Add warning if there is background missing for a cell line in spec
    commands <- spec_commands %>%
        dplyr::mutate(
            command = paste0(spec_command),
            file_name = paste0(file_name, ".json")
        )

    commands_short <- commands %>%
        dplyr::select(source, cell_line, file_name, command)

    ## Run commands through BMA
    if (nosat == TRUE) {
        p <- progress::progress_bar$new(
            total = nrow(commands_short),
            force = TRUE,
            clear = FALSE,
            format =
                " [:bar] :percent eta: :eta elapsed: :elapsedfull"
        ) # Initialise progress bar
        ## for (i in 1:nrow(commands_short)) {
        for (i in seq_len(nrow(commands_short))) {
            shell(paste0(
                bma_path,
                " -model ",
                netw_file_path,
                " -engine VMCAI -nosat -prove ",
                file.path(parse_dir, commands_short$file_name[i]),
                " ",
                commands_short$command[i]
            ))
            p$tick()
        }
        futile.logger::flog.info("nosat mode enabled", name = log_file)
    } else {
        p <- progress::progress_bar$new(
            total = nrow(commands_short),
            force = TRUE,
            clear = FALSE,
            format =
                " [:bar] :percent eta: :eta elapsed: :elapsedfull"
        ) # Initialise progress bar
        for (i in seq_len(nrow(commands_short))) {
            shell(paste0(
                bma_path,
                " -model ",
                netw_file_path,
                " -engine VMCAI -prove ",
                file.path(parse_dir, commands_short$file_name[i]),
                " ",
                commands_short$command[i]
            ))
            p$tick()
        }
    }


    ## ----- Parse Results-----

    ## Get files with progress bar
    data <- NANSEN::parse_biocheck_dir(parse_dir, netw_variables)

    readr::write_csv(data, file.path(results_dir, "parse_results.csv"))


    ## PoC join to spec
    data_commands <- dplyr::full_join(commands,
        data,
        by = c("file_name" = "filename")
    ) %>%
        dplyr::select(
            -spec_command,
            ## -file_name_spec,
            -num_exp_perts,
            ## -source_bkgrd,
            ## -background_command,
            ## -file_name_bkgrd,
            ## -num_back_perts,
            -command,
            -file_name,
            -time,
            -id,
            -range_from,
            -range_to,
            -formula
        ) %>%
        dplyr::rename("gene" = name)

    results <- dplyr::full_join(spec_levels,
        data_commands,
        by = c(
            group_vars,
            "gene"
        )
    ) %>%
        dplyr::select(
            -id,
            -formula,
            -range_from,
            -range_to,
            -perturbation_bma,
            -expected_result_bma
        ) %>%
        dplyr::mutate(
            mean_result = (hi + lo) / 2,
            diff = mean_result - as.numeric(expectation_bma)
        )

    missing_results <- results %>%
        dplyr::filter(!is.na(expectation_bma) &
            (is.na(lo) | is.na(hi) | is.na(mean_result)))
    if (nrow(missing_results) > 0) {
        futile.logger::flog.error(
            warning(
                "Results missing, did you try and get an output without specifying any
    inputs?\n See:\n ",
                paste(utils::capture.output(print(missing_results)), collapse = "\n")
                # https://stackoverflow.com/a/26083626
            ),
            name = log_file
        )
    }

    results_short <- results %>%
        dplyr::filter(!is.na(perturbation) | !is.na(expectation_bma))


    results_score <- results %>%
        dplyr::summarise(score = sum(abs(diff), na.rm = TRUE)) # TODO Do we want
    # na.rm to be here,
    # is this likely to
    # cause problems?

    futile.logger::flog.info(
        paste0(
            "Mismatch Score is: ",
            results_score
        ),
        name = log_file
    )

    results_mismatch <- results %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) %>%
        ## https://stackoverflow.com/a/66253244/10923234
        dplyr::filter(any(diff != 0)) %>%
        ## see https://stackoverflow.com/a/31027426/10923234, get
        ## whole experiment when there is an error, not just the line
        ## with the perturbed gene
        dplyr::filter(!is.na(perturbation) | !is.na(expectation_bma))
    ## remove rows of results where there was neither a perturbation
    ## nor an expected result, as makes harder to compare to original
    ## spec

    ## TODO Write out a full results, results only with genes I meant to
    ## pert included, and only errors. And print score somewhere for long
    ## term plotting. And write out metadata e.g. time run, network version
    ## etc.

    results_short_node_summary <- results_short %>%
        dplyr::select(gene, diff) %>%
        dplyr::group_by(gene) %>%
        dplyr::summarise(
            diff_per_gene = sum(na.omit(diff)),
            abs_diff_per_gene = sum(abs(na.omit(diff)))
        ) %>%
        dplyr::filter(abs_diff_per_gene != 0)

    ## Write out results
    readr::write_csv(
        results,
        file.path(results_dir, "results.csv")
    )
    readr::write_csv(
        results_short,
        file.path(results_dir, "results_short.csv")
    )
    readr::write_csv(
        results_score,
        file.path(results_dir, "results_score.csv")
    )
    readr::write_csv(
        results_mismatch,
        file.path(results_dir, "results_mismatch.csv")
    )
    readr::write_csv(
        results_short_node_summary,
        file.path(results_dir, "results_short_node_summary.csv")
    )


    ## Plot ----
    results_plot <- results_short %>%
        dplyr::filter(!is.na(expectation_bma)) %>%
        tidyr::unite(label, group_vars, gene, sep = "_")

    results_plot$label <- factor(results_plot$label,
        levels = results_plot$label
    )

    ggplot2::ggplot(results_plot, ggplot2::aes(label, diff)) +
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
    ggplot2::ggsave("results_plot.png",
        device = "png",
        height = 1000,
        width = 500,
        units = "mm",
        path = results_dir,
        limitsize = TRUE
    )

    ggplot2::ggplot(results_short_node_summary, ggplot2::aes(gene, abs_diff_per_gene)) +
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
    ggplot2::ggsave("results_short_node_summary.png",
        device = "png"
        # , height = 1500
        # , width = 297
        # , units = "mm"
        , path = results_dir,
        limitsize = TRUE
    )


    results_short_node_summary %>%
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
    ggplot2::ggsave("results_short_node_summary_abs_and_diff.png",
        device = "png"
        # , height = 1500
        # , width = 297
        # , units = "mm"
        , path = results_dir,
        limitsize = TRUE
    )


    results_short %>%
        tidyr::unite(label, group_vars, sep = "_") %>%
        ## need label to be able to seperate bars, otherwise ggplot groups
        ## automatically and only get one bar per gene
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
        ## See https://stackoverflow.com/a/44068137/10923234
        ggplot2::labs(
            title = "Mismatch between modelled and expected results",
            y = "Difference \n (mean model result - expected)",
            x = "Measured Gene"
        ) +
        ggplot2::coord_flip() +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "none")
    ggplot2::ggsave("results_per_pert_per_gene.png",
        device = "png"
        # , height = 1500
        # , width = 297
        # , units = "mm"
        , path = results_dir,
        limitsize = TRUE
    )


    ## End timestamp

    futile.logger::flog.info("End", name = log_file)
}
