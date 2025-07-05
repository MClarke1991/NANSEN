## Copyright 2022 Matthew A. Clarke, Fisher Lab <matthewaclarke1991@gmail.com>

##' Test whether separate experiments have the same grouping factor,
##' and so would be run together in spec testing and possibly give
##' misleading results
##'
##' I expect all rows of an experiment to be together. Therefore the
##' group index for each row should appear all in a group, and at most
##' once. So first I remove all consecutive duplicates, but not
##' non-consecutive duplicates i.e. 3 3 3 4 4 4 3 3 3 becomes 3 4
##' 3. If there is a non-consecutive duplication, this suggests two
##' seperate experiments share a group key, and the spec should be
##' ammended either a) to put all experimental specification for a
##' single experiment on consecutive rows, or b to change the
##' experiment_particular to make clear the difference between the two
##' experiments. So I remove consecutive dupes, find the remaining
##' dupes, match this to the group keys to give a descriptive error
##' message of where the duplications are, and throw and error to
##' force this to be fixed.
##'
##' @title check_spec_groups
##' @param spec specification loaded through \code{\link{import_spec}}
##' @param group_vars columns of specification used to group experiments
##' @return Stop if specification grouping likely to misgroup experiments
##' @export
check_spec_groups <- function(spec, group_vars) {

    gspec <- spec %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) # https://stackoverflow.com/a/66253244/10923234

    group_indices <- gspec %>%
        dplyr::group_indices()

    group_keys <- gspec %>%
        dplyr::group_keys() %>%
        tibble::rowid_to_column() %>%
        dplyr::rename("index" = "rowid")

    ## I expect all rows of an experiment to be together. Therefore
    ## the group index for each row should appear all in a group, and
    ## at most once. So first I remove all consecutive duplicates, but
    ## not non-consecutive duplicates i.e. 3 3 3 4 4 4 3 3 3 becomes 3
    ## 4 3. If there is a non-consecutive duplication, this suggests
    ## two seperate experiments share a group key, and the spec should
    ## be ammended either a) to put all experimental specification for
    ## a single experiment on consecutive rows, or b to change the
    ## experiment_particular to make clear the difference between the
    ## two experiments.

    ## For removal of consecutive duplicates see
    ## https://stackoverflow.com/a/27482914/10923234

    group_indices_non_consec <- group_indices %>%
        tibble::as_tibble() %>%
        dplyr::filter(value!= dplyr::lag(value, default = 1))

    duplicates <- janitor::get_dupes(group_indices_non_consec, value) %>%
        ## select(value) %>%
        dplyr::distinct() %>%
        dplyr::left_join(group_keys, by = c("value" = "index"))

    if(nrow(duplicates) > 0) {
    stop("Unique experiments are expected to be grouped such that they are in a block of consecutive rows, with a unique combination of the columns 'source', 'cell_line' and 'experiment_particular'. In the provided specification the combination of these columns repeats in non-consecutive blocks of rows, suggesting non-unique descriptions of separate experiments, or a single experiment that has been split. Please move all rows of a single experiment to be together, or rename these columns such that different experiments are uniquely identified. The repeated descriptions are:\n",
         paste(capture.output(duplicates), collapse = "\n"),
         call. = FALSE)
    }
}
