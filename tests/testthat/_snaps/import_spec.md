# import_spec handles loserum option correctly

    Code
      result_loserum <- import_spec(spec_path = temp_spec, loserum = TRUE,
        clean_underscores = FALSE, netw_variables = netw_variables)
    Message
      value for "which" not specified, defaulting to c("rows", "cols")
      Rows: 2 Columns: 10
      -- Column specification --------------------------------------------------------
      Delimiter: ","
      chr (7): cell_line, paper_title, Paper DOI, source, experiment_overview, exp...
      dbl (1): perturbation
      lgl (2): expected_result_bma, notes
      
      i Use `spec()` to retrieve the full column specification for this data.
      i Specify the column types or set `show_col_types = FALSE` to quiet this message.

