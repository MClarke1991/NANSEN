# import_spec throws error for non-integer perturbation values

    Code
      import_spec(spec_path = temp_spec, loserum = FALSE, clean_underscores = FALSE,
        netw_variables = netw_variables)
    Condition
      Error:
      ! Column 'perturbation' in spec file contains non-integer values. Gene/node activity levels must be integers. Consider using round() or as.integer() to convert values. Found 3 non-integer value(s) with 3 unique value(s): 1.5, 0.7, 2.3 at position(s): 1, 2, 3

# import_spec throws error for non-integer expected_result_bma values

    Code
      import_spec(spec_path = temp_spec, loserum = FALSE, clean_underscores = FALSE,
        netw_variables = netw_variables)
    Condition
      Error:
      ! Column 'expected_result_bma' in spec file contains non-integer values. Gene/node activity levels must be integers. Consider using round() or as.integer() to convert values. Found 3 non-integer value(s) with 3 unique value(s): 1.5, 0.7, 2.3 at position(s): 1, 2, 3

