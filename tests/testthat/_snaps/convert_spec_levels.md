# convert_spec_levels throws error when numeric input contains non-integers

    Code
      convert_spec_levels(spec, log_file)
    Condition
      Error:
      ! Column 'perturbation_bma' in converted spec values contains non-integer values. Gene/node activity levels must be integers. Consider using round() or as.integer() to convert values. Found 2 non-integer value(s) with 2 unique value(s): 1.5, 2.7 at position(s): 1, 2

