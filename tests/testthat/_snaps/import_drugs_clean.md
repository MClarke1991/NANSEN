# import_drugs_clean throws error for non-integer activity values

    Code
      import_drugs_clean(temp_file, show_col_types = FALSE)
    Condition
      Error:
      ! Column 'activity' in drug file contains non-integer values. Gene/node activity levels must be integers. Consider using round() or as.integer() to convert values. Found 3 non-integer value(s) with 3 unique value(s): 0.5, 1.2, 2.8 at position(s): 1, 2, 3

# import_drugs_clean handles mix of valid and invalid activity values

    Code
      import_drugs_clean(temp_file, show_col_types = FALSE)
    Condition
      Error:
      ! Column 'activity' in drug file contains non-integer values. Gene/node activity levels must be integers. Consider using round() or as.integer() to convert values. Found 1 non-integer value(s) with 1 unique value(s): 2.5 at position(s): 2

