# make_command_args handles missing columns

    Code
      make_command_args(test_df)
    Condition
      Error in `make_command_args()`:
      ! Column id must be numeric. Found values: 

# make_command_args throws error for non-integer activity values

    Code
      make_command_args(test_df_non_integer)
    Condition
      Error:
      ! Column 'activity' in activity data contains non-integer values. Gene/node activity levels must be integers. Consider using round() or as.integer() to convert values. Found 3 non-integer value(s) with 3 unique value(s): 0.5, 1.2, 2.7 at position(s): 1, 2, 3

# make_command_args handles mix of valid and invalid activity values

    Code
      make_command_args(test_df_mixed)
    Condition
      Error:
      ! Column 'activity' in activity data contains non-integer values. Gene/node activity levels must be integers. Consider using round() or as.integer() to convert values. Found 1 non-integer value(s) with 1 unique value(s): 2.5 at position(s): 2

