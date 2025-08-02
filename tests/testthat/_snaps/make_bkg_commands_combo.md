# make_bkg_commands_combo errors when node_col missing from backgrounds

    Code
      make_bkg_commands_combo(backgrounds, netw_variables)
    Condition
      Error in `make_bkg_commands_combo()`:
      ! Error: It looks like the name of the column with node names, set as `node_col`, is not present in 'backgrounds' or 'netw_variables'. If you have renamed it make sure it is consistent between the two arguments and that you supply it with the `node_col` argument

# make_bkg_commands_combo errors when node_col missing from netw_variables

    Code
      make_bkg_commands_combo(backgrounds, netw_variables)
    Condition
      Error in `make_bkg_commands_combo()`:
      ! Error: It looks like the name of the column with node names, set as `node_col`, is not present in 'backgrounds' or 'netw_variables'. If you have renamed it make sure it is consistent between the two arguments and that you supply it with the `node_col` argument

# make_bkg_commands_combo throws error for non-integer activity values

    Code
      make_bkg_commands_combo(backgrounds, netw_variables)
    Condition
      Error:
      ! Column 'activity' in background file contains non-integer values. Gene/node activity levels must be integers. Consider using round() or as.integer() to convert values. Found 3 non-integer value(s) with 3 unique value(s): 0.5, 1.2, 2.7 at position(s): 1, 2, 3

# make_bkg_commands_combo handles mix of valid and invalid activity values

    Code
      make_bkg_commands_combo(backgrounds, netw_variables)
    Condition
      Error:
      ! Column 'activity' in background file contains non-integer values. Gene/node activity levels must be integers. Consider using round() or as.integer() to convert values. Found 1 non-integer value(s) with 1 unique value(s): 2.5 at position(s): 2

