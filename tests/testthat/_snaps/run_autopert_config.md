# run_autopert_config.r handles no command line arguments

    Code
      mock_script_with_args(character(0))
    Condition
      Error:
      ! Usage: Rscript run_autopert_config.r <config_file_path>

# run_autopert_config.r handles multiple command line arguments

    Code
      mock_script_with_args(c("arg1", "arg2"))
    Condition
      Error:
      ! Usage: Rscript run_autopert_config.r <config_file_path>

# run_autopert_config.r handles nonexistent config file

    Code
      mock_script_with_args("nonexistent_config.json")
    Condition
      Error:
      ! Usage: Rscript run_autopert_config.r <config_file_path>

# run_autopert_config.r handles invalid config file

    Code
      mock_script_with_args(invalid_config_file)
    Condition
      Error:
      ! Usage: Rscript run_autopert_config.r <config_file_path>

