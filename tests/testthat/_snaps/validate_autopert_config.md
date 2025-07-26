# validate_autopert_config errors on missing config file

    Code
      validate_autopert_config("nonexistent_config.json")
    Condition
      Error in `validate_autopert_config()`:
      ! Config file not found: nonexistent_config.json

# validate_autopert_config errors on invalid JSON

    Code
      validate_autopert_config(invalid_json_file)
    Condition
      Error in `value[[3L]]()`:
      ! Invalid JSON in config file: lexical error: invalid char in json text.
                                           { invalid json  
                           (right here) ------^

# validate_autopert_config errors on missing required fields

    Code
      validate_autopert_config(config_file)
    Condition
      Error in `validate_autopert_config()`:
      ! Missing required config fields: netw_file_path

---

    Code
      validate_autopert_config(config_file)
    Condition
      Error in `validate_autopert_config()`:
      ! Missing required config fields: spec_path

---

    Code
      validate_autopert_config(config_file)
    Condition
      Error in `validate_autopert_config()`:
      ! Missing required config fields: out_dir

# validate_autopert_config errors on missing referenced files

    Code
      validate_autopert_config(config_file)
    Condition
      Error in `validate_autopert_config()`:
      ! Network file not found: nonexistent_network.json

---

    Code
      validate_autopert_config(config_file)
    Condition
      Error in `validate_autopert_config()`:
      ! Specification file not found: nonexistent_spec.csv

