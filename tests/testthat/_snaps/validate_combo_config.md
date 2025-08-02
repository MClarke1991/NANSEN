# validate_combo_config errors on missing config file

    Code
      validate_combo_config("nonexistent_config.toml")
    Condition
      Error in `validate_combo_config()`:
      ! Config file not found: nonexistent_config.toml

# validate_combo_config errors on invalid TOML

    Code
      validate_combo_config(invalid_toml_file)
    Condition
      Error in `validate_combo_config()`:
      ! Missing required config fields: netw_file_path, backgrounds_path, out_dir

# validate_combo_config errors on missing required fields

    Code
      validate_combo_config(config_file)
    Condition
      Error in `validate_combo_config()`:
      ! Missing required config fields: netw_file_path

---

    Code
      validate_combo_config(config_file)
    Condition
      Error in `validate_combo_config()`:
      ! Missing required config fields: backgrounds_path

---

    Code
      validate_combo_config(config_file)
    Condition
      Error in `validate_combo_config()`:
      ! Missing required config fields: out_dir

# validate_combo_config errors on missing referenced files

    Code
      validate_combo_config(config_file)
    Condition
      Error in `validate_combo_config()`:
      ! Network file not found: nonexistent_network.json

---

    Code
      validate_combo_config(config_file)
    Condition
      Error in `validate_combo_config()`:
      ! Backgrounds file not found: nonexistent_backgrounds.csv

