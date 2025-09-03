# split_combo_results handles missing files

    Code
      split_combo_results(results_prefix = "COMBO_RUN", project_path = "", out_dir = "nonexistent_dir",
        netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
        drug_path = NA, node_col_name = "node")
    Condition
      Error:
      ! 'NANSEN/nonexistent_dir/COMBO_RUN_helper_combo_1/processed_results.csv' does not exist.

# split_combo_results handles invalid network file

    Code
      split_combo_results(results_prefix = "COMBO_RUN", project_path = "", out_dir = test_dir,
        netw_file_path = "nonexistent_network.json", drug_path = NA, node_col_name = "node")
    Condition
      Error:
      ! 'NANSEN/tests/testthat/temp_test_outputs/split_combo_test_invalid_network/COMBO_RUN_nonexistent_network/processed_results.csv' does not exist.

# split_combo_results handles invalid drug file

    Code
      split_combo_results(results_prefix = "COMBO_RUN", project_path = "", out_dir = test_dir,
        netw_file_path = here::here("examples", "combo", "helper_combo_1.json"),
        drug_path = "nonexistent_drugs.csv", node_col_name = "node")
    Condition
      Error:
      ! 'nonexistent_drugs.csv' does not exist in current working directory ('NANSEN/tests/testthat').

