## Copyright 2022 Matthew A. Clarke, Fisher Lab <matthewaclarke1991@gmail.com>

#' Validate combo configuration from TOML file
#' @title validate_combo_config
#' @param config_path path to TOML configuration file
#' @return list with validated configuration parameters
#' @export
validate_combo_config <- function(config_path) {
    
    if (!file.exists(config_path)) {
        stop(paste("Config file not found:", config_path))
    }
    
    tryCatch({
        config <- configr::read.config(config_path, file.type = "toml")
    }, error = function(e) {
        stop(paste("Invalid TOML in config file:", e$message))
    })
    
    required_fields <- c("netw_file_path", "backgrounds_path", "out_dir")
    missing_fields <- setdiff(required_fields, names(config))
    
    if (length(missing_fields) > 0) {
        stop(paste("Missing required config fields:", paste(missing_fields, collapse = ", ")))
    }
    
    if (!file.exists(config$netw_file_path)) {
        stop(paste("Network file not found:", config$netw_file_path))
    }
    
    if (!file.exists(config$backgrounds_path)) {
        stop(paste("Backgrounds file not found:", config$backgrounds_path))
    }
    
    defaults <- list(
        drug_path = NA,
        combo_exclusions_path = NA,
        spec_path = NA,
        project_path = "",
        phenotypes = c("output_a", "output_b"),
        palettes = list("GnBu", "YlOrRd"),
        missing_nodes_perturbed_overide = FALSE,
        missing_nodes_expected_overide = FALSE,
        pheno_only = TRUE,
        use_exclusions = FALSE,
        skip_autopert = FALSE,
        skip_combo_sim = FALSE,
        skip_all_pairs = FALSE,
        skip_combo_drugs_single = TRUE,
        skip_combo_drugs_double = TRUE,
        skip_heatmaps = FALSE,
        skip_heatmaps_uc = FALSE,
        nosat = TRUE,
        loserum = FALSE,
        node_col_name = "node",
        use_vmcai = TRUE,
        background_order = c("cancer", "wt"),
        w_s_node = 4,
        h_s_node = 12.5,
        single_fontsize = 12,
        w_s_druggable = 4,
        h_s_druggable = 7,
        single_druggable_fontsize = 10,
        w_s_drug = 4,
        h_s_drug = 7,
        single_drugs_fontsize = 5,
        w_d_node = 12.5,
        h_d_node = 12.5,
        double_fontsize = 10,
        w_d_druggable = 12.5,
        h_d_druggable = 12.5,
        double_druggable_font_size = 14,
        w_d_drug = 12.5,
        h_d_drug = 12.5,
        double_drugs_font_size = 14,
        node_heat_dir = "node_heatmaps",
        druggable_heat_dir = "druggable_heatmaps",
        drug_heat_dir = "drug_heatmaps",
        node_heat_dir_uc = "unclust_node_heatmaps",
        druggable_heat_dir_uc = "unclust_druggable_heatmaps",
        drug_heat_dir_uc = "unclust_drug_heatmaps",
        drug_conflict_overide = FALSE,
        pipe_dir = "combo_results",
        log_filename = "PipeLog.log"
    )
    
    for (param in names(defaults)) {
        if (!(param %in% names(config))) {
            config[[param]] <- defaults[[param]]
        }
    }
    
    
    return(config)
}