## Copyright 2022 Matthew A. Clarke, Fisher Lab <matthewaclarke1991@gmail.com>

#' Validate autopert configuration from TOML file
#' @title validate_autopert_config
#' @param config_path path to TOML configuration file
#' @return list with validated configuration parameters
#' @export
validate_autopert_config <- function(config_path) {
    
    if (!file.exists(config_path)) {
        stop(paste("Config file not found:", config_path))
    }
    
    tryCatch({
        config <- configr::read.config(config_path, file.type = "toml")
    }, error = function(e) {
        stop(paste("Invalid TOML in config file:", e$message))
    })
    
    required_fields <- c("netw_file_path", "spec_path", "out_dir")
    missing_fields <- setdiff(required_fields, names(config))
    
    if (length(missing_fields) > 0) {
        stop(paste("Missing required config fields:", paste(missing_fields, collapse = ", ")))
    }
    
    if (!file.exists(config$netw_file_path)) {
        stop(paste("Network file not found:", config$netw_file_path))
    }
    
    if (!file.exists(config$spec_path)) {
        stop(paste("Specification file not found:", config$spec_path))
    }
    
    defaults <- list(
        nosat = TRUE,
        loserum = FALSE,
        missing_nodes_perturbed_overide = FALSE,
        missing_nodes_expected_overide = FALSE,
        project_path = NA,
        group_vars = c("source", "cell_line", "experiment_particular")
    )
    
    for (param in names(defaults)) {
        if (!(param %in% names(config))) {
            config[[param]] <- defaults[[param]]
        }
    }
    
    
    return(config)
}