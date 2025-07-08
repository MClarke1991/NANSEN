#' Normalize BMA Path for Cross-Platform Compatibility
#'
#' This function handles the change in R's path handling on Windows.
#' It first tests if the provided path exists, and if not, converts
#' the old Windows path format to the new format.
#'
#' @param bma_path Character string. Path to the BMA executable.
#' @return Character string. The normalized path that exists on the system.
#' @export
#'
normalize_bma_path <- function(bma_path) {
    if (file.exists(bma_path)) {
        return(bma_path)
    } else {
        # Convert old format to new format
        # Remove extra escaping: \\" -> "
        new_path <- gsub('\\\\"', '"', bma_path)
        # Convert backslashes to forward slashes: \\ -> /
        new_path <- gsub('\\\\', '/', new_path)
        
        if (file.exists(new_path)) {
            return(new_path)
        } else {
            stop("BMA executable not found at: ", bma_path, " or ", new_path)
        }
    }
}