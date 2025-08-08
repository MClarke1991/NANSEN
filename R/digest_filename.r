## Copyright 2024 Matthew A. Clarke, Fisher Lab, UCL <matthew.clarke@ucl.ac.uk>

##' Use MD5 hash to create a unique short filename for long
##' perturbations to avoid long paths causing issues on Windows
##'
##' @title digest_filename
##' @param filename filename
##' @param append_json Add ".json" after digesting
##' @return shortened filename

digest_filename <- function(filename, append_json = FALSE) {
    dg <- digest::digest(filename, algo = "md5")

    if (append_json) {
        dg_filename <- paste0(dg, ".json")
    } else {
        dg_filename <- dg
    }

    return(dg_filename)
}
