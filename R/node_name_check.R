## Copyright 2019 Matthew A. Clarke, Fisher Lab <matthewaclarke1991@gmail.com>

#' Check BMA node names
#'
#' Checks for disallowed characters in nodes used in an imported BMA network
#'
#' @param name name/id of a BMA node
#'
#' @return None
#'
#' @export
node_name_check <- function(name){
    if (grepl(" ", name)) {
        stop("Error: Node names cannot contain spaces, see node: ", "'", name, "'")
    } else if (grepl("__", name)) {
        stop("Error: Node names cannot contain double underscores (__), see node: ", "'", name, "'")
    } else if (grepl("Mut", name)) {
        stop("Error: Node names cannot contain 'Mut', see node: ", "'", name, "'")
    } else if (grepl("_cex", name)) {
        stop("Error: Node names cannot contain '_cex', see node: ", "'", name, "'")
    } else if (grepl(".json", name, fixed = TRUE)) {
        stop("Error: Node names cannot contain '.json', see node: ", "'", name, "'")
    } else if (grepl("PERT", name)) {
        stop("Error: Node names cannot contain 'PERT', see node: ", "'", name, "'")
    }
}
