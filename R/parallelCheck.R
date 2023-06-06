#' @title Parallel availability check
#' @description Check availability of parallel package and set parameters
#' @param parallel Logical, should parallel execution be used?
#' @param maxcores upper bound for self-selected number of cores
#' @param ncores number of cores used in parallel computation, self-selected number of cores
#'  is used when \code{is.null(ncpus)} (the default).
#' @details The function checks if package parallel is available. Then, it is checked whether the FORK nodes
#' can be initialized
#' @return A list with two elements:
#' \itemize{
#' \item \code{hasparallel}, a logical flag indicating if parallelization is enabled and
#' \item \code{cl}: parallel socket cluster object, or NULL.
#' }
#' @author J. Bedia, with contributions by J. Bhend and M. de Felice
#' @importFrom parallel detectCores makeCluster
#' @keywords internal
#' @export
#' @family parallel.helpers

parallelCheck <- function(parallel, max.ncores = 16, ncores = NULL) {
    hasparallel <- FALSE
    .cl <- NULL
    if (parallel & grepl("windows", .Platform$OS.type, ignore.case = TRUE)) {
        message("Parallelization is not supported on Windows machines")    
    } 
    if (parallel && requireNamespace("parallel", quietly = TRUE)) {
        max.avail <- min(max(parallel::detectCores() - 1, 1), max.ncores)
        if (is.null(ncores)) {
            ncores <- max.avail 
        }
        if (ncores > 1) {
            .cl <- try(parallel::makeCluster(ncores, type = 'FORK'), silent = TRUE)
            if (!"try-error" %in% class(.cl)) {
                hasparallel <- TRUE
                if (ncores > max.avail) {
                    warning("Maximum number of detected cores available is ",
                            max.avail, " (", ncores, " were selected)",
                            call. = FALSE)
                    ncores <- max.avail
                }
                message("Parallel computing enabled\nNumber of workers: ", ncores)
            } else {
                .cl <- NULL
            }
        }
    } else if (parallel) {
        message("Parallel computing disabled (parallel package is not available)")
    }
    list("hasparallel" = hasparallel, "cl" = .cl)
}
