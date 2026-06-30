#' construct a 'GSONList' object
#'
#' @param ... input GSON objects
#' @return A 'GSONList' instance
#' @export
gsonList <- function(...) {
    obj <- list(...)
    names(obj) <- vapply(seq_along(obj), function(i) {
        gsname <- obj[[i]]@gsname
        if (is.null(gsname) || length(gsname) == 0 || is.na(gsname[1]) || gsname[1] == "") {
            return(paste0("GSON", i))
        }
        gsname[1]
    }, character(1))
    class(obj) <- c("GSONList", "list")
    return(obj)
}

