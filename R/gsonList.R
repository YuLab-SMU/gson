#' construct a 'GSONList' object
#'
#' @param ... input GSON objects
#' @return A 'GSONList' instance
#' @export
gsonList <- function(...) {
    obj <- list(...)
    names(obj) <- vapply(obj, function(g) g@gsname, character(1))
    class(obj) <- c("GSONList", "list")
    return(obj)
}

