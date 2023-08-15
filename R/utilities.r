set_gson <- function(gson, slot, value) {
    slot(gson, slot) <- value
    return(gson)
}

#' @importFrom utils browseURL
#' @importFrom methods slot<-
browseGS <- function(gson, gsid, ...) {
    url <- sub(gson@urlpattern, pattern = "\\{gsid\\}", replacement=gsid)
    browseURL(url, ...)
}
