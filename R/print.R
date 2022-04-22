##' show method for `GSON` instance
##'
##' @name show
##' @docType methods
##' @rdname show-methods
##'
##' @title show method
##' @param object A `GSON` object
##' @return message
##' @importFrom methods show
##' @exportMethod show
##' @usage show(object)
##' @author Guangchuang Yu \url{https://yulab-smu.top}
setMethod("show", signature(object="GSON"),
          function (object){
            print.GSON(object)
          }
)


##' @method print GSON
##' @importFrom glue glue
##' @export
print.GSON <- function(x, ...) {
  ngs <- length(unique(x@gsid2gene$gsid))
  ng <- length(unique(x@gsid2gene$gene))
  species <- x@species
  version <- x@version
  glue::glue(
    ">> {ng} genes annotated by {ngs} gene sets.\n",
    ">> Species: {species}\n", 
    ">> Version: {version}"
  )
}
