#' read and write gson file
#'
#' @rdname IO
#' @param file A gson file
#'
#' @return A `GSON` instance
#' @importFrom jsonlite fromJSON
#' @importFrom stats setNames
#' @export
#'
#' @examples
#' wpfile <- system.file('extdata', "wikipathways-20220310-gmt-Homo_sapiens.gmt", package='gson')
#' x <- read.gmt.wp(wpfile, output = "GSON")
#' f = tempfile(fileext = '.gson')
#' write.gson(x, f)
#' read.gson(f)
read.gson <- function(file) {
  x <- jsonlite::fromJSON(file)
  gsid2gene <- stack(x$gsid2gene)
  gsid2gene <- setNames(gsid2gene[, c(2,1)], c("gsid", "gene"))
  #idx <- which(vapply(x, length, numeric(1)) == 0)
  #for (i in idx) {
  #  x[[i]] <- NULL
  #}


  gsid2name <- x$gsid2name
  if (!is.null(gsid2name)) {
    gsid2name <- as.data.frame(gsid2name)
  }
  
  gene2name <- x$gene2name
  if (length(gene2name) == 0) {
    gene2name <- NULL
  }
  if (!is.null(gene2name)) {
    gene2name <- as.data.frame(gene2name)
  }

  get_value <- function(value, x) {
    result <- x[[value]]
    if (length(result > 0)) {
      result <- as.character(result)
    } else {
      result <- NULL
    }
    result
  }

  gsname <- get_value("gsname", x)
  version <- get_value("version", x)
  accessed_date <- get_value("accessed_date", x)
  keytype <- get_value("keytype", x)
  info <- get_value("info", x)
  
  gson(gsid2gene = gsid2gene, gsid2name = gsid2name,
       gene2name = gene2name, species = x$species,
       gsname = gsname, version = version,
       accessed_date = accessed_date, 
       keytype = keytype,
       info = info)
}

##' @rdname IO
##' @param x A `GSON` instance
##' @importFrom utils packageVersion
##' @importFrom jsonlite toJSON
##' @export
write.gson <- function(x, file = "") {
  res <- jsonlite::toJSON(as.list.GSON(x), pretty = TRUE)
  if (file == "") return(res)

  ### for UTF-8 code error
  ##res <- iconv(res, "ASCII", "UTF-8") # may return NA
  
  # for lexical error: invalid character inside string.
  res <- gsub("\\t", " ", res)
  #info <- paste0("R package: gson v=",  packageVersion("gson"), ", ", Sys.Date())
  cat(res, file = file,  sep = "\n")
}


as.list.GSON <- function(x,  ...) {
  list(
    gsid2gene = split(x@gsid2gene$gene, x@gsid2gene$gsid),
    gsid2name =  as.list(x@gsid2name),
    gene2name = as.list(x@gene2name),
    species = x@species,
    gsname = x@gsname,
    version = x@version,
    accessed_date = x@accessed_date,
    keytype = x@keytype,
    info = x@info
  )
}


