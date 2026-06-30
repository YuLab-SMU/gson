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
  #x <- jsonlite::fromJSON(file)
  x <- yread(file, jsonlite::fromJSON)
  gsid2gene <- stack(x$gsid2gene)
  gsid2gene <- setNames(gsid2gene[, c(2,1)], c("gsid", "gene"))
  gsid2gene$gsid <- as.character(gsid2gene$gsid)
  gsid2gene$gene <- as.character(gsid2gene$gene)

  gsid2name <- get_gson_data_frame("gsid2name", x)
  gene2name <- get_gson_data_frame("gene2name", x)

  species <- get_value("species", x)
  gsname <- get_value("gsname", x)
  version <- get_value("version", x)
  accessed_date <- get_value("accessed_date", x)
  keytype <- get_value("keytype", x)
  urlpattern <- get_value("urlpattern", x)
  info <- get_value("info", x)
  
  gson(gsid2gene = gsid2gene, gsid2name = gsid2name,
       gene2name = gene2name, species = species,
       gsname = gsname, version = version,
       accessed_date = accessed_date, 
       keytype = keytype,
       urlpattern = urlpattern,
       info = info)
}

##' @rdname IO
##' @param x A `GSON` instance
##' @importFrom utils packageVersion
##' @importFrom jsonlite toJSON
##' @export
write.gson <- function(x, file = "") {
  res <- jsonlite::toJSON(as.list.GSON(x), pretty = TRUE, null = "null")
  if (file == "") return(res)

  ### for UTF-8 code error
  ##res <- iconv(res, "ASCII", "UTF-8") # may return NA
  
  # for lexical error: invalid character inside string.
  res <- gsub("\\t", " ", res)
  #info <- paste0("R package: gson v=",  packageVersion("gson"), ", ", Sys.Date())
  cat(res, file = file,  sep = "\n")
}

##' @method as.list GSON
##' @export
as.list.GSON <- function(x,  ...) {
  list(
    gsid2gene = split(x@gsid2gene$gene, x@gsid2gene$gsid),
    gsid2name =  data_frame_to_list(x@gsid2name),
    gene2name = data_frame_to_list(x@gene2name),
    species = x@species,
    gsname = x@gsname,
    version = x@version,
    accessed_date = x@accessed_date,
    keytype = x@keytype,
    urlpattern = x@urlpattern,
    info = x@info
  )
}

data_frame_to_list <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  as.list(x)
}

is_empty_json_value <- function(x) {
  is.null(x) || (is.list(x) && length(x) == 0)
}

get_gson_data_frame <- function(value, x) {
  result <- x[[value]]
  if (is_empty_json_value(result)) {
    return(NULL)
  }
  as.data.frame(result)
}

get_value <- function(value, x) {
  result <- x[[value]]
  if (is_empty_json_value(result) || length(result) == 0) {
    return(NULL)
  }
  as.character(result)
}

