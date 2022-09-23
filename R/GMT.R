##' parse gmt file to a data.frame
##'
##'
##' @title read.gmt
##' @rdname read-gmt
##' @param gmtfile gmt file
##' @importFrom utils stack
##' @export
##' @return data.frame
##' @author Guangchuang Yu
read.gmt <- function(gmtfile) {
  ## getGmt <- get_fun_from_pkg("GSEABase", "getGmt")
  ## geneIds <- get_fun_from_pkg("GSEABase", "geneIds")
  
  ## gmt <- getGmt(con=gmtfile)
  ## ont2gene <- geneIds(gmt) %>% stack
  ## ont2gene <- ont2gene[, c("ind", "values")]
  ## colnames(ont2gene) <- c("ont", "gene")
  
  x <- readLines(gmtfile)
  ## ont2gene <- lapply(x, function(record) {
  ##     y = strsplit(record, "\t")[[1]]
  ##     data.frame(ont=y[1], gene=y[-c(1:2)])
  ## }) %>% do.call('rbind', .)
  
  
  ## first column: gene set name
  ## second column: description
  ## all the others, unequal length for genes
  res <- strsplit(x, "\t")
  names(res) <- vapply(res, function(y) y[1], character(1))
  res <- lapply(res, "[", -c(1:2))
  
  ont2gene <- stack(res)
  ont2gene <- ont2gene[, c("ind", "values")]
  colnames(ont2gene) <- c("term", "gene")
  return(ont2gene)
}

##' @rdname read-gmt
##' @param output one of 'data.frame' or 'GSON'
##' @importFrom rlang .data
##' @importFrom tidyr separate
##' @export
read.gmt.wp <- function(gmtfile, output = "data.frame") {
  output <- match.arg(output, c("data.frame", "gson", "GSON"))
  x <- read.gmt(gmtfile)
  x <- tidyr::separate(x, .data$term, c("name","version","wpid","species"), "%")
  if (output == "data.frame") {
    return(x)
  }
  
  gsid2gene <- data.frame(gsid=x$wpid, gene=x$gene)
  gsid2name <- unique(data.frame(gsid=x$wpid, name=x$name))
  species <- unique(x$species)
  version <- unique(x$version)
  gson(gsid2gene = gsid2gene, 
      gsid2name = gsid2name, 
      gsname = "WikiPathways",
      species = species, 
      version = version)
}
