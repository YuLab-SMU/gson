
#' construct a 'GSON' object
#'
#' @param gsid2gene A data frame with first column of gene set IDs and second column of genes
#' @param gsid2name A data frame with first column of gene set IDs and second column of gene set names
#' @param gene2name A data frame with first column of genes and second column of gene symbols
#' @param species Which species of the genes belongs to
#' @param gsname Name of the gene set (e.g., GO, KEGG, etc.)
#' @param version version of the gene set
#' @param accessed_date date to obtain the gene set data
#' @param keytype keytype of genes
#' @param info extra information
#'
#' @return A 'GSON' instance
#' @importFrom methods new
#' @export
#'
#' @examples
#' wpfile <- system.file('extdata', "wikipathways-20220310-gmt-Homo_sapiens.gmt", package='gson')
#' x <- read.gmt.wp(wpfile)
#' gsid2gene <- data.frame(gsid=x$wpid, gene=x$gene)
#' gsid2name <- unique(data.frame(gsid=x$wpid, name=x$name))
#' species <- unique(x$species)
#' version <- unique(x$version)
#' gson(gsid2gene=gsid2gene, gsid2name=gsid2name, species=species, version=version)
gson <- function(gsid2gene, gsid2name = NULL, gene2name = NULL,
                 species = NULL, gsname = NULL, version = NULL,
                 accessed_date = NULL, keytype = NULL, info = NULL) {

    new("GSON",
        gsid2gene = gsid2gene,
        gsid2name = gsid2name,
        gene2name = gene2name,
        species = species,
        gsname = gsname,
        version = version,
        accessed_date = accessed_date,
        info = info,
        keytype = keytype)
}

