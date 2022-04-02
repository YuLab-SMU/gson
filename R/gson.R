
## clusterProfiler::read.gmt.wp("wikipathways-20220310-gmt-Homo_sapiens.gmt") -> x

## gsid2gene = data.frame(gsid=x$wpid, gene=x$gene)
## gsid2name = unique(data.frame(gsid=x$wpid, name=x$name))
## species = unique(x$species)
## version=unique(x$version)

## y = gson(gsid2gene=gsid2gene, gsid2name=gsid2name, species=species, version=version)


## write.gson(y, file="~/wk.gson")
## read.gson("~/wk.gson")


gson <- function(gsid2gene, gsid2name = NULL, gene2name = NULL,
                 species = NULL, gsname = NULL, version = NULL,
                 accessed_date = NULL, info = NULL) {

    new("GSON",
        gsid2gene = gsid2gene,
        gsid2name = gsid2name,
        gene2name = gene2name,
        species = species,
        gsname = gsname,
        version = version,
        accessed_date = accessed_date,
        info = info)
}


write.gson <- function(x, file = "") {
    res <- jsonlite::toJSON(as.list(x), pretty = TRUE)
    if (file == "") return(res)

    info <- paste0("R package: gson v=",  packageVersion("gson"), ", ", Sys.Date())
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
        info = x@info
    )
}


read.gson <- function(file) {
    x <- jsonlite::fromJSON(file)
    gsid2gene <- stack(x$gsid2gene)
    gsid2gene <- gsid2gene[, c(2,1)]
    names(gsid2gene) <- c("gsid", "gene")

    idx <- which(vapply(x, length, numeric(1)) == 0)
    for (i in idx) {
        x[[i]] <- NULL
    }

    gsid2name <- x@gsid2name
    if (!is.null(gsid2name)) {
        gsid2name <- as.data.frame(gsid2name)
    }

    gene2name <- x@gene2name
    if (!is.null(gene2name)) {
        gene2name <- as.data.frame(gene2name)
    }

    gson(gsid2gene = gsid2name, gsid2name = gsid2name,
         gene2name = gene2name, species = x$species,
         gsname = x$gsname, version = x$version,
         accessed_date = x$accessed_date, info = x$info)
}

