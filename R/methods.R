#' Coerce GSON to a data frame
#'
#' @param x A `GSON` object.
#' @param row.names Unused.
#' @param optional Unused.
#' @param ... Unused.
#'
#' @return A data frame with gene set-gene memberships and optional names.
#' @method as.data.frame GSON
#' @export
as.data.frame.GSON <- function(x, row.names = NULL, optional = FALSE, ...) {
    res <- x@gsid2gene
    if (!is.null(x@gsid2name)) {
        res <- merge(res, x@gsid2name, by = "gsid", all.x = TRUE, sort = FALSE)
    }
    if (!is.null(x@gene2name)) {
        gene2name <- x@gene2name
        colnames(gene2name) <- c("gene", "gene_name")
        res <- merge(res, gene2name, by = "gene", all.x = TRUE, sort = FALSE)
        res <- res[, c(setdiff(colnames(res), c("gene", "gene_name")), "gene", "gene_name"), drop = FALSE]
    }
    rownames(res) <- NULL
    res
}

#' @method length GSON
#' @export
length.GSON <- function(x) {
    length(unique(x@gsid2gene$gsid))
}

#' @method names GSON
#' @export
names.GSON <- function(x) {
    unique(x@gsid2gene$gsid)
}

#' Extract genes from a GSON object
#'
#' @param x A `GSON` object.
#' @param i A gene set ID or numeric index.
#' @param ... Unused.
#'
#' @return A character vector of genes.
#' @rdname extract.GSON
#' @method [[ GSON
#' @export
`[[.GSON` <- function(x, i, ...) {
    gsid <- names(x)
    if (is.numeric(i)) {
        i <- gsid[[i]]
    }
    if (length(i) != 1) {
        stop("i should select exactly one gene set", call. = FALSE)
    }
    if (!i %in% gsid) {
        stop("gene set not found: ", i, call. = FALSE)
    }
    x@gsid2gene$gene[x@gsid2gene$gsid == i]
}
