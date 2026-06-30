#' Validate a GSON object
#'
#' `validate_gson()` checks the core data contract of a `GSON` gene set
#' collection. It is useful after constructing an object manually or reading one
#' from an external source.
#'
#' @param x A `GSON` object.
#' @param error Logical. If `TRUE`, throw an error when validation fails. If
#'   `FALSE`, return a character vector of validation messages.
#'
#' @return `TRUE` if the object is valid. If `error = FALSE`, returns a
#'   character vector of validation messages when invalid.
#' @export
#'
#' @examples
#' x <- gson(data.frame(gsid = "GS1", gene = "gene1"))
#' validate_gson(x)
validate_gson <- function(x, error = TRUE) {
    issues <- character()

    if (!inherits(x, "GSON")) {
        issues <- c(issues, "x should be a GSON object")
        return_gson_validation(issues, error)
    }

    issues <- c(issues, validate_required_table(x@gsid2gene, c("gsid", "gene"), "gsid2gene"))

    if (length(issues) == 0) {
        gsid2gene <- x@gsid2gene
        if (anyDuplicated(gsid2gene[, c("gsid", "gene"), drop = FALSE]) > 0) {
            issues <- c(issues, "gsid2gene contains duplicate gsid-gene memberships")
        }
    }

    if (!is.null(x@gsid2name)) {
        issues <- c(issues, validate_required_table(x@gsid2name, c("gsid", "name"), "gsid2name"))
        if (length(setdiff(x@gsid2name$gsid, x@gsid2gene$gsid)) > 0) {
            issues <- c(issues, "gsid2name contains gsid values not found in gsid2gene")
        }
        if (anyDuplicated(x@gsid2name$gsid) > 0) {
            issues <- c(issues, "gsid2name contains duplicate gsid values")
        }
    }

    if (!is.null(x@gene2name)) {
        issues <- c(issues, validate_required_table(x@gene2name, c("gene", "name"), "gene2name"))
        if (length(setdiff(x@gene2name$gene, x@gsid2gene$gene)) > 0) {
            issues <- c(issues, "gene2name contains gene values not found in gsid2gene")
        }
        if (anyDuplicated(x@gene2name$gene) > 0) {
            issues <- c(issues, "gene2name contains duplicate gene values")
        }
    }

    if (length(x@schema_version) != 1 || is.na(x@schema_version) || x@schema_version == "") {
        issues <- c(issues, "schema_version should be a non-empty length 1 character value")
    }

    for (slot_name in c("species", "gsname", "version", "accessed_date", "keytype", "urlpattern", "info")) {
        value <- slot(x, slot_name)
        if (!is.null(value) && length(value) > 1) {
            issues <- c(issues, paste0(slot_name, " should be length 1 or NULL"))
        }
    }

    return_gson_validation(issues, error)
}

validate_required_table <- function(x, cols, arg) {
    issues <- character()
    if (!is.data.frame(x)) {
        return(paste0(arg, " should be a data.frame"))
    }
    missing_cols <- setdiff(cols, colnames(x))
    if (length(missing_cols) > 0) {
        issues <- c(issues, paste0(arg, " is missing required columns: ", paste(missing_cols, collapse = ", ")))
        return(issues)
    }
    if (nrow(x) == 0) {
        issues <- c(issues, paste0(arg, " should contain at least one row"))
    }
    for (col in cols) {
        value <- x[[col]]
        if (any(is.na(value)) || any(as.character(value) == "")) {
            issues <- c(issues, paste0(arg, "$", col, " should not contain missing or empty values"))
        }
    }
    issues
}

return_gson_validation <- function(issues, error) {
    if (length(issues) == 0) {
        return(TRUE)
    }
    if (isTRUE(error)) {
        stop(paste(issues, collapse = "\n"), call. = FALSE)
    }
    issues
}
