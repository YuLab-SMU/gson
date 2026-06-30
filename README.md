# gson

`gson` provides a lightweight container and exchange format for gene set
collections. A `GSON` object stores gene set membership together with gene set
names, gene identifiers, species, version, source, and URL metadata so the same
collection can be reused across enrichment analysis and related workflows.

The core table is `gsid2gene`, a long data frame with one row per gene set-gene
membership. Optional tables map gene set IDs to readable names (`gsid2name`) and
gene IDs to readable gene names (`gene2name`).

## Installation

```r
remotes::install_github("YuLab-SMU/gson")
```

## Create a Gene Set Collection

```r
library(gson)

gsid2gene <- data.frame(
    gsid = c("GS1", "GS1", "GS2"),
    gene = c("101", "102", "103")
)

gsid2name <- data.frame(
    gsid = c("GS1", "GS2"),
    name = c("cell cycle", "immune response")
)

x <- gson(
    gsid2gene = gsid2gene,
    gsid2name = gsid2name,
    species = "Homo sapiens",
    gsname = "example",
    version = "2026-06-30",
    keytype = "ENTREZID"
)

x
```

## Read and Write GSON

```r
f <- tempfile(fileext = ".gson")

write.gson(x, f)
y <- read.gson(f)
```

The `.gson` file is JSON, so it can be inspected, archived, and passed between
tools without an R-specific binary format.

## Read GMT

```r
gmt <- system.file(
    "extdata",
    "wikipathways-20220310-gmt-Homo_sapiens.gmt",
    package = "gson"
)

wp <- read.gmt.wp(gmt, output = "GSON")
wp
```

For generic GMT files, use `read.gmt()` to obtain a long data frame with
`term` and `gene` columns, then construct a `GSON` object after mapping the term
column to the gene set identifiers used by your workflow.

## Use with Enrichment Analysis

Many enrichment tools need a two-column gene set membership table. A `GSON`
object keeps that table together with the metadata needed to interpret the
result:

```r
term2gene <- x@gsid2gene
term2name <- x@gsid2name
```

Keeping `keytype`, `species`, `version`, and source metadata with the membership
table makes enrichment results easier to reproduce and compare.

## Data Contract

A `GSON` object represents one gene set collection.

- `gsid2gene` is required and contains `gsid` and `gene`.
- `gsid2name` is optional and contains `gsid` and `name`.
- `gene2name` is optional and contains `gene` and `name`.
- `species`, `gsname`, `version`, `accessed_date`, `keytype`, `urlpattern`, and
  `info` describe the source and interpretation of the collection.

