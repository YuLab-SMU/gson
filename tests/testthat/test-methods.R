test_that("GSON exposes basic container methods", {
  x <- gson(
    gsid2gene = data.frame(gsid = c("GS1", "GS1", "GS2"), gene = c("g1", "g2", "g3")),
    gsid2name = data.frame(gsid = c("GS1", "GS2"), name = c("set 1", "set 2")),
    gene2name = data.frame(gene = c("g1", "g2", "g3"), name = c("Gene 1", "Gene 2", "Gene 3"))
  )

  expect_identical(length(x), 2L)
  expect_identical(names(x), c("GS1", "GS2"))
  expect_identical(x[["GS1"]], c("g1", "g2"))
  expect_identical(x[[2]], "g3")
})

test_that("as.data.frame.GSON returns membership with optional names", {
  x <- gson(
    gsid2gene = data.frame(gsid = c("GS1", "GS1"), gene = c("g1", "g2")),
    gsid2name = data.frame(gsid = "GS1", name = "set 1"),
    gene2name = data.frame(gene = c("g1", "g2"), name = c("Gene 1", "Gene 2"))
  )

  y <- as.data.frame(x)

  expect_named(y, c("gsid", "name", "gene", "gene_name"))
  expect_equal(y$gsid, c("GS1", "GS1"))
  expect_equal(y$name, c("set 1", "set 1"))
  expect_equal(y$gene, c("g1", "g2"))
  expect_equal(y$gene_name, c("Gene 1", "Gene 2"))
})

test_that("write.gmt writes gene set IDs, descriptions, and genes", {
  x <- gson(
    gsid2gene = data.frame(gsid = c("GS1", "GS1", "GS2"), gene = c("g1", "g2", "g3")),
    gsid2name = data.frame(gsid = "GS1", name = "set 1")
  )
  f <- tempfile(fileext = ".gmt")

  lines <- write.gmt(x)
  write.gmt(x, f)

  expect_identical(lines, c("GS1\tset 1\tg1\tg2", "GS2\tGS2\tg3"))
  expect_identical(readLines(f), lines)
})
