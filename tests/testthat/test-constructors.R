test_that("gson uses the first two columns documented for mapping tables", {
  x <- gson(
    gsid2gene = data.frame(term = c("A", "A"), value = c("g1", "g2"), ignored = 1:2),
    gsid2name = data.frame(term = "A", label = "set A", ignored = 1),
    gene2name = data.frame(id = "g1", symbol = "Gene 1", ignored = 1)
  )

  expect_identical(colnames(x@gsid2gene), c("gsid", "gene"))
  expect_identical(x@gsid2gene$gsid, c("A", "A"))
  expect_identical(x@gsid2gene$gene, c("g1", "g2"))
  expect_identical(colnames(x@gsid2name), c("gsid", "name"))
  expect_identical(colnames(x@gene2name), c("gene", "name"))
})

test_that("gson validates mapping table inputs", {
  expect_error(gson(list(gsid = "A", gene = "g1")), "gsid2gene should be a data.frame")
  expect_error(gson(data.frame(gsid = "A")), "gsid2gene should have at least 2 columns")
})

test_that("gsonList tolerates missing gsname values", {
  x <- gson(data.frame(gsid = "A", gene = "g1"))
  y <- gson(data.frame(gsid = "B", gene = "g2"), gsname = "named")

  z <- gsonList(x, y)

  expect_s3_class(z, "GSONList")
  expect_identical(names(z), c("GSON1", "named"))
})
