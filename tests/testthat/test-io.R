test_that("minimal GSON objects round-trip through gson files", {
  x <- gson(data.frame(gsid = c("A", "A"), gene = c("g1", "g2")))
  f <- tempfile(fileext = ".gson")

  write.gson(x, f)
  y <- read.gson(f)

  expect_s4_class(y, "GSON")
  expect_identical(y@gsid2gene, x@gsid2gene)
  expect_null(y@gsid2name)
  expect_null(y@gene2name)
  expect_null(y@species)
  expect_null(y@gsname)
})

test_that("optional data frames and metadata round-trip through gson files", {
  x <- gson(
    gsid2gene = data.frame(gsid = c("A", "A"), gene = c("g1", "g2")),
    gsid2name = data.frame(gsid = "A", name = "set A"),
    gene2name = data.frame(gene = c("g1", "g2"), name = c("Gene 1", "Gene 2")),
    species = "human",
    gsname = "test",
    version = "1",
    accessed_date = "2024-02-06",
    keytype = "ENTREZID",
    urlpattern = "https://example.org/{gsid}",
    info = "extra"
  )
  f <- tempfile(fileext = ".gson")

  write.gson(x, f)
  y <- read.gson(f)

  expect_identical(y@gsid2gene, x@gsid2gene)
  expect_identical(y@gsid2name, x@gsid2name)
  expect_identical(y@gene2name, x@gene2name)
  expect_identical(y@species, "human")
  expect_identical(y@gsname, "test")
  expect_identical(y@version, "1")
  expect_identical(y@accessed_date, "2024-02-06")
  expect_identical(y@keytype, "ENTREZID")
  expect_identical(y@urlpattern, "https://example.org/{gsid}")
  expect_identical(y@info, "extra")
})

test_that("legacy empty JSON containers are read as NULL optional fields", {
  f <- tempfile(fileext = ".gson")
  writeLines(
    c(
      "{",
      '  "gsid2gene": {"A": ["g1"]},',
      '  "gsid2name": [],',
      '  "gene2name": [],',
      '  "species": {},',
      '  "gsname": {},',
      '  "version": {},',
      '  "accessed_date": {},',
      '  "keytype": {},',
      '  "urlpattern": {},',
      '  "info": {}',
      "}"
    ),
    f
  )

  x <- read.gson(f)

  expect_identical(x@gsid2gene, data.frame(gsid = "A", gene = "g1"))
  expect_null(x@gsid2name)
  expect_null(x@gene2name)
  expect_null(x@species)
  expect_null(x@gsname)
})
