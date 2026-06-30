test_that("validate_gson accepts valid objects", {
  x <- gson(
    gsid2gene = data.frame(gsid = c("GS1", "GS1"), gene = c("g1", "g2")),
    gsid2name = data.frame(gsid = "GS1", name = "set 1"),
    gene2name = data.frame(gene = c("g1", "g2"), name = c("Gene 1", "Gene 2")),
    species = "Homo sapiens",
    gsname = "example",
    version = "1"
  )

  expect_true(validate_gson(x))
})

test_that("validate_gson reports duplicate memberships", {
  x <- gson(data.frame(gsid = c("GS1", "GS1"), gene = c("g1", "g1")))

  expect_error(validate_gson(x), "duplicate gsid-gene memberships")
  expect_match(validate_gson(x, error = FALSE), "duplicate gsid-gene memberships")
})

test_that("validate_gson reports empty membership tables", {
  x <- gson(data.frame(gsid = character(), gene = character()))

  expect_match(validate_gson(x, error = FALSE), "gsid2gene should contain at least one row")
})

test_that("validate_gson checks optional table references", {
  x <- gson(
    gsid2gene = data.frame(gsid = "GS1", gene = "g1"),
    gsid2name = data.frame(gsid = "GS2", name = "set 2"),
    gene2name = data.frame(gene = "g2", name = "Gene 2")
  )

  issues <- validate_gson(x, error = FALSE)

  expect_true(any(grepl("gsid2name contains gsid values not found in gsid2gene", issues)))
  expect_true(any(grepl("gene2name contains gene values not found in gsid2gene", issues)))
})

test_that("validate_gson reports duplicate optional mapping IDs", {
  x <- gson(
    gsid2gene = data.frame(gsid = "GS1", gene = "g1"),
    gsid2name = data.frame(gsid = c("GS1", "GS1"), name = c("set 1", "set one")),
    gene2name = data.frame(gene = c("g1", "g1"), name = c("Gene 1", "Gene one"))
  )

  issues <- validate_gson(x, error = FALSE)

  expect_true(any(grepl("gsid2name contains duplicate gsid values", issues)))
  expect_true(any(grepl("gene2name contains duplicate gene values", issues)))
})

test_that("validate_gson checks missing values and scalar metadata", {
  x <- gson(
    gsid2gene = data.frame(gsid = "GS1", gene = ""),
    species = c("human", "mouse")
  )

  issues <- validate_gson(x, error = FALSE)

  expect_true(any(grepl("gsid2gene\\$gene should not contain missing or empty values", issues)))
  expect_true(any(grepl("species should be length 1 or NULL", issues)))
})

test_that("validate_gson allows missing metadata", {
  x <- gson(data.frame(gsid = "GS1", gene = "g1"))

  expect_true(validate_gson(x))
})

test_that("validate_gson checks schema version", {
  x <- gson(data.frame(gsid = "GS1", gene = "g1"), schema_version = "")

  expect_match(validate_gson(x, error = FALSE), "schema_version should be a non-empty")
})

test_that("validate_gson reports non-GSON inputs", {
  expect_match(validate_gson(list(), error = FALSE), "x should be a GSON object")
  expect_error(validate_gson(list()), "x should be a GSON object")
})
