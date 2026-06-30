# gson 0.1.0.001

+ add container helpers for GSON objects, including `as.data.frame()`, `length()`, `names()`, `[[`, and `write.gmt()` (2026-06-30, Tue)
+ add `validate_gson()` to check the core GSON data contract and optional mapping table consistency (2026-06-30, Tue)
+ add README documentation describing GSON as a gene set collection container with creation, GSON IO, GMT, and enrichment examples (2026-06-30, Tue)
+ fix `read.gson()`/`write.gson()` round-tripping of missing optional fields and preserve `gsid2gene` character columns (2026-06-30, Tue)
+ normalize the first two columns of GSON mapping tables in `gson()` and add input validation (2026-06-30, Tue)
+ make `gsonList()` robust to GSON objects without `gsname` and add testthat regression tests (2026-06-30, Tue)
+ introduce new slot 'urlpattern' for generating URL of selected gene set (2023-08-16, Wed)
+ use `yulab.utils::yread()` to read GMT file and gson file (2023-08-16, Wed)

# gson 0.1.0

+ fixed `read.gson()` by setting the default values (if empty) to NULL (to make it compatible with 'clusterProfiler') (2023-03-03, Mon)
+ set `gsname="WikiPathways"` in `read.gmt.wp(output = "gson")` (2022-09-23, Fri)

# gson 0.0.9

+ `gsonList` function to construct `GSONList` object and corresponding `print` method implemented (2022-09-06, Tue)
 
# gson 0.0.8

+ remove `iconv` in `write.gson` as it may return NA (2022-08-20, Sat)

# gson 0.0.7

+ add `keytype` slot for `read.gson()` and `write.gson()`. (2022-08-5, Wed)
+ add `keytype` slot for GSON object. (2022-07-13, Wed)
