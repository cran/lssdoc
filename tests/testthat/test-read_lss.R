test_that("read_lss validates its path argument", {
  expect_error(read_lss(123), class = "lssdoc_bad_path")
  expect_error(read_lss(c("a", "b")), class = "lssdoc_bad_path")
})

test_that("read_lss errors on a missing file", {
  expect_error(
    read_lss(tempfile(fileext = ".lss")),
    class = "lssdoc_file_not_found"
  )
})

test_that("read_lss errors on invalid XML", {
  bad <- tempfile(fileext = ".lss")
  writeLines("this is not xml <<<", bad)
  expect_error(read_lss(bad), class = "lssdoc_invalid_xml")
})

test_that("read_lss rejects an empty file without crashing", {
  # The XML pre-validation must catch a non-XML (here empty) file before it
  # reaches libxml2, which on some platforms aborts the session rather than
  # raising a catchable error.
  empty <- tempfile(fileext = ".lss")
  file.create(empty)
  on.exit(unlink(empty), add = TRUE)
  expect_error(read_lss(empty), class = "lssdoc_invalid_xml")
})

test_that("read_lss rejects bytes that are not valid UTF-8", {
  # Starts with "<" so the byte pre-check passes; the 0xFF 0xFE pair in the
  # middle is not valid UTF-8, so the file is refused before parsing.
  f <- tempfile(fileext = ".lss")
  on.exit(unlink(f), add = TRUE)
  writeBin(
    c(charToRaw("<document>"), as.raw(c(0xFF, 0xFE)), charToRaw("</document>")),
    f
  )
  expect_error(read_lss(f), class = "lssdoc_invalid_xml")
})

test_that("read_lss transcodes a UTF-16 export instead of choking on it", {
  xml <- paste0(
    "<document><LimeSurveyDocType>Survey</LimeSurveyDocType>",
    "<DBVersion>700</DBVersion>",
    "<languages><language>en</language></languages></document>"
  )
  body <- iconv(xml, from = "UTF-8", to = "UTF-16LE", toRaw = TRUE)[[1]]
  skip_if(is.null(body), "iconv() has no UTF-16LE converter here")

  f <- tempfile(fileext = ".lss")
  on.exit(unlink(f), add = TRUE)
  writeBin(c(as.raw(c(0xFF, 0xFE)), body), f)   # UTF-16LE BOM

  lss <- read_lss(f)
  expect_s3_class(lss, "lss")
  expect_identical(lss$languages, "en")
  expect_identical(lss$doc_type, "Survey")
})

test_that("a truncated export is refused, never left to libxml2", {
  # The first 2000 bytes of a real export: well-formed prefix, no closing
  # </document>. libxml2 would raise a fatal parse error (on some
  # toolchains an uncatchable one), so read_lss() must refuse it in R.
  src <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(src))

  f <- tempfile(fileext = ".lss")
  on.exit(unlink(f), add = TRUE)
  writeBin(readBin(src, what = "raw", n = 2000L), f)

  expect_error(read_lss(f), class = "lssdoc_invalid_xml")
})

test_that("read_lss rejects XML whose root is not <document>", {
  f <- tempfile(fileext = ".lss")
  on.exit(unlink(f), add = TRUE)
  writeLines("<survey><document>x</document></survey>", f)
  expect_error(read_lss(f), class = "lssdoc_invalid_xml")
})

test_that("read_lss rejects a document without LimeSurveyDocType", {
  f <- tempfile(fileext = ".lss")
  on.exit(unlink(f), add = TRUE)
  writeLines("<document><DBVersion>700</DBVersion></document>", f)
  expect_error(read_lss(f), class = "lssdoc_invalid_xml")
})

test_that("the bundled surveys keep their structure counts", {
  # Guards the encoding/parse rework: same tree, same counts, both files.
  counts <- function(path) {
    lss <- read_lss(path)
    c(
      languages = length(lss$languages),
      groups = nrow(lss$groups),
      questions = nrow(lss$questions),
      subquestions = nrow(lss$subquestions),
      answers = nrow(lss$answers)
    )
  }
  demo <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  audit <- system.file("extdata", "audit_demo.lss", package = "lssdoc")
  skip_if_not(file.exists(demo) && file.exists(audit))

  expect_identical(
    counts(demo),
    c(languages = 4L, groups = 6L, questions = 47L,
      subquestions = 55L, answers = 64L)
  )
  expect_identical(
    counts(audit),
    c(languages = 2L, groups = 2L, questions = 8L,
      subquestions = 3L, answers = 5L)
  )
})

test_that("read_lss rejects XML that is not a survey export", {
  not_survey <- tempfile(fileext = ".lss")
  writeLines(
    "<document><LimeSurveyDocType>Token</LimeSurveyDocType></document>",
    not_survey
  )
  expect_error(read_lss(not_survey), class = "lssdoc_not_a_survey")
})

test_that("read_lss reads the bundled demo survey", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- lss_cached(path)

  expect_s3_class(lss, "lss")
  expect_identical(lss$languages, c("en", "de", "es", "fr"))
  expect_identical(lss$base_language, "fr")
  expect_identical(lss$doc_type, "Survey")

  expect_identical(nrow(lss$groups), 6L)
  expect_identical(nrow(lss$questions), 47L)
  expect_identical(nrow(lss$subquestions), 55L)
  expect_identical(nrow(lss$answers), 64L)
})

test_that("read_lss keeps localized text and distinguishes empty from absent", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- lss_cached(path)

  expect_setequal(unique(lss$question_l10ns$language), c("en", "de", "es", "fr"))
  expect_true(all(c("question", "help") %in% names(lss$question_l10ns)))
  expect_true(any(nzchar(lss$question_l10ns$question)))

  # A present-but-empty <help/> reads as "", never NA.
  expect_false(anyNA(lss$question_l10ns$help))
})

test_that("DBVersion < 400 is rejected with a classed error", {
  tmp <- tempfile(fileext = ".lss")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<document>",
    "<LimeSurveyDocType>Survey</LimeSurveyDocType>",
    "<DBVersion>350</DBVersion>",
    "<languages><language>en</language></languages>",
    "</document>"
  ), tmp)

  expect_error(read_lss(tmp), class = "lssdoc_unsupported_db_version")
})

test_that("DBVersion >= 800 emits a warning but parses", {
  tmp <- tempfile(fileext = ".lss")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<document>",
    "<LimeSurveyDocType>Survey</LimeSurveyDocType>",
    "<DBVersion>800</DBVersion>",
    "<languages><language>en</language></languages>",
    "</document>"
  ), tmp)

  # `expect_warning(...)` returns the warning, not the function result --
  # assign with `<-` inside the expression so we can inspect both.
  expect_warning(
    out <- read_lss(tmp),
    class = "lssdoc_untested_db_version"
  )
  expect_s3_class(out, "lss")
  expect_identical(out$db_version, "800")
})

test_that("a missing DBVersion is warned about but parses", {
  tmp <- tempfile(fileext = ".lss")
  on.exit(unlink(tmp), add = TRUE)
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<document>",
    "<LimeSurveyDocType>Survey</LimeSurveyDocType>",
    "<languages><language>en</language></languages>",
    "</document>"
  ), tmp)

  expect_warning(read_lss(tmp), class = "lssdoc_unknown_db_version")
})
