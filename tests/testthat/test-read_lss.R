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
  lss <- read_lss(path)

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
  lss <- read_lss(path)

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
