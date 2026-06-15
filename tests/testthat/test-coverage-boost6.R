# Final coverage push: the print.lss method, the internal docx
# renderers' argument validation, and a few render_layout helpers whose
# guard branches the normal render path never reaches.

test_that("print.lss summarizes the structure", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  # print.lss writes through cli (its own connection); just confirm it
  # runs cleanly and returns its argument invisibly.
  expect_no_error(print(lss))
  expect_invisible(print(lss))
})

test_that(".render_questionnaire_docx validates lss and output", {
  expect_error(
    lssdoc:::.render_questionnaire_docx(list(), tempfile(fileext = ".docx")),
    class = "lssdoc_bad_lss"
  )
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  expect_error(
    lssdoc:::.render_questionnaire_docx(lss, 123),
    class = "lssdoc_bad_output"
  )
  expect_error(
    lssdoc:::.render_questionnaire_docx(lss, c("a", "b")),
    class = "lssdoc_bad_output"
  )
})

test_that(".render_audit_docx validates lss and output", {
  expect_error(
    lssdoc:::.render_audit_docx(list(), tempfile(fileext = ".docx")),
    class = "lssdoc_bad_lss"
  )
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  expect_error(lssdoc:::.render_audit_docx(lss, 123), class = "lssdoc_bad_output")
})

test_that("lss_inject_update_fields is a no-op on a missing file", {
  expect_identical(
    lssdoc:::lss_inject_update_fields("does-not-exist.docx"),
    "does-not-exist.docx"
  )
})

test_that("lss_header_titles falls back when the survey has no language settings", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  lss2 <- read_lss(path)
  lss2$survey_language_settings <- NULL
  expect_identical(lssdoc:::lss_header_titles(lss2, c("en", "fr")), c("", ""))
  lss$survey_language_settings <- lss$survey_language_settings[0, , drop = FALSE]
  expect_identical(lssdoc:::lss_header_titles(lss, "en"), "")
})

test_that("lss_build_header skips blank titles and returns NULL when all are blank", {
  skip_if_not_installed("officer")
  theme <- lss_render_theme()
  expect_null(lssdoc:::lss_build_header(theme, character(0)))
  expect_null(lssdoc:::lss_build_header(theme, c("", NA_character_, "  ")))
  hdr <- lssdoc:::lss_build_header(theme, c("Real title", "", NA_character_))
  expect_false(is.null(hdr))
})

test_that("lss_content_width_in returns a positive width for each page format", {
  for (pf in c("auto", "A4-portrait", "A4-landscape", "A3")) {
    w <- lssdoc:::lss_content_width_in(pf)
    expect_true(is.numeric(w) && w > 0, info = pf)
  }
})
