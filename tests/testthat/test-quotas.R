# Quota parsing and the show_quotas back-matter section.

test_that("read_lss exposes the quota sections", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  expect_true(!is.null(lss$quotas) && nrow(lss$quotas) >= 1L)
  expect_true(!is.null(lss$quota_members))
  expect_true(!is.null(lss$quota_languagesettings))
})

test_that("show_quotas renders a quota section with resolved conditions", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(out = out, input = path, chrome_lang = "en")
  txt <- paste(
    officer::docx_summary(officer::read_docx(out))$text, collapse = " | "
  )
  expect_true(grepl("Quotas", txt, fixed = TRUE))
  expect_true(grepl("fin2", txt, fixed = TRUE))
  expect_true(grepl("terminate survey", txt, fixed = TRUE))
  # The membership condition resolves the answer code to its label and
  # joins several members with the localized AND connector.
  expect_true(grepl("ST (Student)", txt, fixed = TRUE))
  expect_true(grepl(" AND ", txt, fixed = TRUE))
  # The localized "quota full" message is shown.
  expect_true(grepl("exceeded a quota", txt, fixed = TRUE))
})

test_that("show_quotas = FALSE and quota-less surveys produce no quota section", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  demo <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(demo))

  out_off <- tempfile(fileext = ".docx")
  on.exit(unlink(out_off), add = TRUE)
  render_questionnaire(demo, out_off, chrome_lang = "en", show_quotas = FALSE)
  t_off <- paste(
    officer::docx_summary(officer::read_docx(out_off))$text, collapse = " | "
  )
  expect_false(grepl("exceeded a quota", t_off, fixed = TRUE))

  # A quota-less survey (quotas stripped in memory) renders no quota
  # full message even with the default show_quotas = TRUE.
  lss_noq <- read_lss(demo)
  lss_noq$quotas <- lss_noq$quotas[0, , drop = FALSE]
  lss_noq$quota_members <- lss_noq$quota_members[0, , drop = FALSE]
  out_none <- tempfile(fileext = ".docx")
  on.exit(unlink(out_none), add = TRUE)
  render_questionnaire(lss_noq, out_none, chrome_lang = "en")
  t_none <- paste(
    officer::docx_summary(officer::read_docx(out_none))$text, collapse = " | "
  )
  expect_false(grepl("exceeded a quota", t_none, fixed = TRUE))
})
