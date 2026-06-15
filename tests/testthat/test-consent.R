# The show_consent data-protection / consent front-matter block.

test_that("show_consent renders the policy notice and consent checkbox", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(path, out, chrome_lang = "en")
  txt <- paste(
    officer::docx_summary(officer::read_docx(out))$text, collapse = " | "
  )
  expect_true(grepl("Data protection and consent", txt, fixed = TRUE))
  # The consent checkbox glyph precedes the localized label.
  expect_true(grepl("□", txt))
  expect_true(grepl("I have read and accept", txt, fixed = TRUE))
  # The notice text itself is surfaced.
  expect_true(grepl("does not collect any real data", txt, fixed = TRUE))
})

test_that("show_consent = FALSE omits the consent block", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(path, out, chrome_lang = "en", show_consent = FALSE)
  txt <- paste(
    officer::docx_summary(officer::read_docx(out))$text, collapse = " | "
  )
  expect_false(grepl("Data protection and consent", txt, fixed = TRUE))
})
