test_that("render_questionnaire parses then renders to .docx", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  res <- render_questionnaire(path, out)
  expect_identical(res, out)
  expect_true(file.size(out) > 10000)
})

test_that("render_audit writes an audit-only document", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  # Force English chrome so the assertion can match the exact
  # heading text; the default chrome_lang follows the survey's
  # primary language and would translate it.
  res <- render_audit(path, out, chrome_lang = "en")
  expect_identical(res, out)
  s <- officer::docx_summary(officer::read_docx(out))
  expect_true(any(grepl("Audit findings", s$text)))
})

test_that(".docx_to_pdf reports a clear error when soffice is unavailable", {
  if (!is.null(lssdoc:::lss_find_soffice())) {
    skip("soffice is installed; this path is exercised in pipeline use")
  }
  expect_error(
    .docx_to_pdf(tempfile(fileext = ".docx"), tempfile(fileext = ".pdf")),
    class = "lssdoc_bad_input"
  )
})

test_that("render_questionnaire and render_audit produce PDFs when soffice is present", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  if (is.null(lssdoc:::lss_find_soffice())) {
    skip("LibreOffice not installed in this environment")
  }
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))

  pdf_full <- tempfile(fileext = ".pdf")
  pdf_aud  <- tempfile(fileext = ".pdf")
  on.exit({ unlink(pdf_full); unlink(pdf_aud) }, add = TRUE)

  render_questionnaire(path, pdf_full)
  expect_true(file.exists(pdf_full))
  expect_true(file.size(pdf_full) > 50000)

  render_audit(path, pdf_aud)
  expect_true(file.exists(pdf_aud))
})
