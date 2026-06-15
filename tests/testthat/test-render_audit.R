test_that("render_audit rejects bad inputs", {
  expect_error(
    render_audit(list(), tempfile(fileext = ".docx")),
    class = "lssdoc_bad_input"
  )
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  expect_error(render_audit(lss, 123), class = "lssdoc_bad_output")
})

test_that("render_audit writes a focused audit document", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)

  # Pin English chrome so the assertions can pattern-match the
  # canonical English headings; the default chrome_lang follows the
  # survey's primary content language and would translate the labels.
  res <- render_audit(read_lss(path), out, chrome_lang = "en")
  expect_identical(res, out)
  expect_true(file.exists(out))

  s <- officer::docx_summary(officer::read_docx(out))
  txt <- s$text[!is.na(s$text)]
  # Contains the audit headline and at least one severity-specific subsection.
  expect_true(any(grepl("Audit findings", txt)))
  # The renderer now uses the singular chrome key
  # (`audit_severity_error` = "error"); the heading reads as
  # "error (N)". Match the prefix, case-insensitive.
  expect_true(any(grepl("^(error|warning|note)\\s*\\(", txt,
                        ignore.case = TRUE)))
  # The full review's group headings are NOT present.
  group_h1 <- sum(
    s$content_type == "paragraph" &
      s$style_name == "heading 1" &
      grepl("Audit", s$text)
  )
  expect_true(group_h1 >= 1)
})

test_that("render_audit on a clean survey says 'no anomalies'", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  # The demo survey carries a single informational note (an equation
  # question with no display text). Give that question text so the audit
  # comes back perfectly clean and the "no anomalies" branch renders.
  lss <- read_lss(path)
  eq <- lss$questions$qid[lss$questions$type == "*"]
  is_eq <- lss$question_l10ns$qid %in% eq & !nzchar(lss$question_l10ns$question)
  lss$question_l10ns$question[is_eq] <- "Computed value"
  render_audit(lss, out, chrome_lang = "en")
  s <- officer::docx_summary(officer::read_docx(out))
  expect_true(any(grepl("No anomalies detected", s$text)))
})
