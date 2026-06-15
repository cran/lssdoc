# The bundled `audit_demo.lss` is a small, deliberately flawed survey
# that seeds one instance of every anomaly audit_lss() detects. It backs
# both the audit examples and this regression test: if a detector stops
# firing (or a new one is added without a fixture case), this test flags
# it, and it parses + renders so the demo path stays exercised.

test_that("audit_demo.lss triggers every audit detector", {
  path <- system.file("extdata", "audit_demo.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  au <- audit_lss(read_lss(path))
  checks <- unique(au$findings$check)
  expected <- c(
    "missing_translation",
    "empty_in_all_languages",
    "duplicate_code",
    "code_whitespace",
    "missing_options",
    "missing_subquestions",
    "forward_filter_reference",
    "array_scale_missing_answers",
    "array_scale_missing_subquestions",
    "orphan_subquestion",
    "orphan_answer"
  )
  for (e in expected) {
    expect_true(e %in% checks, info = sprintf("detector '%s' did not fire", e))
  }
  # Errors and warnings are both represented.
  expect_gt(au$n_errors, 0L)
  expect_gt(au$n_warnings, 0L)
})

test_that("a dual-scale array does not raise a false array-scale warning", {
  # demo_survey.lss contains a dual-scale array (type "1") whose answers
  # span scales 0 and 1 while its subquestions live on scale 0 only --
  # the normal dual-scale layout. The array-scale audit must NOT flag it
  # as "answer scale 1 has no subquestions".
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  au <- audit_lss(read_lss(path))
  expect_false("array_scale_missing_subquestions" %in% au$findings$check)
})

test_that("audit_demo.lss parses and renders in both modes without error", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "audit_demo.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out_a <- tempfile(fileext = ".docx")
  out_q <- tempfile(fileext = ".docx")
  on.exit(unlink(c(out_a, out_q)), add = TRUE)
  expect_no_error(render_audit(path, out_a, chrome_lang = "en"))
  expect_no_error(
    render_questionnaire(path, out_q, chrome_lang = "en", show_audit = TRUE)
  )
  expect_true(file.exists(out_a))
  expect_true(file.exists(out_q))
})
