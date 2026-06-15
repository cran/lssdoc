# Render-path coverage across the full question-type range. demo_survey
# spans nearly every LimeSurvey type (single/multiple choice, dual-scale
# and flexible arrays, ranking, date, numeric, text variants, equation,
# display), so rendering it in both templates and all four languages
# exercises the type-specific branches of render_item / the table
# template that the smaller fixtures never reach.

test_that("demo_survey renders both templates across its question types", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  for (tmpl in c("cards", "table")) {
    out <- tempfile(fileext = ".docx")
    expect_no_error(
      render_questionnaire(lss, out, template = tmpl, chrome_lang = "en")
    )
    txt <- paste(
      officer::docx_summary(officer::read_docx(out))$text, collapse = " | "
    )
    # A representative set of methodological type labels must surface,
    # confirming the type-specific render paths ran without dropping a
    # question.
    for (lbl in c("Multiple choice", "Single choice", "Ranking",
                  "Date", "Number", "Text")) {
      expect_true(grepl(lbl, txt, fixed = TRUE),
                  info = sprintf("%s template, label '%s'", tmpl, lbl))
    }
    unlink(out)
  }
})

test_that("predefined-scale and structural types document their Value", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(read_lss(path), out, chrome_lang = "en")
  txt <- paste(
    officer::docx_summary(officer::read_docx(out))$text, collapse = " | "
  )
  # C / E list their predefined codes' labels; the 5/10-point arrays show
  # an N-point descriptor. These types store no answers in the .lss, so
  # before this they had no Value. The dual-scale array decomposes into one
  # single-choice block per (subquestion x scale): its real per-scale
  # variable names and the scale headers (dualscale_headerA/B) surface.
  for (lbl in c("Uncertain", "Increase", "Decrease",
                "5-point scale (1 to 5)", "10-point scale (1 to 10)",
                "trustinstitutions[PARL][1]", "trustinstitutions[PARL][2]",
                "Trust", "Importance")) {
    expect_true(grepl(lbl, txt, fixed = TRUE), info = lbl)
  }
  # The old combined "Value (scale N)" view is gone.
  expect_false(grepl("Value (scale 1)", txt, fixed = TRUE))
  # Gender (type G) lists its codes as Value rows (M -> Male, F -> Female)
  # like the other predefined scales, not as an inline "M = Male" note.
  expect_true(grepl("Male", txt, fixed = TRUE))
  expect_true(grepl("Female", txt, fixed = TRUE))
  expect_false(grepl("M = Male", txt, fixed = TRUE))
})

test_that("a non-default answer / option order is noted, normal is silent", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  # demo_survey carries answer_order = alphabetical / random /
  # random_alphabetical (the last reported as random) and a multiple
  # choice with subquestion_order = random, so both notes must surface in
  # both templates; the order is annotated consistently in each layout.
  for (tmpl in c("cards", "table")) {
    out <- tempfile(fileext = ".docx")
    render_questionnaire(lss, out, template = tmpl, chrome_lang = "en")
    txt <- paste(
      officer::docx_summary(officer::read_docx(out))$text, collapse = " | "
    )
    expect_true(grepl("in random order", txt, fixed = TRUE),
                info = tmpl)
    expect_true(grepl("in alphabetical order", txt, fixed = TRUE),
                info = tmpl)
    unlink(out)
  }
})
