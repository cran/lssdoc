# The variable_names option controls how response-variable names are
# rendered: "brackets" (default) reproduces the CSV/Excel export column
# names exactly, so the variable index matches the raw data file;
# "underscore" uses the EM / SPSS / Stata code form.

test_that("lss_variable_name assembles both styles", {
  expect_identical(lss_variable_name("Q", "WEEK"), "Q[WEEK]")
  expect_identical(lss_variable_name("Q", "WEEK", style = "underscore"), "Q_WEEK")
  expect_identical(lss_variable_name("Q", "PARL", 1L), "Q[PARL][1]")
  expect_identical(lss_variable_name("Q", "PARL", 1L, "underscore"), "Q_PARL_1")
  # Numeric subquestion codes drop their leading zeros (CSV behavior).
  expect_identical(lss_variable_name("Q", "001"), "Q[1]")
  expect_identical(lss_variable_name("Q", "*"), "Q[*]")
})

test_that("brackets (default) reproduce the CSV/Excel column names", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # One representative column per structural family, exactly as it appears
  # in the LimeSurvey CSV/Excel data export.
  expected <- c(
    "sleephours[WEEK]",            # array subquestion
    "nationality[CH]", "nationality[other]",  # multiple choice + other
    "supportsources[1]",           # numeric subquestion code (zero-stripped)
    "trustinstitutions[PARL][1]", "trustinstitutions[PARL][2]",  # dual scale
    "adaptability[NEW_NOW]",       # 2-D array (row x column)
    "mediaquality[REL_EX]",        # 2-D array texts
    "devicerank[59842]",           # ranking position (answer id)
    "langhome[_Ccomment]"          # list-with-comment
  )
  for (tmpl in c("cards", "table")) {
    out <- tempfile(fileext = ".docx")
    render_questionnaire(lss, out, template = tmpl, chrome_lang = "en")
    txt <- paste(
      officer::docx_summary(officer::read_docx(out))$text, collapse = " | "
    )
    for (col in expected) {
      expect_true(grepl(col, txt, fixed = TRUE),
                  info = sprintf("%s template, column '%s'", tmpl, col))
    }
    unlink(out)
  }
})

test_that("variable_names = 'underscore' switches to the code form", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(read_lss(path), out, chrome_lang = "en",
                       variable_names = "underscore")
  txt <- paste(
    officer::docx_summary(officer::read_docx(out))$text, collapse = " | "
  )
  expect_true(grepl("sleephours_WEEK", txt, fixed = TRUE))
  expect_true(grepl("trustinstitutions_PARL_1", txt, fixed = TRUE))
  expect_false(grepl("sleephours[WEEK]", txt, fixed = TRUE))
})
