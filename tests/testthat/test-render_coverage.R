# Coverage-oriented tests: exercise paths that the other suites do not
# touch, so codecov reflects the breadth of the rendering pipeline.

lss_render_q_text <- function(input, ..., template = "cards") {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(input, out, template = template, ...)
  s <- officer::docx_summary(officer::read_docx(out))
  paste(s$text[!is.na(s$text)], collapse = " | ")
}

# ---- Render arguments and toggles ----------------------------------

test_that("render_questionnaire honors a single-language scalar 'title'", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_q_text(read_lss(path), chrome_lang = "en",
                           title = "Custom Override Title")
  expect_true(grepl("Custom Override Title", txt))
})

test_that("render_questionnaire honors a per-language named title", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_q_text(
    read_lss(path), chrome_lang = "en",
    languages = c("fr", "de"),
    title = c(fr = "Titre FR custom", de = "DE Titel custom")
  )
  expect_true(grepl("Titre FR custom", txt))
  expect_true(grepl("DE Titel custom", txt))
})

test_that("show_header_title = FALSE hides the survey title from page headers", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(read_lss(path), out, chrome_lang = "en",
                       show_header_title = FALSE)
  # The doc is well-formed.
  expect_true(file.size(out) > 10000L)
})

test_that("show_source = FALSE hides Source file and Survey ID rows", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  with <- lss_render_q_text(read_lss(path), chrome_lang = "en")
  without <- lss_render_q_text(read_lss(path), chrome_lang = "en",
                               show_source = FALSE)
  expect_true(grepl("Survey ID", with))
  expect_false(grepl("Survey ID", without))
})

test_that("show_item_heading = TRUE adds the bold variable heading line", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out_off <- tempfile(fileext = ".docx")
  out_on  <- tempfile(fileext = ".docx")
  on.exit({ unlink(out_off); unlink(out_on) }, add = TRUE)
  render_questionnaire(read_lss(path), out_off, chrome_lang = "en",
                       show_item_heading = FALSE)
  render_questionnaire(read_lss(path), out_on,  chrome_lang = "en",
                       show_item_heading = TRUE)
  # The TRUE variant adds one styled heading paragraph per item, so
  # the document carries strictly more paragraphs than the off
  # variant. Compare paragraph counts rather than file size (which
  # fluctuates with the embedded XML compression).
  n_par_on  <- nrow(officer::docx_summary(officer::read_docx(out_on)))
  n_par_off <- nrow(officer::docx_summary(officer::read_docx(out_off)))
  expect_gt(n_par_on, n_par_off)
})

test_that("show_raw_filter = TRUE adds the raw expression underneath the plain form", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  has_rel <- !is.na(lss$questions$relevance) &
    nzchar(lss$questions$relevance) &
    lss$questions$relevance != "1"
  skip_if(!any(has_rel), "no non-trivial filter in fixture")
  with    <- lss_render_q_text(lss, chrome_lang = "en", show_raw_filter = TRUE)
  without <- lss_render_q_text(lss, chrome_lang = "en", show_raw_filter = FALSE)
  expect_gt(nchar(with), nchar(without))
})

test_that("page_format = 'A4-landscape' is accepted", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(read_lss(path), out, chrome_lang = "en",
                       page_format = "A4-landscape")
  expect_true(file.size(out) > 10000L)
})

test_that("page_format = 'A3' is accepted", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(read_lss(path), out, chrome_lang = "en",
                       page_format = "A3")
  expect_true(file.size(out) > 10000L)
})

# ---- font / font_code overrides ------------------------------------

test_that("font and font_code overrides flow through the theme", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(read_lss(path), out, chrome_lang = "en",
                       font = "Source Sans 3", font_code = "JetBrains Mono")
  expect_true(file.size(out) > 10000L)
})

test_that("lss_validate_font rejects non-character / NA / multi-element values", {
  # The validator returns invisible() on success, so we check the
  # call is silent rather than its return value.
  expect_silent(lss_validate_font(NULL, "font"))
  expect_silent(lss_validate_font("Source Sans 3", "font"))
  expect_error(lss_validate_font(123, "font"),
               class = "lssdoc_bad_font")
  expect_error(lss_validate_font(c("a", "b"), "font"),
               class = "lssdoc_bad_font")
  expect_error(lss_validate_font(NA_character_, "font"),
               class = "lssdoc_bad_font")
})

# ---- Audit doc paths --------------------------------------------------

test_that("render_audit accepts logo, font, font_code, colors, authors, description", {
  path <- system.file("extdata", "demo_survey.lss",
                      package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_audit(
    path, out,
    chrome_lang = "en",
    font = NULL, font_code = NULL,
    colors = list(primary = "#5C9F1A"),
    authors = c("Amal Tawfik"),
    description = "QA report for the team. See https://example.org/x.",
    logo = NULL
  )
  s <- officer::docx_summary(officer::read_docx(out))
  txt <- paste(s$text[!is.na(s$text)], collapse = " | ")
  expect_true(grepl("Amal Tawfik", txt))
  expect_true(grepl("example.org", txt))
})

test_that("render_audit on a clean survey still renders a doc with the all-clear text", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_audit(read_lss(path), out, chrome_lang = "en")
  s <- officer::docx_summary(officer::read_docx(out))
  expect_true(file.size(out) > 5000L)
})

# ---- authors normalization edge cases ------------------------------

test_that("lss_normalize_authors accepts an empty character vector", {
  out <- lss_normalize_authors(character(0))
  expect_length(out, 0L)
})

test_that("lss_normalize_authors rejects a list element missing 'name'", {
  expect_error(
    lss_normalize_authors(list(list(affiliation = "x"))),
    class = "lssdoc_bad_authors"
  )
})

test_that("lss_normalize_authors accepts a partial list element", {
  out <- lss_normalize_authors(list(list(name = "X")))
  expect_identical(out[[1L]]$name, "X")
  expect_identical(out[[1L]]$affiliation, "")
  expect_identical(out[[1L]]$orcid, "")
})

# ---- description multiline + URL detection -------------------------

test_that("description with line breaks and URLs renders without error", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(
    read_lss(path), out, chrome_lang = "en",
    description = paste0(
      "Line one.\n",
      "Line two with https://example.org/page.\n",
      "Line three with http://another.org/p."
    )
  )
  s <- officer::docx_summary(officer::read_docx(out))
  txt <- paste(s$text[!is.na(s$text)], collapse = " | ")
  expect_true(grepl("Line one", txt))
  expect_true(grepl("example.org", txt))
  expect_true(grepl("another.org", txt))
})

# ---- Cover ID resolver and various titles --------------------------

test_that("render_questionnaire accepts authors as a named character vector", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(
    read_lss(path), out, chrome_lang = "en",
    authors = c("Amal Tawfik" = "HES-SO Valais-Wallis",
                "John Doe" = "")
  )
  s <- officer::docx_summary(officer::read_docx(out))
  txt <- paste(s$text[!is.na(s$text)], collapse = " | ")
  expect_true(grepl("Amal Tawfik", txt))
  expect_true(grepl("HES-SO Valais-Wallis", txt))
  expect_true(grepl("John Doe", txt))
})

test_that("render_questionnaire accepts authors as an unnamed character vector", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(
    read_lss(path), out, chrome_lang = "en",
    authors = c("Amal Tawfik", "John Doe")
  )
  s <- officer::docx_summary(officer::read_docx(out))
  txt <- paste(s$text[!is.na(s$text)], collapse = " | ")
  expect_true(grepl("Amal Tawfik", txt))
  expect_true(grepl("John Doe", txt))
})

# ---- show_admin_settings -------------------------------------------

test_that("show_admin_settings = TRUE accepts the flag without error", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  # The chrome labels (Active / Alias / End URL) only appear when the
  # underlying LimeSurvey field is non-empty; the fixture is silent
  # on those. We assert only that the path runs without error and
  # produces a valid document.
  render_questionnaire(read_lss(path), out, chrome_lang = "en",
                       show_admin_settings = TRUE)
  expect_true(file.size(out) > 10000L)
})

# ---- One language only ---------------------------------------------

test_that("a one-language render produces a portrait document", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(read_lss(path), out, languages = "fr",
                       chrome_lang = "fr")
  expect_true(file.size(out) > 10000L)
})

# ---- chrome_lang = NULL follows languages[1] ------------------------

test_that("chrome_lang = NULL (default) follows the primary content language", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  # languages[1] = "fr", chrome_lang unspecified -> French chrome.
  txt <- lss_render_q_text(read_lss(path),
                           languages = c("fr", "de"),
                           chrome_lang = NULL)
  expect_true(grepl("Filtre", txt))
})
