# Batch 2: precise triggers for the inner branches of guards whose
# condition was already exercised, plus a heavily-mutated render that
# reaches the optional cover / quota / multiple-choice-exclusive paths.

mk_theme2 <- function(lang = "en") {
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings(lang)
  th
}
LL <- c("en", "fr")

# ---- single-line inner branches -----------------------------------------

test_that("lss_render_question_meta_table shows the 'All' filter for a trivial relevance", {
  skip_if_not_installed("flextable"); skip_if_not_installed("officer")
  th <- mk_theme2()
  doc <- officer::read_docx()
  expect_no_error(lss_render_question_meta_table(
    doc, th, item_no = 1L, variable = "q1", type = "L",
    type_label = "Single choice", mandatory = "N",
    relevance = NULL, show_raw_filter = FALSE))
})

test_that("lss_normalize_authors rejects a character vector containing NA", {
  expect_error(lss_normalize_authors(c("Amal", NA_character_)),
               class = "lssdoc_bad_authors")
})

test_that("lss_header_titles yields '' for a language absent from the settings", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  out <- lssdoc:::lss_header_titles(lss, c("en", "zz"))
  expect_identical(unname(out[2]), "")
})

test_that("lss_html_to_text handles multi-run content with surrounding space", {
  expect_identical(lss_html_to_text("  first <sup>2</sup> last  "),
                   "first 2 last")
})

test_that("lss_filter_chrome reads a glyph supplied by the chrome", {
  th <- mk_theme2()
  th$chrome$filter_all <- "ALLZ"
  expect_identical(lssdoc:::lss_filter_chrome(th)$all, "ALLZ")
})

test_that("lss_localized (no index) resolves a present row", {
  l10n <- data.frame(qid = "1", language = "en", question = "Q",
                     stringsAsFactors = FALSE)
  out <- lssdoc:::lss_localized(l10n, "qid", "1", LL, "question")
  expect_identical(out$en$question, "Q")
  expect_true(is.na(out$fr$question))
})

test_that("whitespace audit ignores an empty code without error", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  lss$answers$code[1] <- ""            # empty code -> flag() early-returns
  expect_s3_class(audit_lss(lss), "lss_audit")
})

# ---- multiple-choice exclusive row (card) --------------------------------

test_that("a multiple-choice question with a real exclude_all_others renders its exclusive row", {
  skip_on_cran()
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  mc <- lss$questions$qid[lss$questions$title == "supportsources"][1]
  sq1 <- lss$subquestions$title[lss$subquestions$parent_qid == mc][1]
  qa <- lss$question_attributes
  # point exclude_all_others at a real subquestion code (non-empty value)
  hit <- qa$attribute == "exclude_all_others" & qa$qid == mc
  if (any(hit)) qa$value[hit] <- sq1 else qa <- rbind(qa, data.frame(
    qid = mc, attribute = "exclude_all_others", value = sq1,
    language = "", stringsAsFactors = FALSE))
  lss$question_attributes <- qa
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(lss, out, template = "cards", languages = LL,
                       chrome_lang = "en")
  expect_true(file.exists(out))
})

# ---- cover page: alias / active / privacy settings + description URL -----

test_that("the cover renders optional survey metadata and privacy settings", {
  skip_on_cran()
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  # Inject an alias (language settings) and an 'active' flag (surveys).
  lss$survey_language_settings$surveyls_alias <- "demo-alias"
  lss$surveys$active <- "Y"
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(
    lss, out, template = "cards", languages = LL, chrome_lang = "en",
    show_privacy_settings = TRUE,
    description = "Protocol at https://example.org/study and more text.")
  expect_true(file.exists(out))
})

# ---- quota with a localized name and message -----------------------------

test_that("a quota with a localized name and message renders its rows", {
  skip_on_cran()
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  qls <- lss$quota_languagesettings
  skip_if(is.null(qls) || nrow(qls) == 0L, "no quota language settings")
  qls$quotals_name[1] <- "Student cap"
  qls$quotals_message[1] <- "<p>You have reached the student quota.</p>"
  lss$quota_languagesettings <- qls
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(lss, out, template = "cards", languages = LL,
                       chrome_lang = "en", show_quotas = TRUE)
  expect_true(file.exists(out))
})
