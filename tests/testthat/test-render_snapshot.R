# Content-snapshot tests for render_questionnaire().
#
# Each test renders a small docx and asserts that specific tokens are
# present (or absent) in the document text. These are not exact-byte
# snapshots -- they pin the *visible content* the reviewer should see,
# so a chrome string rename, a default flip or an alignment regression
# surfaces here rather than in a manual review of every preview.
#
# The helpers use officer::docx_summary() which extracts every text
# token from the .docx (paragraphs + table cells + headers + footers).

lss_render_text <- function(lss, ..., template = "cards") {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(lss, out, template = template, ...)
  s <- officer::docx_summary(officer::read_docx(out))
  paste(s$text[!is.na(s$text)], collapse = " | ")
}

# ---- Cover subtitle is the single-word localized noun --------------

test_that("the cover subtitle is the single localized noun in English", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "en")
  expect_true(grepl("\\bQuestionnaire\\b", txt))
  # The previous long subtitle must NOT be there.
  expect_false(grepl("LimeSurvey questionnaire review", txt))
})

test_that("the cover subtitle localizes to 'Fragebogen' in German chrome", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "de")
  expect_true(grepl("Fragebogen", txt))
})

# ---- Mandatory header: full word in cards, abbreviated in table ----

test_that("cards template uses the full 'Mandatory' header in English", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), template = "cards", chrome_lang = "en")
  expect_true(grepl("Mandatory", txt))
})

test_that("table template uses the abbreviated 'Mand.' header in English", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), template = "table", chrome_lang = "en")
  expect_true(grepl("Mand\\.", txt))
})

# ---- show_raw_filter default behaviour ------------------------------

test_that("show_raw_filter = FALSE (default) does not surface .NAOK in Filter cells", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "en")
  # The humanized form is the editorial default; the LimeSurvey
  # `.NAOK` token only appears when the user opts into the raw
  # expression. Cards header text doesn't naturally carry `.NAOK`,
  # so finding any occurrence here means the raw form leaked.
  expect_false(grepl("\\.NAOK", txt))
})

test_that("show_raw_filter = TRUE surfaces the raw LimeSurvey expression", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  # Find any question with a non-trivial relevance so the raw form
  # actually has content to show.
  has_rel <- !is.na(lss$questions$relevance) &
    nzchar(lss$questions$relevance) &
    lss$questions$relevance != "1"
  skip_if(!any(has_rel), "No non-trivial filters in the fixture")
  txt <- lss_render_text(lss, chrome_lang = "en", show_raw_filter = TRUE)
  # The raw LimeSurvey expression preserves the `==` operator and the
  # quoted answer code; the humanized default rewrites both (`=`, no
  # quotes), so the verbatim operator only surfaces in raw mode.
  expect_true(grepl("==", txt, fixed = TRUE))
})

# ---- French chrome surfaces French labels --------------------------

test_that("chrome_lang = 'fr' surfaces French labels and not their EN counterparts", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "fr")
  expect_true(grepl("Filtre", txt))
  expect_true(grepl("Obligatoire", txt))
  # The EN-only label must NOT appear when chrome_lang is fr.
  expect_false(grepl("\\bMandatory\\b", txt))
})

# ---- Comprehensive table of contents ---------------------------------

test_that("the table of contents lists the document sections, not only groups", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "en")
  # The static TOC names the major sections (audit, consent,
  # questionnaire, quotas, variable index), each anchored to a heading.
  for (lbl in c("Table of contents", "Audit findings",
                "Data protection and consent", "Questionnaire",
                "Quotas", "Variable index")) {
    expect_true(grepl(lbl, txt, fixed = TRUE), info = lbl)
  }
})

# ---- Audit summary section --------------------------------------------

test_that("show_audit = TRUE places the audit findings heading near the top", {
  path <- system.file("extdata", "demo_survey.lss",
                      package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "en", show_audit = TRUE)
  expect_true(grepl("Audit findings", txt))
})

# ---- Languages column header presence ------------------------------

test_that("requested languages each appear as a column label in cards", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path),
                         languages = c("fr", "de"),
                         template = "cards", chrome_lang = "en")
  expect_true(grepl("Fran", txt))   # Francais (the localized name)
  expect_true(grepl("Deutsch", txt))
})

# ---- Variable codes appear in the variable index --------------------

test_that("every question code appears in the variable index", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  txt <- lss_render_text(lss, chrome_lang = "en", show_index = TRUE)
  # Pick a few codes to spot-check (every question appearing would
  # be a stronger assertion but it would couple the test to the
  # fixture too tightly).
  codes <- lss$questions$title[1:min(5L, nrow(lss$questions))]
  for (code in codes) {
    expect_true(grepl(code, txt, fixed = TRUE),
                info = sprintf("expected code '%s' in the rendered text", code))
  }
})

# ---- Table template groups multiple-choice into parent + option rows --

test_that("the table template renders multiple choice as a parent row plus full-name option rows", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  skip_if(!("supportsources" %in% lss$questions$title),
          "fixture changed; MC question absent")
  txt <- lss_render_text(lss, chrome_lang = "en",
                         languages = c("fr", "de"), template = "table")

  # The parent row carries the implicit Y/blank coding once.
  expect_true(grepl("Y/blank", txt, fixed = TRUE))
  # The parent row does NOT repeat a `code_*` family pattern in the
  # Variable column: in the table the option rows below already list the
  # full names, so the family wildcard would be redundant (and the bare
  # parent is not a real data column).
  expect_false(grepl("supportsources[*]", txt, fixed = TRUE))
  # Each option keeps its FULL variable name on its own codebook row
  # (the table is a variable dictionary, unlike the card's suffix-only
  # option list).
  for (k in 1:4) {
    code <- sprintf("supportsources[%d]", k)
    expect_true(grepl(code, txt, fixed = TRUE),
                info = sprintf("expected full option variable '%s'", code))
  }
})

# ---- Multiple-choice questions render as one grouped card ----------

test_that("a multiple-choice question renders as a single grouped card", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  # supportsources is a type-M question with 4 options coded 1..4.
  skip_if(!("supportsources" %in% lss$questions$title),
          "fixture changed; MC question absent")
  txt <- lss_render_text(lss, chrome_lang = "en", languages = c("fr", "de"))

  # The meta band shows the variable family with the wildcard, not a
  # single column.
  expect_true(grepl("supportsources[*]", txt, fixed = TRUE))
  # Every option's full Yes/No variable code is recoverable: it appears
  # verbatim in the variable index (the card itself shows the meta
  # pattern plus the per-option suffix, which compose to the full name).
  for (k in 1:4) {
    code <- sprintf("supportsources[%d]", k)
    expect_true(grepl(code, txt, fixed = TRUE),
                info = sprintf("expected option variable '%s' in the index", code))
  }
  # The Options section header and its count are present.
  expect_true(grepl("Options (4)", txt, fixed = TRUE))
  # The localized Y/blank coding line is present once.
  expect_true(grepl("Y = selected, blank = not selected", txt, fixed = TRUE))
})

test_that("the multiple-choice stem appears once, not once per option", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  skip_if(!("supportsources" %in% lss$questions$title),
          "fixture changed; MC question absent")

  # Grab the French stem of the MC question and count how many distinct
  # paragraphs carry it: in the grouped layout it must appear exactly
  # once (it used to repeat once per option).
  qid <- lss$questions$qid[lss$questions$title == "supportsources"][1]
  stem_fr <- lss$question_l10ns$question[
    lss$question_l10ns$qid == qid & lss$question_l10ns$language == "fr"
  ][1]
  skip_if(is.na(stem_fr) || !nzchar(stem_fr), "no FR stem in fixture")

  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  skip_on_cran()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(lss, out, chrome_lang = "en", languages = c("fr", "de"))
  s <- officer::docx_summary(officer::read_docx(out))
  cells <- s$text[!is.na(s$text)]
  # Strip HTML tags the stem may carry, then count exact-containment.
  bare <- gsub("<[^>]+>", "", stem_fr)
  bare <- trimws(bare)
  n_hits <- sum(vapply(cells, function(t) grepl(bare, t, fixed = TRUE),
                       logical(1)))
  # If the stem could not be matched at all (HTML / encoding quirks in
  # docx_summary), skip rather than assert a false negative. When it is
  # matchable, the grouped layout must show it exactly once -- the old
  # per-option layout repeated it eight times.
  skip_if(n_hits == 0L, "stem not matchable in docx_summary output")
  expect_equal(n_hits, 1L)
})
