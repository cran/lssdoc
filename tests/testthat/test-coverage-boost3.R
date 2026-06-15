# Direct unit tests for render_item helpers, targeting the edge branches
# that complete fixture data never exercises (empty-language fallbacks,
# the "Scale N" fallback, the multi-scale answer path, attribute
# formatting), plus a render that injects attributes / a group
# description so the in-render branches that depend on them are reached.

mk_theme <- function(lang = "en") {
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings(lang)
  th
}
ans <- function(code, label) {
  list(code = code, labels = list(en = label, fr = label, de = label, es = label))
}
LANGS <- c("en", "fr")

# ---- lss_value_implicit_row: every descriptor branch + the X null ----

test_that("lss_value_implicit_row maps every response-bearing type and skips display", {
  th <- mk_theme()
  for (ty in c("M","P","Y","G","5","N","K","S","T","U","Q",";",":","D","*","R","|")) {
    row <- lss_value_implicit_row(list(type = ty), LANGS, th)
    expect_false(is.null(row), info = ty)
    expect_true(isTRUE(row$section_header), info = ty)
  }
  expect_null(lss_value_implicit_row(list(type = "X"), LANGS, th))
})

# ---- predefined scales -------------------------------------------------

test_that("lss_predefined_labelled returns the fixed scales and NULL otherwise", {
  expect_length(lss_predefined_labelled("C"), 3L)
  expect_length(lss_predefined_labelled("E"), 3L)
  expect_length(lss_predefined_labelled("Y"), 2L)
  expect_length(lss_predefined_labelled("G"), 2L)
  expect_null(lss_predefined_labelled("L"))
})

test_that("lss_predefined_value_rows covers labelled, N-point and the fallback", {
  th <- mk_theme()
  labelled <- lss_predefined_value_rows(list(type = "C"), LANGS, th)
  expect_true(length(labelled) >= 4L)               # header + 3 codes
  npoint <- lss_predefined_value_rows(list(type = "B"), LANGS, th)
  expect_length(npoint, 11L)                         # header + 10 points
  expect_null(lss_predefined_value_rows(list(type = "L"), LANGS, th))
})

# ---- question attribute access + answer-order note ---------------------

test_that("lss_question_attr_value finds the first non-blank value or NULL", {
  q <- list(attributes = data.frame(
    attribute = c("answer_order", "answer_order"),
    value = c("", "random"), language = c("", ""),
    stringsAsFactors = FALSE
  ))
  expect_identical(lss_question_attr_value(q, "answer_order"), "random")
  expect_null(lss_question_attr_value(q, "nope"))
  expect_null(lss_question_attr_value(list(attributes = NULL), "answer_order"))
})

test_that("lss_answer_order_note maps random/alphabetical and stays quiet otherwise", {
  th <- mk_theme()
  mk <- function(v) list(attributes = data.frame(
    attribute = "answer_order", value = v, language = "",
    stringsAsFactors = FALSE))
  expect_identical(lss_answer_order_note(mk("random"), th), th$chrome$value_order_random)
  expect_identical(lss_answer_order_note(mk("random_alphabetical"), th), th$chrome$value_order_random)
  expect_identical(lss_answer_order_note(mk("alphabetical"), th), th$chrome$value_order_alphabetical)
  expect_null(lss_answer_order_note(mk("normal"), th))
  expect_null(lss_answer_order_note(list(attributes = NULL), th))
})

# ---- lss_apply_order_note: both attach paths + the no-ops --------------

test_that("lss_apply_order_note appends to a descriptor header and styles a plain one", {
  th <- mk_theme()
  # Plain section header (no existing descriptor) -> note becomes its text.
  plain <- list(list(label = "Value",
                     texts = stats::setNames(as.list(rep("", length(LANGS))), LANGS),
                     section_header = TRUE))
  out1 <- lss_apply_order_note(plain, "shown at random", LANGS, th)
  expect_true(isTRUE(out1[[1]]$section_with_text))
  expect_match(out1[[1]]$texts[["en"]], "random")
  # Header that already carries a descriptor -> note appended to it.
  withtext <- list(list(label = "Value",
                        texts = stats::setNames(rep(list("Numeric input"), length(LANGS)), LANGS),
                        section_header = TRUE, section_with_text = TRUE))
  out2 <- lss_apply_order_note(withtext, "shown at random", LANGS, th)
  expect_match(out2[[1]]$texts[["en"]], "Numeric input shown at random")
  # No-ops: null note, empty rows, no section header present.
  expect_identical(lss_apply_order_note(plain, NULL, LANGS, th), plain)
  expect_identical(lss_apply_order_note(list(), "x", LANGS, th), list())
  norow <- list(list(label = "Foo", texts = list(en = "a", fr = "b")))
  expect_identical(lss_apply_order_note(norow, "x", LANGS, th), norow)
})

# ---- dual-scale header: per-language, empty-language fallback, NULL -----

test_that("lss_dualscale_header resolves headers, falls back and returns NULL", {
  # Empty-language value -> used as fallback for every language column.
  q_empty_lang <- list(attributes = data.frame(
    attribute = c("dualscale_headerA", "dualscale_headerB"),
    value = c("Importance", "Satisfaction"), language = c("", ""),
    stringsAsFactors = FALSE))
  hA <- lss_dualscale_header(q_empty_lang, 1L, LANGS)
  expect_identical(hA[["en"]], "Importance")
  expect_identical(hA[["fr"]], "Importance")
  hB <- lss_dualscale_header(q_empty_lang, 2L, LANGS)
  expect_identical(hB[["en"]], "Satisfaction")
  # Per-language value wins over the empty-language fallback.
  q_lang <- list(attributes = data.frame(
    attribute = c("dualscale_headerA", "dualscale_headerA"),
    value = c("Trust", "Confiance"), language = c("en", "fr"),
    stringsAsFactors = FALSE))
  hL <- lss_dualscale_header(q_lang, 1L, LANGS)
  expect_identical(hL[["en"]], "Trust")
  expect_identical(hL[["fr"]], "Confiance")
  # No attributes, or all-blank values -> NULL.
  expect_null(lss_dualscale_header(list(attributes = NULL), 1L, LANGS))
  q_blank <- list(attributes = data.frame(
    attribute = "dualscale_headerA", value = "  ", language = "",
    stringsAsFactors = FALSE))
  expect_null(lss_dualscale_header(q_blank, 1L, LANGS))
})

# ---- subquestion facet: scale header, "Scale N" fallback, column, NULL -

test_that("lss_subq_facet prefers the header, falls back to 'Scale N', then column", {
  th <- mk_theme()
  scale_h <- list(header = list(en = "Trust", fr = "Confiance"), index = 1L)
  expect_identical(lss_subq_facet(scale_h, NULL, "en", th), "Trust")
  scale_blank <- list(header = list(en = "  "), index = 2L)
  expect_match(lss_subq_facet(scale_blank, NULL, "en", th), "2$")
  scale_nohdr <- list(header = NULL, index = 3L)
  expect_match(lss_subq_facet(scale_nohdr, NULL, "en", th), "3$")
  column <- list(texts = list(en = list(question = "Today")))
  expect_identical(lss_subq_facet(NULL, column, "en", th), "Today")
  expect_null(lss_subq_facet(NULL, NULL, "en", th))
})

# ---- value rows: section helper + multi-scale answer path --------------

test_that("lss_value_section_rows emits a Value header plus one row per code", {
  th <- mk_theme()
  rows <- lss_value_section_rows(list(ans("1", "Low"), ans("2", "High")), LANGS, th)
  expect_length(rows, 3L)
  expect_true(isTRUE(rows[[1]]$section_header))
  expect_identical(rows[[2]]$label, "1")
})

test_that("lss_answer_rows covers single-scale, dual-scale with/without headers, empty", {
  th <- mk_theme()
  a <- list(ans("1", "Low"), ans("2", "High"))
  single <- lss_answer_rows(list(answers = a, scales = NULL), LANGS, th)
  expect_length(single, 3L)
  # Dual-scale WITH per-scale headers.
  q_hdr <- list(answers = a, scales = list(a, a), attributes = data.frame(
    attribute = c("dualscale_headerA", "dualscale_headerB"),
    value = c("Importance", "Satisfaction"), language = c("", ""),
    stringsAsFactors = FALSE))
  rows_hdr <- lss_answer_rows(q_hdr, LANGS, th)
  expect_true(any(vapply(rows_hdr, function(r) isTRUE(r$section_with_text), logical(1))))
  # Dual-scale WITHOUT headers -> plain scale section headers.
  q_nohdr <- list(answers = a, scales = list(a, a), attributes = NULL)
  rows_nohdr <- lss_answer_rows(q_nohdr, LANGS, th)
  expect_true(length(rows_nohdr) >= 6L)
  # No answers -> empty.
  expect_identical(lss_answer_rows(list(answers = list()), LANGS, th), list())
})

# ---- attribute rows + formatting + exclusivity -------------------------

test_that("lss_format_attr hides exclusion attrs and titlecases the rest", {
  expect_null(lss_format_attr("exclude_all_others", c(en = "x"), "en"))
  expect_null(lss_format_attr("exclude_all_others_auto", c(en = "x"), "en"))
  fmt <- lss_format_attr("validation", c(en = "len > 0"), "en")
  expect_identical(fmt$label, "Validation")
  expect_identical(fmt$texts[["en"]], "len > 0")
})

test_that("lss_attr_rows renders present attrs, uses the empty-language fallback, skips noise", {
  th <- mk_theme()
  # validation present per-language.
  q1 <- list(attributes = data.frame(
    attribute = "validation", value = "len>0", language = "en",
    stringsAsFactors = FALSE))
  r1 <- lss_attr_rows(q1, c("en"), th, c("validation"))
  expect_length(r1, 1L)
  # value only on the empty-language row -> fallback fills the column.
  q2 <- list(attributes = data.frame(
    attribute = "prefix", value = "CHF", language = "",
    stringsAsFactors = FALSE))
  r2 <- lss_attr_rows(q2, LANGS, th, c("prefix"))
  expect_identical(r2[[1]]$texts[["fr"]], "CHF")
  # exclude_all_others is filtered out by lss_format_attr.
  q3 <- list(attributes = data.frame(
    attribute = "exclude_all_others", value = "SQ1", language = "",
    stringsAsFactors = FALSE))
  expect_length(lss_attr_rows(q3, LANGS, th, c("exclude_all_others")), 0L)
  # empty show_attrs / no attributes -> no rows.
  expect_length(lss_attr_rows(q1, LANGS, th, character(0)), 0L)
  expect_length(lss_attr_rows(list(attributes = NULL), LANGS, th, c("prefix")), 0L)
})

test_that("lss_exclusive_codes and lss_exclusive_row target only the named subquestion", {
  th <- mk_theme()
  q <- list(code = "Q1", attributes = data.frame(
    attribute = "exclude_all_others", value = "SQ1, SQ2", language = "",
    stringsAsFactors = FALSE))
  expect_identical(lss_exclusive_codes(q), c("SQ1", "SQ2"))
  expect_null(lss_exclusive_row(q, list(code = "SQ3"), LANGS, th))
  row <- lss_exclusive_row(q, list(code = "SQ1"), LANGS, th)
  expect_true(isTRUE(row$span_note))
  # No / blank attribute -> no codes.
  expect_identical(lss_exclusive_codes(list(attributes = NULL)), character(0))
  q_blank <- list(attributes = data.frame(
    attribute = "exclude_all_others", value = "  ", language = "",
    stringsAsFactors = FALSE))
  expect_identical(lss_exclusive_codes(q_blank), character(0))
})

# ---- lang block + group description, exercised through a real render ----

test_that("a group description and answer-order note render in both templates", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  # Inject a group description (all four languages) so the group-intro
  # language block (otherwise dead -- every demo group has an empty
  # description) is rendered.
  gl <- lss$group_l10ns
  gl$description[gl$gid == gl$gid[1]] <- "Intro text for this group."
  lss$group_l10ns <- gl

  # Both templates render the group-intro text: cards as the language
  # block under the group title, table as a banded description row under
  # the group-name row.
  for (tmpl in c("cards", "table")) {
    out <- tempfile(fileext = ".docx")
    on.exit(unlink(out), add = TRUE)
    render_questionnaire(lss, out, template = tmpl,
                         languages = c("en", "de", "es", "fr"),
                         chrome_lang = "en", show_help = TRUE)
    expect_true(file.exists(out))
    txt <- paste(officer::docx_summary(officer::read_docx(out))$text, collapse = " | ")
    expect_true(grepl("Intro text for this group", txt, fixed = TRUE),
                info = tmpl)
  }
})

# ---- PDF conversion path (only when LibreOffice is installed) ----------

test_that(".docx_to_pdf validates inputs and reports a missing converter", {
  expect_error(.docx_to_pdf(123, tempfile(fileext = ".pdf")), class = "lssdoc_bad_input")
  expect_error(.docx_to_pdf(tempfile(fileext = ".docx"), tempfile()), class = "lssdoc_bad_input")
  docx <- tempfile(fileext = ".docx")
  writeLines("x", docx); on.exit(unlink(docx), add = TRUE)
  expect_error(.docx_to_pdf(docx, c("a", "b")), class = "lssdoc_bad_output")
})
