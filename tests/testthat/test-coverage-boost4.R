# Direct unit tests for the render_table_template helpers, targeting the
# value/filter/question paragraph branches and the implicit-coding and
# "Other:" prompt logic that the fixture data does not fully exercise.

mk_theme4 <- function(lang = "en") {
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings(lang)
  th
}
ans4 <- function(code) list(code = code, labels = list(en = code, fr = code))

test_that("lss_table_value_codes collapses sequential codes and lists the rest", {
  expect_identical(lss_table_value_codes(lapply(1:5, ans4)), "1-5")
  expect_identical(lss_table_value_codes(list(ans4("1"), ans4("2"), ans4("99"))), "1, 2, 99")
  expect_identical(lss_table_value_codes(list()), "")
  expect_identical(lss_table_value_codes(list(ans4("a"), ans4("b"))), "a, b")
})

test_that("lss_table_implicit_value_text maps each type and NULLs the rest", {
  th <- mk_theme4()
  for (ty in c("M","P","Y","G","5","N","K","S","T","U","D","*","R","|")) {
    expect_false(is.null(lss_table_implicit_value_text(list(type = ty), th)), info = ty)
  }
  expect_null(lss_table_implicit_value_text(list(type = "X"), th))
})

test_that("lss_table_other_prompt resolves the customized prompt or falls back", {
  expect_identical(lss_table_other_prompt(list(attributes = NULL), "en"), "Other:")
  q_none <- list(attributes = data.frame(attribute = "prefix", value = "x",
                                         language = "", stringsAsFactors = FALSE))
  expect_identical(lss_table_other_prompt(q_none, "en"), "Other:")
  q_lang <- list(attributes = data.frame(
    attribute = "other_replace_text", value = c("Please specify", "Bitte"),
    language = c("en", "de"), stringsAsFactors = FALSE))
  expect_identical(lss_table_other_prompt(q_lang, "en"), "Please specify")
  q_empty <- list(attributes = data.frame(
    attribute = "other_replace_text", value = "Autre, precisez",
    language = "", stringsAsFactors = FALSE))
  expect_identical(lss_table_other_prompt(q_empty, "fr"), "Autre, precisez")
  q_blank <- list(attributes = data.frame(
    attribute = "other_replace_text", value = "", language = "en",
    stringsAsFactors = FALSE))
  expect_identical(lss_table_other_prompt(q_blank, "en"), "Other:")
})

test_that("lss_table_value_paragraph covers every value-domain branch", {
  skip_if_not_installed("flextable")
  th <- mk_theme4()
  cp <- officer::fp_text(font.size = 9)
  dp <- officer::fp_text(font.size = 9, italic = TRUE)
  # Other and MC-parent rows -> empty cell (early returns).
  expect_no_error(lss_table_value_paragraph(list(kind = "other"), th, cp, dp))
  expect_no_error(lss_table_value_paragraph(
    list(kind = "subq", mc_parent = TRUE, q = list(type = "M", answers = list())), th, cp, dp))
  # Enumerated answers -> empty cell.
  expect_no_error(lss_table_value_paragraph(
    list(kind = "leaf", q = list(type = "L", answers = list(ans4("1")))), th, cp, dp))
  # Predefined labelled (Y) -> empty cell.
  expect_no_error(lss_table_value_paragraph(
    list(kind = "leaf", q = list(type = "Y", answers = list())), th, cp, dp))
  # Short implicit tokens.
  for (ty in c("M","P","5","A","B")) {
    expect_no_error(lss_table_value_paragraph(
      list(kind = "leaf", q = list(type = ty, answers = list())), th, cp, dp))
  }
  # Open-ended descriptors + the X em-dash + the default fallback.
  for (ty in c("N","K",":","S","T","U",";","Q","D","*","R","|","X","ZZZ")) {
    expect_no_error(lss_table_value_paragraph(
      list(kind = "leaf", q = list(type = ty, answers = list())), th, cp, dp))
  }
})

test_that("lss_table_filter_paragraph shows the humanized form and optional raw", {
  skip_if_not_installed("flextable")
  th <- mk_theme4()
  pp <- officer::fp_text(font.size = 9)
  rp <- officer::fp_text(font.size = 8, italic = TRUE)
  # Trivial / absent relevance -> "1", no raw line.
  expect_no_error(lss_table_filter_paragraph(NULL, th, pp, rp, show_raw = TRUE))
  expect_no_error(lss_table_filter_paragraph("1", th, pp, rp, show_raw = TRUE))
  # Non-trivial relevance whose humanized form differs -> raw line added.
  expect_no_error(lss_table_filter_paragraph('workstatus == "1"', th, pp, rp, show_raw = TRUE))
  expect_no_error(lss_table_filter_paragraph('workstatus == "1"', th, pp, rp, show_raw = FALSE))
})

test_that("lss_table_question_paragraph stacks stem, subq+facet, help and the empty marker", {
  skip_if_not_installed("flextable")
  th <- mk_theme4()
  # Subquestion row WITH a second-axis facet (scale_header) + help.
  row_subq <- list(
    kind = "subq",
    parent_text = list(en = "How much do you trust ...?"),
    subq_text = list(en = "Parliament"),
    scale_header = list(en = "Trust"),
    help = list(en = "Pick one")
  )
  expect_no_error(lss_table_question_paragraph(row_subq, "en", th, show_help = TRUE))
  # Other row -> uses the Other prompt path.
  row_other <- list(kind = "other",
                    other_q = list(attributes = NULL),
                    parent_text = list(en = "x"))
  expect_no_error(lss_table_question_paragraph(row_other, "en", th, show_help = TRUE))
  # Empty everything -> the muted empty marker branch.
  row_empty <- list(kind = "leaf", parent_text = list(en = ""), help = list(en = ""))
  expect_no_error(lss_table_question_paragraph(row_empty, "en", th, show_help = TRUE))
})
