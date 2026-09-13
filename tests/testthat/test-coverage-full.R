# Targeted tests closing the remaining coverage gaps: edge branches of
# internal helpers reached by direct calls with fabricated inputs, plus a
# few "everything on" renders that exercise the optional render paths.

mk_theme <- function(lang = "en") {
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings(lang)
  th
}
L2 <- c("en", "fr")

# ---- read_lss: BOM handling + invalid pre-check --------------------------

test_that("read_lss strips a UTF-8 BOM and still parses", {
  f <- tempfile(fileext = ".lss")
  on.exit(unlink(f), add = TRUE)
  xml <- paste0("<document><LimeSurveyDocType>Survey</LimeSurveyDocType>",
                "<DBVersion>400</DBVersion>",
                "<languages><language>en</language></languages></document>")
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(xml)), f)
  expect_s3_class(read_lss(f), "lss")
})

test_that("read_lss strips a UTF-16 BOM then rejects non-XML content", {
  f <- tempfile(fileext = ".lss")
  on.exit(unlink(f), add = TRUE)
  writeBin(c(as.raw(c(0xFF, 0xFE)), charToRaw("not xml at all")), f)
  expect_error(read_lss(f), class = "lssdoc_invalid_xml")
})

# ---- render_item helpers -------------------------------------------------

test_that("lss_render_item_table returns the doc unchanged for empty rows", {
  skip_if_not_installed("officer")
  th <- mk_theme()
  doc <- officer::read_docx()
  expect_identical(lss_render_item_table(doc, th, L2, list()), doc)
})

test_that("lss_question_attr_value returns NULL when all values are blank", {
  q <- list(attributes = data.frame(
    attribute = "answer_order", value = c("", "  "), language = c("", ""),
    stringsAsFactors = FALSE))
  expect_null(lss_question_attr_value(q, "answer_order"))
})

test_that("lss_answer_rows skips an empty scale in a dual-scale bundle", {
  th <- mk_theme()
  a <- list(list(code = "1", labels = list(en = "Low", fr = "Bas")))
  q <- list(answers = a, scales = list(a, list()), attributes = NULL)
  rows <- lss_answer_rows(q, L2, th)
  expect_true(length(rows) >= 1L)
})

test_that("lss_apply_order_note appends to an existing descriptor header", {
  th <- mk_theme()
  vr <- list(list(label = "Value",
                  texts = stats::setNames(rep(list("Numeric input"), 2L), L2),
                  section_header = TRUE, section_with_text = TRUE))
  out <- lss_apply_order_note(vr, "shown at random", L2, th)
  expect_match(out[[1]]$texts[["en"]], "Numeric input shown at random", fixed = TRUE)
})

test_that("lss_render_lang_block renders with a visible header row", {
  skip_if_not_installed("flextable"); skip_if_not_installed("officer")
  th <- mk_theme()
  doc <- officer::read_docx()
  expect_no_error(
    lss_render_lang_block(doc, list(en = "Hello", fr = "Bonjour"), L2, th,
                          show_header = TRUE)
  )
})

test_that("lss_attr_rows uses the empty-language fallback value", {
  th <- mk_theme()
  q <- list(attributes = data.frame(
    attribute = "validation", value = "len > 0", language = "",
    stringsAsFactors = FALSE))
  rows <- lss_attr_rows(q, L2, th, c("validation"))
  expect_identical(rows[[1]]$texts[["fr"]], "len > 0")
})

# ---- render_meta_table ---------------------------------------------------

test_that("lss_table_polish styles the meta-header and the code column", {
  skip_if_not_installed("flextable")
  th <- mk_theme()
  df <- data.frame(code = c("1", "2"), en = "", fr = "",
                   check.names = FALSE, stringsAsFactors = FALSE)
  ft <- flextable::flextable(df)
  expect_no_error(lss_table_polish(ft, th, lang_cols = L2,
                                   meta_header = TRUE, has_code = TRUE))
})

test_that("lss_render_question_meta_table humanizes a non-trivial filter", {
  skip_if_not_installed("flextable"); skip_if_not_installed("officer")
  th <- mk_theme()
  doc <- officer::read_docx()
  expect_no_error(lss_render_question_meta_table(
    doc, th, item_no = 1L, variable = "q1", type = "L",
    type_label = "Single choice", mandatory = "N",
    relevance = "workstatus == \"1\"", show_raw_filter = TRUE))
})

# ---- render_utils: lss_compose ordered list + line break -----------------

test_that("lss_compose handles ordered/nested lists and explicit line breaks", {
  skip_if_not_installed("flextable")
  th <- mk_theme()
  expect_no_error(lss_compose("<ol><li>one<ul><li>deep</li></ul></li></ol>", th))
  expect_no_error(lss_compose("line one<br>line two", th))
})

# ---- chrome_strings: unknown type falls back to the baked label ----------

test_that("lss_localized_type_label falls back to q$type_label for unknown types", {
  th <- mk_theme()
  expect_identical(
    lss_localized_type_label(list(type = "ZZZ", type_label = "Plugin thing"), th),
    "Plugin thing")
})

# ---- html: trimming of leading/trailing whitespace runs ------------------

test_that("lss_html_to_text trims surrounding whitespace runs", {
  expect_identical(lss_html_to_text("  <b>hi</b>  "), "hi")
  expect_identical(lss_html_to_text("<p></p>"), "")
})

# ---- render_filter: glyphs resolved from chrome --------------------------

test_that("lss_filter_chrome reads glyphs from the theme chrome when present", {
  th <- mk_theme()
  g <- lss_filter_chrome(th)
  expect_true(is.list(g) && !is.null(g$all))
})

# ---- render_theme: character-vector authors ------------------------------

test_that("lss_normalize_authors accepts a named character vector", {
  out <- lss_normalize_authors(c("Amal Tawfik" = "HES-SO"))
  expect_identical(out[[1]]$name, "Amal Tawfik")
  expect_identical(out[[1]]$affiliation, "HES-SO")
})

# ---- lss_model: NULL l10n + index without a language column --------------

test_that("lss_localized returns all-NA when the l10n table is NULL", {
  out <- lssdoc:::lss_localized(NULL, "qid", "1", L2, c("question"))
  expect_true(is.na(out$en$question))
})

test_that("lss_build_l10n_index returns an empty env when columns are missing", {
  idx <- lssdoc:::lss_build_l10n_index(
    data.frame(qid = "1", stringsAsFactors = FALSE), "qid")
  expect_identical(length(ls(idx)), 0L)
})

# ---- render_layout: section props "auto" + header titles from settings ---

test_that("lss_render_section_props resolves auto to portrait", {
  skip_if_not_installed("officer")
  expect_no_error(lssdoc:::lss_render_section_props("auto", 2L))
})

test_that("lss_header_titles reads titles from the survey language settings", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  out <- lssdoc:::lss_header_titles(lss, c("en", "fr"))
  expect_length(out, 2L)
})

# ---- audit_lss: empty whitespace value + forward ref to unknown var ------

test_that("a filter that references an unknown variable is not a forward ref", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  q_ord <- order(suppressWarnings(as.integer(lss$questions$question_order)))
  lss$questions$relevance[q_ord[1]] <- "nonexistentvar.NAOK == 1"
  a <- audit_lss(lss)
  fwd <- a$findings[a$findings$check == "forward_filter_reference", ]
  expect_equal(nrow(fwd), 0L)
})

# ---- render_table_template helpers ---------------------------------------

test_that("lss_table_other_prompt falls back through language then empty then default", {
  q_lang <- list(attributes = data.frame(
    attribute = "other_replace_text", value = "Please specify",
    language = "en", stringsAsFactors = FALSE))
  expect_identical(lss_table_other_prompt(q_lang, "en"), "Please specify")
  expect_identical(lss_table_other_prompt(q_lang, "fr"), "Other:")
})

test_that("lss_table_question_paragraph composes subq with a facet and help", {
  skip_if_not_installed("flextable")
  th <- mk_theme()
  row <- list(kind = "subq",
              parent_text = list(en = "Trust in ...?"),
              subq_text = list(en = "Parliament"),
              scale_header = list(en = "Trust"),
              help = list(en = "pick one"))
  expect_no_error(lss_table_question_paragraph(row, "en", th, show_help = TRUE))
})

# ---- render_toc: consent detection + empty localized block ---------------

test_that("lss_consent_present is FALSE without policy settings", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  expect_type(lss_consent_present(lss, c("en", "fr")), "logical")
  lss$survey_language_settings <- NULL
  expect_false(lss_consent_present(lss, c("en", "fr")))
})

test_that("lss_render_localized_block no-ops without language settings", {
  skip_if_not_installed("officer")
  th <- mk_theme()
  doc <- officer::read_docx()
  lss <- list(survey_language_settings = NULL)
  expect_identical(
    lss_render_localized_block(doc, lss, c("en"), th, "surveyls_welcometext", "W"),
    doc)
})

# ---- kitchen-sink renders: every optional section on ---------------------

test_that("a render with every option enabled exercises the optional paths", {
  skip_on_cran()
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  # Inject an "Other:" custom prompt, a group description, and welcome /
  # end text so the corresponding render branches are reached.
  qa <- lss$question_attributes
  nat <- lss$questions$qid[lss$questions$title == "nationality"][1]
  lss$question_attributes <- rbind(qa, data.frame(
    qid = nat, attribute = "other_replace_text",
    value = "Please specify", language = "", stringsAsFactors = FALSE))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(
    lss, out, template = "cards", languages = c("en", "de", "es", "fr"),
    chrome_lang = "en", show_help = TRUE, show_audit = TRUE,
    show_index = TRUE, show_quotas = TRUE, show_raw_filter = TRUE)
  expect_true(file.exists(out))
})

test_that("rendering the audit-demo survey surfaces inline audit markers", {
  skip_on_cran()
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  path <- system.file("extdata", "audit_demo.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  for (tmpl in c("cards", "table")) {
    render_questionnaire(read_lss(path), out, template = tmpl,
                         chrome_lang = "en", show_audit = TRUE, show_help = TRUE)
    expect_true(file.exists(out))
  }
})
