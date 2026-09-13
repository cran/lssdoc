# Coverage for the dense "table" (codebook) template: the row-builder
# closures (dual-scale / 2-D array, multiple-choice exclusive note,
# array "Other") and the render-side branches (scale-header rows, empty
# group name, empty answer label, the exclusive Field marker). Each test
# either calls an internal helper directly with a crafted model or a
# crafted row list, or asserts on the row kinds produced.

mk_theme_tt <- function(lang = "en") {
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings(lang)
  th
}
L2 <- c("en", "fr")

# ---- lss_render_table_template: empty rows guard (line 20) ---------------

test_that("lss_render_table_template returns the doc unchanged for empty rows", {
  skip_if_not_installed("officer")
  th <- mk_theme_tt()
  doc <- officer::read_docx()
  state <- lss_render_state(NULL)
  out <- lss_render_table_template(
    doc, list(), L2, th,
    show_help = TRUE, show_attrs = character(0), state = state
  )
  expect_identical(out, doc)
})

# ---- lss_render_table_template: crafted mixed rows -----------------------
# Covers the render-side branches that never fire for a clean demo:
#  * scale_header row      -> df$Value assignment (57), compose `next` (176),
#                             polish bg + bold Value (1154-1155)
#  * group row w/ empty name -> the `name <- ""` fallback (199)
#  * value row w/ empty label -> the em-dash empty-marker compose (219-224)
#  * mc_exclusive row      -> the Field="Exclusive" assignment (71)

test_that("lss_render_table_template renders scale-header, empty-name, empty-label and exclusive rows", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  th <- mk_theme_tt()
  doc <- officer::read_docx()
  state <- lss_render_state(NULL)
  rows <- list(
    list(kind = "group", name_by_lang = list(en = "", fr = "")),
    list(kind = "scale_header", text = "Value (scale 1)"),
    list(kind = "value", code = "1", labels = list(en = "", fr = "")),
    list(kind = "mc_exclusive", text = "When checked, clears others")
  )
  out <- lss_render_table_template(
    doc, rows, L2, th,
    show_help = TRUE, show_attrs = character(0), state = state
  )
  expect_s3_class(out, "rdocx")
})

# ---- lss_table_text_row: NULL / empty language settings (line 305) -------

test_that("lss_table_text_row returns NULL when the survey has no language settings", {
  lss_no_ls <- list(survey_language_settings = NULL)
  expect_null(
    lss_table_text_row(lss_no_ls, L2, "surveyls_welcometext", "welcome")
  )
  lss_zero <- list(survey_language_settings = data.frame(
    surveyls_language = character(0), surveyls_welcometext = character(0),
    stringsAsFactors = FALSE
  ))
  expect_null(
    lss_table_text_row(lss_zero, L2, "surveyls_welcometext", "welcome")
  )
})

# ---- row builder: multi-scale value rows (lines 435, 440-443) ------------
# A leaf question whose $scales carries two bundles, the second empty, so
# emit_value_rows_for() emits a "scale_header" row (440-443) for the first
# and skips the empty second bundle with `next` (435).

test_that("emit_value_rows_for emits a scale-header row and skips an empty scale", {
  th <- mk_theme_tt()
  a1 <- list(code = "1", labels = list(en = "Low", fr = "Bas"),
             aid = "10", scale_id = "0", sortorder = "0")
  q <- list(
    type = "L", code = "qA", mandatory = "N", relevance = NULL, other = "N",
    texts = list(en = list(question = "Stem A", help = NULL),
                 fr = list(question = "Tige A", help = NULL)),
    answers = list(a1),
    scales = list(list(a1), list()),   # scale 1: one answer; scale 2: empty
    subquestions = NULL, attributes = NULL
  )
  g <- list(questions = list(q))
  state <- lss_render_state(NULL)
  rows <- lss_table_template_rows_for_group(
    g, L2, th, show_help = TRUE, state = state, show_groups = FALSE
  )
  kinds <- vapply(rows, function(r) as.character(r$kind), character(1L))
  expect_true("scale_header" %in% kinds)
  # exactly one value row survives (the empty scale contributed none)
  expect_identical(sum(kinds == "value"), 1L)
})

# ---- row builder: non-dual array subquestions -> sq_scale (line 592) -----
# and the array-level "Other" free-text row (lines 657-664).

test_that("a non-dual array with subquestions and Other builds sq/value/other rows", {
  th <- mk_theme_tt()
  mk_sq <- function(code, q, fr) list(
    code = code, scale_id = "0",
    texts = list(en = list(question = q, help = NULL),
                 fr = list(question = fr, help = NULL))
  )
  q <- list(
    type = "F", code = "qF", mandatory = "N", relevance = NULL, other = "Y",
    texts = list(en = list(question = "Array stem", help = NULL),
                 fr = list(question = "Tige", help = NULL)),
    answers = list(list(code = "1", labels = list(en = "Yes", fr = "Oui")),
                   list(code = "2", labels = list(en = "No",  fr = "Non"))),
    scales = NULL,
    subquestions = list(mk_sq("SQ1", "Row 1", "Ligne 1"),
                        mk_sq("SQ2", "Row 2", "Ligne 2")),
    attributes = NULL
  )
  g <- list(questions = list(q))
  state <- lss_render_state(NULL)
  rows <- lss_table_template_rows_for_group(
    g, L2, th, show_help = TRUE, state = state, show_groups = FALSE
  )
  kinds <- vapply(rows, function(r) as.character(r$kind), character(1L))
  expect_true("subq" %in% kinds)
  expect_true("other" %in% kinds)
})

# ---- row builder: dual-scale array, no dualscale header (lines 635-637) --
# A type "1" question whose $scales has two bundles (dual_scale = TRUE) but
# no dualscale_header* attribute, so lss_dualscale_header() returns NULL and
# the "Scale N" fallback header is used.

test_that("a dual-scale array without a header attribute falls back to 'Scale N'", {
  th <- mk_theme_tt()
  a0 <- list(code = "1", labels = list(en = "Low", fr = "Bas"), scale_id = "0")
  a1 <- list(code = "1", labels = list(en = "A",   fr = "A"),   scale_id = "1")
  q <- list(
    type = "1", code = "qD", mandatory = "N", relevance = NULL, other = "N",
    texts = list(en = list(question = "Dual stem", help = NULL),
                 fr = list(question = "Tige duale", help = NULL)),
    answers = list(a0, a1),
    scales = list("0" = list(a0), "1" = list(a1)),   # length 2 -> dual scale
    subquestions = list(list(
      code = "SQ1", scale_id = "0",
      texts = list(en = list(question = "Sub 1", help = NULL),
                   fr = list(question = "Sous 1", help = NULL))
    )),
    attributes = NULL   # no dualscale_headerA/B -> NULL header -> fallback
  )
  g <- list(questions = list(q))
  state <- lss_render_state(NULL)
  rows <- lss_table_template_rows_for_group(
    g, L2, th, show_help = TRUE, state = state, show_groups = FALSE
  )
  subq_rows <- Filter(function(r) identical(r$kind, "subq"), rows)
  expect_true(length(subq_rows) >= 1L)
  # chrome$item_scale (en) is "Scale"; fallback header reads "Scale 1"
  expect_match(subq_rows[[1]]$scale_header[["en"]], "^Scale ")
})

# ---- row builder: multiple-choice exclusive note (lines 528-531) ---------

test_that("emit_multiple_choice emits an exclusive-option note row", {
  th <- mk_theme_tt()
  mk_sq <- function(code, q, fr) list(
    code = code,
    texts = list(en = list(question = q, help = NULL),
                 fr = list(question = fr, help = NULL))
  )
  q <- list(
    type = "M", code = "qM", mandatory = "N", relevance = NULL, other = "N",
    texts = list(en = list(question = "MC stem", help = NULL),
                 fr = list(question = "Tige MC", help = NULL)),
    answers = NULL, scales = NULL,
    subquestions = list(mk_sq("SQ1", "Opt 1", "Opt 1 fr"),
                        mk_sq("SQ2", "Opt 2", "Opt 2 fr")),
    attributes = data.frame(
      attribute = "exclude_all_others", value = "SQ1", language = "",
      stringsAsFactors = FALSE
    )
  )
  g <- list(questions = list(q))
  state <- lss_render_state(NULL)
  rows <- lss_table_template_rows_for_group(
    g, L2, th, show_help = TRUE, state = state, show_groups = FALSE
  )
  kinds <- vapply(rows, function(r) as.character(r$kind), character(1L))
  expect_true("mc_exclusive" %in% kinds)
})

# ---- lss_table_question_paragraph: subq facet + empty add_line ----------
# Line 910 (`else f`): facet present but subquestion label empty -> the cell
# shows just the facet. Line 881 (add_line early return): a subq row whose
# label AND facet are both empty makes add_line() bail on empty text.

test_that("lss_table_question_paragraph handles a facet-only subq and an empty subq label", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  th <- mk_theme_tt()
  # facet present, subq label empty -> `else f` branch (line 910)
  facet_only <- list(
    kind = "subq",
    parent_text = list(en = "Stem"),
    subq_text = list(en = ""),
    scale_header = list(en = "Trust"),
    help = NULL
  )
  p1 <- lss_table_question_paragraph(facet_only, "en", th, show_help = FALSE)
  expect_false(is.null(p1))
  # subq label empty AND no facet -> add_line() called with "" -> return()
  empty_subq <- list(
    kind = "subq",
    parent_text = list(en = "Stem"),
    subq_text = list(en = ""),
    scale_header = NULL,
    help = NULL
  )
  p2 <- lss_table_question_paragraph(empty_subq, "en", th, show_help = FALSE)
  expect_false(is.null(p2))
})

# ---- %||_% null-coalesce helper (line 1214) ------------------------------

test_that("%||_% returns the fallback for NULL / empty and the value otherwise", {
  expect_identical(`%||_%`(NULL, "fallback"), "fallback")
  expect_identical(`%||_%`("", "fallback"), "fallback")
  expect_identical(`%||_%`("x", "fallback"), "x")
})
