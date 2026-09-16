# Coverage for the remaining reachable branches (batch 4).
# Drafted per source region, then validated. Each test targets specific
# uncovered lines in render_table_template / render_item / render_toc /
# render_cover / html / render_filter.


# ============================================================ html
test_that("lss_trim_runs returns early when every run is a pure empty non-linebreak", {
  # Non-empty on entry (passes the length-0 input guard) but every run is
  # empty text with no linebreak, so the keep filter drops them all and the
  # length-0 early return (return(runs)) executes.
  out <- lss_trim_runs(list(list(text = ""), list(text = "")))
  expect_length(out, 0L)
  expect_identical(out, list())
})

# ============================================================ render_filter
test_that("lss_filter_chrome resolves present tokens and falls back on empty or missing ones", {
  # theme$chrome is a non-NULL list, so lss_filter_chrome() does NOT take the
  # early `return(defaults)` on the guard line and instead evaluates the inner
  # pick() for every token. Mixing present values, an empty string, an explicit
  # NULL, and an absent key forces BOTH outcomes of the fallback line:
  #   if (is.null(v) || !nzchar(v)) fallback else v
  theme <- list(chrome = list(
    filter_all      = "Tous",   # present -> `else v` branch
    filter_and      = "ET",     # present -> `else v` branch
    filter_or       = "",       # empty string -> !nzchar(v) -> fallback
    filter_answered = NULL,     # explicit NULL element -> is.null(v) -> fallback
    filter_matches  = "corr."   # present -> `else v` branch
    # filter_empty deliberately absent -> lookup yields NULL -> is.null(v) -> fallback
  ))
  g <- lssdoc:::lss_filter_chrome(theme)
  # value branch (theme token kept)
  expect_identical(g$all,      "Tous")
  expect_identical(g$and,      "ET")
  expect_identical(g$matches,  "corr.")
  # fallback branch (English default returned)
  expect_identical(g$or,       "OR")           # empty string -> fallback
  expect_identical(g$answered, "is answered")  # explicit NULL -> fallback
  expect_identical(g$empty,    "is empty")     # absent key -> fallback
  # never-localized set glyphs still come from defaults
  expect_identical(g$inset,    "∈")
  expect_identical(g$notinset, "∉")
})

# ============================================================ render_cover
test_that("cover fallback branches fire for empty titles and null-valued metadata", {
  # A full render of the four-language demo costs about a minute and the
  # point here is a branch of the cover renderer, not the document: run
  # it locally and in CI, keep it out of the CRAN check budget.
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  lss <- demo_lss()

  # Drop every survey_language_settings row. lss_header_titles() then
  # yields empty titles, so the cover title loop hits the
  # "(untitled survey)" fallback (line 50), and the admin block's
  # ls_primary collapses to NULL, so pull_ls() takes its
  # null-ls_primary early return (line 129).
  lss$survey_language_settings <-
    lss$survey_language_settings[0, , drop = FALSE]

  # Present-but-NA survey flags: the admin pull_survey("active") and the
  # privacy pull_survey("anonymized") both take their is.na() early
  # returns (lines 137 and 165).
  lss$surveys$active     <- NA_character_
  lss$surveys$anonymized <- NA_character_

  out <- tempfile(fileext = ".docx")
  expect_no_error(
    render_questionnaire(
      lss, out,
      languages             = c("en", "fr"),
      show_admin_settings   = TRUE,
      show_privacy_settings = TRUE,
      # The empty middle line makes lss_split_text_urls() return an empty
      # chunk list, exercising the `next` in the description renderer
      # (line 297).
      description = "Pilot protocol.\n\nSee https://example.org/end."
    )
  )
  expect_true(file.exists(out))
})

test_that("cover admin block returns empty for an NA language-settings value", {
  # A full render of the four-language demo costs about a minute and the
  # point here is a branch of the cover renderer, not the document: run
  # it locally and in CI, keep it out of the CRAN check budget.
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")

  lss <- demo_lss()

  # Keep the primary-language row so ls_primary is non-empty (line 129's
  # guard is skipped), but blank the alias to NA so
  # pull_ls("surveyls_alias") hits the inner is.na() early return
  # (line 131).
  lss$survey_language_settings$surveyls_alias <- NA_character_

  out <- tempfile(fileext = ".docx")
  expect_no_error(
    render_questionnaire(
      lss, out,
      languages           = c("en", "fr"),
      show_admin_settings = TRUE
    )
  )
  expect_true(file.exists(out))
})

# ============================================================ render_toc
test_that("lss_render_quotas falls back to the qid and marks a memberless quota", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- lss_cached(path)
  th <- lss_render_theme(); th$chrome <- lss_chrome_strings("en")
  qm <- lss$quota_members
  # Keep only quota 52's members so quota 53 becomes memberless -> its
  # condition cell takes the empty-marker else branch (line 152). Then add a
  # member of quota 52 whose qid is absent from the questions table, so
  # q_title() takes its `else qid` branch (line 83).
  qm <- qm[qm$quota_id == "52", , drop = FALSE]
  qm <- rbind(qm, data.frame(id = "900", sid = qm$sid[1], qid = "99999999",
                             quota_id = "52", code = "ZZ",
                             stringsAsFactors = FALSE))
  lss$quota_members <- qm
  doc <- officer::read_docx()
  out <- lss_render_quotas(doc, lss, c("en", "fr"), th)
  expect_s3_class(out, "rdocx")
})

test_that("lss_render_quotas handles missing answer and quota language settings", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- lss_cached(path)
  th <- lss_render_theme(); th$chrome <- lss_chrome_strings("en")
  # answer_l10ns NULL -> ans_label() early-returns NA (line 86, reached because
  # the demo quotas carry members). quota_languagesettings NULL -> qls_field()
  # early-returns NA (line 94); every language's message is then NA so the
  # loop hits `next` (line 159) and no chunk is built, so the empty-message
  # marker paragraph is used (line 171).
  lss$answer_l10ns <- NULL
  lss$quota_languagesettings <- NULL
  doc <- officer::read_docx()
  out <- lss_render_quotas(doc, lss, c("en", "fr"), th)
  expect_s3_class(out, "rdocx")
})

test_that("lss_render_quotas uses a localized quota name for a displayed language", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- lss_cached(path)
  th <- lss_render_theme(); th$chrome <- lss_chrome_strings("en")
  qls <- lss$quota_languagesettings
  # Give the English rows a non-empty name so the name-resolution loop takes
  # its found-a-name branch (line 127) for a language that is actually
  # displayed (the demo ships every quotals_name empty).
  qls$quotals_name[qls$quotals_language == "en"] <- "English cap"
  lss$quota_languagesettings <- qls
  doc <- officer::read_docx()
  out <- lss_render_quotas(doc, lss, c("en", "fr"), th)
  expect_s3_class(out, "rdocx")
})

test_that("lss_consent_present is FALSE when the policy notice is turned off", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- lss_cached(path)
  # showsurveypolicynotice == "0" -> the notice is explicitly off, so the
  # function returns FALSE at the policy-off guard (line 234).
  lss$surveys$showsurveypolicynotice <- "0"
  expect_false(lss_consent_present(lss, c("en", "fr")))
})

test_that("lss_render_consent handles an absent notice column and an empty label", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- lss_cached(path)
  th <- lss_render_theme(); th$chrome <- lss_chrome_strings("en")
  ls <- lss$survey_language_settings
  # Drop the notice column so getf("surveyls_policy_notice", ...) takes its
  # field-absent branch (line 263) when has() probes it. Blank the French
  # label so the label cell function takes its empty-label branch (line 301),
  # while English keeps a label so the consent section still renders.
  ls$surveyls_policy_notice <- NULL
  ls$surveyls_policy_notice_label[ls$surveyls_language == "fr"] <- ""
  lss$survey_language_settings <- ls
  doc <- officer::read_docx()
  out <- lss_render_consent(doc, lss, c("en", "fr"), th)
  expect_s3_class(out, "rdocx")
})

# ============================================================ render_item

# ---- render_item.R uncovered-branch coverage ---------------------------
# Helpers scoped to this file so they never clash with mk_theme in the
# other coverage files.

ri_theme <- function(lang = "en") {
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings(lang)
  th
}
ri_state <- function(show_heading = FALSE) {
  st <- lss_render_state(NULL)
  st$show_item_heading <- show_heading
  st$show_raw_filter <- FALSE
  st
}
ri_audit <- function(code, severity = "error") {
  list(by_code = stats::setNames(
    list(data.frame(severity = severity, stringsAsFactors = FALSE)), code))
}
RI_L <- c("en", "fr")

test_that("ranking item shows the audit marker and a Help row (render_item 292, 327-331)", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  th <- ri_theme()
  q <- list(
    qid = "100", code = "devicerank", type = "R", type_label = "Ranking",
    mandatory = "N", relevance = "1", other = "N",
    texts = list(
      en = list(question = "Rank these devices", help = "Drag to order"),
      fr = list(question = "Classez ces appareils", help = "Glissez pour ordonner")
    ),
    answers = list(
      list(aid = "501", code = "phone", scale_id = "0", sortorder = "0",
           labels = list(en = "Phone", fr = "Telephone")),
      list(aid = "502", code = "tablet", scale_id = "0", sortorder = "1",
           labels = list(en = "Tablet", fr = "Tablette"))
    ),
    scales = NULL, subquestions = NULL, attributes = NULL
  )
  st <- ri_state(show_heading = TRUE)
  doc <- officer::read_docx()
  expect_no_error(
    doc <- lss_render_ranking_item(
      doc, q, RI_L, th, show_help = TRUE, show_attrs = character(0),
      audit_idx = ri_audit("devicerank", "error"), state = st)
  )
  expect_s3_class(doc, "rdocx")
  expect_identical(st$item_no, 1L)
})

test_that("multiple-choice item shows the audit marker on its heading (render_item 401)", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  th <- ri_theme()
  q <- list(
    qid = "200", code = "supportsources", type = "M",
    type_label = "Multiple choice", mandatory = "N", relevance = "1",
    other = "N",
    texts = list(
      en = list(question = "Where do you get support?", help = NA_character_),
      fr = list(question = "Sources de soutien", help = NA_character_)
    ),
    answers = list(), scales = NULL,
    subquestions = list(
      list(qid = "201", code = "family", scale_id = "0", relevance = "1",
           texts = list(en = list(question = "Family", help = NA_character_),
                        fr = list(question = "Famille", help = NA_character_)),
           attributes = NULL),
      list(qid = "202", code = "friends", scale_id = "0", relevance = "1",
           texts = list(en = list(question = "Friends", help = NA_character_),
                        fr = list(question = "Amis", help = NA_character_)),
           attributes = NULL)
    ),
    attributes = NULL
  )
  st <- ri_state(show_heading = TRUE)
  doc <- officer::read_docx()
  expect_no_error(
    doc <- lss_render_multiple_choice(
      doc, q, RI_L, th, show_help = FALSE, show_attrs = character(0),
      audit_idx = ri_audit("supportsources", "warning"), state = st)
  )
  expect_s3_class(doc, "rdocx")
})

test_that("subquestion item shows the audit marker on its heading (render_item 680)", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  th <- ri_theme()
  q <- list(
    qid = "300", code = "arr1", type = "F", type_label = "Array",
    mandatory = "N", relevance = "1", other = "N",
    texts = list(en = list(question = "How much do you agree?", help = NA_character_),
                 fr = list(question = "Etes-vous d accord", help = NA_character_)),
    answers = list(
      list(aid = "601", code = "1", scale_id = "0", sortorder = "0",
           labels = list(en = "Low", fr = "Bas")),
      list(aid = "602", code = "2", scale_id = "0", sortorder = "1",
           labels = list(en = "High", fr = "Haut"))
    ),
    scales = NULL, subquestions = NULL, attributes = NULL
  )
  sq <- list(qid = "301", code = "SQ1", scale_id = "0", relevance = "1",
             texts = list(en = list(question = "Item one", help = NA_character_),
                          fr = list(question = "Article un", help = NA_character_)),
             attributes = NULL)
  item_code <- "arr1[SQ1]"
  st <- ri_state(show_heading = TRUE)
  doc <- officer::read_docx()
  expect_no_error(
    doc <- lss_render_subq_item(
      doc, q, sq, RI_L, th, item_code = item_code, show_help = FALSE,
      show_attrs = character(0), audit_idx = ri_audit(item_code, "note"),
      state = st)
  )
  expect_s3_class(doc, "rdocx")
})

test_that("subquestion facet falls back to the facet alone when the row label is empty (render_item 727)", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  th <- ri_theme()
  q <- list(
    qid = "300", code = "arr1", type = "F", type_label = "Array",
    mandatory = "N", relevance = "1", other = "N",
    texts = list(en = list(question = "Rate the items", help = NA_character_),
                 fr = list(question = "Evaluez les items", help = NA_character_)),
    answers = list(
      list(aid = "601", code = "1", scale_id = "0", sortorder = "0",
           labels = list(en = "Yes", fr = "Oui")),
      list(aid = "602", code = "2", scale_id = "0", sortorder = "1",
           labels = list(en = "No", fr = "Non"))
    ),
    scales = NULL, subquestions = NULL, attributes = NULL
  )
  # Empty EN subquestion text -> s_ok FALSE -> the else branch (facet alone);
  # non-empty FR text -> s_ok TRUE -> the paste0 branch. Both arms of 727.
  sq <- list(qid = "301", code = "SQ1", scale_id = "0", relevance = "1",
             texts = list(en = list(question = "", help = NA_character_),
                          fr = list(question = "Article un", help = NA_character_)),
             attributes = NULL)
  csq <- list(qid = "302", code = "COL1", scale_id = "1", relevance = "1",
              texts = list(en = list(question = "Today", help = NA_character_),
                           fr = list(question = "Maintenant", help = NA_character_)),
              attributes = NULL)
  st <- ri_state()
  doc <- officer::read_docx()
  expect_no_error(
    doc <- lss_render_subq_item(
      doc, q, sq, RI_L, th, item_code = "arr1[SQ1_COL1]", show_help = FALSE,
      show_attrs = character(0), audit_idx = NULL, state = st, column = csq)
  )
  expect_s3_class(doc, "rdocx")
})

test_that("subquestion item emits the exclusive row when the parent names it (render_item 758)", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  th <- ri_theme()
  q <- list(
    qid = "300", code = "arr1", type = "F", type_label = "Array",
    mandatory = "N", relevance = "1", other = "N",
    texts = list(en = list(question = "Rate the items", help = NA_character_),
                 fr = list(question = "Evaluez les items", help = NA_character_)),
    answers = list(
      list(aid = "601", code = "1", scale_id = "0", sortorder = "0",
           labels = list(en = "Yes", fr = "Oui")),
      list(aid = "602", code = "2", scale_id = "0", sortorder = "1",
           labels = list(en = "No", fr = "Non"))
    ),
    scales = NULL, subquestions = NULL,
    attributes = data.frame(qid = "300", attribute = "exclude_all_others",
                            value = "SQ1", language = "",
                            stringsAsFactors = FALSE)
  )
  sq <- list(qid = "301", code = "SQ1", scale_id = "0", relevance = "1",
             texts = list(en = list(question = "Item one", help = NA_character_),
                          fr = list(question = "Article un", help = NA_character_)),
             attributes = NULL)
  st <- ri_state()
  doc <- officer::read_docx()
  expect_no_error(
    doc <- lss_render_subq_item(
      doc, q, sq, RI_L, th, item_code = "arr1[SQ1]", show_help = FALSE,
      show_attrs = character(0), audit_idx = NULL, state = st)
  )
  expect_s3_class(doc, "rdocx")
})

test_that("compound array with an NA subquestion scale_id hits the '0' fallback (render_item 202)", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  th <- ri_theme()
  info <- lss_type_info("F")
  q <- list(
    qid = "400", code = "arrx", type = "F", type_label = "Array",
    mandatory = "N", relevance = "1", other = "N",
    texts = list(en = list(question = "Rate the items", help = NA_character_),
                 fr = list(question = "Evaluez les items", help = NA_character_)),
    answers = list(
      list(aid = "701", code = "1", scale_id = "0", sortorder = "0",
           labels = list(en = "Yes", fr = "Oui")),
      list(aid = "702", code = "2", scale_id = "0", sortorder = "1",
           labels = list(en = "No", fr = "Non"))
    ),
    scales = NULL,
    subquestions = list(
      list(qid = "401", code = "R1", scale_id = NA_character_, relevance = "1",
           texts = list(en = list(question = "Row one", help = NA_character_),
                        fr = list(question = "Ligne un", help = NA_character_)),
           attributes = NULL)
    ),
    attributes = NULL
  )
  st <- ri_state()
  doc <- officer::read_docx()
  expect_no_error(
    doc <- lss_render_compound_question(
      doc, q, RI_L, th, show_help = FALSE, show_attrs = character(0),
      show_technical_attrs = FALSE, audit_idx = NULL, info = info, state = st)
  )
  expect_s3_class(doc, "rdocx")
})

test_that("Other item prompt falls back to 'Other:' when no other_replace_text row exists (render_item 540)", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  th <- ri_theme()
  q <- list(code = "natx", type = "L", relevance = "1",
            attributes = data.frame(qid = "500", attribute = "prefix",
                                    value = "pre", language = "",
                                    stringsAsFactors = FALSE))
  st <- ri_state()
  doc <- officer::read_docx()
  expect_no_error(
    doc <- lss_render_other_item(doc, q, RI_L, th, audit_idx = NULL, state = st)
  )
  expect_s3_class(doc, "rdocx")
})

test_that("Other item prompt uses a per-language value then the generic fallback (render_item 542, 545)", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  th <- ri_theme()
  # other_replace_text present for EN only: EN returns via the language hit
  # (542); FR finds no language and no empty-language value (545).
  q <- list(code = "naty", type = "L", relevance = "1",
            attributes = data.frame(qid = "500", attribute = "other_replace_text",
                                    value = "Please specify your country",
                                    language = "en", stringsAsFactors = FALSE))
  st <- ri_state()
  doc <- officer::read_docx()
  expect_no_error(
    doc <- lss_render_other_item(doc, q, RI_L, th, audit_idx = NULL, state = st)
  )
  expect_s3_class(doc, "rdocx")
})

test_that("lss_dualscale_header returns NULL when the header attribute is absent (render_item 880)", {
  q <- list(attributes = data.frame(attribute = "prefix", value = "x",
                                    language = "", stringsAsFactors = FALSE))
  expect_null(lss_dualscale_header(q, 1L, RI_L))
})

test_that("lss_apply_order_note uses the note as text when the header text is blank (render_item 1361)", {
  th <- ri_theme()
  vr <- list(list(label = "Value", texts = list(en = "", fr = ""),
                  section_header = TRUE, section_with_text = TRUE))
  out <- lss_apply_order_note(vr, "shown at random", RI_L, th)
  expect_identical(out[[1]]$texts[["en"]], "shown at random")
  expect_identical(out[[1]]$texts[["fr"]], "shown at random")
})

test_that("lss_attr_rows blank fallback and all-empty skip (render_item 1481, 1483)", {
  th <- ri_theme()
  # 1481: a language-specific value present for EN/DE but not for the
  # requested FR, and no empty-language fallback -> "" for FR, row kept.
  q1 <- list(attributes = data.frame(
    attribute = c("prefix", "prefix"), value = c("EUR", "CHF"),
    language = c("en", "de"), stringsAsFactors = FALSE))
  rows <- lss_attr_rows(q1, RI_L, th, "prefix")
  expect_length(rows, 1L)
  expect_identical(rows[[1]]$texts[["fr"]], "")

  # 1483: the attribute exists but every value is blank -> the whole row
  # is skipped via `next`.
  q2 <- list(attributes = data.frame(
    attribute = "suffix", value = "   ", language = "en",
    stringsAsFactors = FALSE))
  expect_length(lss_attr_rows(q2, RI_L, th, "suffix"), 0L)
})


# ============================================================ render_table_template
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
