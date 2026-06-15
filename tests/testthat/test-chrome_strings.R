test_that("lss_chrome_strings returns a localized string set for every supported language", {
  required <- c(
    "cover_subtitle_review", "cover_subtitle_audit",
    "cover_source_file", "cover_survey_id", "cover_languages",
    "cover_groups", "cover_questions", "cover_subquestions",
    "cover_answer_options", "cover_last_modified", "cover_generated",
    "toc_title", "welcome_text_title", "end_text_title", "variable_index_title",
    "consent_title",
    "quotas_title", "quota_limit", "quota_when_full", "quota_active",
    "quota_inactive", "quota_condition", "quota_message",
    "quota_action_terminate", "quota_action_confirm",
    "meta_no", "meta_variable", "meta_type", "meta_mandatory", "meta_filter",
    "item_language", "item_question", "item_subquestion",
    "item_help", "item_value", "item_options", "item_value_scale_fmt",
    "item_exclusive",
    "mandatory_yes", "mandatory_no", "mandatory_soft",
    "filter_all", "filter_and", "filter_or", "filter_answered",
    "filter_empty", "filter_in", "filter_matches",
    "type_single_choice", "type_multiple_choice", "type_text", "type_number",
    "type_date", "type_ranking", "type_file_upload", "type_computed",
    "type_display", "type_text_short", "type_text_long", "type_text_other",
    "type_single_choice_with_comment", "type_multiple_choice_with_comment",
    "value_multi_y_blank", "value_multi_y_blank_with_comment",
    "value_yes_no", "value_gender", "value_5point", "value_numeric_input",
    "value_free_text_short", "value_free_text",
    "value_date_input", "value_computed", "value_ranking", "value_file_upload",
    "exclusive_text_fmt",
    "audit_findings_title", "audit_no_anomalies", "audit_summary_fmt",
    "audit_col_severity", "audit_col_check", "audit_col_location",
    "audit_col_language", "audit_col_message",
    "audit_severity_error", "audit_severity_warning", "audit_severity_note",
    "orcid_label"
  )
  for (lang in c("en", "fr", "de", "es", "it")) {
    pack <- lss_chrome_strings(lang)
    expect_type(pack, "list")
    missing <- setdiff(required, names(pack))
    expect_identical(
      missing, character(0),
      info = sprintf("language %s missing keys: %s",
                     lang, paste(missing, collapse = ", "))
    )
    # Every value must be a single non-empty string -- no NA, no
    # blank, no accidental NULL.
    vals <- vapply(pack[required], function(v) {
      is.character(v) && length(v) == 1L && !is.na(v) && nzchar(v)
    }, logical(1L))
    expect_true(
      all(vals),
      info = sprintf("language %s has empty/NA keys: %s",
                     lang, paste(required[!vals], collapse = ", "))
    )
  }
})

test_that("lss_resolve_chrome_lang defaults to languages[1] when supported, else English", {
  expect_identical(lss_resolve_chrome_lang(NULL, c("fr", "de")), "fr")
  expect_identical(lss_resolve_chrome_lang(NULL, c("de", "fr")), "de")
  expect_identical(lss_resolve_chrome_lang(NULL, "es"), "es")
  expect_identical(lss_resolve_chrome_lang(NULL, "it"), "it")
  # Content language not in the chrome whitelist -> fall back to EN.
  expect_identical(lss_resolve_chrome_lang(NULL, "ja"), "en")
  expect_identical(lss_resolve_chrome_lang(NULL, character(0)), "en")
  # Explicit values take precedence even when they differ from content.
  expect_identical(lss_resolve_chrome_lang("en", c("fr", "de")), "en")
  expect_identical(lss_resolve_chrome_lang("fr", c("de")), "fr")
})

test_that("lss_resolve_chrome_lang rejects unknown explicit values", {
  expect_error(
    lss_resolve_chrome_lang("ja", "fr"),
    class = "lssdoc_bad_chrome_lang"
  )
  expect_error(
    lss_resolve_chrome_lang(123, "fr"),
    class = "lssdoc_bad_chrome_lang"
  )
  expect_error(
    lss_resolve_chrome_lang(c("en", "fr"), "fr"),
    class = "lssdoc_bad_chrome_lang"
  )
})

test_that("lss_localized_type_label uses the chrome strings", {
  theme <- lss_render_theme()
  for (lang in c("en", "fr", "de", "es", "it")) {
    theme$chrome <- lss_chrome_strings(lang)
    expect_identical(
      lss_localized_type_label(list(type = "L"), theme),
      theme$chrome$type_single_choice
    )
    expect_identical(
      lss_localized_type_label(list(type = "M"), theme),
      theme$chrome$type_multiple_choice
    )
    expect_identical(
      lss_localized_type_label(list(type = "N"), theme),
      theme$chrome$type_number
    )
  }
})

test_that("the rendered document contains chrome strings in the requested language", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  out_fr <- tempfile(fileext = ".docx")
  on.exit(unlink(out_fr), add = TRUE)
  render_questionnaire(lss, out_fr, languages = c("fr", "de"), chrome_lang = "fr")
  s_fr <- officer::docx_summary(officer::read_docx(out_fr))
  txt_fr <- paste(s_fr$text[!is.na(s_fr$text)], collapse = " | ")
  # A handful of FR-specific chrome strings should appear; we do not
  # check every key to keep the test resilient to minor wording tweaks.
  expect_true(grepl("Sous-questions", txt_fr))
  expect_true(grepl("Table des matières", txt_fr))
  expect_true(grepl("Choix unique|Choix multiple", txt_fr))
  expect_true(grepl("Valeur|Question", txt_fr))
  # And the English baseline should NOT leak through.
  expect_false(grepl("Table of contents", txt_fr))
  expect_false(grepl("Subquestions", txt_fr))
})

test_that("chrome_lang = 'en' forces English chrome even with FR/DE content", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  out_en <- tempfile(fileext = ".docx")
  on.exit(unlink(out_en), add = TRUE)
  render_questionnaire(lss, out_en, languages = c("fr", "de"), chrome_lang = "en")
  s_en <- officer::docx_summary(officer::read_docx(out_en))
  txt_en <- paste(s_en$text[!is.na(s_en$text)], collapse = " | ")
  expect_true(grepl("Table of contents", txt_en))
  expect_true(grepl("Subquestions", txt_en))
  expect_false(grepl("Sous-questions", txt_en))
  expect_false(grepl("Table des matières", txt_en))
})
