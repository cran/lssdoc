# Multilingual emission (0.3.0, step 4).
#
# The reference for the format is `inst/extdata/demo_survey.lss`, a real
# four-language LimeSurvey export: one row per entity AND per language in
# every `*_l10ns` table, one `surveys_languagesettings` row per language,
# one `quota_languagesettings` row per quota and per language, and a
# localized question attribute carrying its language code -- without it,
# LimeSurvey silently ignores the attribute on import. The facts are written
# out at the top of `R/write_lss.R`; the tests below hold the emitter to them.

skip_if_no_docx <- function() {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
}

# Two real languages, every localizable text translated: survey title,
# welcome, end text, group title and description, question text and help,
# option labels, the other label, the quota name and message.
bi_spec <- function(languages = c("fr", "en")) {
  lss_spec(
    title = example_loc("Enquete bilingue", "Bilingual survey", languages),
    languages = languages,
    welcome = example_loc(c("Bienvenue.", "Merci de repondre."),
                          c("Welcome.", "Thank you for answering."),
                          languages),
    end_text = example_loc("Fin du questionnaire.", "End of the questionnaire.",
                           languages),
    groups = list(list(
      title = example_loc("Profil", "Profile", languages),
      description = example_loc("Quelques questions sur vous.",
                                "A few questions about you.", languages),
      questions = list(
        list(code = "statut", kind = "single", mandatory = TRUE,
             text = example_loc("Quel est votre statut ?",
                                "What is your status?", languages),
             help = example_loc("Une seule reponse.", "One answer only.",
                                languages),
             options = list(
               list(text = example_loc("Salarie", "Employee", languages)),
               list(text = example_loc("Independant", "Self-employed",
                                       languages)),
               list(text = example_loc("Autre, merci de preciser",
                                       "Other, please specify", languages),
                    other = TRUE))),
        list(code = "avis", kind = "array",
             text = example_loc("Votre avis ?", "Your opinion?", languages),
             rows = list(
               list(text = example_loc("Le rythme", "The pace", languages)),
               list(text = example_loc("Le contenu", "The content", languages))),
             columns = list(
               list(text = example_loc("Bien", "Good", languages)),
               list(text = example_loc("Moyen", "Average", languages))))))),
    quotas = list(list(
      question = "statut", code = "2",
      name = example_loc("Independants", "Self-employed", languages),
      message = example_loc("Merci, le questionnaire s'arrete ici.",
                            "Thank you, the questionnaire ends here.",
                            languages)))
  )
}

write_spec <- function(spec) {
  out <- tempfile(fileext = ".lss")
  suppressMessages(write_lss(spec, out))
  out
}


test_that("every declared language reaches the file, base language last", {
  lss <- read_lss(write_spec(bi_spec()))

  # `<languages>` is a membership set written in LimeSurvey's own order --
  # additional languages first, base language LAST -- so the set is what
  # matters and the base language comes from `surveys.language`
  expect_true(setequal(lss$languages, c("fr", "en")))
  expect_identical(lss$languages[[length(lss$languages)]], "fr")
  expect_identical(lss$base_language, "fr")
  expect_identical(lss$surveys$additional_languages[[1L]], "en")

  # one surveys_languagesettings row per language, the translated texts in it
  sls <- as.data.frame(lss$survey_language_settings)
  expect_identical(sls$surveyls_language, c("fr", "en"))
  expect_identical(sls$surveyls_title, c("Enquete bilingue", "Bilingual survey"))
  expect_match(sls$surveyls_welcometext[[2L]], "Welcome", fixed = TRUE)
  expect_match(sls$surveyls_endtext[[2L]], "End of the questionnaire",
               fixed = TRUE)
})

test_that("each l10n table holds one row per entity and per language", {
  lss <- read_lss(write_spec(bi_spec()))

  n_groups <- nrow(as.data.frame(lss$groups))
  n_questions <- nrow(as.data.frame(lss$questions))
  n_subquestions <- nrow(as.data.frame(lss$subquestions))
  n_answers <- nrow(as.data.frame(lss$answers))

  gl <- as.data.frame(lss$group_l10ns)
  ql <- as.data.frame(lss$question_l10ns)
  al <- as.data.frame(lss$answer_l10ns)

  expect_identical(nrow(gl), 2L * n_groups)
  # question_l10ns covers questions AND subquestions
  expect_identical(nrow(ql), 2L * (n_questions + n_subquestions))
  expect_identical(nrow(al), 2L * n_answers)

  for (df in list(gl, ql, al)) {
    expect_identical(sort(unique(df$language)), c("en", "fr"))
    expect_identical(as.integer(table(df$language)),
                     rep(nrow(df) %/% 2L, 2L))
  }

  # grouped by entity, all languages together, as the real export groups them
  expect_identical(gl$language, rep(c("fr", "en"), times = n_groups))
  expect_identical(ql$language,
                   rep(c("fr", "en"), times = n_questions + n_subquestions))
  expect_identical(al$language, rep(c("fr", "en"), times = n_answers))

  # ids stay unique inside each table
  for (df in list(gl, ql, al)) expect_false(anyDuplicated(df$id) > 0L)

  # and the translations really are the translations
  expect_true(all(c("Profil", "Profile") %in% gl$group_name))
  expect_true(all(c("Quel est votre statut ?", "What is your status?") %in%
                    ql$question))
  expect_true(all(c("Bien", "Good") %in% al$answer))
})

test_that("quota_languagesettings carries one row per quota and language", {
  lss <- read_lss(write_spec(bi_spec()))

  qls <- as.data.frame(lss$quota_languagesettings)
  expect_identical(nrow(qls), 2L)
  expect_identical(qls$quotals_language, c("fr", "en"))
  expect_identical(qls$quotals_quota_id, c("1", "1"))
  expect_false(anyDuplicated(qls$quotals_id) > 0L)
  expect_identical(qls$quotals_name, c("Independants", "Self-employed"))
  expect_match(qls$quotals_message[[2L]], "ends here", fixed = TRUE)

  # the quota row itself stays language-less
  expect_identical(nrow(as.data.frame(lss$quotas)), 1L)
})

test_that("other_replace_text is emitted once per language, with its code", {
  lss <- read_lss(write_spec(bi_spec()))
  attrs <- as.data.frame(lss$question_attributes)

  orp <- attrs[attrs$attribute == "other_replace_text", , drop = FALSE]
  expect_identical(nrow(orp), 2L)
  expect_identical(orp$language, c("fr", "en"))
  expect_identical(orp$value,
                   c("Autre, merci de preciser", "Other, please specify"))

  # a localized attribute without its language code is silently ignored by
  # LimeSurvey: none may be emitted empty
  expect_false(any(!nzchar(orp$language)))
  # global attributes stay language-less
  globals <- attrs[attrs$attribute != "other_replace_text", , drop = FALSE]
  if (nrow(globals)) expect_true(all(!nzchar(globals$language)))
})

test_that("an author-supplied localized attribute is emitted per language", {
  langs <- c("fr", "en")
  spec <- lss_spec(
    title = example_loc("Attributs", "Attributes", langs),
    languages = langs,
    groups = list(list(
      title = example_loc("Profil", "Profile", langs),
      questions = list(
        # a plain value is repeated in every language
        list(code = "salaire", kind = "numeric",
             text = example_loc("Votre salaire ?", "Your salary?", langs),
             attributes = list(prefix = "€", display_columns = "2")),
        # a per-language value is resolved language by language
        list(code = "classement", kind = "ranking",
             text = example_loc("Classez ces elements.", "Rank these.", langs),
             attributes = list(choice_title = c(fr = "A classer",
                                                en = "To rank")),
             options = list(
               list(text = example_loc("Le salaire", "Pay", langs)),
               list(text = example_loc("L'ambiance", "Atmosphere", langs))))))))

  attrs <- as.data.frame(read_lss(write_spec(spec))$question_attributes)

  pre <- attrs[attrs$attribute == "prefix", , drop = FALSE]
  expect_identical(pre$language, langs)
  expect_identical(pre$value, c("€", "€"))

  ct <- attrs[attrs$attribute == "choice_title", , drop = FALSE]
  expect_identical(ct$language, langs)
  expect_identical(ct$value, c("A classer", "To rank"))

  # a name LimeSurvey does not localize stays language-less, as before
  dc <- attrs[attrs$attribute == "display_columns", , drop = FALSE]
  expect_identical(nrow(dc), 1L)
  expect_identical(dc$language, "")
})

test_that("a localized attribute missing a declared language is refused", {
  spec <- bi_spec()
  spec$groups[[1]]$questions[[1]]$attributes <-
    list(printable_help = c(fr = "Aide imprimable"))
  expect_error(write_lss(spec, tempfile(fileext = ".lss")),
               class = "lssdoc_bad_spec")
})

test_that("settings carry per-language values for the surveyls_* fields", {
  out <- tempfile(fileext = ".lss")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(write_lss(
    bi_spec(), out,
    settings = list(
      anonymized = "Y",
      # LimeSurvey really does vary these by language
      surveyls_dateformat = c(fr = "5", en = "2"),
      surveyls_numberformat = "1",
      surveyls_description = list(fr = "Version francaise",
                                  en = "English version"))))
  lss <- read_lss(out)
  sls <- as.data.frame(lss$survey_language_settings)

  expect_identical(sls$surveyls_language, c("fr", "en"))
  expect_identical(sls$surveyls_dateformat, c("5", "2"))
  # a scalar reaches every language, exactly as before
  expect_identical(sls$surveyls_numberformat, c("1", "1"))
  expect_identical(sls$surveyls_description,
                   c("Version francaise", "English version"))
  # a `surveys`-table override is unaffected by the split
  expect_identical(lss$surveys$anonymized[[1L]], "Y")
})

test_that("a per-language setting is validated", {
  # a declared language missing from the value is an error, not a fallback
  expect_error(
    write_lss(bi_spec(), tempfile(fileext = ".lss"),
              settings = list(surveyls_dateformat = c(fr = "5"))),
    class = "lssdoc_bad_spec")
  expect_error(
    write_lss(bi_spec(), tempfile(fileext = ".lss"),
              settings = list(surveyls_dateformat = c(fr = 5, en = 2))),
    class = "lssdoc_bad_settings")
  expect_error(
    write_lss(bi_spec(), tempfile(fileext = ".lss"),
              settings = list(surveyls_dateformat = NULL)),
    class = "lssdoc_bad_settings")
})

test_that("loc_strict() refuses the fallback loc_text() allows", {
  x <- list(fr = "Bonjour", en = "Hello")
  expect_identical(loc_strict(x, "en", "test value"), "Hello")
  expect_identical(loc_strict(c(fr = "a", en = "b"), "en", "test value"), "b")
  # a plain value is not localized: it applies to every language
  expect_identical(loc_strict("CHF", "de", "test value"), "CHF")
  expect_identical(loc_strict(NULL, "de", "test value"), "")
  expect_null(loc_strict(NULL, "de", "test value", default = NULL))

  # where a rendering path legitimately falls back, the emitter aborts
  expect_identical(loc_text(x, "de"), "Bonjour")
  expect_error(loc_strict(x, "de", "test value"), class = "lssdoc_bad_spec")
  expect_error(loc_strict(c(fr = "a"), "en", "test value"),
               class = "lssdoc_bad_spec")
})

test_that("audit_lss() finds no missing translation in a bilingual file", {
  audit <- audit_lss(read_lss(write_spec(bi_spec())))
  expect_true(setequal(audit$languages, c("fr", "en")))
  expect_identical(sum(audit$findings$check == "missing_translation"), 0L)
  expect_identical(sum(audit$findings$check == "empty_in_all_languages"), 0L)
})

test_that("three languages are written, the untranslated one included", {
  langs <- c("fr", "en", "de")
  lss <- read_lss(write_spec(bi_spec(langs)))

  expect_true(setequal(lss$languages, langs))
  expect_identical(lss$base_language, "fr")
  expect_identical(lss$surveys$additional_languages[[1L]], "en de")

  sls <- as.data.frame(lss$survey_language_settings)
  expect_identical(sls$surveyls_language, langs)

  gl <- as.data.frame(lss$group_l10ns)
  expect_identical(gl$language, langs)

  orp <- as.data.frame(lss$question_attributes)
  orp <- orp[orp$attribute == "other_replace_text", , drop = FALSE]
  expect_identical(orp$language, langs)

  expect_identical(
    sum(audit_lss(lss)$findings$check == "missing_translation"), 0L)
})

test_that("the example spec can be built in several languages", {
  spec <- lss_example_spec(kinds = c("single", "array", "text"),
                           languages = c("fr", "en"))
  expect_identical(spec$languages, c("fr", "en"))
  expect_identical(spec$title,
                   list(fr = "Questionnaire d'exemple lssdoc",
                        en = "lssdoc example questionnaire"))
  expect_identical(names(spec$welcome), c("fr", "en"))
  expect_length(spec$welcome$fr, 2L)
  q <- spec$groups[[1]]$questions[[1]]
  expect_identical(names(q$text), c("fr", "en"))
  expect_identical(q$options[[1]]$text$en, "Yes, I take part")

  # a language with no wording of its own reuses the English, tagged
  three <- lss_example_spec(kinds = "single", languages = c("fr", "en", "de"))
  expect_identical(three$title$de, "lssdoc example questionnaire [de]")

  # the monolingual generator is untouched
  mono <- lss_example_spec(kinds = "single", lang = "fr")
  expect_identical(mono$languages, "fr")
  expect_identical(mono$title, list(fr = "Questionnaire d'exemple lssdoc"))
})

test_that("a bilingual spec survives the Word form round trip", {
  # A full Word form round trip (write, re-read, compare): exhaustive
  # and expensive, and covered again by the form tests. Local and CI.
  skip_on_cran()
  skip_if_no_docx()
  spec <- lss_example_spec(kinds = c("single", "multiple", "array", "text"),
                           languages = c("fr", "en"))
  path <- tempfile(fileext = ".docx")
  on.exit(unlink(path), add = TRUE)

  write_form_docx(spec, path, lang = "fr")
  back <- read_form_docx(path)
  expect_identical(back$languages, c("fr", "en"))
  expect_identical(canonical_spec(back), canonical_spec(spec))

  # and the survey both describe is the same file, byte for byte
  a <- write_spec(spec)
  b <- write_spec(back)
  on.exit(unlink(c(a, b)), add = TRUE)
  expect_identical(readBin(a, "raw", file.size(a)),
                   readBin(b, "raw", file.size(b)))

  lss <- read_lss(b)
  expect_true(setequal(lss$languages, c("fr", "en")))
  expect_identical(lss$base_language, "fr")
  expect_identical(
    sort(unique(as.data.frame(lss$question_l10ns)$language)), c("en", "fr"))
})

test_that("render_questionnaire() renders a bilingual emitted file", {
  skip_if_no_docx()
  lss_file <- write_spec(bi_spec())
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)

  suppressMessages(
    render_questionnaire(lss_file, out, languages = c("fr", "en")))
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("monolingual emission is unchanged by multilingual support", {
  # Every place the multilingual loops touch, checked on a single-language
  # spec: one l10n row per entity, a contiguous shared id sequence, no
  # additional language, one <languages> child, `quotals_id` still the quota
  # index. Byte identity against the committed 0.2.0 validation file is
  # checked by `dev/make_validation_lss.R`, whose output is git-ignored.
  spec <- bi_spec("fr")
  out <- write_spec(spec)
  lss <- read_lss(out)

  expect_identical(lss$languages, "fr")
  expect_identical(lss$base_language, "fr")
  expect_identical(lss$surveys$additional_languages[[1L]], "")

  sls <- as.data.frame(lss$survey_language_settings)
  expect_identical(nrow(sls), 1L)
  expect_identical(sls$surveyls_language, "fr")

  gl <- as.data.frame(lss$group_l10ns)
  ql <- as.data.frame(lss$question_l10ns)
  al <- as.data.frame(lss$answer_l10ns)
  expect_identical(nrow(gl), nrow(as.data.frame(lss$groups)))
  expect_identical(nrow(ql), nrow(as.data.frame(lss$questions)) +
                     nrow(as.data.frame(lss$subquestions)))
  expect_identical(nrow(al), nrow(as.data.frame(lss$answers)))

  ids <- as.integer(c(gl$id, ql$id, al$id))
  expect_identical(sort(ids), seq_len(length(ids)))

  qls <- as.data.frame(lss$quota_languagesettings)
  expect_identical(qls$quotals_id, qls$quotals_quota_id)

  orp <- as.data.frame(lss$question_attributes)
  orp <- orp[orp$attribute == "other_replace_text", , drop = FALSE]
  expect_identical(orp$language, "fr")

  # the <languages> element holds exactly one child
  doc <- xml2::read_xml(out)
  expect_identical(
    xml2::xml_text(xml2::xml_find_all(doc, "/document/languages/language")),
    "fr")
})
