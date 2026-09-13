# The reference spec exercises every v0 mechanism at once: all six kinds,
# a native other option with a custom position, multi-code exclusives, a
# capped multiple and a capped ranking, the three relevance forms, and an
# end-of-survey quota.
full_spec <- function() {
  lss_spec(
    title = "Enquête de démonstration",
    language = "fr",
    welcome = c("Premier paragraphe.", "Deuxième paragraphe."),
    end_text = "<p>Fin.</p>",
    groups = list(
      list(title = "Consentement", questions = list(
        list(code = "consent", kind = "single", mandatory = TRUE,
             text = "Acceptez-vous ?",
             options = list(list(text = "Oui"), list(text = "Non")))
      )),
      list(title = "Corps", questions = list(
        list(code = "note", kind = "display",
             text = "Un texte affiché sans saisie."),
        list(code = "profil", kind = "single", text = "Votre profil ?",
             relevance = "consent = 1",
             options = list(
               list(text = "Employé·e"), list(text = "Indépendant·e"),
               list(text = "Autre, merci de préciser", other = TRUE))),
        list(code = "soutiens", kind = "multiple", mandatory = TRUE,
             text = "Quels soutiens ?", max_answers = 2,
             relevance = "profil in [1, autre]",
             other_position = "specific", other_position_code = "3",
             options = list(
               list(text = "Du temps"), list(text = "De l'argent"),
               list(text = "Des conseils"),
               list(text = "Autre soutien, merci de préciser", other = TRUE),
               list(text = "Aucun soutien", exclusive = TRUE),
               list(text = "Je ne sais pas", exclusive = TRUE))),
        list(code = "aspects", kind = "array", text = "Évaluez :",
             rows = list(list(text = "Le contenu"), list(text = "Le rythme")),
             columns = list(list(text = "Bien"), list(text = "Moyen"),
                            list(text = "Mauvais"))),
        list(code = "classement", kind = "ranking", mandatory = TRUE,
             text = "Classez :", max_answers = 2,
             relevance = "count(soutiens) >= 1",
             options = list(list(text = "Alpha"), list(text = "Beta"),
                            list(text = "Gamma"))),
        list(code = "commentaire", kind = "text", text = "Un commentaire ?")
      ))
    ),
    quotas = list(list(question = "consent", code = "2",
                       message = "Merci, le questionnaire s'arrête ici."))
  )
}

write_full <- function() {
  out <- tempfile(fileext = ".lss")
  suppressMessages(write_lss(full_spec(), out))
  out
}

section_df <- function(lss, name) as.data.frame(lss[[name]])

test_that("write_lss produces a file that read_lss accepts and audit_lss clears", {
  out <- write_full()
  expect_true(file.exists(out))
  lss <- read_lss(out)
  expect_s3_class(lss, "lss")
  expect_identical(lss$languages, "fr")

  audit <- audit_lss(out)
  findings <- audit$findings
  expect_identical(sum(findings$severity == "error"), 0L)
})

test_that("questions carry the right types, mandatory flags and order", {
  lss <- read_lss(write_full())
  q <- section_df(lss, "questions")
  q <- q[order(as.integer(q$gid), as.integer(q$question_order)), ]
  expect_identical(q$title,
    c("consent", "note", "profil", "soutiens", "aspects", "classement",
      "commentaire"))
  expect_identical(q$type, c("L", "X", "L", "M", "F", "R", "T"))
  expect_identical(q$mandatory, c("Y", "N", "N", "Y", "N", "Y", "N"))
})

test_that("options land in the correct sections with contiguous codes", {
  lss <- read_lss(write_full())
  q <- section_df(lss, "questions")
  subs <- section_df(lss, "subquestions")
  ans <- section_df(lss, "answers")

  soutiens <- q$qid[q$title == "soutiens"]
  expect_identical(subs$title[subs$parent_qid == soutiens], c("1", "2", "3", "4", "5"))

  profil <- q$qid[q$title == "profil"]
  expect_identical(ans$code[ans$qid == profil], c("1", "2"))
  expect_identical(q$other[q$title == "profil"], "Y")

  aspects <- q$qid[q$title == "aspects"]
  expect_identical(subs$title[subs$parent_qid == aspects], c("1", "2"))
  expect_identical(ans$code[ans$qid == aspects], c("1", "2", "3"))
})

test_that("relevance equations are translated to ExpressionScript", {
  lss <- read_lss(write_full())
  q <- section_df(lss, "questions")
  rel <- stats::setNames(q$relevance, q$title)
  expect_identical(unname(rel["profil"]), 'consent.NAOK == "1"')
  expect_identical(unname(rel["soutiens"]),
                   '(profil.NAOK == "1" or profil.NAOK == "-oth-")')
  expect_match(rel[["classement"]],
               "count\\(soutiens_1\\.NAOK, soutiens_2\\.NAOK, soutiens_3\\.NAOK, soutiens_4\\.NAOK, soutiens_5\\.NAOK\\) >= 1")
})

test_that("attributes: localized other text, position, exclusives, caps", {
  lss <- read_lss(write_full())
  q <- section_df(lss, "questions")
  at <- section_df(lss, "question_attributes")
  of <- function(code, name) {
    rows <- at[at$qid == q$qid[q$title == code] & at$attribute == name, ]
    rows
  }
  # localized attribute MUST carry the language, or LimeSurvey ignores it
  ort <- of("soutiens", "other_replace_text")
  expect_identical(ort$value, "Autre soutien, merci de préciser")
  expect_identical(ort$language, "fr")

  expect_identical(of("soutiens", "other_position")$value, "specific")
  expect_identical(of("soutiens", "other_position_code")$value, "3")
  expect_identical(of("soutiens", "exclude_all_others")$value, "4;5")
  expect_identical(of("soutiens", "max_answers")$value, "2")
  expect_identical(of("classement", "min_answers")$value, "1")
})

test_that("quota terminates the survey on the declared answer", {
  lss <- read_lss(write_full())
  q <- section_df(lss, "questions")
  quota <- section_df(lss, "quotas")
  members <- section_df(lss, "quota_members")
  expect_identical(quota$qlimit, "0")
  expect_identical(quota$action, "1")
  expect_identical(members$code, "2")
  expect_identical(members$qid, as.character(q$qid[q$title == "consent"]))
})

test_that("survey settings merge defaults, overrides and texts", {
  out <- tempfile(fileext = ".lss")
  suppressMessages(
    write_lss(full_spec(), out, settings = list(anonymized = "Y"))
  )
  lss <- read_lss(out)
  sv <- section_df(lss, "surveys")
  expect_identical(sv$anonymized, "Y")
  expect_identical(sv$template, "vanilla")
  expect_identical(sv$expires, "")

  ls_row <- section_df(lss, "survey_language_settings")
  expect_identical(ls_row$surveyls_title, "Enquête de démonstration")
  expect_identical(ls_row$surveyls_welcometext,
                   "<p>Premier paragraphe.</p><p>Deuxième paragraphe.</p>")
  expect_identical(ls_row$surveyls_endtext, "<p>Fin.</p>")
})

# ---- specification validation ----------------------------------------------

minimal <- function(...) {
  q <- list(...)
  lss_spec(title = "T", groups = list(list(title = "G", questions = list(
    list(code = "base", kind = "multiple", text = "B",
         options = list(list(text = "Un"), list(text = "Deux"),
                        list(text = "Autre, merci de préciser", other = TRUE))),
    q
  ))))
}

test_that("lss_spec rejects malformed questions with precise errors", {
  expect_error(minimal(code = "2x", kind = "single", text = "Q",
                       options = list(list(text = "A"), list(text = "B"))),
               class = "lssdoc_bad_spec")
  expect_error(minimal(code = "base", kind = "single", text = "Q",
                       options = list(list(text = "A"), list(text = "B"))),
               class = "lssdoc_bad_spec")   # duplicate code
  expect_error(minimal(code = "q", kind = "wat", text = "Q"),
               class = "lssdoc_bad_spec")
  expect_error(minimal(code = "q", kind = "single", text = "Q",
                       options = list(list(text = "Seule"))),
               class = "lssdoc_bad_spec")   # one option only
  expect_error(minimal(code = "q", kind = "ranking", text = "Q",
                       options = list(list(text = "A"), list(text = "B"),
                                      list(text = "Autre", other = TRUE))),
               class = "lssdoc_bad_spec")   # other on a ranking
  expect_error(minimal(code = "q", kind = "multiple", text = "Q", max_answers = 3,
                       options = list(list(text = "A"), list(text = "B"))),
               class = "lssdoc_bad_spec")   # cap >= options
})

test_that("lss_spec rejects broken relevance references", {
  expect_error(minimal(code = "q", kind = "single", text = "Q",
                       relevance = "fantome = 1",
                       options = list(list(text = "A"), list(text = "B"))),
               class = "lssdoc_bad_spec")
  expect_error(minimal(code = "q", kind = "single", text = "Q",
                       relevance = "base = 9",
                       options = list(list(text = "A"), list(text = "B"))),
               class = "lssdoc_bad_spec")
  expect_error(minimal(code = "q", kind = "single", text = "Q",
                       relevance = "n'importe quoi",
                       options = list(list(text = "A"), list(text = "B"))),
               class = "lssdoc_bad_spec")
  # forward reference: cites a question defined later
  expect_error(
    lss_spec(title = "T", groups = list(list(title = "G", questions = list(
      list(code = "avant", kind = "single", text = "A", relevance = "apres = 1",
           options = list(list(text = "x"), list(text = "y"))),
      list(code = "apres", kind = "single", text = "B",
           options = list(list(text = "x"), list(text = "y")))
    )))),
    class = "lssdoc_bad_spec")
  # count() on a single-choice question
  expect_error(minimal(code = "q", kind = "single", text = "Q",
                       relevance = "count(q) >= 1",
                       options = list(list(text = "A"), list(text = "B"))),
               class = "lssdoc_bad_spec")
})

test_that("lss_spec validates quotas and other placement", {
  base_group <- list(title = "G", questions = list(
    list(code = "c", kind = "single", text = "C?",
         options = list(list(text = "Oui"), list(text = "Non")))))
  expect_error(
    lss_spec(title = "T", groups = list(base_group),
             quotas = list(list(question = "c", code = "9", message = "m"))),
    class = "lssdoc_bad_spec")
  expect_error(
    lss_spec(title = "T", groups = list(base_group),
             quotas = list(list(question = "zz", code = "1", message = "m"))),
    class = "lssdoc_bad_spec")
  expect_error(minimal(code = "q", kind = "multiple", text = "Q",
                       other_position = "specific", other_position_code = "9",
                       options = list(list(text = "A"), list(text = "B"),
                                      list(text = "Autre", other = TRUE))),
               class = "lssdoc_bad_spec")
})

test_that("auto-numbering skips the other option and respects explicit codes", {
  spec <- lss_spec(title = "T", groups = list(list(title = "G", questions = list(
    list(code = "q", kind = "single", text = "Q",
         options = list(list(text = "A"),
                        list(text = "Autre, merci de préciser", other = TRUE),
                        list(text = "B"),
                        list(text = "Zéro", code = "99"),
                        list(text = "C")))
  ))))
  opts <- spec$groups[[1]]$questions[[1]]$options
  codes <- vapply(opts, function(o) if (is.null(o$code)) "-oth-" else o$code,
                  character(1))
  expect_identical(codes, c("1", "-oth-", "2", "99", "100"))
})

# ---- regressions from the adversarial review -------------------------------

test_that("user strings with braces keep classed errors", {
  expect_error(minimal(code = "q", kind = "single", text = "Q",
                       relevance = "a = {oops}",
                       options = list(list(text = "A"), list(text = "B"))),
               class = "lssdoc_bad_spec")
})

test_that("sid must be a whole positive number", {
  expect_error(write_lss(full_spec(), tempfile(), sid = "abc"),
               class = "lssdoc_bad_sid")
  expect_error(write_lss(full_spec(), tempfile(), sid = 100001.9),
               class = "lssdoc_bad_sid")
})

test_that("the other option cannot be exclusive", {
  expect_error(minimal(code = "q", kind = "multiple", text = "Q",
                       options = list(list(text = "A"), list(text = "B"),
                                      list(text = "Autre", other = TRUE,
                                           exclusive = TRUE))),
               class = "lssdoc_bad_spec")
})

test_that("= and in cannot target a multiple-choice question", {
  expect_error(minimal(code = "q", kind = "single", text = "Q",
                       relevance = "base = 1",
                       options = list(list(text = "A"), list(text = "B"))),
               class = "lssdoc_bad_spec")
})

test_that("explicit option codes follow the LimeSurvey format", {
  expect_error(minimal(code = "q", kind = "single", text = "Q",
                       options = list(list(text = "A", code = "-oth-"),
                                      list(text = "B"))),
               class = "lssdoc_bad_spec")
  expect_error(minimal(code = "q", kind = "single", text = "Q",
                       options = list(list(text = "A", code = "toolong"),
                                      list(text = "B"))),
               class = "lssdoc_bad_spec")
})

test_that("a cap equal to the option count is rejected", {
  expect_error(minimal(code = "q", kind = "multiple", text = "Q",
                       max_answers = 2,
                       options = list(list(text = "A"), list(text = "B"))),
               class = "lssdoc_bad_spec")
})

test_that("group titles and question texts must be single strings", {
  expect_error(
    lss_spec(title = "T", groups = list(list(title = c("A", "B"),
      questions = list(list(code = "q", kind = "text", text = "Q"))))),
    class = "lssdoc_bad_spec")
  expect_error(
    lss_spec(title = "T", groups = list(list(title = "G",
      questions = list(list(code = "q", kind = "text",
                            text = c("Q1", "Q2")))))),
    class = "lssdoc_bad_spec")
})

test_that("settings values and reserved names are validated", {
  expect_error(write_lss(full_spec(), tempfile(),
                         settings = list(language = "en")),
               class = "lssdoc_bad_settings")
  expect_error(write_lss(full_spec(), tempfile(),
                         settings = list(anonymized = TRUE)),
               class = "lssdoc_bad_settings")
  expect_error(write_lss(full_spec(), tempfile(),
                         settings = list(showprogress = NULL)),
               class = "lssdoc_bad_settings")
  expect_error(write_lss(full_spec(), tempfile(), settings = list("Y")),
               class = "lssdoc_bad_settings")
})

test_that("a plain optional uncapped ranking carries no min_answers", {
  spec <- lss_spec(title = "T", groups = list(list(title = "G", questions = list(
    list(code = "rk", kind = "ranking", text = "Classez",
         options = list(list(text = "A"), list(text = "B")))))))
  out <- tempfile(fileext = ".lss")
  suppressMessages(write_lss(spec, out))
  at <- as.data.frame(read_lss(out)$question_attributes)
  expect_false(any(at$attribute == "min_answers"))
})

test_that("a multi-element welcome always gets paragraph wrapping", {
  spec <- lss_spec(title = "T", welcome = c("<b>Bienvenue</b>", "Suite."),
                   groups = list(list(title = "G", questions = list(
    list(code = "q", kind = "text", text = "Q")))))
  out <- tempfile(fileext = ".lss")
  suppressMessages(write_lss(spec, out))
  ls_row <- as.data.frame(read_lss(out)$survey_language_settings)
  expect_identical(ls_row$surveyls_welcometext,
                   "<p><b>Bienvenue</b></p><p>Suite.</p>")
})

test_that("a spec mutated after construction is revalidated at write time", {
  spec <- full_spec()
  spec$groups[[2]]$questions[[3]]$relevance <- "du grand n'importe quoi"
  expect_error(write_lss(spec, tempfile()), class = "lssdoc_bad_spec")
})

# ---- corpus-attested kind coverage -----------------------------------------

test_that("every corpus-attested kind emits its type, theme and sections", {
  opts2 <- list(list(text = "Un"), list(text = "Deux"))
  spec <- lss_spec(title = "Types", groups = list(list(title = "G", questions = list(
    list(code = "sg", kind = "single", text = "?", options = opts2),
    list(code = "dd", kind = "dropdown", text = "?", options = opts2),
    list(code = "sc", kind = "singlecomment", text = "?", options = opts2),
    list(code = "mu", kind = "multiple", text = "?", options = opts2),
    list(code = "ar", kind = "array", text = "?", rows = opts2, columns = opts2),
    list(code = "a5", kind = "array5", text = "?", rows = opts2),
    list(code = "a10", kind = "array10", text = "?", rows = opts2),
    list(code = "ayn", kind = "arrayyesno", text = "?", rows = opts2),
    list(code = "atr", kind = "arraytrend", text = "?", rows = opts2),
    list(code = "rk", kind = "ranking", text = "?", options = opts2),
    list(code = "mt", kind = "multitext", text = "?", options = opts2),
    list(code = "mn", kind = "multinumeric", text = "?", options = opts2),
    list(code = "tx", kind = "text", text = "?"),
    list(code = "st", kind = "shorttext", text = "?"),
    list(code = "ht", kind = "hugetext", text = "?"),
    list(code = "nm", kind = "numeric", text = "?"),
    list(code = "dt", kind = "date", text = "?"),
    list(code = "yn", kind = "yesno", text = "?"),
    list(code = "ge", kind = "gender", text = "?"),
    list(code = "fp", kind = "fivepoint", text = "?"),
    list(code = "di", kind = "display", text = "?"),
    list(code = "apres", kind = "single", text = "?",
         relevance = "yn = Y", options = opts2),
    list(code = "apres2", kind = "single", text = "?",
         relevance = "fp in [3, 4]", options = opts2)
  ))))
  out <- tempfile(fileext = ".lss")
  suppressMessages(write_lss(spec, out))
  lss <- read_lss(out)
  audit <- audit_lss(out)
  expect_identical(sum(audit$findings$severity == "error"), 0L)

  q <- as.data.frame(lss$questions)
  types <- stats::setNames(q$type, q$title)
  expect_identical(unname(types[c("sg", "dd", "sc", "mu", "ar")]),
                   c("L", "!", "O", "M", "F"))
  expect_identical(unname(types[c("a5", "a10", "ayn", "atr", "rk")]),
                   c("A", "B", "C", "E", "R"))
  expect_identical(unname(types[c("mt", "mn", "tx", "st", "ht")]),
                   c("Q", "K", "T", "S", "U"))
  expect_identical(unname(types[c("nm", "dt", "yn", "ge", "fp", "di")]),
                   c("N", "D", "Y", "G", "5", "X"))

  themes <- stats::setNames(q$question_theme_name, q$title)
  expect_identical(unname(themes[c("a5", "yn", "fp", "dd")]),
                   c("arrays/5point", "yesno", "5pointchoice", "list_dropdown"))

  subs <- as.data.frame(lss$subquestions)
  ans <- as.data.frame(lss$answers)
  where <- function(code) {
    qid <- q$qid[q$title == code]
    c(subs = sum(subs$parent_qid == qid), ans = sum(ans$qid == qid))
  }
  expect_identical(where("dd"), c(subs = 0L, ans = 2L))
  expect_identical(where("mt"), c(subs = 2L, ans = 0L))
  expect_identical(where("a5"), c(subs = 2L, ans = 0L))
  expect_identical(where("yn"), c(subs = 0L, ans = 0L))
  expect_identical(where("fp"), c(subs = 0L, ans = 0L))

  rel <- stats::setNames(q$relevance, q$title)
  expect_identical(unname(rel["apres"]), 'yn.NAOK == "Y"')
  expect_identical(unname(rel["apres2"]), '(fp.NAOK == "3" or fp.NAOK == "4")')
})

test_that("implicit-scale and row-only kinds reject stray options", {
  expect_error(minimal(code = "q", kind = "yesno", text = "?",
                       options = list(list(text = "Oui"), list(text = "Non"))),
               class = "lssdoc_bad_spec")
  expect_error(minimal(code = "q", kind = "array5", text = "?",
                       rows = list(list(text = "A")),
                       columns = list(list(text = "1"))),
               class = "lssdoc_bad_spec")
  expect_error(minimal(code = "q", kind = "fivepoint", text = "?",
                       relevance = "base = 1"),
               class = "lssdoc_bad_spec")
  # relevance citing an implicit code that does not exist
  expect_error(
    lss_spec(title = "T", groups = list(list(title = "G", questions = list(
      list(code = "yn", kind = "yesno", text = "?"),
      list(code = "q", kind = "text", text = "?", relevance = "yn = X"))))),
    class = "lssdoc_bad_spec")
})

# ---- schema version (LSS_DBVERSION) ----------------------------------------

test_that("an unknown kind is rejected, never silently degraded", {
  expect_error(minimal(code = "q", kind = "carrousel", text = "?"),
               class = "lssdoc_bad_spec")
  expect_error(minimal(code = "q", kind = "", text = "?"),
               class = "lssdoc_bad_spec")
})

test_that("write_lss() emits the DBVersion the package targets", {
  out <- tempfile(fileext = ".lss")
  on.exit(unlink(out), add = TRUE)
  write_lss(minimal(code = "q1", kind = "text", text = "Votre avis ?"), out)

  xml <- rawToChar(readBin(out, "raw", file.size(out)))
  expect_true(grepl("<DBVersion>700</DBVersion>", xml, fixed = TRUE,
                    useBytes = TRUE))
  expect_true(grepl(paste0("<DBVersion>", LSS_DBVERSION, "</DBVersion>"),
                    xml, fixed = TRUE, useBytes = TRUE))
})

# Smallest document `read_lss()` accepts: doc type, DBVersion, languages.
db_version_fixture <- function(version) {
  tmp <- tempfile(fileext = ".lss")
  writeLines(c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<document>",
    "<LimeSurveyDocType>Survey</LimeSurveyDocType>",
    paste0("<DBVersion>", version, "</DBVersion>"),
    "<languages><language>en</language></languages>",
    "</document>"
  ), tmp)
  tmp
}

test_that("read_lss() warns when the export is newer than LSS_DBVERSION", {
  tmp <- db_version_fixture(999)
  on.exit(unlink(tmp), add = TRUE)

  expect_warning(lss <- read_lss(tmp), class = "lssdoc_newer_dbversion")
  expect_s3_class(lss, "lss")
  expect_identical(lss$db_version, "999")
})

test_that("read_lss() stays quiet at the targeted DBVersion", {
  tmp <- db_version_fixture(LSS_DBVERSION)
  on.exit(unlink(tmp), add = TRUE)

  expect_no_warning(lss <- read_lss(tmp))
  expect_identical(lss$db_version, LSS_DBVERSION)
})

# ---- languages --------------------------------------------------------------

one_group <- list(list(title = "G", questions = list(
  list(code = "q1", kind = "text", text = "Q"))))

bilingual_spec <- function() {
  lss_spec(
    title = c(fr = "Enquete", en = "Survey"),
    languages = c("fr", "en"),
    welcome = list(fr = c("Bonjour", "Merci"), en = "Hello"),
    end_text = c(fr = "Fin", en = "The end"),
    groups = list(list(
      title = c(fr = "Profil", en = "Profile"),
      questions = list(
        list(code = "q1", kind = "single",
             text = c(fr = "Votre statut ?", en = "Your status?"),
             help = c(fr = "Une seule reponse", en = "One answer only"),
             options = list(
               list(text = c(fr = "Employe", en = "Employee")),
               list(text = c(fr = "Independant", en = "Self-employed")),
               list(text = c(fr = "Autre", en = "Other"), other = TRUE)))))),
    quotas = list(list(question = "q1", code = "2",
                       name = c(fr = "Independants", en = "Self-employed"),
                       message = c(fr = "Merci", en = "Thanks")))
  )
}

test_that("language is a backward-compatible alias for languages", {
  by_alias <- lss_spec(title = "T", groups = one_group, language = "fr")
  by_vector <- lss_spec(title = "T", groups = one_group, languages = "fr")
  expect_identical(by_alias, by_vector)
  expect_identical(by_alias, lss_spec(title = "T", groups = one_group))
  expect_identical(by_vector$languages, "fr")
  expect_identical(by_vector$language, "fr")

  # agreeing on the primary language is fine; disagreeing is not
  expect_identical(
    lss_spec(title = "T", groups = one_group,
             languages = "de", language = "de")$language, "de")
  expect_error(
    lss_spec(title = "T", groups = one_group,
             languages = c("fr", "en"), language = "en"),
    class = "lssdoc_bad_spec")
  expect_error(
    lss_spec(title = "T", groups = one_group, languages = c("fr", "fr")),
    class = "lssdoc_bad_spec")
})

test_that("per-language texts are stored as a named list over the languages", {
  spec <- bilingual_spec()
  expect_identical(spec$languages, c("fr", "en"))
  expect_identical(spec$language, "fr")
  expect_identical(spec$title, list(fr = "Enquete", en = "Survey"))
  expect_identical(spec$welcome, list(fr = c("Bonjour", "Merci"), en = "Hello"))
  expect_identical(spec$end_text, list(fr = "Fin", en = "The end"))

  g <- spec$groups[[1]]
  expect_identical(g$title, list(fr = "Profil", en = "Profile"))
  q <- g$questions[[1]]
  expect_identical(q$text, list(fr = "Votre statut ?", en = "Your status?"))
  expect_identical(q$help, list(fr = "Une seule reponse", en = "One answer only"))
  expect_identical(q$options[[1]]$text, list(fr = "Employe", en = "Employee"))
  expect_identical(spec$quotas[[1]]$message, list(fr = "Merci", en = "Thanks"))

  # a plain string stays a string, stored under the primary language
  mono <- lss_spec(title = "T", groups = one_group)
  expect_identical(mono$title, list(fr = "T"))
})

test_that("write_lss() refuses a multi-language spec, for now", {
  err <- expect_error(write_lss(bilingual_spec(), tempfile(fileext = ".lss")),
                      class = "lssdoc_unsupported_multilang")
  expect_match(conditionMessage(err), "0.3.0", fixed = TRUE)
  expect_match(conditionMessage(err), "en", fixed = TRUE)
})

test_that("a declared language with no text is refused", {
  err <- expect_error(
    lss_spec(title = c(fr = "T", en = "T"), languages = c("fr", "en"),
             groups = list(list(title = c(fr = "G", en = "G"), questions = list(
               list(code = "q1", kind = "text", text = c(fr = "Q")))))),
    class = "lssdoc_bad_spec")
  expect_match(conditionMessage(err), "en", fixed = TRUE)

  # a plain string covers the primary language only
  expect_error(
    lss_spec(title = "T", languages = c("fr", "en"), groups = one_group),
    class = "lssdoc_bad_spec")
})

test_that("texts cannot use an undeclared language, nor skip the primary one", {
  err <- expect_error(
    lss_spec(title = c(fr = "T", de = "T"), groups = one_group),
    class = "lssdoc_bad_spec")
  expect_match(conditionMessage(err), "de", fixed = TRUE)
  expect_error(
    lss_spec(title = c(en = "T"), languages = "fr", groups = one_group),
    class = "lssdoc_bad_spec")
})

test_that("a monolingual spec still round-trips through write and read", {
  spec <- lss_spec(
    title = "Titre", welcome = "Bienvenue", end_text = "Fin",
    groups = list(list(title = "Groupe", questions = list(
      list(code = "q1", kind = "single", text = "Question ?", help = "Aide",
           options = list(list(text = "Oui"), list(text = "Non"),
                          list(text = "Autre", other = TRUE)))))),
    quotas = list(list(question = "q1", code = "2",
                       name = "Refus", message = "Merci")))
  out <- tempfile(fileext = ".lss")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(write_lss(spec, out))
  lss <- read_lss(out)

  expect_identical(lss$languages, "fr")
  ls_row <- as.data.frame(lss$survey_language_settings)
  expect_identical(ls_row$surveyls_title, "Titre")
  expect_identical(ls_row$surveyls_welcometext, "<p>Bienvenue</p>")
  expect_identical(ls_row$surveyls_endtext, "<p>Fin</p>")
  expect_identical(as.data.frame(lss$group_l10ns)$group_name, "Groupe")
  ql <- as.data.frame(lss$question_l10ns)
  expect_identical(ql$question, "Question ?")
  expect_identical(ql$help, "Aide")
  expect_identical(as.data.frame(lss$answer_l10ns)$answer, c("Oui", "Non"))
  at <- as.data.frame(lss$question_attributes)
  expect_identical(at$value[at$attribute == "other_replace_text"], "Autre")
  qls <- as.data.frame(lss$quota_languagesettings)
  expect_identical(qls$quotals_name, "Refus")
  expect_identical(qls$quotals_message, "Merci")
})
