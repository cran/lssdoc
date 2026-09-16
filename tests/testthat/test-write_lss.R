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

test_that("a quota hangs on any question with a single coded answer", {
  scaled <- function(kind, code) {
    lss_spec(title = "T", groups = list(list(title = "G", questions = list(
      list(code = "q", kind = kind, text = "Q?")))),
      quotas = list(list(question = "q", code = code, message = "m")))
  }
  # a fixed scale declares no option: the quota names one of the kind's own
  # codes, and a gender quota is the commonest real quota there is
  expect_s3_class(scaled("gender", "M"), "lss_spec")
  expect_s3_class(scaled("yesno", "N"), "lss_spec")
  expect_s3_class(scaled("fivepoint", "5"), "lss_spec")
  expect_error(scaled("gender", "X"), class = "lssdoc_bad_spec")
  expect_error(scaled("gender", "1"), class = "lssdoc_bad_spec")
  # ... and the emitted file names the question and the code as typed
  spec <- scaled("gender", "F")
  file <- tempfile(fileext = ".lss")
  suppressMessages(write_lss(spec, file))
  lss <- read_lss(file)
  expect_identical(lss$quota_members$code, "F")
  expect_identical(
    lss$questions$title[lss$questions$qid == lss$quota_members$qid], "q")

  # a declared option list still works, and a kind holding several answers
  # (or none) still cannot carry a quota
  listed <- function(kind) {
    lss_spec(title = "T", groups = list(list(title = "G", questions = list(
      list(code = "q", kind = kind, text = "Q?",
           options = list(list(text = "A"), list(text = "B")))))),
      quotas = list(list(question = "q", code = "2", message = "m")))
  }
  expect_s3_class(listed("dropdown"), "lss_spec")
  expect_s3_class(listed("singlecomment"), "lss_spec")
  expect_error(listed("multiple"), class = "lssdoc_bad_spec")
  expect_error(
    lss_spec(title = "T", groups = list(list(title = "G", questions = list(
      list(code = "q", kind = "text", text = "Q?")))),
      quotas = list(list(question = "q", code = "1", message = "m"))),
    class = "lssdoc_bad_spec")
})

test_that("an item code is as wide as the table LimeSurvey stores it in", {
  arrayed <- function(row_code, col_code) {
    lss_spec(title = "T", groups = list(list(title = "G", questions = list(
      list(code = "q", kind = "array", text = "Q?",
           rows = list(list(code = row_code, text = "L")),
           columns = list(list(code = col_code, text = "C")))))))
  }
  # array ROWS are subquestions (`questions.title`, varchar(20)): a real
  # export uses the width -- `STRESS` in inst/extdata/demo_survey.lss
  expect_s3_class(arrayed("STRESS", "1"), "lss_spec")
  expect_s3_class(arrayed("12345678901234567890", "1"), "lss_spec")
  expect_error(arrayed("123456789012345678901", "1"), class = "lssdoc_bad_spec")
  # array COLUMNS are answers (`answers.code`, varchar(5))
  expect_s3_class(arrayed("R1", "12345"), "lss_spec")
  expect_error(arrayed("R1", "STRESS"), class = "lssdoc_bad_spec")
  expect_error(arrayed("R1", "STRESS"), regexp = "stores this list as answers")

  # the same split on option lists: a multiple stores them as subquestions,
  # a single as answers
  optioned <- function(kind, code) {
    lss_spec(title = "T", groups = list(list(title = "G", questions = list(
      list(code = "q", kind = kind, text = "Q?",
           options = list(list(code = code, text = "A"),
                          list(code = "b", text = "B")))))))
  }
  expect_s3_class(optioned("multiple", "STRESS"), "lss_spec")
  expect_s3_class(optioned("multitext", "STRESS"), "lss_spec")
  expect_error(optioned("single", "STRESS"), class = "lssdoc_bad_spec")
  expect_error(optioned("ranking", "STRESS"), class = "lssdoc_bad_spec")
  # a purely numeric code is what real exports use for array rows, so the
  # rule is letters and digits, never "a letter first"
  expect_s3_class(optioned("multiple", "12"), "lss_spec")
  expect_error(optioned("multiple", "a_b"), class = "lssdoc_bad_spec")

  # and the width follows the STORAGE, not the field name
  expect_identical(option_code_width("array", "rows"), 20L)
  expect_identical(option_code_width("array", "columns"), 5L)
  expect_identical(option_code_width("multiple", "options"), 20L)
  expect_identical(option_code_width("single", "options"), 5L)
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

# ---- the consolidated kind table --------------------------------------------

test_that("lss_kinds holds one complete row per authorable kind", {
  expect_s3_class(lss_kinds, "data.frame")
  expect_identical(nrow(lss_kinds), 21L)
  expect_false(as.logical(anyDuplicated(lss_kinds$kind)))
  expect_false(as.logical(anyDuplicated(lss_kinds$type)))
  expect_false(as.logical(anyDuplicated(lss_kinds$theme)))

  # row order is user-visible: it is pasted into the unknown-kind error
  expect_identical(
    lss_kinds$kind,
    c("single", "dropdown", "singlecomment", "multiple",
      "array", "array5", "array10", "arrayyesno", "arraytrend", "ranking",
      "multitext", "multinumeric", "text", "shorttext", "hugetext",
      "numeric", "date", "yesno", "gender", "fivepoint", "display")
  )

  # every kind maps to a LimeSurvey type letter and a theme name
  expect_true(all(nzchar(lss_kinds$type)))
  expect_true(all(nchar(lss_kinds$type) == 1L))
  expect_true(all(nzchar(lss_kinds$theme)))
  expect_identical(
    lss_kinds$type,
    c("L", "!", "O", "M", "F", "A", "B", "C", "E", "R", "Q", "K",
      "T", "S", "U", "N", "D", "Y", "G", "5", "X")
  )
  expect_identical(
    lss_kinds$theme,
    c("listradio", "list_dropdown", "list_with_comment", "multiplechoice",
      "arrays/array", "arrays/5point", "arrays/10point",
      "arrays/yesnouncertain", "arrays/increasesamedecrease", "ranking",
      "multipleshorttext", "multiplenumeric", "longfreetext",
      "shortfreetext", "hugefreetext", "numerical", "date", "yesno",
      "gender", "5pointchoice", "boilerplate")
  )
})

test_that("the accessors reproduce the former per-kind vectors", {
  # relevance targets: `=` / `in` (formerly single_valued_kinds)
  expect_identical(
    kinds_where("relevance_role", "scalar"),
    c("single", "dropdown", "singlecomment", "yesno", "gender", "fivepoint")
  )
  # count() targets
  expect_identical(kinds_where("relevance_role", "count"), "multiple")

  # kinds carrying no options and no rows (formerly no_option_kinds)
  expect_identical(
    kinds_where("options", "forbidden"),
    c("text", "shorttext", "hugetext", "numeric", "date",
      "yesno", "gender", "fivepoint", "display")
  )
  # row-only arrays: rows required, implicit scale (formerly row_only_kinds)
  expect_identical(
    intersect(kinds_where("rows", "required"),
              kinds_where("columns", "forbidden")),
    c("array5", "array10", "arrayyesno", "arraytrend")
  )
  # the only kind needing both rows and columns
  expect_identical(
    intersect(kinds_where("rows", "required"),
              kinds_where("columns", "required")),
    "array"
  )
  # native other option (formerly other_kinds)
  expect_identical(kinds_where("other_allowed"),
                   c("single", "dropdown", "multiple"))
  expect_identical(kinds_where("exclusive_allowed"), "multiple")
  # a quota names one answer code of a one-answer question: every scalar kind
  # that has codes, the implicit scales included (a gender quota is the
  # commonest real quota there is)
  expect_identical(
    kinds_where("quota_target"),
    c("single", "dropdown", "singlecomment", "yesno", "gender", "fivepoint"))
  expect_identical(kinds_where("quota_target"),
                   kinds_where("relevance_role", "scalar"))
  expect_identical(kinds_where("implicit_min_answers"), "ranking")
  expect_identical(kinds_where("collects_response", FALSE), "display")
  expect_identical(kinds_where("max_answers_rule", "below_n"), "multiple")
  expect_identical(kinds_where("max_answers_rule", "at_most_n"), "ranking")

  # minimum option counts (formerly option_kinds), names and integer type
  minima <- stats::setNames(lss_kinds$min_options, lss_kinds$kind)
  expect_identical(
    minima[!is.na(minima)],
    c(single = 2L, dropdown = 2L, singlecomment = 2L, multiple = 2L,
      ranking = 2L, multitext = 1L, multinumeric = 1L)
  )

  # emission routing (formerly the switch() in lss_emitter)
  expect_identical(kinds_where("answers_from", "options"),
                   c("single", "dropdown", "singlecomment", "ranking"))
  expect_identical(kinds_where("answers_from", "columns"), "array")
  expect_identical(kinds_where("subquestions_from", "options"),
                   c("multiple", "multitext", "multinumeric"))
  expect_identical(kinds_where("subquestions_from", "rows"),
                   c("array", "array5", "array10", "arrayyesno", "arraytrend"))
})

test_that("the kind accessors are total and type-stable", {
  expect_true(is_kind("single"))
  expect_false(is_kind("nonsense"))
  expect_false(is_kind(NULL))
  expect_false(is_kind(1))
  expect_false(is_kind(character(0)))
  expect_false(is_kind(c("single", "text")))
  expect_false(is_kind(NA_character_))

  expect_identical(kind_field("text", "min_options"), NA_integer_)
  expect_identical(kind_field("multitext", "min_options"), 1L)
  expect_identical(kind_field("single", "min_options"), 2L)
  expect_identical(kind_field("nonsense", "type"), NA)
  expect_identical(kind_field(NULL, "type"), NA)
  expect_identical(kind_field("display", "collects_response"), FALSE)

  # implicit scales: a character vector, or NULL so `%||%` falls back
  expect_identical(kind_implicit_codes("yesno"), c("Y", "N"))
  expect_identical(kind_implicit_codes("gender"), c("M", "F"))
  expect_identical(kind_implicit_codes("fivepoint"), as.character(1:5))
  expect_null(kind_implicit_codes("single"))
  expect_null(kind_implicit_codes("text"))
  expect_null(kind_implicit_codes("nonsense"))

  row <- kind_row("array")
  expect_identical(nrow(row), 1L)
  expect_identical(row$type, "F")
  expect_identical(row$theme, "arrays/array")
})

test_that("the write-side table joins the read-side type taxonomy", {
  rt <- lss_question_types()
  expect_true(all(lss_kinds$type %in% rt$code))
  m <- match(lss_kinds$type, rt$code)
  expect_identical(rt$has_answers[m], !is.na(lss_kinds$answers_from))
  expect_identical(rt$has_subquestions[m], !is.na(lss_kinds$subquestions_from))
  expect_identical(rt$display_only[m], !lss_kinds$collects_response)
})

test_that("lss_kinds_reference() flattens the table for documentation", {
  ref <- lss_kinds_reference()
  expect_s3_class(ref, "data.frame")
  expect_identical(nrow(ref), nrow(lss_kinds))
  expect_identical(ref$kind, lss_kinds$kind)
  expect_identical(ref$label, lss_kinds$label)

  # knitr-ready: every cell short text, no list column, no NA, no logical
  expect_true(all(vapply(ref, is.character, logical(1))))
  expect_false(anyNA(unlist(ref, use.names = FALSE)))

  expect_identical(ref$implicit_codes[ref$kind == "yesno"], "Y, N")
  expect_identical(ref$implicit_codes[ref$kind == "fivepoint"], "1, 2, 3, 4, 5")
  expect_identical(ref$implicit_codes[ref$kind == "single"], "")
  expect_identical(ref$other[ref$kind == "dropdown"], "yes")
  expect_identical(ref$other[ref$kind == "ranking"], "")
  expect_identical(ref$min_options[ref$kind == "single"], "2")
  expect_identical(ref$min_options[ref$kind == "text"], "")
  expect_identical(ref$relevance[ref$kind == "multiple"], "count")
  expect_identical(ref$relevance[ref$kind == "text"], "")
  expect_identical(ref$max_answers[ref$kind == "ranking"], "at_most_n")
  expect_identical(ref$collects_response[ref$kind == "display"], "")
})

test_that("lss_kinds_deferred gives a reason for every excluded type", {
  expect_s3_class(lss_kinds_deferred, "data.frame")
  expect_identical(lss_kinds_deferred$type,
                   c("P", "H", "1", ";", ":", "*", "|", "I"))
  expect_true(all(nzchar(lss_kinds_deferred$reason)))

  # deferred means: a real LimeSurvey type that is not authorable
  expect_length(intersect(lss_kinds_deferred$type, lss_kinds$type), 0L)
  rt <- lss_question_types()
  expect_true(all(lss_kinds_deferred$type %in% rt$code))
  expect_identical(lss_kinds_deferred$label,
                   rt$label[match(lss_kinds_deferred$type, rt$code)])
  # authorable plus deferred covers the whole read-side taxonomy
  expect_identical(sort(c(lss_kinds$type, lss_kinds_deferred$type)),
                   sort(rt$code))
})

test_that("lss_spec_defaults is the single source of the authoring defaults", {
  expect_identical(lss_spec_defaults$mandatory, FALSE)
  expect_identical(lss_spec_defaults$relevance, "1")
  expect_identical(lss_spec_defaults$other, FALSE)
  expect_identical(lss_spec_defaults$exclusive, FALSE)
  expect_identical(lss_spec_defaults$option_code_from, 1L)
  expect_identical(lss_spec_defaults$language, "fr")
  expect_identical(lss_spec_defaults$primary_language, 1L)

  # a spec that declares none of them comes out carrying exactly these
  spec <- lss_spec(title = "T", groups = list(list(
    title = "G",
    questions = list(list(code = "q1", kind = "single", text = "Q ?",
                          options = list("A", "B"))))))
  q <- spec$groups[[1L]]$questions[[1L]]
  expect_identical(spec$language, lss_spec_defaults$language)
  expect_identical(q$mandatory, lss_spec_defaults$mandatory)
  expect_null(q$relevance)
  expect_identical(vapply(q$options, function(o) o$code, character(1)),
                   c("1", "2"))
  expect_identical(vapply(q$options, function(o) o$other, logical(1)),
                   rep(lss_spec_defaults$other, 2L))
  expect_identical(translate_relevance(NULL, list()),
                   lss_spec_defaults$relevance)
})

# ---- user-visible output, pinned -------------------------------------------

test_that("print.lss_spec() output is stable", {
  spec <- lss_spec(
    title = c(fr = "Enquete", en = "Survey"),
    languages = c("fr", "en"),
    groups = list(list(
      title = c(fr = "Profil", en = "Profile"),
      questions = list(
        list(code = "q1", kind = "yesno",
             text = c(fr = "Etes-vous d'accord ?", en = "Do you agree?")),
        list(code = "note", kind = "display",
             text = c(fr = "Merci.", en = "Thanks."))))))
  expect_snapshot(print(spec))
})

test_that("the unknown-kind error lists every authorable kind", {
  expect_snapshot(
    error = TRUE,
    lss_spec(title = "T", groups = list(list(
      title = "G",
      questions = list(list(code = "q1", kind = "wat", text = "Texte ?")))))
  )
})

# ---- 0.3.0 model additions: group description and quota limit ---------------

test_that("a group takes an optional localized description", {
  spec <- lss_spec(
    title = "T", languages = "fr",
    groups = list(list(
      title = "G", description = "Une introduction au groupe.",
      questions = list(list(code = "q1", kind = "yesno", text = "Oui ?")))))
  # canonical form: a named list over the declared languages, like every
  # other localizable text
  expect_identical(spec$groups[[1L]]$description,
                   list(fr = "Une introduction au groupe."))

  out <- tempfile(fileext = ".lss")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(write_lss(spec, out))
  x <- xml2::read_xml(out)
  desc <- xml2::xml_text(xml2::xml_find_all(
    x, "//group_l10ns/rows/row/description"))
  expect_identical(desc, "Une introduction au groupe.")
})

test_that("a group without a description emits an empty one, as before", {
  spec <- lss_spec(
    title = "T", languages = "fr",
    groups = list(list(
      title = "G",
      questions = list(list(code = "q1", kind = "yesno", text = "Oui ?")))))
  expect_null(spec$groups[[1L]]$description)
  out <- tempfile(fileext = ".lss")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(write_lss(spec, out))
  x <- xml2::read_xml(out)
  expect_identical(
    xml2::xml_text(xml2::xml_find_all(x, "//group_l10ns/rows/row/description")),
    "")
})

test_that("a multi-language group description obeys the translation rule", {
  expect_error(
    lss_spec(
      title = c(fr = "T", en = "T"), languages = c("fr", "en"),
      groups = list(list(
        title = c(fr = "G", en = "G"), description = c(fr = "Intro."),
        questions = list(list(code = "q1", kind = "yesno",
                              text = c(fr = "Oui ?", en = "Yes?")))))),
    class = "lssdoc_bad_spec"
  )
})

quota_spec <- function(limit) {
  lss_spec(
    title = "T", languages = "fr",
    groups = list(list(title = "G", questions = list(
      list(code = "q1", kind = "single", text = "Oui ou non ?",
           options = list(list(text = "Oui"), list(text = "Non")))))),
    quotas = list(c(list(question = "q1", code = "2", message = "Fin."),
                    if (is.null(limit)) NULL else list(limit = limit))))
}

test_that("a quota takes an optional whole-number limit, emitted as qlimit", {
  spec <- quota_spec(250L)
  expect_identical(spec$quotas[[1L]]$limit, 250L)
  out <- tempfile(fileext = ".lss")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(write_lss(spec, out))
  x <- xml2::read_xml(out)
  expect_identical(
    xml2::xml_text(xml2::xml_find_all(x, "//quota/rows/row/qlimit")), "250")

  # a numeric that happens to be whole is accepted and stored as an integer
  expect_identical(quota_spec(12)$quotas[[1L]]$limit, 12L)
})

test_that("a quota without a limit keeps the historical qlimit of zero", {
  spec <- quota_spec(NULL)
  expect_null(spec$quotas[[1L]]$limit)
  out <- tempfile(fileext = ".lss")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(write_lss(spec, out))
  x <- xml2::read_xml(out)
  expect_identical(
    xml2::xml_text(xml2::xml_find_all(x, "//quota/rows/row/qlimit")), "0")
})

test_that("an unusable quota limit is refused", {
  expect_error(quota_spec(-1L), class = "lssdoc_bad_spec")
  expect_error(quota_spec("beaucoup"), class = "lssdoc_bad_spec")
  expect_error(quota_spec(2.5), class = "lssdoc_bad_spec")
  expect_error(quota_spec(c(1L, 2L)), class = "lssdoc_bad_spec")
})
