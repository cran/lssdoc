# Step 3b: read a real LimeSurvey export back into an authoring spec.
#
# The contract has two halves and both are tested here:
#
# * the ROUND TRIP -- `write_lss()` then `read_lss()` then `as_lss_spec()`
#   describes the same survey as the specification it started from. The
#   comparison runs on `canonical_spec()`, never on the raw objects: the
#   contract carries a survey, not an R field order.
# * the REFUSAL -- everything a `.lss` can hold and an `lss_spec` cannot is
#   listed, once, with the question it belongs to. Strict mode aborts with the
#   whole list; permissive mode drops the same list and warns.

conv_spec <- function(spec, languages = NULL) {
  file <- tempfile(fileext = ".lss")
  suppressMessages(write_lss(spec, file))
  read_lss(file)
}

# The LimeSurvey 7 re-export adds attributes of its own (`max_subquestions` on
# a ranking). They are not a conversion defect: they are what LimeSurvey wrote
# into the file, and the converter passes them through as it must. Comparing a
# re-export with our own emission therefore compares the surveys, not the
# additions.
drop_attrs <- function(canon, drop) {
  canon$groups <- lapply(canon$groups, function(g) {
    g$questions <- lapply(g$questions, function(q) {
      if (!is.null(q$attributes)) {
        q$attributes <- q$attributes[setdiff(names(q$attributes), drop)]
        if (!length(q$attributes)) q$attributes <- NULL
      }
      q
    })
    g
  })
  canon
}

dev_file <- function(name) {
  testthat::test_path("..", "..", "dev", "validation", name)
}

# ---- (a) round trips ---------------------------------------------------------

test_that("a monolingual specification survives write -> read -> convert", {
  spec <- lss_example_spec(lang = "fr")
  lss <- conv_spec(spec)
  expect_warning(back <- as_lss_spec(lss), class = "lssdoc_lossy_conversion")
  expect_s3_class(back, "lss_spec")
  expect_identical(canonical_spec(back), canonical_spec(spec))
})

test_that("a bilingual specification survives write -> read -> convert", {
  spec <- lss_example_spec(languages = c("fr", "en"))
  lss <- conv_spec(spec)
  expect_warning(back <- as_lss_spec(lss), class = "lssdoc_lossy_conversion")
  expect_identical(back$languages, c("fr", "en"))
  expect_identical(canonical_spec(back), canonical_spec(spec))
})

test_that("the round trip is a fixed point: converting twice changes nothing", {
  spec <- lss_example_spec(languages = c("fr", "en"))
  once <- suppressWarnings(as_lss_spec(conv_spec(spec)))
  twice <- suppressWarnings(as_lss_spec(conv_spec(once)))
  expect_identical(canonical_spec(twice), canonical_spec(once))
})

test_that("the only warning of a clean round trip is the HTML flattening", {
  # `write_lss()` wraps the welcome and end texts in <p> blocks, so reading
  # them back is a real (and reported) loss of structure; nothing else is.
  spec <- lss_example_spec(lang = "fr")
  res <- lss_convert_spec(conv_spec(spec))
  expect_identical(nrow(res$unconvertible), 0L)
  expect_setequal(res$html, c("survey welcome text", "survey end text"))
  expect_identical(res$filled, character(0))
  expect_identical(res$notes, character(0))
})

# ---- (b) the LimeSurvey 7 re-export -----------------------------------------

test_that("the LimeSurvey 7 re-export converts strictly and matches our own file", {
  # Reads a file from dev/, which no source tarball carries: on CRAN this
  # block can only skip anyway. Saying so up front keeps the local and
  # CRAN timings comparable.
  skip_on_cran()
  ls7 <- dev_file("limesurvey_survey_100001.lss")
  ours <- dev_file("lssdoc_0.2.0_validation.lss")
  skip_if_not(file.exists(ls7), "LimeSurvey 7 re-export not available")
  skip_if_not(file.exists(ours), "0.2.0 validation file not generated")

  expect_warning(spec7 <- as_lss_spec(read_lss(ls7)),
                 class = "lssdoc_lossy_conversion")
  expect_s3_class(spec7, "lss_spec")

  codes <- unlist(lapply(spec7$groups, function(g) {
    vapply(g$questions, `[[`, character(1), "code")
  }))
  kinds <- unlist(lapply(spec7$groups, function(g) {
    vapply(g$questions, `[[`, character(1), "kind")
  }))
  expect_length(codes, 21L)
  expect_setequal(kinds, lss_kinds$kind)

  # every relevance form write_lss() emits comes back in the spec syntax
  relevance <- unlist(lapply(spec7$groups, function(g) {
    stats::setNames(lapply(g$questions, function(q) q$relevance),
                    vapply(g$questions, `[[`, character(1), "code"))
  }))
  expect_identical(relevance[["Q04MultiOtherExcl"]], "Q01SingleOther = 1")
  expect_identical(relevance[["Q07Array"]], "Q02DropdownOther in [1, autre]")
  expect_identical(relevance[["Q14Text"]], "count(Q04MultiOtherExcl) >= 2")
  expect_identical(relevance[["Q20FivePoint"]], "Q06YesNo = Y")

  expect_length(spec7$quotas, 1L)
  expect_identical(spec7$quotas[[1L]]$question, "Q01SingleOther")
  expect_identical(spec7$quotas[[1L]]$code, "2")

  # up to the ids, and up to the attribute LimeSurvey 7 added on re-export,
  # the two files describe the same survey
  spec_ours <- suppressWarnings(as_lss_spec(read_lss(ours)))
  expect_identical(drop_attrs(canonical_spec(spec7), "max_subquestions"),
                   canonical_spec(spec_ours))
})

# ---- (c) the corpus bench ----------------------------------------------------

bench_one <- function(path) {
  lss <- suppressWarnings(lss_cached(path))
  items <- lss_unconvertible(lss)
  strict <- tryCatch({
    suppressWarnings(as_lss_spec(lss, strict = TRUE))
    "converted"
  }, lssdoc_unconvertible = function(e) "refused")
  spec <- suppressWarnings(as_lss_spec(lss, strict = FALSE))
  file <- tempfile(fileext = ".lss")
  suppressMessages(write_lss(spec, file))
  list(items = items, strict = strict, spec = spec,
       reread = suppressWarnings(read_lss(file)))
}

test_that("bench: demo_survey.lss refuses its deferred types and its quotas", {
  # A corpus bench on the 367 KB demo: convert it twice, write the
  # result out and read it back. It belongs to the local and CI runs;
  # the audit_demo bench below covers the same contract on a small file.
  skip_on_cran()
  b <- bench_one(system.file("extdata", "demo_survey.lss", package = "lssdoc"))
  expect_identical(b$strict, "refused")
  expect_gt(nrow(b$items), 0L)
  expect_named(b$items, c("code", "item", "reason"))
  # the four LimeSurvey types lssdoc does not author yet
  expect_setequal(b$items$code[b$items$item == "type"],
                  c("imc", "adaptability", "trustinstitutions", "mediaquality"))
  # the gender quota now converts (a fixed scale has codes like any other);
  # only the quota built on two questions is left
  expect_identical(sum(b$items$item == "quota"), 1L)
  expect_match(b$items$reason[b$items$item == "quota"], "combines 2 question")
  expect_length(b$spec$quotas, 1L)
  expect_identical(b$spec$quotas[[1L]]$code, "M")
  expect_s3_class(b$spec, "lss_spec")
  expect_identical(b$spec$languages[[1L]], "fr")
  expect_true(nrow(b$reread$questions) > 0L)
})

test_that("bench: demo_survey.lss is refused with one error listing every item", {
  lss <- suppressWarnings(
    demo_lss())
  items <- lss_unconvertible(lss)
  err <- expect_error(as_lss_spec(lss), class = "lssdoc_unconvertible")
  expect_identical(err$items, items)
  msg <- conditionMessage(err)
  for (code in unique(items$code)) expect_match(msg, code, fixed = TRUE)
})

test_that("what a survey carries outside the spec model is reported once", {
  lss <- suppressWarnings(
    demo_lss())
  res <- lss_convert_spec(lss)
  # one note, not one per field: the point is that the author knows
  expect_length(grep("^survey setting", res$notes), 1L)
  expect_match(paste(res$notes, collapse = " "), "write_lss", fixed = TRUE)
  # a file lssdoc emitted itself carries nothing outside the model
  own <- lss_convert_spec(conv_spec(lss_example_spec(lang = "fr")))
  expect_identical(own$notes, character(0))
})

test_that("bench: audit_demo.lss keeps only what the spec can express", {
  b <- bench_one(system.file("extdata", "audit_demo.lss", package = "lssdoc"))
  expect_identical(b$strict, "refused")
  refusals <- paste(trimws(b$items$code), b$items$item)
  expect_true("age relevance" %in% refusals)   # a foreign equation
  expect_true("age code" %in% refusals)        # a duplicate variable name
  expect_true("comment code" %in% refusals)    # a code with a trailing space
  expect_true("satisf shape" %in% refusals)    # a single choice with no option
  expect_true("group 2 title" %in% refusals)   # a group with no title
  # what survives is a valid, writable, re-readable specification
  expect_s3_class(b$spec, "lss_spec")
  expect_length(b$spec$groups, 1L)
  expect_identical(b$spec$languages, c("en", "fr"))
  expect_true(nrow(b$reread$questions) >= 1L)
})

test_that("bench: the bilingual validation file converts whole", {
  # Reads a file from dev/, which no source tarball carries: on CRAN this
  # block can only skip anyway. Saying so up front keeps the local and
  # CRAN timings comparable.
  skip_on_cran()
  path <- dev_file("lssdoc_0.3.0_bilingual.lss")
  skip_if_not(file.exists(path), "bilingual validation file not generated")
  b <- bench_one(path)
  expect_identical(b$strict, "converted")
  expect_identical(nrow(b$items), 0L)
  expect_identical(b$spec$languages, c("fr", "en"))
  expect_identical(canonical_spec(b$spec),
                   canonical_spec(lss_example_spec(languages = c("fr", "en"))))
})

# ---- (d) strict mode lists every item at once --------------------------------

fake_lss <- function(questions, question_l10ns, ...,
                     languages = "fr", answers = NULL, answer_l10ns = NULL) {
  out <- list(
    file = "<synthetic>", db_version = LSS_DBVERSION, doc_type = "Survey",
    languages = languages, base_language = languages[[1L]],
    surveys = data.frame(sid = "1", language = languages[[1L]],
                         stringsAsFactors = FALSE),
    survey_language_settings = data.frame(
      surveyls_language = languages, surveyls_title = "Synthetic survey",
      surveyls_welcometext = "", surveyls_endtext = "",
      stringsAsFactors = FALSE),
    groups = data.frame(gid = "1", sid = "1", group_order = "1",
                        stringsAsFactors = FALSE),
    group_l10ns = data.frame(gid = "1", group_name = "G", description = "",
                             language = languages, stringsAsFactors = FALSE),
    questions = questions, question_l10ns = question_l10ns,
    subquestions = NULL, answers = answers, answer_l10ns = answer_l10ns,
    question_attributes = NULL, conditions = NULL, quotas = NULL,
    quota_members = NULL, quota_languagesettings = NULL)
  # `...` REPLACES a section rather than shadowing it, so a test can hand in
  # its own `groups` or `subquestions` and still get a well-formed object
  extra <- list(...)
  for (nm in names(extra)) out[[nm]] <- extra[[nm]]
  structure(out, class = "lss")
}

# One single-choice question with two coded answers: the smallest survey that
# has a row in every structural section the sweep below reads.
one_choice <- function(..., grelevance = "1", assessment_value = c("0", "0")) {
  questions <- data.frame(
    qid = "1", parent_qid = "0", sid = "1", gid = "1", type = "L",
    title = "q1", preg = "", other = "N", mandatory = "N", encrypted = "N",
    question_order = "1", scale_id = "0", relevance = "1", modulename = "",
    same_default = "0", same_script = "0",
    question_theme_name = "listradio", stringsAsFactors = FALSE)
  l10ns <- data.frame(id = "1", qid = "1", question = "Oui ou non ?",
                      help = "", language = "fr", script = "",
                      stringsAsFactors = FALSE)
  answers <- data.frame(
    aid = c("1", "2"), qid = "1", code = c("1", "2"),
    sortorder = c("0", "1"), assessment_value = assessment_value,
    scale_id = "0", stringsAsFactors = FALSE)
  answer_l10ns <- data.frame(
    id = c("1", "2"), aid = c("1", "2"), answer = c("Oui", "Non"),
    language = "fr", stringsAsFactors = FALSE)
  groups <- data.frame(gid = "1", sid = "1", group_order = "1",
                       grelevance = grelevance, randomization_group = "",
                       stringsAsFactors = FALSE)
  fake_lss(questions, l10ns, answers = answers, answer_l10ns = answer_l10ns,
           groups = groups, ...)
}

two_bad_questions <- function() {
  questions <- data.frame(
    qid = c("1", "2", "3"), parent_qid = "0", sid = "1", gid = "1",
    type = c("N", "1", "S"), title = c("ok", "dual", "hand"),
    other = "N", mandatory = "N", question_order = c("1", "2", "3"),
    scale_id = "0", relevance = c("1", "1", "ok.NAOK >= 18"),
    question_theme_name = c("numerical", "arrays/dualscale", "shortfreetext"),
    stringsAsFactors = FALSE)
  l10ns <- data.frame(
    id = as.character(1:3), qid = c("1", "2", "3"),
    question = c("Age?", "Dual scale", "Comment"), help = "",
    language = "fr", script = "", stringsAsFactors = FALSE)
  fake_lss(questions, l10ns)
}

test_that("strict refuses once and names every unconvertible item", {
  lss <- two_bad_questions()
  items <- lss_unconvertible(lss)
  expect_identical(nrow(items), 2L)
  expect_identical(items$code, c("dual", "hand"))
  expect_identical(items$item, c("type", "relevance"))

  err <- expect_error(as_lss_spec(lss), class = "lssdoc_unconvertible")
  msg <- conditionMessage(err)
  expect_match(msg, "dual", fixed = TRUE)
  expect_match(msg, "hand", fixed = TRUE)
  expect_identical(err$items, items)
})

test_that("permissive mode drops the same items with one warning", {
  lss <- two_bad_questions()
  expect_warning(spec <- as_lss_spec(lss, strict = FALSE),
                 class = "lssdoc_lossy_conversion")
  expect_s3_class(spec, "lss_spec")
  expect_length(spec$groups[[1L]]$questions, 1L)
  expect_identical(spec$groups[[1L]]$questions[[1L]]$code, "ok")
  w <- tryCatch(as_lss_spec(lss, strict = FALSE),
                lssdoc_lossy_conversion = function(w) w)
  expect_identical(w$items, lss_unconvertible(lss))
  expect_match(conditionMessage(w), "dual", fixed = TRUE)
  expect_match(conditionMessage(w), "hand", fixed = TRUE)
})

test_that("a survey with nothing convertible left is refused in both modes", {
  questions <- data.frame(
    qid = "1", parent_qid = "0", sid = "1", gid = "1", type = "*",
    title = "eq", other = "N", mandatory = "N", question_order = "1",
    scale_id = "0", relevance = "1", question_theme_name = "equation",
    stringsAsFactors = FALSE)
  l10ns <- data.frame(id = "1", qid = "1", question = "E", help = "",
                      language = "fr", script = "", stringsAsFactors = FALSE)
  lss <- fake_lss(questions, l10ns)
  expect_error(as_lss_spec(lss, strict = FALSE), class = "lssdoc_unconvertible")
  expect_error(as_lss_spec(lss, strict = TRUE), class = "lssdoc_unconvertible")
})

test_that("the input and the flag are validated", {
  expect_error(as_lss_spec(list()), class = "lssdoc_bad_lss")
  expect_error(as_lss_spec(two_bad_questions(), strict = NA),
               class = "lssdoc_bad_input")
  empty <- structure(list(groups = NULL, questions = NULL), class = "lss")
  expect_error(as_lss_spec(empty), class = "lssdoc_bad_lss")
})

# ---- (e) missing translations and HTML --------------------------------------

test_that("a missing translation is filled from the base language and reported", {
  questions <- data.frame(
    qid = "1", parent_qid = "0", sid = "1", gid = "1", type = "S",
    title = "q1", other = "N", mandatory = "N", question_order = "1",
    scale_id = "0", relevance = "1", question_theme_name = "shortfreetext",
    stringsAsFactors = FALSE)
  l10ns <- data.frame(
    id = c("1", "2"), qid = "1", question = c("Comment?", ""), help = "",
    language = c("fr", "en"), script = "", stringsAsFactors = FALSE)
  lss <- fake_lss(questions, l10ns, languages = c("fr", "en"))
  res <- lss_convert_spec(lss)
  expect_identical(res$filled, "text of question q1 [en]")
  q <- res$spec$groups[[1L]]$questions[[1L]]
  expect_identical(q$text, list(fr = "Comment?", en = "Comment?"))
  expect_warning(as_lss_spec(lss), class = "lssdoc_lossy_conversion")
})

test_that("a base language missing where a translation exists is filled from it", {
  # a survey drafted in English and translated into its own base language
  # afterwards: the English help texts must not vanish because the French
  # ones are still empty
  questions <- data.frame(
    qid = "1", parent_qid = "0", sid = "1", gid = "1", type = "S",
    title = "q1", other = "N", mandatory = "N", question_order = "1",
    scale_id = "0", relevance = "1", question_theme_name = "shortfreetext",
    stringsAsFactors = FALSE)
  l10ns <- data.frame(
    id = c("1", "2"), qid = "1", question = c("Comment?", "How?"),
    help = c("", "Write freely"), language = c("fr", "en"), script = "",
    stringsAsFactors = FALSE)
  res <- lss_convert_spec(fake_lss(questions, l10ns, languages = c("fr", "en")))
  expect_identical(nrow(res$unconvertible), 0L)
  # the direction is part of the report: filled INTO fr, FROM en
  expect_identical(res$filled, "help of question q1 [fr, from en]")
  q <- res$spec$groups[[1L]]$questions[[1L]]
  expect_identical(q$help, list(fr = "Write freely", en = "Write freely"))

  # a field blank in every language really is absent, and says nothing
  l10ns$help <- c("", "")
  res <- lss_convert_spec(fake_lss(questions, l10ns, languages = c("fr", "en")))
  expect_identical(res$filled, character(0))
  expect_null(res$spec$groups[[1L]]$questions[[1L]]$help)
})

test_that("HTML is flattened to plain text and the field is named", {
  questions <- data.frame(
    qid = "1", parent_qid = "0", sid = "1", gid = "1", type = "S",
    title = "q1", other = "N", mandatory = "N", question_order = "1",
    scale_id = "0", relevance = "1", question_theme_name = "shortfreetext",
    stringsAsFactors = FALSE)
  l10ns <- data.frame(
    id = "1", qid = "1",
    question = "<p>First <b>line</b></p><p>Second line</p>", help = "",
    language = "fr", script = "", stringsAsFactors = FALSE)
  res <- lss_convert_spec(fake_lss(questions, l10ns))
  expect_identical(res$html, "text of question q1")
  expect_identical(res$spec$groups[[1L]]$questions[[1L]]$text,
                   list(fr = "First line\nSecond line"))
  w <- tryCatch(as_lss_spec(fake_lss(questions, l10ns)),
                lssdoc_lossy_conversion = function(w) w)
  expect_match(conditionMessage(w), "text of question q1", fixed = TRUE)
})

test_that("a text with no markup is kept character for character", {
  expect_false(conv_has_markup("Salaire net (en CHF) ?"))
  expect_true(conv_has_markup("<p>x</p>"))
  expect_true(conv_has_markup("R&amp;D"))
  st <- conv_state()
  expect_identical(conv_plain(st, "a  double  space", "f"), "a  double  space")
  expect_identical(st$html, character(0))
})

# ---- (e2) the structural columns the spec model does not carry ---------------

test_that("a structural column the spec cannot carry is never lost in silence", {
  lss <- one_choice(grelevance = "age.NAOK > 18",
                    assessment_value = c("5", "0"))
  res <- lss_convert_spec(lss)
  # a group display equation decides what the respondent meets: a refusal,
  # exactly like a foreign question-level equation
  expect_true("groups.grelevance" %in% res$unconvertible$item)
  expect_match(
    res$unconvertible$reason[res$unconvertible$item == "groups.grelevance"],
    "age.NAOK > 18", fixed = TRUE)
  expect_identical(res$unconvertible$code[res$unconvertible$item == "groups.grelevance"],
                   "group 1")
  # an assessment value changes nothing a respondent sees: a note
  expect_false("answers.assessment_value" %in% res$unconvertible$item)
  expect_match(paste(res$notes, collapse = " | "),
               "answers.assessment_value", fixed = TRUE)
  expect_error(as_lss_spec(lss), class = "lssdoc_unconvertible")
  # permissive mode says both halves out loud: the refusal and the note
  said <- character(0)
  withCallingHandlers(
    as_lss_spec(lss, strict = FALSE),
    lssdoc_lossy_conversion = function(cnd) {
      said <<- c(said, conditionMessage(cnd))
      invokeRestart("muffleWarning")
    })
  expect_match(paste(said, collapse = " "), "grelevance", fixed = TRUE)
  expect_match(paste(said, collapse = " "), "assessment_value", fixed = TRUE)
})

test_that("a blank structural cell is not stated, and not a deviation", {
  # real exports write `grelevance` as the empty string as often as "1", and
  # both mean "always shown": refusing it would refuse audit_demo.lss
  res <- lss_convert_spec(one_choice(grelevance = ""))
  expect_identical(nrow(res$unconvertible), 0L)
  expect_identical(res$notes, character(0))
  expect_identical(nrow(lss_convert_spec(one_choice())$unconvertible), 0L)
})

test_that("every refused structural column is a respondent-visible one", {
  refuse <- function(...) {
    res <- lss_convert_spec(one_choice(...))
    res$unconvertible$item
  }
  # per-question JavaScript and a validation regex
  l10ns <- data.frame(id = "1", qid = "1", question = "Oui ou non ?",
                      help = "", language = "fr",
                      script = "alert('hello')", stringsAsFactors = FALSE)
  expect_true("question_l10ns.script" %in% refuse(question_l10ns = l10ns))

  questions <- data.frame(
    qid = "1", parent_qid = "0", sid = "1", gid = "1", type = "L",
    title = "q1", preg = "/[0-9]+/", other = "N", mandatory = "N",
    encrypted = "Y", question_order = "1", scale_id = "0", relevance = "1",
    modulename = "", same_default = "0", same_script = "0",
    question_theme_name = "listradio", stringsAsFactors = FALSE)
  items <- refuse(questions = questions)
  expect_true(all(c("questions.preg", "questions.encrypted") %in% items))

  # the sweep is declared once and read once: the emitter's constants and the
  # refusals are a subset of the columns it names
  expect_true(all(lss_structural_refusals %in%
                    unlist(lapply(lss_structural_constants, names))))
})

# ---- (e3) questions and languages that belong nowhere -------------------------

test_that("a question in no group of the survey is refused by name", {
  questions <- data.frame(
    qid = c("1", "2"), parent_qid = "0", sid = "1",
    gid = c("1", "99"), type = "S", title = c("kept", "lost"),
    other = "N", mandatory = "N", question_order = c("1", "1"),
    scale_id = "0", relevance = "1",
    question_theme_name = "shortfreetext", stringsAsFactors = FALSE)
  l10ns <- data.frame(id = c("1", "2"), qid = c("1", "2"),
                      question = c("Un", "Deux"), help = "", language = "fr",
                      script = "", stringsAsFactors = FALSE)
  items <- lss_unconvertible(fake_lss(questions, l10ns))
  expect_identical(items$code, "lost")
  expect_identical(items$item, "group")
  expect_match(items$reason, "belongs to no group", fixed = TRUE)

  # and its code takes part in the duplicate scan like any other
  questions$title <- c("kept", "kept")
  items <- lss_unconvertible(fake_lss(questions, l10ns))
  expect_match(items$reason, "duplicates another question", fixed = TRUE)
})

test_that("a multilingual survey with no declared base language is refused", {
  questions <- data.frame(
    qid = "1", parent_qid = "0", sid = "1", gid = "1", type = "S",
    title = "q1", other = "N", mandatory = "N", question_order = "1",
    scale_id = "0", relevance = "1",
    question_theme_name = "shortfreetext", stringsAsFactors = FALSE)
  l10ns <- data.frame(id = c("1", "2"), qid = "1", question = "Comment?",
                      help = "", language = c("fr", "en"), script = "",
                      stringsAsFactors = FALSE)
  lss <- fake_lss(questions, l10ns, languages = c("en", "fr"),
                  base_language = NA_character_)
  items <- lss_unconvertible(lss)
  expect_true("languages" %in% items$item)
  expect_match(items$reason[items$item == "languages"],
               "does not declare its base language", fixed = TRUE)

  # a single declared language is unambiguous: no base language, no refusal
  mono <- fake_lss(questions, l10ns[1L, , drop = FALSE], languages = "fr",
                   base_language = NA_character_)
  expect_identical(nrow(lss_unconvertible(mono)), 0L)
})

# ---- (e4) rows the kind's own filters would discard ---------------------------

test_that("a row on a second answer scale refuses the question, with the count", {
  lss <- one_choice()
  lss$answers <- rbind(lss$answers, data.frame(
    aid = "3", qid = "1", code = "3", sortorder = "2",
    assessment_value = "0", scale_id = "1", stringsAsFactors = FALSE))
  lss$answer_l10ns <- rbind(lss$answer_l10ns, data.frame(
    id = "3", aid = "3", answer = "Peut-etre", language = "fr",
    stringsAsFactors = FALSE))
  items <- lss_unconvertible(lss)
  expect_identical(items$item, "answers")
  expect_match(items$reason, "1 of its 3 <answers> row(s)", fixed = TRUE)
  expect_match(items$reason, "scale_id other than 0", fixed = TRUE)
})

test_that("a row in a section the kind does not carry refuses the question", {
  # a single choice keeps its options in <answers>; subquestion rows on it
  # are a shape lssdoc does not emit, not rows to trim away
  lss <- one_choice(subquestions = data.frame(
    qid = c("10", "11"), parent_qid = "1", sid = "1", gid = "1", type = "L",
    title = c("a", "b"), question_order = c("1", "2"), scale_id = "0",
    relevance = "1", stringsAsFactors = FALSE))
  items <- lss_unconvertible(lss)
  expect_identical(items$item, "subquestions")
  expect_match(items$reason, "2 row(s) in the <subquestions> section",
               fixed = TRUE)
  expect_match(items$reason, "\"single\"", fixed = TRUE)
})

# ---- (f) relevance -----------------------------------------------------------

test_that("every equation write_lss() emits translates back", {
  expect_null(conv_relevance("1"))
  expect_null(conv_relevance(""))
  expect_identical(conv_relevance('Q1.NAOK == "1"'), "Q1 = 1")
  expect_identical(conv_relevance('Q1 == "1"'), "Q1 = 1")
  expect_identical(conv_relevance('Q1.NAOK=="Y"'), "Q1 = Y")
  expect_identical(conv_relevance('Q1.NAOK == "-oth-"'), "Q1 = autre")
  expect_identical(conv_relevance('(Q1.NAOK == "1" or Q1.NAOK == "-oth-")'),
                   "Q1 in [1, autre]")
  expect_identical(conv_relevance('(Q1 == "1"  or  Q1 == "2")'), "Q1 in [1, 2]")
  expect_identical(conv_relevance("count(Q1_1.NAOK, Q1_2.NAOK) >= 2"),
                   "count(Q1) >= 2")
  expect_identical(conv_relevance("count(Q1_1, Q1_2, Q1_3) < 3"),
                   "count(Q1) < 3")
})

test_that("a foreign equation is never guessed at", {
  expect_true(is.na(conv_relevance("Q1.NAOK == 1")))          # unquoted
  expect_true(is.na(conv_relevance('Q1.NAOK == "1" and Q2.NAOK == "2"')))
  expect_true(is.na(conv_relevance('(Q1.NAOK == "1" or Q2.NAOK == "2")')))
  expect_true(is.na(conv_relevance("count(Q1_1.NAOK, Q2_1.NAOK) >= 2")))
  expect_true(is.na(conv_relevance("intval(Q1.NAOK) > 18")))
})

test_that("a filter on a question that was dropped is dropped too", {
  questions <- data.frame(
    qid = c("1", "2"), parent_qid = "0", sid = "1", gid = "1",
    type = c("*", "S"), title = c("eq", "q2"), other = "N", mandatory = "N",
    question_order = c("1", "2"), scale_id = "0",
    relevance = c("1", 'eq.NAOK == "1"'),
    question_theme_name = c("equation", "shortfreetext"),
    stringsAsFactors = FALSE)
  l10ns <- data.frame(
    id = c("1", "2"), qid = c("1", "2"), question = c("E", "Q2"), help = "",
    language = "fr", script = "", stringsAsFactors = FALSE)
  items <- lss_unconvertible(fake_lss(questions, l10ns))
  expect_identical(items$code, c("eq", "q2"))
  expect_identical(items$item, c("type", "relevance"))
})

# ---- (g) the form renderer ---------------------------------------------------

test_that("write_form_docx() renders a parsed survey non-strictly", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  # Any parsed survey exercises the non-strict path; the eight-question
  # audit fixture is one, and carries unconvertible items of its own, so
  # the drop-and-render branch is the one taken. Rendering the
  # 47-question demo as a Word form instead costs forty seconds.
  lss <- suppressWarnings(
    flawed_lss())
  out <- tempfile(fileext = ".docx")
  suppressWarnings(suppressMessages(
    write_form_docx(lss, out, lang = "fr", strict = FALSE)))
  expect_true(file.exists(out))
  expect_gt(file.size(out), 0)
})

test_that("write_form_docx() on a parsed survey is strict by default", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- suppressWarnings(
    flawed_lss())
  expect_error(write_form_docx(lss, tempfile(fileext = ".docx")),
               class = "lssdoc_unconvertible")
})

test_that("a clean survey renders as a form without any refusal", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  spec <- lss_example_spec(kinds = c("single", "multiple", "text"), lang = "fr")
  lss <- conv_spec(spec)
  out <- tempfile(fileext = ".docx")
  suppressWarnings(suppressMessages(write_form_docx(lss, out, lang = "fr")))
  expect_true(file.exists(out))
})
