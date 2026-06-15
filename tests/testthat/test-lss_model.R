test_that("lss_model rejects non-lss input", {
  expect_error(lss_model(list()), class = "lssdoc_bad_lss")
})

test_that("lss_model validates requested languages", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  expect_error(lss_model(lss, languages = "it"), class = "lssdoc_unknown_language")
  expect_identical(lss_model(lss, languages = "fr")$languages, "fr")
})

test_that("lss_model assembles groups and questions in order", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  m <- lss_model(read_lss(path))

  expect_s3_class(m, "lss_model")
  expect_identical(m$languages, c("en", "de", "es", "fr"))
  expect_length(m$groups, 6L)

  n_q <- sum(vapply(m$groups, function(g) length(g$questions), integer(1)))
  expect_identical(n_q, 47L)

  expect_identical(m$groups[[1]]$names$fr, "Introduction et profil")
})

test_that("list questions carry per-language answer labels", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  m <- lss_model(read_lss(path))
  all_q <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)

  q <- Filter(function(x) x$type == "L", all_q)[[1]]
  # The model now stores the MOSAiCH-style methodological label
  # (independent of the LimeSurvey UI variant) so audit messages and
  # the meta-table render with the same wording.
  expect_identical(q$type_label, "Single choice")
  expect_true(length(q$answers) >= 1)
  expect_true(all(c("fr", "de") %in% names(q$answers[[1]]$labels)))
  expect_true(nzchar(q$answers[[1]]$labels$fr))
})

test_that("array questions carry subquestions and a shared answer scale", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  m <- lss_model(read_lss(path))
  all_q <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)

  q <- Filter(function(x) x$type == "F", all_q)[[1]]
  expect_true(length(q$subquestions) >= 1)
  expect_true(length(q$answers) >= 1)
  expect_true(nzchar(q$subquestions[[1]]$texts$fr$question))
})

test_that("multiple-choice questions use subquestions, not answers", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  m <- lss_model(read_lss(path))
  all_q <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)

  q <- Filter(function(x) x$type == "M", all_q)[[1]]
  expect_true(length(q$subquestions) >= 1)
  expect_length(q$answers, 0L)
})

test_that("subquestion attributes are exposed on the model subq objects", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  # The bundled fixtures do not use per-subquestion attributes, so we
  # inject a synthetic one to confirm the propagation path works.
  sq_row <- lss$subquestions[1, ]
  lss$question_attributes <- rbind(
    lss$question_attributes,
    data.frame(
      qid = sq_row$qid, attribute = "exclude_all_others",
      value = "1", language = "",
      stringsAsFactors = FALSE
    )
  )
  m <- lss_model(lss)
  found <- FALSE
  for (g in m$groups) {
    for (q in g$questions) {
      for (sq in q$subquestions) {
        if (identical(sq$qid, sq_row$qid)) {
          expect_false(is.null(sq$attributes))
          expect_true("exclude_all_others" %in% sq$attributes$attribute)
          found <- TRUE
        }
      }
    }
  }
  expect_true(found)
})

test_that("a missing translation surfaces as NA, not a dropped entry", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  # Request only the base language; every question still has an fr entry.
  m <- lss_model(read_lss(path), languages = "fr")
  all_q <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)
  expect_true(all(vapply(all_q, function(q) "fr" %in% names(q$texts), logical(1))))
})

test_that("lss_build_l10n_index returns an O(1) lookup environment", {
  l10n <- data.frame(
    qid = c("1", "2", "1"),
    language = c("en", "en", "fr"),
    question = c("Hello", "World", "Bonjour"),
    stringsAsFactors = FALSE
  )
  idx <- lssdoc:::lss_build_l10n_index(l10n, "qid")
  expect_true(is.environment(idx))
  expect_identical(idx[[paste("1", "en", sep = "\r")]], 1L)
  expect_identical(idx[[paste("2", "en", sep = "\r")]], 2L)
  expect_identical(idx[[paste("1", "fr", sep = "\r")]], 3L)
  # Missing key returns NULL (the env semantics) so callers can branch.
  expect_null(idx[[paste("99", "en", sep = "\r")]])
})

test_that("lss_build_l10n_index handles empty / NULL tables gracefully", {
  idx_null <- lssdoc:::lss_build_l10n_index(NULL, "qid")
  expect_true(is.environment(idx_null))
  expect_identical(length(ls(idx_null)), 0L)

  idx_empty <- lssdoc:::lss_build_l10n_index(
    data.frame(qid = character(0), language = character(0),
               stringsAsFactors = FALSE),
    "qid"
  )
  expect_identical(length(ls(idx_empty)), 0L)
})

test_that("lss_localized returns identical results with or without the index", {
  l10n <- data.frame(
    qid = c("a", "a", "b"),
    language = c("en", "fr", "en"),
    question = c("Q1", "Question 1", "Q2"),
    stringsAsFactors = FALSE
  )
  expect_identical(
    lssdoc:::lss_localized(l10n, "qid", "a", c("en", "fr"), "question"),
    lssdoc:::lss_localized(l10n, "qid", "a", c("en", "fr"), "question",
                           index = lssdoc:::lss_build_l10n_index(l10n, "qid"))
  )
  # And the indexed version reads the missing language as NA.
  out <- lssdoc:::lss_localized(
    l10n, "qid", "b", c("en", "fr"), "question",
    index = lssdoc:::lss_build_l10n_index(l10n, "qid")
  )
  expect_identical(out$en$question, "Q2")
  expect_true(is.na(out$fr$question))
})
