test_that("audit_lss rejects objects that are not lss", {
  expect_error(audit_lss(list()), class = "lssdoc_bad_input")
})

test_that("the demo survey has no error- or warning-level findings", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  a <- audit_lss(read_lss(path))
  expect_s3_class(a, "lss_audit")
  # The demo survey is editorially clean: its only finding is an
  # informational note (an equation question carries no display text).
  expect_false(any(a$findings$severity %in% c("error", "warning")))
  expect_s3_class(as.data.frame(a), "data.frame")
})

test_that("an empty (non-equation) question text is flagged as an error", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Blank the first non-equation question's text in every language; a
  # real question (unlike an equation) must carry text, so this is an
  # error-severity finding.
  i <- which(lss$questions$type != "*")[1]
  qid <- lss$questions$qid[i]
  lss$question_l10ns$question[lss$question_l10ns$qid == qid] <- ""

  a <- audit_lss(lss)
  empties <- a$findings[
    a$findings$check == "empty_in_all_languages" &
      grepl(lss$questions$title[i], a$findings$location, fixed = TRUE),
  ]
  expect_true(nrow(empties) >= 1)
  expect_true(all(empties$severity == "error"))
})

test_that("a missing translation in one language is detected", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Blank out the fr text of the first question that has one.
  l10n <- lss$question_l10ns
  fr <- which(l10n$language == "fr" & nzchar(l10n$question))[1]
  l10n$question[fr] <- ""
  lss$question_l10ns <- l10n

  a <- audit_lss(lss)
  miss <- a$findings[a$findings$check == "missing_translation", ]
  expect_true(any(miss$language == "fr"))
})

test_that("duplicate question codes are flagged as errors", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Force a duplicate variable code.
  lss$questions$title[2] <- lss$questions$title[1]

  a <- audit_lss(lss)
  dups <- a$findings[a$findings$check == "duplicate_code", ]
  expect_true(nrow(dups) >= 1)
  expect_true(all(dups$severity == "error"))
})

test_that("orphan subquestions are detected", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  lss$subquestions$parent_qid[1] <- "999999999"

  a <- audit_lss(lss)
  expect_true(any(a$findings$check == "orphan_subquestion"))
})

test_that("an empty equation text is a note, not an error", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Turn the first question into an empty equation in every language.
  lss$questions$type[1] <- "*"
  qid <- lss$questions$qid[1]
  is_q <- lss$question_l10ns$qid == qid
  lss$question_l10ns$question[is_q] <- ""

  a <- audit_lss(lss)
  this <- a$findings[
    a$findings$check == "empty_in_all_languages" &
      grepl(lss$questions$title[1], a$findings$location, fixed = TRUE),
  ]
  expect_true(nrow(this) >= 1)
  expect_true(all(this$severity == "note"))
})

test_that("a filter referencing a later variable is flagged as an error", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Sort questions by display order so we can pick the first and a
  # later one deterministically.
  q_ord <- order(suppressWarnings(as.integer(lss$questions$question_order)))
  earlier_code <- lss$questions$title[q_ord[1]]
  later_code   <- lss$questions$title[q_ord[2]]

  # Inject a filter on the EARLIER question that references the LATER
  # question's code -- a forward reference.
  lss$questions$relevance[q_ord[1]] <- paste0(later_code, ".NAOK == 1")

  a <- audit_lss(lss)
  fwd <- a$findings[a$findings$check == "forward_filter_reference", ]
  expect_true(nrow(fwd) >= 1)
  expect_true(all(fwd$severity == "error"))
  expect_true(any(grepl(earlier_code, fwd$location, fixed = TRUE)))
  expect_true(any(grepl(later_code, fwd$message, fixed = TRUE)))
})

test_that("a backward filter reference does not trigger forward_filter_reference", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  q_ord <- order(suppressWarnings(as.integer(lss$questions$question_order)))
  earlier_code <- lss$questions$title[q_ord[1]]

  # Inject a filter on a LATER question that references an EARLIER
  # code -- a legitimate backward reference.
  lss$questions$relevance[q_ord[3]] <- paste0(earlier_code, ".NAOK == 1")

  a <- audit_lss(lss)
  fwd <- a$findings[a$findings$check == "forward_filter_reference", ]
  # No forward refs introduced -- the only one would have been ours,
  # which is now backward.
  expect_equal(nrow(fwd), 0L)
})

test_that("an array whose subquestion scales do not match the answer scales is flagged", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Find any question that has BOTH answers and subquestions.
  qid <- NA
  for (id in unique(lss$questions$qid)) {
    has_a <- !is.null(lss$answers)      && any(lss$answers$qid == id)
    has_s <- !is.null(lss$subquestions) && any(lss$subquestions$parent_qid == id)
    if (has_a && has_s) { qid <- id; break }
  }
  skip_if(is.na(qid), "No array-style question in the fixture")

  # Force one subquestion to claim a `scale_id` no answer option uses.
  sq_rows <- which(lss$subquestions$parent_qid == qid)
  lss$subquestions$scale_id[sq_rows[1]] <- "99"

  a <- audit_lss(lss)
  scale_findings <- a$findings[
    a$findings$check == "array_scale_missing_answers",
  ]
  expect_true(nrow(scale_findings) >= 1)
  expect_true(all(scale_findings$severity == "warning"))
})

test_that("whitespace in a question code is flagged as a warning", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  lss$questions$title[1] <- paste0(lss$questions$title[1], " ")

  a <- audit_lss(lss)
  ws <- a$findings[a$findings$check == "code_whitespace", ]
  expect_true(nrow(ws) >= 1)
  expect_true(all(ws$severity == "warning"))
})

test_that("whitespace inside a code is also caught", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Interior space, not just leading/trailing.
  lss$questions$title[1] <- "q 1"

  a <- audit_lss(lss)
  ws <- a$findings[a$findings$check == "code_whitespace", ]
  expect_true(nrow(ws) >= 1)
})

test_that("print.lss_audit paginates and respects n = Inf", {
  path <- system.file("extdata", "demo_survey.lss",
                      package = "lssdoc")
  skip_if_not(file.exists(path))
  a <- audit_lss(read_lss(path))
  skip_if(a$n_findings < 2L, "Need >=2 findings to test pagination")

  # Default cap (20) -- output must mention the remaining count when
  # there are more than 20 findings.
  out <- utils::capture.output(print(a))
  out <- paste(out, collapse = "\n")
  if (a$n_findings > 20L) {
    expect_match(out, "more finding")
  } else {
    expect_no_match(out, "more finding")
  }

  # n = Inf -- never truncate.
  out_full <- utils::capture.output(print(a, n = Inf))
  out_full <- paste(out_full, collapse = "\n")
  expect_no_match(out_full, "more finding")
})

test_that("as.data.frame keeps a stable column set regardless of findings", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  a <- audit_lss(read_lss(path))
  expect_s3_class(as.data.frame(a), "data.frame")
  # Identity in structure: same columns whether empty or populated.
  expect_setequal(
    names(as.data.frame(a)),
    c("severity", "check", "location", "language", "message")
  )
})
