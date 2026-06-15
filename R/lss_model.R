#' Assemble a denormalized, render-ready model from an `lss` object
#'
#' Join the structural and localized sections of a parsed `.lss` into a
#' per-group, per-question model keyed by language. This is the backbone
#' consumed by [audit_lss()] and [render_questionnaire()]: it resolves the
#' display order, attaches each question's localized texts, answer options,
#' subquestions, and attributes, and labels the question type. Language
#' identifiers and all user text are preserved verbatim.
#'
#' @param lss An `lss` object from [read_lss()].
#' @param languages Character vector of language codes to include, in order.
#'   Defaults to all languages of the survey. Requesting a language absent
#'   from the survey is an error.
#'
#' @return An object of class `lss_model`: a list with `file`, `languages`,
#'   `base_language`, and `groups`. Each group has `gid`, `order`, localized
#'   `names`/`descriptions` (named by language), and `questions`. Each
#'   question has `qid`, `code`, `type`, `type_label`, `mandatory`,
#'   `relevance`, localized `texts` (each language a list of `question` and
#'   `help`), `answers`, `subquestions`, `scales`, and `attributes`.
#'
#' @keywords internal
#' @noRd
lss_model <- function(lss, languages = NULL) {
  if (!inherits(lss, "lss")) {
    lssdoc_abort(
      "{.arg lss} must be an {.cls lss} object from {.fn read_lss}.",
      class = "lssdoc_bad_lss"
    )
  }
  langs <- lss_resolve_languages(lss, languages)

  groups <- lss$groups
  group_order <- order(suppressWarnings(as.integer(groups$group_order)))
  groups <- groups[group_order, , drop = FALSE]

  questions <- lss$questions
  q_order <- order(suppressWarnings(as.integer(questions$question_order)))
  questions <- questions[q_order, , drop = FALSE]

  # Pre-index every localized table once, by (key, language). The
  # lookup inside `lss_localized()` becomes O(1) instead of O(rows),
  # which matters for surveys with hundreds of questions or many
  # languages.
  g_idx <- lss_build_l10n_index(lss$group_l10ns,    "gid")
  q_idx <- lss_build_l10n_index(lss$question_l10ns, "qid")
  a_idx <- lss_build_l10n_index(lss$answer_l10ns,   "aid")

  group_models <- lapply(seq_len(nrow(groups)), function(i) {
    gid <- groups$gid[i]
    gtext <- lss_localized(
      lss$group_l10ns, "gid", gid, langs,
      c("group_name", "description"), index = g_idx
    )
    gquestions <- questions[questions$gid == gid, , drop = FALSE]
    list(
      gid = gid,
      order = groups$group_order[i],
      names = lapply(gtext, function(x) x$group_name),
      descriptions = lapply(gtext, function(x) x$description),
      questions = lapply(
        seq_len(nrow(gquestions)),
        function(j) lss_question_model(lss, gquestions[j, , drop = FALSE], langs,
                                       q_idx = q_idx, a_idx = a_idx)
      )
    )
  })

  structure(
    list(
      file = lss$file,
      languages = langs,
      base_language = lss$base_language,
      groups = group_models
    ),
    class = "lss_model"
  )
}

#' Resolve and validate the requested languages
#' @keywords internal
#' @noRd
lss_resolve_languages <- function(lss, languages) {
  available <- lss$languages
  if (is.null(languages)) {
    return(available)
  }
  missing <- setdiff(languages, available)
  if (length(missing) > 0) {
    lssdoc_abort(
      c(
        "Requested language{?s} not in the survey: {.val {missing}}.",
        "i" = "Available: {.val {available}}."
      ),
      class = "lssdoc_unknown_language"
    )
  }
  languages
}

#' Build the model for a single (main) question
#' @keywords internal
#' @noRd
lss_question_model <- function(lss, qrow, langs,
                               q_idx = NULL, a_idx = NULL) {
  qid <- qrow$qid
  info <- lss_type_info(qrow$type)

  answers <- NULL
  scales <- NULL
  if (isTRUE(info$has_answers) && !is.null(lss$answers)) {
    answers <- lss_answer_models(lss, qid, langs, a_idx = a_idx)
    if (isTRUE(info$has_scales)) {
      scales <- split(answers, vapply(answers, function(a) a$scale_id, character(1)))
    }
  }

  subquestions <- NULL
  if (isTRUE(info$has_subquestions) && !is.null(lss$subquestions)) {
    subquestions <- lss_subquestion_models(lss, qid, langs, q_idx = q_idx)
  }

  attrs <- NULL
  if (!is.null(lss$question_attributes)) {
    attrs <- lss$question_attributes[lss$question_attributes$qid == qid, , drop = FALSE]
  }

  texts <- lss_localized(
    lss$question_l10ns, "qid", qid, langs, c("question", "help"),
    index = q_idx
  )

  list(
    qid = qid,
    code = qrow$title,
    type = qrow$type,
    type_label = lss_methodological_label(qrow$type, qrow$question_theme_name),
    theme = qrow$question_theme_name,
    mandatory = qrow$mandatory,
    relevance = qrow$relevance,
    other = qrow$other,
    texts = texts,
    answers = answers,
    scales = scales,
    subquestions = subquestions,
    attributes = attrs
  )
}

#' Build the per-language answer-option models for a question
#' @keywords internal
#' @noRd
lss_answer_models <- function(lss, qid, langs, a_idx = NULL) {
  ans <- lss$answers[lss$answers$qid == qid, , drop = FALSE]
  if (nrow(ans) == 0) {
    return(list())
  }
  ord <- order(
    suppressWarnings(as.integer(ans$scale_id)),
    suppressWarnings(as.integer(ans$sortorder))
  )
  ans <- ans[ord, , drop = FALSE]
  lapply(seq_len(nrow(ans)), function(i) {
    aid <- ans$aid[i]
    labels <- lss_localized(lss$answer_l10ns, "aid", aid, langs, "answer",
                            index = a_idx)
    list(
      aid = aid,
      code = ans$code[i],
      scale_id = ans$scale_id[i],
      sortorder = ans$sortorder[i],
      labels = lapply(labels, function(x) x$answer)
    )
  })
}

#' Build the per-language subquestion models for a question
#' @keywords internal
#' @noRd
lss_subquestion_models <- function(lss, qid, langs, q_idx = NULL) {
  sq <- lss$subquestions[lss$subquestions$parent_qid == qid, , drop = FALSE]
  if (nrow(sq) == 0) {
    return(list())
  }
  ord <- order(
    suppressWarnings(as.integer(sq$scale_id)),
    suppressWarnings(as.integer(sq$question_order))
  )
  sq <- sq[ord, , drop = FALSE]
  lapply(seq_len(nrow(sq)), function(i) {
    sqid <- sq$qid[i]
    texts <- lss_localized(lss$question_l10ns, "qid", sqid, langs,
                           c("question", "help"), index = q_idx)
    # LimeSurvey stores per-subquestion attributes (`exclude_all_others`,
    # display rules, ...) in the same `question_attributes` table keyed
    # by the subquestion's own qid. Surface them so renderers can show
    # subq-level flags alongside the inherited parent meta.
    attrs <- if (!is.null(lss$question_attributes)) {
      lss$question_attributes[
        lss$question_attributes$qid == sqid, ,
        drop = FALSE
      ]
    } else {
      NULL
    }
    list(
      qid = sqid,
      code = sq$title[i],
      scale_id = sq$scale_id[i],
      relevance = sq$relevance[i],
      texts = texts,
      attributes = attrs
    )
  })
}

#' Collect localized columns for one key across languages
#'
#' Returns a list named by language; each element is a named list of the
#' requested columns. A language with no matching row yields `NA` values, so
#' missing translations are visible to the audit rather than silently
#' dropped.
#'
#' @param index Optional environment produced by [lss_build_l10n_index()].
#'   When supplied, the row lookup is O(1) instead of O(nrow(l10n)). When
#'   `NULL`, falls back to a full table scan so the helper remains usable
#'   standalone (e.g. in tests).
#'
#' @keywords internal
#' @noRd
lss_localized <- function(l10n, key_col, key, langs, cols, index = NULL) {
  out <- lapply(langs, function(lg) {
    values <- stats::setNames(as.list(rep(NA_character_, length(cols))), cols)
    if (is.null(l10n)) return(values)

    row_idx <- if (!is.null(index)) {
      index[[paste(key, lg, sep = "\r")]]
    } else {
      hit <- l10n[[key_col]] == key & l10n$language == lg
      hit[is.na(hit)] <- FALSE
      pos <- which(hit)
      if (length(pos) == 0L) NULL else pos[1]
    }
    if (is.null(row_idx)) return(values)

    row <- l10n[row_idx, , drop = FALSE]
    for (cc in cols) {
      if (cc %in% names(row)) values[[cc]] <- row[[cc]]
    }
    values
  })
  stats::setNames(out, langs)
}

#' Build a (key, language) -> row-index environment for an l10n table.
#'
#' The resulting environment has hash semantics; lookups are O(1). The
#' key is built as `paste(key_value, language, sep = "\r")` -- the
#' `\r` separator is safe because LimeSurvey identifiers and language
#' codes never contain it.
#'
#' @keywords internal
#' @noRd
lss_build_l10n_index <- function(l10n, key_col) {
  env <- new.env(hash = TRUE, parent = emptyenv())
  if (is.null(l10n) || nrow(l10n) == 0L) return(env)
  if (!(key_col %in% names(l10n)) || !("language" %in% names(l10n))) {
    return(env)
  }
  keys <- paste(l10n[[key_col]], l10n$language, sep = "\r")
  for (i in seq_along(keys)) {
    k <- keys[i]
    if (is.null(env[[k]])) env[[k]] <- i
  }
  env
}
