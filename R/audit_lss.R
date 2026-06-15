#' Audit a LimeSurvey survey for reviewable anomalies
#'
#' Inspect a LimeSurvey survey and flag anomalies that can be detected
#' without any AI. The audit guides a human reviewer; it does not
#' silently correct anything. Every finding names a precise location
#' and a severity.
#'
#' Checks performed:
#' * **Missing translations** -- a question, help, answer, or subquestion
#'   text present in at least one language but empty in another.
#' * **Empty in all languages** -- a translatable text empty in every
#'   language.
#' * **Duplicate codes** -- a question variable code repeated in the
#'   survey, or an answer/subquestion code repeated within one question.
#' * **Whitespace in codes** -- a question, subquestion or answer code
#'   containing leading, trailing or interior whitespace (likely a typo;
#'   causes subtle bugs in the data export).
#' * **Missing options for the type** -- a question whose type requires
#'   answer options or subquestions but has none (per the type taxonomy).
#' * **Forward filter references** -- a relevance expression that names
#'   a variable appearing at or after the filtered question (the value
#'   is not yet collected when the filter is evaluated).
#' * **Array-scale inconsistencies** -- an array (single or dual) whose
#'   subquestions reference a `scale_id` that has no answer options, or
#'   vice versa.
#' * **Orphan references** -- a subquestion or answer pointing to a
#'   question that does not exist.
#'
#' @param input Either a path to a `.lss` file (character string) or a
#'   pre-parsed `lss` object returned by [read_lss()]. Passing a path
#'   parses it on the fly; passing an `lss` object avoids re-parsing
#'   when the same survey is also rendered in the same session.
#'
#' @return An object of class `lss_audit`: a list with `file`,
#'   `languages`, summary counts, and a `findings` data frame
#'   (`severity`, `check`, `location`, `language`, `message`). It has
#'   a `print()` method and an `as.data.frame()` method.
#'
#' @seealso [render_audit()] to write the same findings to a Word or
#'   PDF document.
#'
#' @examples
#' # A deliberately flawed demo survey ships with the package, seeded
#' # with every anomaly the audit detects.
#' demo <- system.file("extdata", "audit_demo.lss", package = "lssdoc")
#' audit_lss(demo)
#' @export
audit_lss <- function(input) {
  lss <- lss_resolve_input(input)

  langs <- lss$languages
  model <- lss_model(lss, languages = langs)

  findings <- lss_finding_collector()

  for (group in model$groups) {
    gname <- lss_first_label(group$names, langs)
    lss_audit_text(
      findings, group$names, langs,
      location = lss_locate("Group", gname),
      kind = "group name",
      empty_severity = "warning"
    )

    for (q in group$questions) {
      qloc <- lss_locate("Question", q$code)

      # Question text and help across languages. An equation legitimately
      # carries no display text (its formula lives in the attributes), so an
      # empty equation text is a note rather than an error.
      q_text <- lapply(langs, function(l) q$texts[[l]]$question)
      names(q_text) <- langs
      lss_audit_text(
        findings, q_text, langs,
        location = qloc, kind = "question text",
        empty_severity = if (identical(q$type, "*")) "note" else "error"
      )

      q_help <- lapply(langs, function(l) q$texts[[l]]$help)
      names(q_help) <- langs
      lss_audit_text(
        findings, q_help, langs,
        location = qloc, kind = "help text",
        empty_severity = NA, # help may legitimately be empty everywhere
        missing_severity = "note"
      )

      # Type expectations.
      info <- lss_type_info(q$type)
      if (isTRUE(info$has_answers) && length(q$answers) == 0) {
        findings$add(
          "warning", "missing_options", qloc, NA_character_,
          sprintf(
            "Type '%s' expects answer options, but none are defined.",
            q$type_label
          )
        )
      }
      if (isTRUE(info$has_subquestions) && length(q$subquestions) == 0) {
        findings$add(
          "warning", "missing_subquestions", qloc, NA_character_,
          sprintf(
            "Type '%s' expects subquestions, but none are defined.",
            q$type_label
          )
        )
      }

      # Answer-option labels and duplicate answer codes (per scale).
      if (length(q$answers) > 0) {
        lss_audit_codes(
          findings,
          codes = vapply(q$answers, function(a) a$code, character(1)),
          groups = vapply(q$answers, function(a) a$scale_id, character(1)),
          location = qloc, kind = "answer code"
        )
        for (a in q$answers) {
          lss_audit_text(
            findings, a$labels, langs,
            location = lss_locate("Answer", paste0(q$code, " = ", a$code)),
            kind = "answer text",
            empty_severity = "warning"
          )
        }
      }

      # Subquestion texts and duplicate subquestion codes.
      if (length(q$subquestions) > 0) {
        lss_audit_codes(
          findings,
          codes = vapply(q$subquestions, function(s) s$code, character(1)),
          groups = vapply(q$subquestions, function(s) s$scale_id, character(1)),
          location = qloc, kind = "subquestion code"
        )
        for (s in q$subquestions) {
          s_text <- lapply(langs, function(l) s$texts[[l]]$question)
          names(s_text) <- langs
          lss_audit_text(
            findings, s_text, langs,
            location = lss_locate("Subquestion", paste0(q$code, " / ", s$code)),
            kind = "subquestion text",
            empty_severity = "warning"
          )
        }
      }
    }
  }

  # Survey-wide duplicate question codes.
  if (!is.null(lss$questions)) {
    lss_audit_codes(
      findings,
      codes = lss$questions$title,
      groups = rep("", nrow(lss$questions)),
      location = lss_locate("Survey", NA),
      kind = "question code"
    )
  }

  # Whitespace in identifier codes: leading, trailing or interior
  # spaces are almost always typos and cause subtle bugs at data
  # export (column names match exactly, including the whitespace).
  lss_audit_whitespace_codes(findings, lss)

  # Forward references in routing filters: a question's relevance
  # expression mentions a variable that appears LATER in the survey,
  # which means the answer is not yet collected when the filter is
  # evaluated. Almost always a survey-design bug.
  lss_audit_forward_refs(findings, model)

  # Array-scale consistency: every subquestion of a given array
  # question should share the answer-options scale that the question
  # type declares.
  lss_audit_array_scales(findings, model)

  # Orphan structural references.
  lss_audit_orphans(findings, lss)

  findings_df <- findings$as_data_frame()
  structure(
    list(
      file = lss$file,
      languages = langs,
      n_findings = nrow(findings_df),
      n_errors = sum(findings_df$severity == "error"),
      n_warnings = sum(findings_df$severity == "warning"),
      n_notes = sum(findings_df$severity == "note"),
      findings = findings_df
    ),
    class = "lss_audit"
  )
}

#' Mutable collector for audit findings
#' @keywords internal
#' @noRd
lss_finding_collector <- function() {
  store <- new.env(parent = emptyenv())
  store$rows <- list()
  list(
    add = function(severity, check, location, language, message) {
      store$rows[[length(store$rows) + 1L]] <- data.frame(
        severity = severity,
        check = check,
        location = location,
        language = language,
        message = message,
        stringsAsFactors = FALSE
      )
      invisible()
    },
    as_data_frame = function() {
      if (length(store$rows) == 0) {
        return(data.frame(
          severity = character(0),
          check = character(0),
          location = character(0),
          language = character(0),
          message = character(0),
          stringsAsFactors = FALSE
        ))
      }
      out <- do.call(rbind, store$rows)
      sev_rank <- match(out$severity, c("error", "warning", "note"))
      out[order(sev_rank, out$check, out$location), , drop = FALSE]
    }
  )
}

#' Build a concise human-readable location string
#' @keywords internal
#' @noRd
lss_locate <- function(kind, code) {
  if (is.null(code) || is.na(code) || !nzchar(code)) {
    return(kind)
  }
  paste0(kind, " '", code, "'")
}

#' First non-empty localized label across languages
#' @keywords internal
#' @noRd
lss_first_label <- function(values_by_lang, langs) {
  for (l in langs) {
    v <- values_by_lang[[l]]
    if (!is.null(v) && !is.na(v) && nzchar(trimws(v))) {
      return(v)
    }
  }
  NA_character_
}

#' Check a localized text set for missing translations / emptiness
#'
#' @param missing_severity Severity for a translation missing in some but not
#'   all languages. Defaults to `"warning"`.
#' @param empty_severity Severity for a text empty in every language, or `NA`
#'   to skip that check (e.g. optional help text).
#' @keywords internal
#' @noRd
lss_audit_text <- function(findings, values_by_lang, langs, location, kind,
                           empty_severity = "warning",
                           missing_severity = "warning") {
  present <- vapply(langs, function(l) {
    v <- values_by_lang[[l]]
    !is.null(v) && !is.na(v) && nzchar(trimws(v))
  }, logical(1))

  if (!any(present)) {
    if (!is.na(empty_severity)) {
      findings$add(
        empty_severity, "empty_in_all_languages", location, NA_character_,
        sprintf("The %s is empty in every language.", kind)
      )
    }
    return(invisible())
  }
  if (!all(present)) {
    missing <- langs[!present]
    for (l in missing) {
      findings$add(
        missing_severity, "missing_translation", location, l,
        sprintf("The %s is missing in '%s' but present in other languages.", kind, l)
      )
    }
  }
  invisible()
}

#' Flag duplicate codes within groups
#' @keywords internal
#' @noRd
lss_audit_codes <- function(findings, codes, groups, location, kind) {
  codes <- as.character(codes)
  key <- paste(groups, codes, sep = "\r")
  dup <- key %in% key[duplicated(key)]
  for (d in unique(codes[dup])) {
    findings$add(
      "error", "duplicate_code", location, NA_character_,
      sprintf("Duplicate %s: '%s'.", kind, d)
    )
  }
  invisible()
}

#' Flag question / subquestion / answer codes carrying whitespace
#'
#' A code like `"q1 "` (with trailing space) matches `"q1"` only if
#' the consumer trims; in practice this means the column at data
#' export is named exactly `"q1 "`, with the space, and downstream
#' joins / scripts that look up `"q1"` silently fail. Severity:
#' `warning`.
#'
#' @keywords internal
#' @noRd
lss_audit_whitespace_codes <- function(findings, lss) {
  flag <- function(value, kind, location) {
    if (is.null(value) || is.na(value) || !nzchar(value)) return()
    trimmed <- trimws(value)
    if (!identical(trimmed, value) || grepl("\\s", trimmed)) {
      findings$add(
        "warning", "code_whitespace", location, NA_character_,
        sprintf("The %s '%s' contains whitespace; LimeSurvey will export it verbatim, which usually breaks downstream lookups.",
                kind, value)
      )
    }
  }
  if (!is.null(lss$questions) && nrow(lss$questions) > 0L) {
    for (i in seq_len(nrow(lss$questions))) {
      flag(lss$questions$title[i], "question code",
           lss_locate("Question", lss$questions$title[i]))
    }
  }
  if (!is.null(lss$subquestions) && nrow(lss$subquestions) > 0L) {
    for (i in seq_len(nrow(lss$subquestions))) {
      flag(lss$subquestions$title[i], "subquestion code",
           lss_locate("Subquestion", lss$subquestions$title[i]))
    }
  }
  if (!is.null(lss$answers) && nrow(lss$answers) > 0L) {
    for (i in seq_len(nrow(lss$answers))) {
      flag(lss$answers$code[i], "answer code",
           lss_locate("Answer", lss$answers$code[i]))
    }
  }
  invisible()
}

#' Flag relevance expressions that reference a variable appearing at or
#' after the filtered question.
#'
#' The reviewer almost always reads "filter references later variable"
#' as a design bug: the answer is not yet collected when the filter is
#' evaluated. We resolve references against question codes AND
#' subquestion codes (in both the bare form `SQ1` and the parent
#' `parent_SQ1` form), so a filter on a subq that lives in an earlier
#' question does not trigger a false positive.
#'
#' @keywords internal
#' @noRd
lss_audit_forward_refs <- function(findings, model) {
  # Build position index. Each variable code (question or subquestion)
  # is mapped to the display order of the FIRST question that owns
  # it. Subquestions inherit their parent's position so a filter on
  # `Q1_SQ1` at Q2 reads as backward (correct).
  position <- list()
  pos <- 0L
  for (group in model$groups) {
    for (q in group$questions) {
      pos <- pos + 1L
      position[[q$code]] <- pos
      if (!is.null(q$subquestions)) {
        for (s in q$subquestions) {
          position[[s$code]] <- pos
          position[[paste0(q$code, "_", s$code)]] <- pos
        }
      }
    }
  }

  pos <- 0L
  for (group in model$groups) {
    for (q in group$questions) {
      pos <- pos + 1L
      rel <- q$relevance
      if (is.null(rel) || is.na(rel) || !nzchar(rel) || identical(rel, "1")) next

      # Extract every variable code referenced via `X.NAOK`.
      matches <- regmatches(
        rel, gregexpr("\\b([A-Za-z][A-Za-z0-9_]*)\\.NAOK\\b", rel, perl = TRUE)
      )[[1]]
      refs <- unique(sub("\\.NAOK$", "", matches))
      for (ref in refs) {
        ref_pos <- position[[ref]]
        if (is.null(ref_pos)) next  # Unknown ref: could be a calc
                                    # field or unsupported construct.
        if (ref_pos >= pos) {
          findings$add(
            "error", "forward_filter_reference",
            lss_locate("Question", q$code), NA_character_,
            sprintf(
              "Filter references variable '%s' (item %d), which is not asked before this question (item %d).",
              ref, ref_pos, pos
            )
          )
        }
      }
    }
  }
  invisible()
}

#' Flag array-type questions whose answer-options scales do not match
#' the scales used by their subquestions.
#'
#' For array (`F`), dual-scale array (`1`) and similar, every
#' subquestion's `scale_id` should be matched by at least one answer
#' option with the same `scale_id`, and vice versa. A mismatch is a
#' methodological bug: rows or columns will be unlabelled in the data
#' export.
#'
#' @keywords internal
#' @noRd
lss_audit_array_scales <- function(findings, model) {
  for (group in model$groups) {
    for (q in group$questions) {
      if (is.null(q$subquestions) || length(q$subquestions) == 0L) next
      if (is.null(q$answers) || length(q$answers) == 0L) next
      # Dual-scale arrays (type "1") legitimately carry two answer
      # scales (0 and 1) while their subquestions live on scale 0 only:
      # the two scales are the two answer sets, not mirrored subquestion
      # groups. The subquestion/answer scale correspondence this check
      # verifies does not apply to them, so skip to avoid a false
      # positive ("answer scale 1 has no subquestions").
      if (identical(q$type, "1")) next

      ans_scales <- unique(vapply(q$answers,
                                  function(a) as.character(a$scale_id),
                                  character(1)))
      sq_scales <- unique(vapply(q$subquestions,
                                 function(s) as.character(s$scale_id),
                                 character(1)))

      missing_in_ans <- setdiff(sq_scales, ans_scales)
      missing_in_sq <- setdiff(ans_scales, sq_scales)

      if (length(missing_in_ans) > 0L) {
        findings$add(
          "warning", "array_scale_missing_answers",
          lss_locate("Question", q$code), NA_character_,
          sprintf(
            "Subquestions reference scale_id '%s' but no answer options are defined for it.",
            paste(missing_in_ans, collapse = "', '")
          )
        )
      }
      if (length(missing_in_sq) > 0L) {
        findings$add(
          "warning", "array_scale_missing_subquestions",
          lss_locate("Question", q$code), NA_character_,
          sprintf(
            "Answer options reference scale_id '%s' but no subquestions are defined for it.",
            paste(missing_in_sq, collapse = "', '")
          )
        )
      }
    }
  }
  invisible()
}

#' Flag subquestions and answers that reference a missing question
#' @keywords internal
#' @noRd
lss_audit_orphans <- function(findings, lss) {
  qids <- if (is.null(lss$questions)) character(0) else lss$questions$qid

  if (!is.null(lss$subquestions) && nrow(lss$subquestions) > 0) {
    orphan <- !(lss$subquestions$parent_qid %in% qids)
    for (i in which(orphan)) {
      findings$add(
        "error", "orphan_subquestion",
        lss_locate("Subquestion", lss$subquestions$title[i]), NA_character_,
        sprintf(
          "Subquestion points to question id '%s', which does not exist.",
          lss$subquestions$parent_qid[i]
        )
      )
    }
  }

  if (!is.null(lss$answers) && nrow(lss$answers) > 0) {
    orphan <- !(lss$answers$qid %in% qids)
    for (i in which(orphan)) {
      findings$add(
        "error", "orphan_answer",
        lss_locate("Answer", lss$answers$code[i]), NA_character_,
        sprintf(
          "Answer points to question id '%s', which does not exist.",
          lss$answers$qid[i]
        )
      )
    }
  }
  invisible()
}

#' Print an `lss_audit` object
#'
#' Pretty-printed audit summary on the console, capped at the first
#' `n` findings. Severity-based bullet symbols (errors, warnings,
#' notes) mirror what is shown in the audit table inside the
#' rendered `.docx`.
#'
#' @param x An `lss_audit` object returned by [audit_lss()].
#' @param ... Currently ignored.
#' @param n Maximum number of findings to print. Defaults to `20`. Set
#'   to `Inf` to print every finding. The remaining count, when any, is
#'   summarized at the bottom with a hint to use `as.data.frame()` for
#'   the full list.
#'
#' @return The audit object, invisibly.
#'
#' @keywords internal
#' @export
print.lss_audit <- function(x, ..., n = 20L) {
  cli::cli_h1("lssdoc audit")
  cli::cli_text("{.field File}: {.path {x$file}}")
  cli::cli_text("{.field Languages}: {.val {x$languages}}")

  if (x$n_findings == 0) {
    cli::cli_alert_success("No anomalies detected.")
    return(invisible(x))
  }

  cli::cli_text(
    "{.strong {x$n_findings}} finding{?s}: ",
    "{x$n_errors} error{?s}, {x$n_warnings} warning{?s}, {x$n_notes} note{?s}."
  )

  total <- nrow(x$findings)
  cap <- if (is.finite(n)) min(as.integer(n), total) else total
  symbols <- c(error = "x", warning = "warning", note = "i")
  for (i in seq_len(cap)) {
    f <- x$findings[i, ]
    where <- if (is.na(f$language)) f$location else paste0(f$location, " [", f$language, "]")
    cli::cli_bullets(stats::setNames(
      paste0("{.strong ", where, "}: ", f$message),
      symbols[[f$severity]]
    ))
  }
  if (cap < total) {
    remaining <- total - cap
    cli::cli_text(
      "{.emph \u2026 and {remaining} more finding{?s}.} ",
      "Use {.code as.data.frame(x)} to see them all, or ",
      "{.code print(x, n = Inf)} to expand here."
    )
  }
  invisible(x)
}

#' @export
as.data.frame.lss_audit <- function(x, ...) {
  x$findings
}
