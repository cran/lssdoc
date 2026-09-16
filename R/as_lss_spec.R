# Read side of the authoring loop: turn a PARSED survey (`lss`) back into an
# authoring SPECIFICATION (`lss_spec`).
#
# `read_lss()` -> `as_lss_spec()` -> `write_form_docx()` is the "modify an
# existing questionnaire" entry point of the 0.3.0 contract, and
# `write_lss(as_lss_spec(read_lss(write_lss(spec))))` is the loop that lets the
# corpus bench compare a survey with itself after a full turn.
#
# The conversion is NARROWER than the file it reads: an `lss` is whatever
# LimeSurvey exports, an `lss_spec` is what lssdoc can author. Everything the
# spec model does not carry is therefore reported, never dropped in silence --
# refused as UNCONVERTIBLE (strict) or dropped with a warning (non-strict).

# ---- bookkeeping -------------------------------------------------------------

#' A fresh conversion context
#'
#' `unconv` collects the items the spec model cannot carry (one row of the
#' `code` / `item` / `reason` table each), `html` the fields whose HTML had to
#' be flattened, `filled` the translations taken from the base language, and
#' `notes` every other lossy adjustment. Nothing is signalled here: the caller
#' decides, once, what to do with the whole harvest.
#' @keywords internal
#' @noRd
conv_state <- function() {
  st <- new.env(parent = emptyenv())
  st$unconv <- list()
  st$html <- character(0)
  st$filled <- character(0)
  st$notes <- character(0)
  st
}

#' Record one unconvertible item
#' @keywords internal
#' @noRd
conv_refuse <- function(st, code, item, reason) {
  st$unconv[[length(st$unconv) + 1L]] <- list(
    code = paste(as.character(code), collapse = " "),
    item = paste(as.character(item), collapse = " "),
    reason = paste(as.character(reason), collapse = " ")
  )
  invisible(NULL)
}

#' Record one lossy adjustment that still leaves a valid spec
#' @keywords internal
#' @noRd
conv_note <- function(st, ...) {
  st$notes <- unique(c(st$notes, paste0(...)))
  invisible(NULL)
}

#' The unconvertible items as a data frame
#' @keywords internal
#' @noRd
conv_table <- function(st) {
  if (!length(st$unconv)) {
    return(data.frame(code = character(0), item = character(0),
                      reason = character(0), stringsAsFactors = FALSE))
  }
  out <- data.frame(
    code = vapply(st$unconv, `[[`, character(1), "code"),
    item = vapply(st$unconv, `[[`, character(1), "item"),
    reason = vapply(st$unconv, `[[`, character(1), "reason"),
    stringsAsFactors = FALSE
  )
  row.names(out) <- NULL
  out
}

# ---- text -------------------------------------------------------------------

#' Does this string carry HTML markup or an entity?
#'
#' The test decides whether flattening the value to plain text is a no-op (a
#' bare string is kept verbatim, character for character) or a genuine loss
#' (a `<p>` boundary becomes a line break, an inline mark disappears). Only
#' the second case is reported, so the warning names the fields that really
#' lost something.
#' @keywords internal
#' @noRd
conv_has_markup <- function(x) {
  if (is.null(x) || !length(x) || is.na(x)) return(FALSE)
  grepl("<[A-Za-z!/]", x) ||
    grepl("&(#[0-9]+|#[xX][0-9A-Fa-f]+|[A-Za-z][A-Za-z0-9]{1,31});", x)
}

#' Flatten one stored value to plain text, reporting the loss
#' @keywords internal
#' @noRd
conv_plain <- function(st, x, label) {
  if (is.null(x) || !length(x) || is.na(x)) return("")
  if (!conv_has_markup(x)) return(as.character(x))
  st$html <- unique(c(st$html, label))
  lss_html_to_text(x)
}

#' Turn the per-language raw values of one field into a localized spec text
#'
#' A language whose stored text is absent or empty is filled from the base
#' language and reported: real exports do have missing translations -- that is
#' what `audit_lss()` flags -- and the author fixes them in the form.
#'
#' The BASE language can be the empty one: a survey drafted in English and
#' translated into its own base language afterwards has English help texts and
#' no French ones. Dropping the field then would throw away the translations
#' that do exist, so the base is filled from the first language that has text
#' and the direction is reported (`"help of question Q1 [fr, from en]"`).
#' `NULL` -- the field is absent -- is therefore returned only when EVERY
#' language is blank, which is the truth about the file and never a silent
#' loss; the callers that require the field (survey and group titles, question
#' and item wordings) turn that `NULL` into a refusal naming the field.
#' @keywords internal
#' @noRd
conv_finish <- function(st, raw, langs, label) {
  blank <- function(v) is.na(v) || !nzchar(trimws(v))
  if (blank(raw[[1L]])) {
    source <- which(!vapply(raw, blank, logical(1)))
    if (!length(source)) return(NULL)   # absent in every language
    raw[[1L]] <- raw[[source[[1L]]]]
    st$filled <- unique(c(st$filled, paste0(
      label, " [", langs[[1L]], ", from ", langs[[source[[1L]]]], "]")))
  }
  base_raw <- raw[[1L]]
  filled <- character(0)
  for (k in seq_along(langs)) {
    v <- raw[[k]]
    if (k > 1L && blank(v)) {
      raw[[k]] <- base_raw
      filled <- c(filled, langs[[k]])
    }
  }
  if (length(filled)) {
    st$filled <- unique(c(
      st$filled, paste0(label, " [", paste(filled, collapse = ", "), "]")))
  }
  out <- lapply(seq_along(langs), function(k) conv_plain(st, raw[[k]], label))
  names(out) <- langs
  out
}

#' Raw per-language values out of an `*_l10ns` table, keyed by entity id
#' @keywords internal
#' @noRd
conv_raw_l10n <- function(tbl, index, key, col, langs) {
  vapply(langs, function(lg) {
    if (is.null(tbl) || !col %in% names(tbl)) return(NA_character_)
    i <- index[[paste(key, lg, sep = "\r")]]
    if (is.null(i)) return(NA_character_)
    v <- tbl[[col]][[i]]
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1), USE.NAMES = FALSE)
}

#' Raw per-language values out of a table that has one row per language
#' @keywords internal
#' @noRd
conv_raw_lang <- function(tbl, index, col, langs) {
  vapply(langs, function(lg) {
    if (is.null(tbl) || !col %in% names(tbl)) return(NA_character_)
    i <- index[[lg]]
    if (is.null(i)) return(NA_character_)
    v <- tbl[[col]][[i]]
    if (is.null(v)) NA_character_ else as.character(v)
  }, character(1), USE.NAMES = FALSE)
}

#' A language -> row-index environment for a one-row-per-language table
#' @keywords internal
#' @noRd
conv_lang_index <- function(tbl, lang_col) {
  env <- new.env(hash = TRUE, parent = emptyenv())
  if (is.null(tbl) || !nrow(tbl) || !lang_col %in% names(tbl)) return(env)
  for (i in seq_len(nrow(tbl))) {
    k <- tbl[[lang_col]][[i]]
    if (!is.na(k) && nzchar(k) && is.null(env[[k]])) env[[k]] <- i
  }
  env
}

#' One cell of a section data frame, or a default
#'
#' `read_lss()` reads what the file declares: a column a section does not
#' carry is simply absent, and `df$missing[[i]]` would be an error rather than
#' a missing value. Every read of an optional column goes through this.
#' @keywords internal
#' @noRd
conv_cell <- function(df, name, i = 1L, default = NA_character_) {
  if (is.null(df) || !nrow(df) || !name %in% names(df) || i > nrow(df)) {
    return(default)
  }
  v <- df[[name]][[i]]
  if (is.null(v) || is.na(v)) default else as.character(v)
}

#' One line out of a multi-line validator message
#'
#' The refusal table has one line per item; a cli message folded over several
#' bullets would break the alignment of the report and of the error listing
#' every item at once.
#' @keywords internal
#' @noRd
conv_flat <- function(x) {
  trimws(gsub("[[:space:]]+", " ", paste(as.character(x), collapse = " ")))
}

# ---- relevance --------------------------------------------------------------

#' Translate an ExpressionScript equation back into the spec mini-language
#'
#' The exact inverse of `translate_relevance()`, and nothing more: the three
#' forms `write_lss()` emits, plus the same forms written without `.NAOK` and
#' with different spacing, which is what a LimeSurvey re-export gives back.
#' Any other equation -- a hand-written condition, an unquoted comparison, a
#' conjunction -- is NOT translated: the spec has no syntax for it, and
#' guessing would author a filter that differs from the one respondents met.
#'
#' @return A length-one character (the spec condition), `NULL` for the
#'   always-true equation, or `NA_character_` when the equation is foreign.
#' @keywords internal
#' @noRd
conv_relevance <- function(eq) {
  eq <- trimws(as.character(eq %||% ""))
  if (!nzchar(eq) || identical(eq, "1")) return(NULL)
  spec_code <- function(x) ifelse(x == "-oth-", "autre", x)

  # count(Q_1.NAOK, Q_2.NAOK, ...) >= n
  m <- regmatches(eq, regexec(
    "^count\\(\\s*(.+?)\\s*\\)\\s*(>=|>|==|<=|<)\\s*([0-9]+)$", eq))[[1L]]
  if (length(m) == 4L) {
    fields <- trimws(strsplit(m[2L], ",")[[1L]])
    vars <- vapply(fields, function(f) {
      p <- regmatches(f, regexec(
        "^([A-Za-z][A-Za-z0-9]*)_[A-Za-z0-9]+(\\.NAOK)?$", f))[[1L]]
      if (length(p) == 3L) p[2L] else NA_character_
    }, character(1), USE.NAMES = FALSE)
    if (anyNA(vars) || length(unique(vars)) != 1L) return(NA_character_)
    return(sprintf("count(%s) %s %s", vars[[1L]], m[3L], m[4L]))
  }

  # (Q.NAOK == "1" or Q.NAOK == "-oth-")  ->  Q in [1, autre]
  eq_one <- function(x) {
    p <- regmatches(x, regexec(
      "^([A-Za-z][A-Za-z0-9]*)(\\.NAOK)?\\s*==\\s*\"([^\"]*)\"$", x))[[1L]]
    if (length(p) == 4L) c(p[2L], p[4L]) else NULL
  }
  m <- regmatches(eq, regexec("^\\(\\s*(.*\\S)\\s*\\)$", eq))[[1L]]
  if (length(m) == 2L) {
    parts <- trimws(strsplit(m[2L], "\\s+or\\s+")[[1L]])
    pairs <- lapply(parts, eq_one)
    if (any(vapply(pairs, is.null, logical(1)))) return(NA_character_)
    vars <- vapply(pairs, `[[`, character(1), 1L)
    if (length(unique(vars)) != 1L) return(NA_character_)
    values <- spec_code(vapply(pairs, `[[`, character(1), 2L))
    return(sprintf("%s in [%s]", vars[[1L]], paste(values, collapse = ", ")))
  }

  # Q.NAOK == "1"  ->  Q = 1
  one <- eq_one(eq)
  if (!is.null(one)) return(sprintf("%s = %s", one[[1L]], spec_code(one[[2L]])))
  NA_character_
}

# ---- the structural columns the spec model does not carry --------------------

# Every column `write_lss()` re-emits as a CONSTANT, with the value it emits,
# per section. These columns are read by NO other part of the conversion: the
# spec has no field for them, so whatever the file stores is replaced on the
# way out by the value below. Where the stored value already IS that value
# nothing is lost; where it differs, something is, and it has to be said.
#
# Keep this list in step with `write_lss()`: adding a constant to the emitter
# without adding it here re-opens a silent-loss path.
lss_structural_constants <- list(
  groups = list(grelevance = "1", randomization_group = ""),
  questions = list(preg = "", encrypted = "N", same_default = "0",
                   modulename = "", same_script = "0"),
  subquestions = list(relevance = "1", preg = "", encrypted = "N",
                      same_default = "0", modulename = "", same_script = "0"),
  question_l10ns = list(script = ""),
  answers = list(assessment_value = "0"),
  quotas = list(autoload_url = "0"),
  quota_languagesettings = list(quotals_urldescrip = "")
)

# Of those columns, the ones that change what a RESPONDENT meets -- a group
# shown on an equation or in random order, a subquestion filtered by an array
# filter, a validation regex, per-question JavaScript, encrypted storage. A
# specification that quietly dropped one of them would author a different
# questionnaire, so they are refused exactly as a foreign question-level
# equation is. The rest (assessment values, an autoloaded URL, `same_default`,
# `modulename`, `same_script`, a quota URL description) change nothing a
# respondent sees and are noted.
lss_structural_refusals <- c("grelevance", "randomization_group", "relevance",
                             "preg", "script", "encrypted")

#' Report the structural columns a conversion cannot carry
#'
#' The sweep the spec model owes the file: for every column `write_lss()`
#' re-emits as a constant, compare what the file stores with what would come
#' back out. A blank or missing cell is "not stated", never a deviation --
#' real exports write `grelevance` as the empty string as often as `"1"`, and
#' both mean "always shown".
#' @keywords internal
#' @noRd
conv_structural <- function(st, lss) {
  # who a row belongs to, in the words of the report
  owner_of <- function(section, tbl, rows) {
    title_of <- function(ids) {
      out <- as.character(ids)
      for (src in list(lss$questions, lss$subquestions)) {
        if (is.null(src) || !nrow(src) ||
            !all(c("qid", "title") %in% names(src))) next
        hit <- match(out, as.character(src$qid))
        found <- !is.na(hit)
        out[found] <- as.character(src$title)[hit[found]]
      }
      out
    }
    col <- switch(section,
                  groups = "gid", questions = "title", subquestions = "title",
                  question_l10ns = "qid", answers = "qid", quotas = "name",
                  quota_languagesettings = "quotals_quota_id", NULL)
    if (is.null(col) || !col %in% names(tbl)) return(paste0("row ", rows))
    value <- as.character(tbl[[col]][rows])
    value <- switch(section,
                    question_l10ns = title_of(value),
                    answers = title_of(value),
                    groups = paste0("group ", value),
                    quota_languagesettings = paste0("quota ", value),
                    value)
    value[is.na(value) | !nzchar(value)] <- "(unnamed)"
    value
  }

  for (section in names(lss_structural_constants)) {
    tbl <- lss[[section]]
    if (is.null(tbl) || !nrow(tbl)) next
    for (nm in names(lss_structural_constants[[section]])) {
      if (!nm %in% names(tbl)) next
      emitted <- lss_structural_constants[[section]][[nm]]
      stored <- trimws(as.character(tbl[[nm]]))
      # blank == not stated: never read as a deviation
      rows <- which(!is.na(stored) & nzchar(stored) & stored != emitted)
      if (!length(rows)) next
      who <- unique(owner_of(section, tbl, rows))
      shown <- paste(utils::head(who, 6L), collapse = ", ")
      if (length(who) > 6L) {
        shown <- paste0(shown, " and ", length(who) - 6L, " more")
      }
      what <- paste0(section, ".", nm)
      if (nm %in% lss_structural_refusals) {
        conv_refuse(
          st, shown, what,
          paste0("it carries a value a specification has no field for (\"",
                 utils::head(unique(stored[rows]), 1L),
                 "\"), and write_lss() would re-emit \"", emitted,
                 "\" instead, changing what the respondent meets"))
      } else {
        conv_note(
          st, esc(what), " differs from the value write_lss() re-emits (\"",
          esc(emitted), "\") on ", esc(shown),
          "; a specification has no field for it")
      }
    }
  }
  invisible(NULL)
}

# ---- the conversion ---------------------------------------------------------

#' Report what a survey carries outside the specification model
#'
#' A specification describes a questionnaire, not a LimeSurvey installation:
#' the `surveys` and `surveys_languagesettings` fields beyond title, welcome
#' and end text (date format, e-mail templates, policy texts, ...), the legacy
#' `conditions` table, and attributes hung on a subquestion have no slot in it
#' and `write_lss()` re-emits its own defaults instead. One note, once, rather
#' than sixty: the point is that the author knows, not that they read a list.
#' @keywords internal
#' @noRd
conv_outside_model <- function(st, lss, qids) {
  differs <- function(tbl, defaults, ignore) {
    if (is.null(tbl) || !nrow(tbl)) return(character(0))
    out <- character(0)
    for (nm in setdiff(names(tbl), ignore)) {
      value <- unique(as.character(tbl[[nm]]))
      value <- value[!is.na(value)]
      if (length(value) && !all(value == as.character(defaults[[nm]] %||% ""))) {
        out <- c(out, nm)
      }
    }
    out
  }
  fields <- c(
    differs(lss$surveys, lss_default_surveys_fields,
            c("sid", "language", "additional_languages")),
    differs(lss$survey_language_settings, lss_default_language_settings,
            c("surveyls_survey_id", "surveyls_language", "surveyls_title",
              "surveyls_welcometext", "surveyls_endtext")))
  if (length(fields)) {
    shown <- utils::head(fields, 6L)
    conv_note(st, "survey setting", if (length(fields) > 1L) "s", " ",
              esc(paste(shown, collapse = ", ")),
              if (length(fields) > 6L) paste0(" and ", length(fields) - 6L, " more"),
              " have no place in a specification; write_lss() re-emits its own defaults")
  }
  if (!is.null(lss$conditions) && nrow(lss$conditions)) {
    conv_note(st, nrow(lss$conditions),
              " row(s) of the legacy conditions table are dropped; a specification carries the relevance equations only")
  }
  qa <- lss$question_attributes
  if (!is.null(qa) && nrow(qa) && "qid" %in% names(qa)) {
    orphan <- sum(!qa$qid %in% qids)
    if (orphan) {
      conv_note(st, orphan,
                " question attribute(s) hang on a subquestion and are dropped; a specification carries question-level attributes only")
    }
  }
  invisible(NULL)
}

#' Convert one parsed survey, collecting everything that is lost
#'
#' The single engine behind [as_lss_spec()] and `lss_unconvertible()`: it never
#' signals, it returns what it built and what it could not build, so the two
#' modes differ only in what they do with the harvest.
#'
#' @return A list with `spec` (the assembled `lss_spec`, or `NULL` when what
#'   survives cannot form one), `unconvertible` (the `code` / `item` / `reason`
#'   data frame), `html`, `filled` and `notes`.
#' @keywords internal
#' @noRd
lss_convert_spec <- function(lss) {
  if (!inherits(lss, "lss")) {
    lssdoc_abort(
      c("{.arg lss} must be an {.cls lss} object from {.fn read_lss}.",
        "x" = "Got {.cls {class(lss)[1]}}."),
      class = "lssdoc_bad_lss"
    )
  }
  if (is.null(lss$groups) || !nrow(lss$groups) ||
      is.null(lss$questions) || !nrow(lss$questions)) {
    lssdoc_abort(
      c("This {.cls lss} object carries no group or no question.",
        "i" = "{.fn as_lss_spec} converts a parsed survey; read one with {.fn read_lss}."),
      class = "lssdoc_bad_lss"
    )
  }
  st <- conv_state()

  # ---- languages: the base language first, as the spec requires ----
  # `<languages>` lists the ADDITIONAL languages first and the base language
  # LAST, so `languages[[1]]` is never the base one: the base language is
  # `surveys.language`, which `read_lss()` keeps as `base_language`. Without
  # it, a multilingual file gives no way to tell which language belongs in the
  # spec's first slot, and every localized read below would shift with the
  # guess -- so it is refused rather than guessed. A single declared language
  # is unambiguous and keeps the silent path.
  base <- lss$base_language
  langs <- as.character(lss$languages %||% character(0))
  langs <- unique(langs[!is.na(langs) & nzchar(langs)])
  if (!is.null(base) && !is.na(base) && nzchar(base)) {
    langs <- unique(c(base, langs))
  } else if (length(langs) > 1L) {
    conv_refuse(
      st, "survey", "languages",
      paste0("the survey does not declare its base language (surveys.language), and a specification's first language IS its base language; it declares ",
             paste(langs, collapse = ", "), " in LimeSurvey's own order, base language last"))
  }
  if (!length(langs)) langs <- lss_spec_defaults$language

  # ---- survey-level texts ----
  sls <- lss$survey_language_settings
  sls_idx <- conv_lang_index(sls, "surveyls_language")
  title <- conv_finish(st, conv_raw_lang(sls, sls_idx, "surveyls_title", langs),
                       langs, "survey title")
  if (is.null(title)) {
    conv_refuse(st, "survey", "title",
                "the survey has no title in its base language")
  }
  lines <- function(x) {
    if (is.null(x)) return(NULL)
    lapply(x, function(v) strsplit(v, "\n", fixed = TRUE)[[1L]])
  }
  welcome <- lines(conv_finish(
    st, conv_raw_lang(sls, sls_idx, "surveyls_welcometext", langs),
    langs, "survey welcome text"))
  end_text <- lines(conv_finish(
    st, conv_raw_lang(sls, sls_idx, "surveyls_endtext", langs),
    langs, "survey end text"))

  # ---- indexes ----
  g_idx <- lss_build_l10n_index(lss$group_l10ns, "gid")
  q_idx <- lss_build_l10n_index(lss$question_l10ns, "qid")
  a_idx <- lss_build_l10n_index(lss$answer_l10ns, "aid")

  questions <- lss$questions
  if ("parent_qid" %in% names(questions)) {
    keep <- is.na(questions$parent_qid) | questions$parent_qid %in% c("0", "")
    questions <- questions[keep, , drop = FALSE]
  }

  groups <- lss$groups
  groups <- groups[order(suppressWarnings(as.integer(groups$group_order))), ,
                   drop = FALSE]

  defined <- list()      # convertible questions so far, for relevance checks
  seen <- character(0)    # every question code met, kept or refused
  qid_kept <- list()     # qid -> question code, for the quotas
  enumerated <- character(0)  # every qid some group claimed, kept or dropped

  out_groups <- list()
  for (gi in seq_len(nrow(groups))) {
    gid <- groups$gid[[gi]]
    g_label <- paste0("group ", gi)
    g_title <- conv_finish(st, conv_raw_l10n(lss$group_l10ns, g_idx, gid,
                                             "group_name", langs),
                           langs, paste0("title of ", g_label))
    g_desc <- conv_finish(st, conv_raw_l10n(lss$group_l10ns, g_idx, gid,
                                            "description", langs),
                          langs, paste0("description of ", g_label))
    gq <- questions[!is.na(questions$gid) & questions$gid == gid, , drop = FALSE]
    gq <- gq[order(suppressWarnings(as.integer(gq$question_order))), ,
             drop = FALSE]
    # claimed by a group, whatever becomes of the group: what is left over
    # after the loop belongs to no group at all
    enumerated <- c(enumerated, as.character(gq$qid))

    if (is.null(g_title)) {
      conv_refuse(st, g_label, "title",
                  "the group has no title in the base language; its questions are dropped with it")
      next
    }

    out_q <- list()
    for (j in seq_len(nrow(gq))) {
      q <- conv_question(st, lss, gq[j, , drop = FALSE], langs,
                         q_idx = q_idx, a_idx = a_idx, defined = defined,
                         seen = seen)
      seen <- c(seen, conv_cell(gq, "title", j, default = ""))
      if (is.null(q)) next
      defined[[q$code]] <- q
      qid_kept[[gq$qid[[j]]]] <- q$code
      out_q[[length(out_q) + 1L]] <- q
    }
    if (!length(out_q)) {
      if (!nrow(gq)) {
        conv_note(st, esc(g_label),
                  " holds no question and is dropped (a spec group needs one)")
      }
      next
    }
    out_groups[[length(out_groups) + 1L]] <- list(
      title = g_title, description = g_desc, questions = out_q)
  }

  # A question the loop over groups never reached belongs to no group of this
  # survey -- an orphan or missing `gid`, which real exports do carry, since
  # `audit_lss()` exists precisely because they are damaged. It is the last
  # path by which a whole question could vanish without a line in the report,
  # and its code takes part in the duplicate scan like any other.
  orphans <- which(!as.character(questions$qid) %in% enumerated)
  for (k in orphans) {
    code <- conv_cell(questions, "title", k, default = "")
    label <- if (nzchar(trimws(code))) code else paste0("qid ", questions$qid[[k]])
    conv_refuse(
      st, label, "group",
      paste0("it belongs to no group of this survey",
             if (code %in% seen) ", and its code duplicates another question's"))
    seen <- c(seen, code)
  }

  quotas <- conv_quotas(st, lss, langs, defined, qid_kept)
  conv_outside_model(st, lss, as.character(questions$qid))
  conv_structural(st, lss)

  spec <- NULL
  if (!is.null(title) && length(out_groups)) {
    spec <- tryCatch(
      lss_spec(title = title, groups = out_groups, languages = langs,
               welcome = welcome, end_text = end_text, quotas = quotas),
      lssdoc_bad_spec = function(e) {
        conv_refuse(st, "survey", "specification", conv_flat(conditionMessage(e)))
        NULL
      }
    )
  }
  list(spec = spec, unconvertible = conv_table(st),
       html = st$html, filled = st$filled, notes = st$notes)
}

#' Convert one question row, or refuse it
#'
#' Returns the question list of an `lss_spec`, or `NULL` after recording why
#' the question cannot be authored. Every refusal is recorded against the
#' question's own code, so the report names the question the author has to
#' look at.
#' @keywords internal
#' @noRd
conv_question <- function(st, lss, qrow, langs, q_idx, a_idx, defined, seen) {
  qid <- qrow$qid[[1L]]
  code <- conv_cell(qrow, "title", default = "")
  label <- if (nzchar(trimws(code))) code else paste0("qid ", qid)
  type <- conv_cell(qrow, "type", default = "")
  theme <- conv_cell(qrow, "question_theme_name", default = "")
  mandatory <- conv_cell(qrow, "mandatory", default = "N")

  # ---- kind ----
  i <- match(type, lss_kinds$type)
  if (is.na(i)) {
    d <- match(type, lss_kinds_deferred$type)
    reason <- if (is.na(d)) {
      paste0("LimeSurvey type \"", type, "\" is not a type lssdoc can author")
    } else {
      paste0(lss_kinds_deferred$label[[d]], " (type \"", type,
             "\") is not authorable: ", lss_kinds_deferred$reason[[d]])
    }
    return(conv_refuse(st, label, "type", reason))
  }
  kind <- lss_kinds$kind[[i]]
  map <- lss_kinds[i, , drop = FALSE]
  if (nzchar(theme) && !identical(theme, map$theme)) {
    conv_note(st, "question ", esc(label), ": theme ", esc(theme),
              " replaced by the authorable ", esc(map$theme))
  }

  # ---- code ----
  if (!grepl("^[A-Za-z][A-Za-z0-9]{0,19}$", code)) {
    return(conv_refuse(
      st, label, "code",
      "the question code is not a spec code (a letter, then letters and digits, at most 20)"))
  }
  if (code %in% seen) {
    # against every code SEEN, not only the kept ones: two questions sharing a
    # variable name is a property of the file, and dropping the first for
    # another reason must not make the second look sound
    return(conv_refuse(st, label, "code",
                       "duplicate question code: codes are variable names and must be unique"))
  }

  # ---- texts ----
  text <- conv_finish(st, conv_raw_l10n(lss$question_l10ns, q_idx, qid,
                                        "question", langs),
                      langs, paste0("text of question ", label))
  if (is.null(text)) {
    return(conv_refuse(st, label, "text",
                       "the question has no wording in the base language"))
  }
  help <- conv_finish(st, conv_raw_l10n(lss$question_l10ns, q_idx, qid,
                                        "help", langs),
                      langs, paste0("help of question ", label))

  q <- list(code = code, kind = kind, text = text, help = help,
            mandatory = mandatory %in% c("Y", "S"))
  if (identical(mandatory, "S")) {
    conv_note(st, "question ", esc(label),
              ": soft mandatory read as mandatory (the spec has a single flag)")
  }

  # ---- option lists ----
  items <- conv_items(st, lss, qid, map, langs, q_idx, a_idx, label)
  if (is.null(items)) return(NULL)
  for (field in names(items)) q[[field]] <- items[[field]]

  # ---- attributes, the "other" option, caps and exclusions ----
  q <- conv_attributes(st, lss, qid, q, map, langs, label,
                       other = identical(conv_cell(qrow, "other", default = "N"), "Y"))
  if (is.null(q)) return(NULL)

  # ---- shape, as the validator will read it ----
  shaped <- tryCatch({
    validate_question_shape(q); validate_options(q); validate_other(q)
    validate_caps(q); TRUE
  }, lssdoc_bad_spec = function(e) conv_flat(conditionMessage(e)))
  if (!isTRUE(shaped)) {
    return(conv_refuse(st, label, "shape", shaped))
  }

  # ---- relevance ----
  equation <- conv_cell(qrow, "relevance", default = "")
  rel <- conv_relevance(equation)
  if (length(rel) == 1L && is.na(rel)) {
    return(conv_refuse(
      st, label, "relevance",
      paste0("the display condition \"", trimws(equation),
             "\" is not one of the three conditions the spec can express")))
  }
  if (!is.null(rel)) {
    ok <- tryCatch({ validate_relevance(code, rel, defined); TRUE },
                   lssdoc_bad_spec = function(e) conv_flat(conditionMessage(e)))
    if (!isTRUE(ok)) return(conv_refuse(st, label, "relevance", ok))
    q$relevance <- rel
  }
  q
}

#' The option / row / column lists of one question
#'
#' Which spec field each LimeSurvey section feeds is read off the kind table,
#' exactly as `write_lss()` reads it to emit them, so the two stay inverse by
#' construction.
#' @return A named list of spec fields, or `NULL` after a refusal.
#' @keywords internal
#' @noRd
conv_items <- function(st, lss, qid, map, langs, q_idx, a_idx, label) {
  # Rows of the question that the two filters below would throw away: those
  # on another answer scale (a dual-scale question keeps half its answers and
  # loses the other half), and those in the section this kind does not read at
  # all. Either way the stored shape is not the shape lssdoc would emit, which
  # makes the question unconvertible -- not silently trimmable.
  mine <- function(tbl, key) {
    if (is.null(tbl) || !nrow(tbl) || !key %in% names(tbl)) return(NULL)
    rows <- tbl[!is.na(tbl[[key]]) & tbl[[key]] == qid, , drop = FALSE]
    if (nrow(rows)) rows else NULL
  }
  off_scale <- function(rows) {
    if (is.null(rows) || !"scale_id" %in% names(rows)) return(0L)
    scale <- suppressWarnings(as.integer(rows$scale_id))
    sum(!is.na(scale) & scale != 0L)
  }
  stray <- function(section, rows, field) {
    if (is.null(rows)) return(FALSE)
    if (is.na(field)) {
      conv_refuse(
        st, label, section,
        paste0("it stores ", nrow(rows), " row(s) in the <", section,
               "> section, which a \"", map$kind,
               "\" does not carry; the question does not have the shape lssdoc emits"))
      return(TRUE)
    }
    n <- off_scale(rows)
    if (n) {
      conv_refuse(
        st, label, section,
        paste0(n, " of its ", nrow(rows), " <", section,
               "> row(s) sit on another answer scale (scale_id other than 0); the second scale is not authorable and dropping it would lose half the question"))
      return(TRUE)
    }
    FALSE
  }
  ans_rows <- mine(lss$answers, "qid")
  sq_rows <- mine(lss$subquestions, "parent_qid")
  lost <- c(stray("answers", ans_rows, map$answers_from),
            stray("subquestions", sq_rows, map$subquestions_from))
  if (any(lost)) return(NULL)

  answers <- function() {
    ans <- ans_rows
    if (is.null(ans)) return(list())
    scale <- suppressWarnings(as.integer(ans$scale_id))
    ans <- ans[is.na(scale) | scale == 0L, , drop = FALSE]
    if (!nrow(ans)) return(list())
    ans <- ans[order(suppressWarnings(as.integer(ans$sortorder))), , drop = FALSE]
    lapply(seq_len(nrow(ans)), function(k) list(
      code = as.character(ans$code[[k]]),
      text = conv_finish(st, conv_raw_l10n(lss$answer_l10ns, a_idx,
                                           ans$aid[[k]], "answer", langs),
                         langs, paste0("answer ", ans$code[[k]],
                                       " of question ", label))))
  }
  subquestions <- function() {
    sq <- sq_rows
    if (is.null(sq)) return(list())
    scale <- suppressWarnings(as.integer(sq$scale_id))
    sq <- sq[is.na(scale) | scale == 0L, , drop = FALSE]
    if (!nrow(sq)) return(list())
    sq <- sq[order(suppressWarnings(as.integer(sq$question_order))), , drop = FALSE]
    lapply(seq_len(nrow(sq)), function(k) list(
      code = as.character(sq$title[[k]]),
      text = conv_finish(st, conv_raw_l10n(lss$question_l10ns, q_idx,
                                           sq$qid[[k]], "question", langs),
                         langs, paste0("subquestion ", sq$title[[k]],
                                       " of question ", label))))
  }

  out <- list()
  if (!is.na(map$subquestions_from)) out[[map$subquestions_from]] <- subquestions()
  if (!is.na(map$answers_from)) out[[map$answers_from]] <- answers()

  for (field in names(out)) {
    opts <- out[[field]]
    empty <- vapply(opts, function(o) is.null(o$text), logical(1))
    if (any(empty)) {
      conv_refuse(st, label, field,
                  paste0("item ", opts[[which(empty)[1L]]]$code %||% "",
                         " has no label in the base language"))
      return(NULL)
    }
  }
  out
}

#' Read the question attributes back into spec fields
#'
#' `write_lss()` turns four spec fields into attributes (`max_answers`, the
#' other option's label and position, `exclude_all_others`) and passes the rest
#' through; this reads exactly those four back and passes the rest through the
#' other way. An attribute LimeSurvey stores per language comes back keyed by
#' language code, which is the shape `write_lss()` fans out again.
#' @return The question, or `NULL` after a refusal.
#' @keywords internal
#' @noRd
conv_attributes <- function(st, lss, qid, q, map, langs, label, other) {
  qa <- lss$question_attributes
  rows <- NULL
  if (!is.null(qa) && nrow(qa) &&
      all(c("qid", "attribute", "value") %in% names(qa))) {
    rows <- qa[!is.na(qa$qid) & qa$qid == qid, , drop = FALSE]
    if (!nrow(rows)) rows <- NULL
  }
  has_lang <- !is.null(rows) && "language" %in% names(rows)
  of <- function(name) {
    if (is.null(rows)) return(NULL)
    hit <- rows[rows$attribute == name, , drop = FALSE]
    if (!nrow(hit)) NULL else hit
  }
  # One value for an attribute LimeSurvey stores language-less. A file that
  # nonetheless carries language rows for it yields the base-language value,
  # never the first row of an arbitrary order.
  global <- function(name) {
    hit <- of(name)
    if (is.null(hit)) return(NULL)
    if (has_lang) {
      blank <- which(is.na(hit$language) | !nzchar(hit$language))
      if (length(blank)) return(as.character(hit$value[[blank[[1L]]]]))
      at_base <- which(!is.na(hit$language) & hit$language == langs[[1L]])
      if (length(at_base)) return(as.character(hit$value[[at_base[[1L]]]]))
    }
    as.character(hit$value[[1L]])
  }
  localized <- function(name) {
    hit <- of(name)
    if (is.null(hit) || !has_lang) return(NULL)
    raw <- conv_raw_lang(hit, conv_lang_index(hit, "language"), "value", langs)
    if (all(is.na(raw))) return(NULL)
    conv_finish(st, raw, langs,
                paste0("attribute ", name, " of question ", label))
  }
  used <- character(0)

  # ---- the native other option ----
  if (isTRUE(other)) {
    if (!isTRUE(map$other_allowed)) {
      return(conv_refuse(
        st, label, "other",
        paste0("it carries a native \"other\" option, which the spec only allows on ",
               paste(kinds_where("other_allowed"), collapse = ", "))))
    }
    used <- c(used, "other_replace_text")
    txt <- localized("other_replace_text")
    if (is.null(txt)) {
      plain <- global("other_replace_text")
      if (!is.null(plain) && nzchar(plain)) {
        txt <- stats::setNames(
          lapply(langs, function(lg) conv_plain(st, plain, paste0(
            "other label of question ", label))), langs)
      }
    }
    if (is.null(txt)) {
      # LimeSurvey shows its own wording when the attribute is absent; the
      # spec needs a real label, so the form word of each language is used
      txt <- stats::setNames(lapply(langs, function(lg) {
        lss_chrome_strings(lss_resolve_chrome_lang(NULL, lg))$form_other
      }), langs)
      conv_note(st, "question ", esc(label),
                ": the \"other\" option carries no label; the default wording is used")
    }
    q$options <- c(q$options %||% list(), list(list(text = txt, other = TRUE)))
    pos <- global("other_position")
    if (!is.null(pos) && nzchar(pos)) {
      used <- c(used, "other_position")
      if (!pos %in% c("beginning", "end", "specific")) {
        return(conv_refuse(st, label, "other_position",
                           paste0("unknown position \"", pos, "\"")))
      }
      q$other_position <- pos
      if (identical(pos, "specific")) {
        used <- c(used, "other_position_code")
        q$other_position_code <- global("other_position_code") %||% ""
      }
    }
  }

  # ---- exclusive options ----
  excl <- global("exclude_all_others")
  if (!is.null(excl) && nzchar(excl)) {
    used <- c(used, "exclude_all_others")
    if (!isTRUE(map$exclusive_allowed)) {
      return(conv_refuse(
        st, label, "exclude_all_others",
        "an exclusive option is a multiple-choice mechanism the spec does not carry on this kind"))
    }
    codes <- trimws(strsplit(excl, ";", fixed = TRUE)[[1L]])
    codes <- codes[nzchar(codes)]
    unknown <- setdiff(codes, option_codes(q$options))
    if (length(unknown)) {
      return(conv_refuse(
        st, label, "exclude_all_others",
        paste0("it cites option code(s) ", paste(unknown, collapse = ", "),
               " this question does not have")))
    }
    q$options <- lapply(q$options, function(o) {
      if (!isTRUE(o$other) && !is.null(o$code) && o$code %in% codes) {
        o$exclusive <- TRUE
      }
      o
    })
  }

  # ---- the answer cap ----
  cap <- global("max_answers")
  if (!is.null(cap) && nzchar(cap) && !identical(map$max_answers_rule, "none")) {
    used <- c(used, "max_answers")
    n <- suppressWarnings(as.integer(cap))
    if (is.na(n)) {
      return(conv_refuse(st, label, "max_answers",
                         paste0("the cap \"", cap, "\" is not a whole number")))
    }
    q$max_answers <- n
  }

  # ---- everything else passes through, as LimeSurvey stores it ----
  if (!is.null(rows)) {
    keep <- list()
    for (nm in setdiff(unique(rows$attribute), used)) {
      value <- NULL
      if (nm %in% lss_i18n_attributes) value <- localized(nm)
      if (is.null(value)) {
        value <- global(nm)
        hit <- of(nm)
        if (nm %in% lss_i18n_attributes && has_lang &&
            !is.null(value) && nzchar(trimws(value))) {
          # localized() found no value under any DECLARED language although
          # LimeSurvey localizes this attribute, yet the file holds one: what
          # goes into the spec is a single stored row that `write_lss()` will
          # repeat under every language. That is a translation nobody wrote,
          # so it is said rather than emitted in silence. (An attribute that
          # is empty in every language loses nothing and is not reported.)
          conv_note(st, "question ", esc(label), ": attribute ", esc(nm),
                    " has no value in any declared language; one stored value is reused for all of them")
        }
        if (!is.null(hit) && nrow(hit) > 1L && has_lang &&
            any(!is.na(hit$language) & nzchar(hit$language)) &&
            !nm %in% lss_i18n_attributes) {
          conv_note(st, "question ", esc(label), ": attribute ", esc(nm),
                    " is stored per language although LimeSurvey does not localize it; the base-language value is kept")
        }
      }
      if (is.null(value)) next
      keep[[nm]] <- value
    }
    if (length(keep)) q$attributes <- keep
  }
  q
}

#' Convert the end-of-survey quotas
#'
#' The spec carries exactly the quota `write_lss()` emits: one member, one
#' single-choice target, the terminate action. A quota built on several
#' members, on a question the spec cannot target, or with another action is
#' refused rather than reduced to something the author did not write.
#' @keywords internal
#' @noRd
conv_quotas <- function(st, lss, langs, defined, qid_kept) {
  qt <- lss$quotas
  if (is.null(qt) || !nrow(qt)) return(list())
  qm <- lss$quota_members
  qls <- lss$quota_languagesettings
  out <- list()
  for (k in seq_len(nrow(qt))) {
    id <- conv_cell(qt, "id", k, default = as.character(k))
    admin <- conv_cell(qt, "name", k, default = "")
    label <- paste0("quota ", if (nzchar(admin)) admin else id)
    members <- if (is.null(qm) || !nrow(qm) || !"quota_id" %in% names(qm)) {
      NULL
    } else {
      qm[!is.na(qm$quota_id) & qm$quota_id == id, , drop = FALSE]
    }
    if (is.null(members) || nrow(members) != 1L) {
      conv_refuse(st, label, "quota",
                  paste0("it combines ", if (is.null(members)) 0L else nrow(members),
                         " question(s); the spec expresses a quota on exactly one"))
      next
    }
    action <- conv_cell(qt, "action", k, default = "1")
    if (!identical(action, "1")) {
      conv_refuse(st, label, "quota",
                  paste0("its action is \"", action,
                         "\"; the spec only expresses the terminate action"))
      next
    }
    target <- qid_kept[[as.character(members$qid[[1L]])]]
    if (is.null(target)) {
      conv_refuse(st, label, "quota",
                  "its question is not part of the converted survey")
      next
    }
    q <- defined[[target]]
    if (!isTRUE(kind_field(q$kind, "quota_target"))) {
      conv_refuse(st, label, "quota",
                  paste0("its question is a \"", q$kind,
                         "\"; the spec puts a quota on a question with a single coded answer (",
                         paste(kinds_where("quota_target"), collapse = ", "), ")"))
      next
    }
    value <- as.character(members$code[[1L]])
    # a fixed scale (yesno, gender, fivepoint) declares no option: its codes
    # are the kind's own -- a gender quota is the commonest real quota there is
    codes <- kind_implicit_codes(q$kind) %||% option_codes(q$options)
    if (!value %in% codes) {
      conv_refuse(st, label, "quota",
                  paste0("answer code \"", value, "\" does not exist on ", target))
      next
    }
    rows <- if (is.null(qls) || !nrow(qls)) NULL else {
      qls[!is.na(qls$quotals_quota_id) & qls$quotals_quota_id == id, , drop = FALSE]
    }
    idx <- conv_lang_index(rows, "quotals_language")
    name <- conv_finish(st, conv_raw_lang(rows, idx, "quotals_name", langs),
                        langs, paste0("name of ", label))
    if (is.null(name) && nzchar(admin)) {
      name <- stats::setNames(rep(list(admin), length(langs)), langs)
    }
    message <- conv_finish(st, conv_raw_lang(rows, idx, "quotals_message", langs),
                           langs, paste0("message of ", label))
    if (!is.null(rows) && nrow(rows) &&
        any(nzchar(rows$quotals_url %||% "") & !is.na(rows$quotals_url))) {
      conv_note(st, esc(label), ": its end URL is dropped (the spec has no URL field)")
    }
    if (!identical(conv_cell(qt, "active", k, default = "1"), "1")) {
      conv_note(st, esc(label), ": it is inactive in the export and is written active")
    }
    out[[length(out) + 1L]] <- list(
      question = target, code = value, name = name, message = message,
      limit = conv_limit(conv_cell(qt, "qlimit", k, default = "0")))
  }
  out
}

#' A quota limit as a whole number at or above zero
#' @keywords internal
#' @noRd
conv_limit <- function(x) {
  n <- suppressWarnings(as.integer(x))
  if (length(n) != 1L || is.na(n) || n < 0L) 0L else n
}

# ---- the two modes ----------------------------------------------------------

#' Everything a survey carries that an `lss_spec` cannot
#'
#' The table both modes of [as_lss_spec()] report and the tests read: one row
#' per item the spec model refuses, with the question code (or the group /
#' quota label), what about it, and why. Zero rows means the survey converts
#' whole.
#' @param lss An `lss` object from [read_lss()].
#' @return A data frame with columns `code`, `item` and `reason`.
#' @keywords internal
#' @noRd
lss_unconvertible <- function(lss) {
  lss_convert_spec(lss)$unconvertible
}

#' Report the lossy adjustments, once
#' @keywords internal
#' @noRd
conv_warn_lossy <- function(res) {
  html <- res$html
  filled <- res$filled
  notes <- res$notes
  if (!length(html) && !length(filled) && !length(notes)) return(invisible(NULL))
  lssdoc_warn(
    c("Converting this survey into an {.fn lss_spec} is lossy.",
      if (length(html)) {
        c("!" = "HTML flattened to plain text in {length(html)} field{?s}: {.val {html}}.")
      },
      if (length(filled)) {
        c("!" = "Missing translation{?s} filled from the base language: {.val {filled}}.")
      },
      if (length(notes)) stats::setNames(notes, rep("!", length(notes))),
      "i" = "Review the result, or fix the survey in the Word authoring form."),
    class = "lssdoc_lossy_conversion"
  )
}

#' Turn a parsed LimeSurvey survey into an authoring specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' \strong{Experimental.} Convert an `lss` object -- a real LimeSurvey export
#' read by [read_lss()] -- into the [lss_spec()] object the authoring side of
#' the package works with. It is the entry point for *modifying an existing
#' questionnaire*: read the `.lss` a colleague sends, render it as the Word
#' authoring form with [write_form_docx()], let the author edit the form, and
#' hand the result back to [read_form_docx()] and [write_lss()].
#'
#' The conversion is deliberately narrower than the file it reads. An `lss`
#' object is whatever LimeSurvey exported; an `lss_spec` is what lssdoc can
#' author and re-emit. Everything in between is reported: refused outright
#' (`strict = TRUE`) or dropped with a warning (`strict = FALSE`). Nothing is
#' ever lost in silence.
#'
#' @param lss An `lss` object from [read_lss()].
#' @param strict Logical. `TRUE` (default) refuses the whole survey with a
#'   single classed error (`lssdoc_unconvertible`) listing **every**
#'   unconvertible item. `FALSE` drops those items -- and the filters and
#'   quotas that depend on them -- and returns a valid specification, with one
#'   warning (`lssdoc_lossy_conversion`) listing what was dropped.
#'
#' @return An [lss_spec()] object.
#'
#' @section What is converted:
#' * **Kinds**: the LimeSurvey type letter maps to the authoring kind through
#'   the kind table (`lss_kinds` in the sources). A question theme other than
#'   the authorable one is replaced by it and reported.
#' * **Texts**: survey title, welcome and end text, group titles and
#'   descriptions, question wordings and help, option, row and column labels,
#'   the "other" label and the quota name and message, each taken per language
#'   from the `*_l10ns` tables. HTML is flattened to plain text -- paragraphs
#'   and `<br>` become line breaks, inline marks are dropped -- and every field
#'   that really carried markup is named in the lossy warning.
#' * **Languages**: all the survey's languages, the base language
#'   (`surveys.language`) first, as [lss_spec()] requires. A text missing in a
#'   language is filled from the base language and reported: real exports do
#'   have missing translations -- that is what [audit_lss()] flags -- and the
#'   author fixes them in the form. A text missing in the BASE language but
#'   present in another is filled the other way round, and the report names
#'   the direction (`"help of question Q1 [fr, from en]"`), so the
#'   translations a survey drafted in another language already has are never
#'   thrown away.
#' * **Structure**: options, rows and columns from `answers` and
#'   `subquestions`; the native "other" option from `other = "Y"` and
#'   `other_replace_text`; exclusive options from `exclude_all_others`; the cap
#'   from `max_answers`; the "other" position from `other_position` and
#'   `other_position_code`. Every other question attribute passes through
#'   unchanged into the question's `attributes`.
#' * **Filters**: the three equations [write_lss()] emits -- `Q.NAOK == "1"`,
#'   `(Q.NAOK == "1" or Q.NAOK == "-oth-")` and
#'   `count(Q_1.NAOK, ...) >= n` -- are translated back into `Q = 1`,
#'   `Q in [1, autre]` and `count(Q) >= n`, with or without `.NAOK`.
#' * **Quotas**: a single-member quota with the terminate action becomes a
#'   `code = value` condition on its question -- on any kind holding a single
#'   coded answer, the implicit scales included, so a quota on the gender
#'   question (`M` / `F`) converts like a quota on a declared option code.
#'
#' @section What is refused:
#' Each of the following is one row of the unconvertible report (question
#' code, item, reason), and all of them are listed at once:
#'
#' * a question whose type is not authorable -- the eight deferred LimeSurvey
#'   types (`P`, `H`, `1`, `;`, `:`, `*`, `|`, `I`) and any unknown one;
#' * a question code that is not a spec code, or a duplicate of an earlier one;
#' * a question or option with no wording in the base language, or a shape the
#'   [lss_spec()] validator refuses (too few options, an array without rows or
#'   columns, a cap larger than the option list);
#' * a display condition in any other form than the three above -- a
#'   hand-written equation is never guessed at;
#' * a quota combining several questions, built on a question the spec cannot
#'   target, or carrying an action other than "terminate";
#' * a group with no title in the base language (its questions go with it),
#'   and a question belonging to no group at all;
#' * a question storing rows on a second answer scale, or in a section its
#'   type does not carry: the shape on file is not the shape lssdoc emits,
#'   and half an option list is not a smaller option list;
#' * a structural column carrying something a specification has no field for
#'   and [write_lss()] would silently replace by its own constant, where that
#'   changes what the respondent meets: a group display equation
#'   (`groups.grelevance`) or randomization, a per-subquestion relevance (an
#'   array filter), a validation regex (`preg`), per-question JavaScript
#'   (`question_l10ns.script`), encrypted storage (`encrypted`). The columns
#'   that change nothing a respondent sees -- assessment values,
#'   `same_default`, `modulename`, `same_script`, a quota URL description --
#'   are noted in the lossy warning instead;
#' * a multilingual survey that does not declare its base language: a
#'   specification's first language IS its base language, and `<languages>`
#'   lists the base one last.
#'
#' @examples
#' # The bundled flawed survey uses LimeSurvey question types lssdoc does
#' # not author yet, and a filter it cannot express, so it converts only in
#' # the permissive mode -- which names everything it had to drop.
#' flawed <- system.file("extdata", "audit_demo.lss", package = "lssdoc")
#' spec <- suppressWarnings(as_lss_spec(read_lss(flawed), strict = FALSE))
#' spec
#' @seealso [read_lss()], [lss_spec()], [write_form_docx()], [write_lss()].
#' @export
as_lss_spec <- function(lss, strict = TRUE) {
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    lssdoc_abort("{.arg strict} must be {.code TRUE} or {.code FALSE}.",
                 class = "lssdoc_bad_input")
  }
  res <- lss_convert_spec(lss)
  items <- res$unconvertible
  n <- nrow(items)
  bullets <- if (n) {
    stats::setNames(
      sprintf("%s -- %s: %s", esc(items$code), esc(items$item), esc(items$reason)),
      rep("x", n))
  } else {
    character(0)
  }

  if (n && strict) {
    lssdoc_abort(
      c("This survey carries {n} item{?s} {.fn lss_spec} cannot express.",
        bullets,
        "i" = "Pass {.code strict = FALSE} to drop them and convert the rest."),
      class = "lssdoc_unconvertible", items = items
    )
  }
  if (is.null(res$spec)) {
    lssdoc_abort(
      c("Nothing convertible is left in this survey.",
        bullets,
        "i" = "A specification needs a title and at least one group with one question."),
      class = "lssdoc_unconvertible", items = items
    )
  }
  if (n) {
    lssdoc_warn(
      c("Dropped {n} item{?s} {.fn lss_spec} cannot express.",
        bullets,
        "i" = "The specification describes the rest of the survey."),
      class = "lssdoc_lossy_conversion", items = items
    )
  }
  conv_warn_lossy(res)
  res$spec
}
