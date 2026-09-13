#' Build and validate a survey specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' \strong{Experimental.} Assemble a survey specification -- the
#' authoring-side counterpart of the `lss` object -- that [write_lss()]
#' can turn into an importable LimeSurvey `.lss` file. The specification
#' is validated in depth at construction time, because LimeSurvey itself
#' imports silently: a mistyped attribute, a filter referencing a missing
#' answer code, or a cap larger than the option list are all accepted on
#' import and only surface once respondents hit them.
#'
#' @param title Character. Survey title shown to respondents.
#' @param groups List of groups. Each group is a list with `title`
#'   (character) and `questions` (list of question specifications, see
#'   Details).
#' @param languages Character vector of language codes, the primary
#'   language first (e.g. `c("fr", "en")`). Defaults to `"fr"`. See the
#'   *Languages* section.
#' @param language Character. Backward-compatible alias for a
#'   single-language survey: `language = "fr"` is `languages = "fr"`.
#'   Passing both is allowed only when `language` is `languages[1]`.
#' @param welcome Character vector of welcome-text paragraphs, or a single
#'   string starting with `<` used verbatim as HTML. Optional.
#' @param end_text Character vector of end-page paragraphs, or a single
#'   string starting with `<` used verbatim as HTML. Optional.
#' @param quotas List of end-of-survey quotas. Each element is a list with
#'   `question` (code of a single-choice question), `code` (the answer
#'   code that triggers the quota), `message` (text shown to the
#'   respondent) and optionally `name`. A quota emitted by [write_lss()]
#'   has limit zero and terminates the survey -- the LimeSurvey mechanism
#'   for "if the person declines, end here".
#'
#' @return An object of class `lss_spec`: the validated specification with
#'   normalized questions (auto-numbered option codes filled in).
#'
#' @details
#' Each question is a list with fields:
#'
#' * `code` -- stable technical code (letters then letters/digits, at most
#'   20 characters), unique across the survey. Codes become variable names
#'   in the data and are pinned once fieldwork starts.
#' * `kind` -- the question type. Choice kinds: `"single"` (radio list),
#'   `"dropdown"`, `"singlecomment"` (list with comment), `"multiple"`,
#'   `"ranking"`. Array kinds: `"array"` (rows and columns),
#'   `"array5"`, `"array10"`, `"arrayyesno"`, `"arraytrend"` (rows only,
#'   the scale is implicit). Item batteries: `"multitext"`,
#'   `"multinumeric"` (one field per option). Scalar kinds:
#'   `"text"`, `"shorttext"`, `"hugetext"`, `"numeric"`, `"date"`,
#'   `"yesno"` (implicit Y/N), `"gender"` (implicit M/F), `"fivepoint"`
#'   (implicit 1-5). Plus `"display"` (text shown without input). Every
#'   kind maps to a LimeSurvey type attested by real exports; types that
#'   would require an unverified mechanism (dual-scale arrays of texts or
#'   numbers, equations, file upload) are deliberately not supported yet.
#' * `text` -- the question wording. `mandatory` -- logical, default
#'   `FALSE`. `help` -- optional help text shown under the wording.
#' * `options` -- for `single`, `multiple` and `ranking`: list of options,
#'   each a list with `text` and optionally `code`, `other = TRUE`
#'   (native LimeSurvey "other" with a free-text field; `single` and
#'   `multiple` only) and `exclusive = TRUE` (`multiple` only; unchecks
#'   every other box). Options without a `code` are numbered `1..n` in
#'   order, skipping the `other` option, which LimeSurvey codes natively.
#' * `rows` / `columns` -- for `array`: the subquestions and the answer
#'   scale, same shape as `options`.
#' * `relevance` -- display condition in a minimal syntax:
#'   `code = 1`, `code in [1, 2, autre]`, `count(code) >= 2` (at least n
#'   boxes ticked in a multiple-choice question). The keyword `autre`
#'   designates the native "other" option. Conditions may only reference
#'   questions defined earlier in the survey.
#' * `max_answers` -- cap for `multiple` (strictly below the number of
#'   options) and `ranking` (at most the number of items).
#' * `other_position` -- where the "other" option is displayed:
#'   `"end"` (LimeSurvey default), `"beginning"`, or `"specific"`
#'   together with `other_position_code`, the code of the option AFTER
#'   which "other" appears. In practice "other" usually belongs before
#'   the "none of the above"-type exclusive options, which the default
#'   position puts it after.
#' * `attributes` -- optional named list of extra global question
#'   attributes passed through verbatim (e.g. `display_columns`).
#'
#' @section Languages:
#' `languages` declares the survey languages, the primary one first;
#' `languages[1]` is the language [write_lss()] emits. Every localizable
#' text -- survey title, welcome and end texts, group titles, question
#' texts and help, option, row and column labels, the "other" label, quota
#' names and messages -- accepts either a plain string (read as the
#' primary language) or a named character vector or list keyed by language
#' code:
#'
#' ```r
#' lss_spec(
#'   title = c(fr = "Enquete", en = "Survey"),
#'   languages = c("fr", "en"),
#'   groups = list(list(
#'     title = c(fr = "Profil", en = "Profile"),
#'     questions = list(list(
#'       code = "q1", kind = "yesno",
#'       text = c(fr = "Etes-vous d'accord ?", en = "Do you agree?")))))
#' )
#' ```
#'
#' The spec keeps one canonical form (a named list over the declared
#' languages) and is strict: as soon as several languages are declared,
#' every text must supply every one of them. A missing translation is
#' precisely what [audit_lss()] flags when reading a `.lss`, so the spec
#' refuses to author one. In this version [write_lss()] emits the primary
#' language only, and errors with class `lssdoc_unsupported_multilang` on
#' a spec that declares more than one; multi-language emission is planned
#' for 0.3.0.
#'
#' @examples
#' spec <- lss_spec(
#'   title = "Demo",
#'   languages = "fr",
#'   groups = list(list(
#'     title = "Profil",
#'     questions = list(
#'       list(code = "consent", kind = "single", text = "Participez-vous ?",
#'            mandatory = TRUE,
#'            options = list(list(text = "Oui"), list(text = "Non"))),
#'       list(code = "raisons", kind = "multiple", text = "Pourquoi ?",
#'            relevance = "consent = 1", max_answers = 2,
#'            options = list(
#'              list(text = "Une raison"), list(text = "Une autre"),
#'              list(text = "Encore une"),
#'              list(text = "Aucune raison", exclusive = TRUE),
#'              list(text = "Autre raison", other = TRUE)))
#'     )
#'   ))
#' )
#' spec$groups[[1]]$questions[[2]]$options[[4]]$code
#' @seealso [write_lss()] to emit the `.lss` file, [read_lss()] and
#'   [audit_lss()] to read it back and check it.
#' @export
lss_spec <- function(title,
                     groups,
                     languages = NULL,
                     language = NULL,
                     welcome = NULL,
                     end_text = NULL,
                     quotas = NULL) {
  languages <- resolve_languages(languages, language)
  if (is.null(title) || (!is.character(title) && !is.list(title))) {
    lssdoc_abort("{.arg title} must be a single non-empty string.",
                 class = "lssdoc_bad_spec")
  }
  if (!is.list(groups) || !length(groups)) {
    lssdoc_abort("{.arg groups} must be a non-empty list of groups.",
                 class = "lssdoc_bad_spec")
  }

  spec <- list(
    title = title, languages = languages, language = languages[[1L]],
    welcome = welcome, end_text = end_text,
    groups = groups, quotas = quotas %||% list()
  )
  spec <- spec_normalize(spec)
  primary_title <- loc_text(spec$title)
  if (!is.character(primary_title) || length(primary_title) != 1L ||
      !nzchar(primary_title)) {
    lssdoc_abort("{.arg title} must be a single non-empty string.",
                 class = "lssdoc_bad_spec")
  }
  spec_validate(spec)
  structure(spec, class = "lss_spec")
}

#' @export
print.lss_spec <- function(x, ...) {
  n_q <- sum(vapply(x$groups, function(g) {
    sum(vapply(g$questions, function(q) q$kind != "display", logical(1)))
  }, integer(1)))
  title <- loc_text(x$title, x$language)
  langs <- x$languages %||% x$language
  cli::cli_text("<lss_spec> {.val {title}} ({langs})")
  cli::cli_text("{length(x$groups)} group{?s}, {n_q} question{?s}, {length(x$quotas)} quota{?s}")
  invisible(x)
}


spec_kinds <- c(
  "single", "dropdown", "singlecomment", "multiple",
  "array", "array5", "array10", "arrayyesno", "arraytrend", "ranking",
  "multitext", "multinumeric", "text", "shorttext", "hugetext",
  "numeric", "date", "yesno", "gender", "fivepoint", "display"
)

# kinds whose answer is a single value, usable as a `=` / `in` relevance target
single_valued_kinds <- c("single", "dropdown", "singlecomment",
                         "yesno", "gender", "fivepoint")
# kinds with a fixed implicit scale: no options in the spec, and these are
# the values a relevance condition may cite
implicit_codes <- list(yesno = c("Y", "N"), gender = c("M", "F"),
                       fivepoint = as.character(1:5))
# kinds carrying a list of options, with the minimum count required
option_kinds <- c(single = 2L, dropdown = 2L, singlecomment = 2L,
                  multiple = 2L, ranking = 2L, multitext = 1L,
                  multinumeric = 1L)
row_only_kinds <- c("array5", "array10", "arrayyesno", "arraytrend")
no_option_kinds <- c("text", "shorttext", "hugetext", "numeric", "date",
                     "yesno", "gender", "fivepoint", "display")
other_kinds <- c("single", "dropdown", "multiple")

# ---- languages and localized texts ------------------------------------------

#' Resolve the declared languages from `languages` / `language`
#'
#' `language` is the original single-language argument, kept as an alias:
#' `languages[1]` is the primary language, the one `write_lss()` emits.
#' Giving both is allowed as long as they agree on the primary language.
#' @keywords internal
#' @noRd
resolve_languages <- function(languages, language) {
  if (!is.null(language)) {
    if (!is.character(language) || length(language) != 1L || is.na(language) ||
        !nzchar(trimws(language))) {
      lssdoc_abort("{.arg language} must be a single non-empty language code.",
                   class = "lssdoc_bad_spec")
    }
    language <- trimws(language)
  }
  if (!is.null(languages)) {
    if (!is.character(languages) || !length(languages) || anyNA(languages) ||
        !all(nzchar(trimws(languages)))) {
      lssdoc_abort(
        "{.arg languages} must be a character vector of non-empty language codes.",
        class = "lssdoc_bad_spec"
      )
    }
    languages <- trimws(languages)
    repeated <- unique(languages[duplicated(languages)])
    if (length(repeated)) {
      lssdoc_abort(
        "{.arg languages} must be unique: {.val {repeated}} appear{?s} twice.",
        class = "lssdoc_bad_spec"
      )
    }
  }
  if (is.null(languages)) {
    return(language %||% "fr")
  }
  if (!is.null(language) && !identical(language, languages[[1L]])) {
    primary <- languages[[1L]]
    lssdoc_abort(
      c("{.arg language} and {.arg languages} disagree.",
        "x" = "{.arg language} is {.val {language}}, but the primary language {.code languages[1]} is {.val {primary}}.",
        "i" = "Pass {.arg languages} alone, or make {.arg language} its first element."),
      class = "lssdoc_bad_spec"
    )
  }
  languages
}

#' Normalize one localizable text to a named list over the declared languages
#'
#' Every user-facing text of a spec accepts either a plain string (the
#' primary language) or a named character vector / named list keyed by
#' language code. Internally there is exactly one representation -- a
#' named list ordered as `languages` -- so the emitter never has to guess.
#' Multi-language specs are strict: a declared language with no text would
#' import as an empty question, which is precisely what `audit_lss()`
#' flags on read, so the spec refuses to author it.
#'
#' @param x The user-supplied text, or `NULL`.
#' @param languages The declared language codes; the first is primary.
#' @param field A human label for the field, already brace-escaped, used
#'   in error messages.
#' @return A named list over the declared languages, or `NULL` when the
#'   text is absent (`NULL` or empty).
#' @keywords internal
#' @noRd
spec_localize <- function(x, languages, field) {
  primary <- languages[[1L]]
  bad_shape <- function() {
    lssdoc_abort(
      c(paste0("The ", field, " must be a string, or a named list or vector keyed by language code."),
        "i" = "Declared language{?s}: {.val {languages}}."),
      class = "lssdoc_bad_spec"
    )
  }

  if (is.null(x)) return(NULL)
  if (is.list(x)) {
    if (!length(x) || is.null(names(x)) || any(!nzchar(names(x)))) bad_shape()
    ok <- vapply(x, function(v) is.character(v) && length(v) >= 1L && !anyNA(v),
                 logical(1))
    if (!all(ok)) {
      lssdoc_abort(
        paste0("The ", field, " must be a character string for every language."),
        class = "lssdoc_bad_spec"
      )
    }
    value <- lapply(x, as.character)
  } else if (is.character(x)) {
    if (anyNA(x)) bad_shape()
    if (is.null(names(x))) {
      # a plain string carries no language of its own: it is the primary
      # text, and an empty one counts as no text at all
      if (!length(x) || all(!nzchar(trimws(x)))) return(NULL)
      value <- stats::setNames(list(x), primary)
    } else {
      if (any(!nzchar(names(x)))) bad_shape()
      value <- as.list(x)
    }
  } else {
    bad_shape()
  }

  repeated <- unique(names(value)[duplicated(names(value))])
  if (length(repeated)) {
    lssdoc_abort(
      paste0("The ", field, " gives {.val {repeated}} more than once."),
      class = "lssdoc_bad_spec"
    )
  }
  unknown <- setdiff(names(value), languages)
  if (length(unknown)) {
    lssdoc_abort(
      c(paste0("The ", field, " uses undeclared language code{?s} {.val {unknown}}."),
        "i" = "Declared language{?s}: {.val {languages}}."),
      class = "lssdoc_bad_spec"
    )
  }
  if (!primary %in% names(value)) {
    lssdoc_abort(
      c(paste0("The ", field, " does not give the primary language {.val {primary}}."),
        "i" = "{.code languages[1]} is the language {.fn write_lss} emits."),
      class = "lssdoc_bad_spec"
    )
  }
  if (length(languages) > 1L) {
    absent <- setdiff(languages, names(value))
    if (length(absent)) {
      lssdoc_abort(
        c(paste0("The ", field, " has no text for {.val {absent}}."),
          "i" = "Every declared language needs every text: a missing translation is exactly what {.fn audit_lss} flags on read."),
        class = "lssdoc_bad_spec"
      )
    }
  }
  value[intersect(languages, names(value))]
}

#' Read one language out of a canonical localized text
#'
#' Falls back to the primary (first) text when the language is absent, and
#' returns `x` untouched when it is not a localized value yet, so the
#' validators work on specs built by hand as well.
#' @keywords internal
#' @noRd
loc_text <- function(x, language = NULL, default = "") {
  if (is.null(x)) return(default)
  if (!is.list(x)) return(x)
  if (!length(x)) return(default)
  if (!is.null(language) && !is.null(x[[language]])) return(x[[language]])
  x[[1L]]
}

# ---- normalization ---------------------------------------------------------

#' Fill in defaults, localize every text, and auto-number option codes
#'
#' Options without an explicit `code` are numbered `1..n` in order. The
#' `other` option is skipped: LimeSurvey codes it natively (`-oth-`), and
#' giving it a rank would leave a hole in the sequence.
#' @keywords internal
#' @noRd
spec_normalize <- function(spec) {
  langs <- spec$languages
  spec$title <- spec_localize(spec$title, langs, "survey title")
  spec$welcome <- spec_localize(spec$welcome, langs, "welcome text")
  spec$end_text <- spec_localize(spec$end_text, langs, "end text")

  spec$groups <- lapply(seq_along(spec$groups), function(gi) {
    g <- spec$groups[[gi]]
    g$title <- spec_localize(g$title, langs, paste0("title of group ", gi))
    g$questions <- lapply(g$questions, function(q) {
      label <- paste0("question ", dQuote(esc(q$code %||% ""), FALSE))
      q$mandatory <- isTRUE(q$mandatory)
      q$text <- spec_localize(q$text, langs, paste0("text of ", label))
      q$help <- spec_localize(q$help, langs, paste0("help of ", label))
      for (field in c("options", "rows", "columns")) {
        if (!is.null(q[[field]])) {
          q[[field]] <- normalize_options(q[[field]], langs,
                                          paste0(field, " of ", label))
        }
      }
      q
    })
    g
  })

  spec$quotas <- lapply(seq_along(spec$quotas), function(k) {
    qu <- spec$quotas[[k]]
    qu$name <- spec_localize(qu$name, langs, paste0("name of quota ", k))
    qu$message <- spec_localize(qu$message, langs, paste0("message of quota ", k))
    qu
  })

  spec
}

normalize_options <- function(options, languages, field) {
  n <- 0L
  out <- vector("list", length(options))
  for (k in seq_along(options)) {
    o <- options[[k]]
    if (is.character(o)) o <- list(text = o)
    if (!is.list(o)) {
      lssdoc_abort(
        paste0("Every item of the ", field, " must be a string or a list."),
        class = "lssdoc_bad_spec"
      )
    }
    o$other <- isTRUE(o$other)
    o$exclusive <- isTRUE(o$exclusive)
    o$text <- spec_localize(o$text, languages,
                            paste0("text of item ", k, " in the ", field))
    if (o$other) {
      o$code <- NULL
    } else if (is.null(o$code)) {
      n <- n + 1L
      o$code <- as.character(n)
    } else {
      o$code <- as.character(o$code)
      num <- suppressWarnings(as.integer(o$code))
      if (!is.na(num)) n <- max(n, num)
    }
    out[[k]] <- o
  }
  out
}

# ---- validation ------------------------------------------------------------

# Any user-supplied value injected into a cli message needs its braces
# escaped, otherwise glue interprets them as markup and replaces the
# classed error with an unreadable cli one.
esc <- function(x) gsub("}", "}}", gsub("{", "{{", as.character(x), fixed = TRUE), fixed = TRUE)

spec_abort <- function(code, ...) {
  lssdoc_abort(
    c(paste0("Invalid specification for question {.val ", esc(code), "}."), ...),
    class = "lssdoc_bad_spec",
    call = rlang::caller_env(2)
  )
}

#' Validate a normalized specification in depth
#'
#' LimeSurvey will not report any of this: it imports what it understands
#' and silently drops or ignores the rest. Every rule here was earned on a
#' real deployment.
#' @keywords internal
#' @noRd
spec_validate <- function(spec) {
  seen <- character(0)
  # questions defined so far, for relevance reference checks
  defined <- list()

  for (g in spec$groups) {
    g_title <- loc_text(g$title)
    if (!is.character(g_title) || length(g_title) != 1L || !nzchar(g_title)) {
      lssdoc_abort("Every group needs a non-empty {.field title}.",
                   class = "lssdoc_bad_spec")
    }
    if (!is.list(g$questions) || !length(g$questions)) {
      lssdoc_abort(
        paste0("Group {.val ", esc(g_title), "} has no questions."),
        class = "lssdoc_bad_spec"
      )
    }
    for (q in g$questions) {
      code <- q$code %||% ""
      if (!is.character(code) || length(code) != 1L ||
          !grepl("^[A-Za-z][A-Za-z0-9]{0,19}$", code)) {
        code <- paste(as.character(code), collapse = " ")
        lssdoc_abort(
          c(paste0("Invalid question code {.val ", esc(code), "}."),
            "x" = "Codes start with a letter, use only letters and digits, at most 20 characters."),
          class = "lssdoc_bad_spec"
        )
      }
      if (code %in% seen) {
        spec_abort(code, "x" = "Duplicate question code: codes are variable names and must be unique.")
      }
      seen <- c(seen, code)

      if (!(q$kind %||% "") %in% spec_kinds) {
        spec_abort(code, "x" = paste0(
          "Unknown kind {.val ", q$kind %||% "", "}: use one of ",
          paste0('"', spec_kinds, '"', collapse = ", "), "."))
      }
      q_text <- loc_text(q$text)
      if (!is.character(q_text) || length(q_text) != 1L || !nzchar(q_text)) {
        spec_abort(code, "x" = "The question {.field text} is empty.")
      }

      validate_question_shape(q)
      validate_options(q)
      validate_other(q)
      validate_caps(q)
      if (!is.null(q$relevance)) {
        validate_relevance(q$code, q$relevance, defined)
      }
      defined[[code]] <- q
    }
  }

  for (quota in spec$quotas) {
    target <- defined[[quota$question %||% ""]]
    if (is.null(target)) {
      lssdoc_abort(
        paste0("Quota references unknown question {.val ", esc(quota$question %||% ""), "}."),
        class = "lssdoc_bad_spec"
      )
    }
    if (target$kind != "single") {
      lssdoc_abort(
        paste0("Quota on {.val ", esc(quota$question), "}: quotas require a single-choice question."),
        class = "lssdoc_bad_spec"
      )
    }
    codes <- option_codes(target$options)
    if (!(quota$code %||% "") %in% codes) {
      lssdoc_abort(
        paste0("Quota on {.val ", esc(quota$question), "}: answer code {.val ",
               esc(quota$code %||% ""), "} does not exist."),
        class = "lssdoc_bad_spec"
      )
    }
  }
  invisible(spec)
}

option_codes <- function(options) {
  vapply(Filter(function(o) !isTRUE(o$other), options %||% list()),
         function(o) o$code, character(1))
}

validate_question_shape <- function(q) {
  minimum <- option_kinds[q$kind]
  if (!is.na(minimum) && length(q$options %||% list()) < minimum) {
    spec_abort(q$code, "x" = paste0(
      "{.val ", q$kind, "} needs at least ", minimum, " {.field options}."))
  }
  if (q$kind == "array") {
    if (!length(q$rows %||% list()) || !length(q$columns %||% list())) {
      spec_abort(q$code, "x" = "An array needs non-empty {.field rows} and {.field columns}.")
    }
  }
  if (q$kind %in% row_only_kinds) {
    if (!length(q$rows %||% list())) {
      spec_abort(q$code, "x" = paste0("{.val ", q$kind, "} needs non-empty {.field rows}."))
    }
    if (length(q$columns %||% list())) {
      spec_abort(q$code, "x" = paste0(
        "{.val ", q$kind, "} carries an implicit scale: {.field columns} must stay empty."))
    }
  }
  if (q$kind %in% no_option_kinds &&
      (length(q$options %||% list()) || length(q$rows %||% list()))) {
    spec_abort(q$code, "x" = paste0("{.val ", q$kind, "} questions carry no options."))
  }
}

validate_options <- function(q) {
  for (field in c("options", "rows", "columns")) {
    opts <- q[[field]]
    if (is.null(opts)) next
    codes <- option_codes(opts)
    if (anyDuplicated(codes)) {
      spec_abort(q$code, "x" = paste0("Duplicate option codes in {.field ", field, "}."))
    }
    bad <- codes[!grepl("^[A-Za-z0-9]{1,5}$", codes)]
    if (length(bad)) {
      spec_abort(q$code, "x" = paste0(
        "Invalid option code {.val ", esc(bad[1L]),
        "} in {.field ", field,
        "}: 1-5 letters or digits (LimeSurvey stores answer codes in 5 characters)."))
    }
    empty <- vapply(opts, function(o) {
      txt <- loc_text(o$text)
      !is.character(txt) || length(txt) != 1L || !nzchar(trimws(txt))
    }, logical(1))
    if (any(empty)) {
      spec_abort(q$code, "x" = paste0("Empty option text in {.field ", field, "}."))
    }
    if (field != "options") {
      if (any(vapply(opts, function(o) isTRUE(o$other), logical(1)))) {
        spec_abort(q$code, "x" = "Array rows and columns cannot carry an {.field other} option.")
      }
    }
  }
  # exclusive is a multiple-choice mechanism (exclude_all_others)
  if (q$kind != "multiple" &&
      any(vapply(q$options %||% list(), function(o) isTRUE(o$exclusive), logical(1)))) {
    spec_abort(q$code, "x" = "{.field exclusive} options only exist on {.val multiple} questions.")
  }
}

validate_other <- function(q) {
  others <- sum(vapply(q$options %||% list(), function(o) isTRUE(o$other), logical(1)))
  if (any(vapply(q$options %||% list(),
                 function(o) isTRUE(o$other) && isTRUE(o$exclusive), logical(1)))) {
    spec_abort(q$code,
      "x" = "The {.field other} option cannot be {.field exclusive}: LimeSurvey's exclusion mechanism only addresses coded options.")
  }
  if (others > 1L) {
    spec_abort(q$code, "x" = "At most one option can be {.field other}.")
  }
  if (others == 1L && !q$kind %in% other_kinds) {
    spec_abort(q$code,
      "x" = "The native {.field other} option only exists on {.val single} and {.val multiple} questions.",
      "i" = "For a ranking, add it as a regular rankable item without a free-text field.")
  }
  pos <- q$other_position
  if (!is.null(pos)) {
    if (others == 0L) {
      spec_abort(q$code, "x" = "{.field other_position} set but no {.field other} option.")
    }
    if (!pos %in% c("beginning", "end", "specific")) {
      spec_abort(q$code, "x" = '{.field other_position} must be "beginning", "end" or "specific".')
    }
    if (pos == "specific") {
      after <- as.character(q$other_position_code %||% "")
      if (!after %in% option_codes(q$options)) {
        spec_abort(q$code,
          "x" = paste0("{.field other_position_code} {.val ", esc(after),
                       "} is not an option code of this question."))
      }
    }
  }
}

validate_caps <- function(q) {
  cap <- q$max_answers
  if (is.null(cap)) return(invisible())
  if (!q$kind %in% c("multiple", "ranking")) {
    spec_abort(q$code, "x" = "{.field max_answers} only applies to {.val multiple} and {.val ranking}.")
  }
  cap <- suppressWarnings(as.integer(cap))
  n <- length(option_codes(q$options))
  if (is.na(cap) || cap < 1L) {
    spec_abort(q$code, "x" = "{.field max_answers} must be a positive integer.")
  }
  if (q$kind == "multiple" && cap >= n) {
    spec_abort(q$code, "x" = paste0("{.field max_answers} (", cap,
                                    ") must be below the number of options."))
  }
  if (q$kind == "ranking" && cap > n) {
    spec_abort(q$code, "x" = paste0("{.field max_answers} (", cap,
                                    ") exceeds the number of rankable items."))
  }
}

#' Parse the minimal relevance syntax and check its references
#'
#' Three forms: `code = value`, `code in [v1, v2, ...]`,
#' `count(code) >= n` (any comparator). The keyword `autre` maps to the
#' native other option. A condition may only cite questions defined
#' EARLIER: LimeSurvey cannot filter on a question that has not been
#' asked yet, and imports such an equation without complaint -- the
#' question then simply never shows.
#' @keywords internal
#' @noRd
validate_relevance <- function(code, expr, defined) {
  expr <- trimws(expr)
  ref_error <- function(...) spec_abort(code, ...)

  check_ref <- function(var, values, count = FALSE) {
    target <- defined[[var]]
    if (is.null(target)) {
      ref_error("x" = paste0(
        "{.field relevance} cites {.val ", esc(var),
        "}, which is not defined earlier in the survey."))
    }
    if (count && target$kind != "multiple") {
      ref_error("x" = paste0("count() requires a {.val multiple} question, and {.val ",
                             esc(var), "} is {.val ", target$kind, "}."))
    }
    if (!count && !target$kind %in% single_valued_kinds) {
      ref_error("x" = paste0(
        "{.field relevance} with = or in requires a single-valued question, and {.val ",
        esc(var), "} is {.val ", target$kind,
        "}. For a multiple-choice target, use count()."))
    }
    codes <- implicit_codes[[target$kind]] %||% option_codes(target$options)
    has_other <- any(vapply(target$options %||% list(),
                            function(o) isTRUE(o$other), logical(1)))
    for (v in values) {
      if (tolower(v) == "autre") {
        if (!has_other) {
          ref_error("x" = paste0("{.field relevance} uses {.val autre} but {.val ",
                                 var, "} has no other option."))
        }
      } else if (!v %in% codes) {
        ref_error("x" = paste0("{.field relevance} cites {.val ", var, " = ", v,
                               "}, but that option code does not exist."))
      }
    }
  }

  m <- regmatches(expr, regexec("^count\\(([A-Za-z][A-Za-z0-9]*)\\)\\s*(>=|>|==|<=|<)\\s*([0-9]+)$", expr))[[1]]
  if (length(m) == 4L) {
    check_ref(m[2], character(0), count = TRUE)
    return(invisible())
  }
  m <- regmatches(expr, regexec("^([A-Za-z][A-Za-z0-9]*)\\s+in\\s+\\[([^]]+)\\]$", expr))[[1]]
  if (length(m) == 3L) {
    check_ref(m[2], trimws(strsplit(m[3], ",")[[1]]))
    return(invisible())
  }
  m <- regmatches(expr, regexec("^([A-Za-z][A-Za-z0-9]*)\\s*=\\s*([A-Za-z0-9-]+)$", expr))[[1]]
  if (length(m) == 3L) {
    check_ref(m[2], m[3])
    return(invisible())
  }
  spec_abort(code,
    "x" = paste0("Unrecognized {.field relevance} syntax: {.val ", esc(expr), "}."),
    "i" = "Use code = 1, code in [1, 2, autre] or count(code) >= 2.")
}

`%||%` <- function(x, y) if (is.null(x)) y else x
