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
#'   (character), optionally `description` (character, a localizable
#'   introduction shown above the group), and `questions` (list of
#'   question specifications, see Details).
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
#'   `question` (code of a question holding a single coded answer:
#'   `"single"`, `"dropdown"`, `"singlecomment"`, `"yesno"`, `"gender"` or
#'   `"fivepoint"`), `code` (the answer
#'   code that triggers the quota -- a declared option code, or one of the
#'   kind's implicit codes for the fixed scales: `Y`/`N`, `M`/`F`, `1`-`5`),
#'   `message` (text shown to the
#'   respondent) and optionally `name` and `limit` (a whole number at or
#'   above zero; omitted, it stays the historical zero). A quota emitted
#'   by [write_lss()] terminates the survey -- the LimeSurvey mechanism
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
#'   kind maps to a LimeSurvey type attested by real exports; eight
#'   further LimeSurvey types are deferred, each with its own reason, in
#'   `lss_kinds_deferred` in the sources.
#' * `text` -- the question wording. `mandatory` -- logical, default
#'   `FALSE`. `help` -- optional help text shown under the wording.
#' * `options` -- for every kind that takes an option list (`single`,
#'   `dropdown`, `singlecomment`, `multiple`, `ranking`, `multitext`,
#'   `multinumeric`): list of options, each a list with `text` and
#'   optionally `code`, `other = TRUE` (native LimeSurvey "other" with a
#'   free-text field; `single`, `dropdown` and `multiple` only) and
#'   `exclusive = TRUE` (`multiple` only; unchecks every other box).
#'   Options without a `code` are numbered `1..n` in order, skipping the
#'   `other` option, which LimeSurvey codes natively. An explicit code is
#'   letters and digits, and its length follows the table LimeSurvey stores
#'   the list in: **5 characters** for a list emitted as answers (`single`,
#'   `dropdown`, `singlecomment`, `ranking` options, and `array` columns --
#'   `answers.code` is a `varchar(5)`), **20 characters** for a list emitted
#'   as subquestions (`multiple`, `multitext`, `multinumeric` options, and
#'   `array` and implicit-scale array rows -- `questions.title` is a
#'   `varchar(20)`, the same column as a question code).
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
#' * `attributes` -- optional named list of extra question attributes
#'   passed through verbatim (e.g. `display_columns`). A name LimeSurvey
#'   stores per language (`prefix`, `suffix`, `choice_title`,
#'   `printable_help`, ...) is emitted once per declared language by
#'   [write_lss()], and may be given either as one string for every
#'   language or keyed by language code.
#'
#' @section Languages:
#' `languages` declares the survey languages, the primary one first;
#' `languages[1]` is the base language [write_lss()] emits, and every other
#' declared language is written alongside it. Every localizable
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
#' refuses to author one. [write_lss()] writes every declared language:
#' `languages[1]` becomes the survey's base language and the others its
#' additional languages, each localized section carrying one row per
#' language.
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
    title = title, languages = languages,
    language = languages[[lss_spec_defaults$primary_language]],
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
    sum(vapply(g$questions,
               function(q) isTRUE(kind_field(q$kind, "collects_response")),
               logical(1)))
  }, integer(1)))
  title <- loc_text(x$title, x$language)
  langs <- x$languages %||% x$language
  cli::cli_text("<lss_spec> {.val {title}} ({langs})")
  cli::cli_text("{length(x$groups)} group{?s}, {n_q} question{?s}, {length(x$quotas)} quota{?s}")
  invisible(x)
}


# ---- the kind table ---------------------------------------------------------

#' Describe one authorable kind
#'
#' Defaults describe a plain scalar kind (a free-text box): no options, no
#' rows, no emitted sections, no relevance role. A row is therefore written
#' as its deviations from that baseline.
#'
#' @param kind Authoring name accepted in the `kind` field of a question.
#' @param type LimeSurvey type letter written to `questions$type` (and copied
#'   onto every subquestion row).
#' @param theme LimeSurvey 6 `question_theme_name`.
#' @param label Human-readable name, for documentation only.
#' @param family One of `"choice"`, `"array"`, `"battery"`, `"scalar"`,
#'   `"display"`; grouping for documentation only.
#' @param options,rows,columns What validation does with each spec field:
#'   `"required"`, `"forbidden"` or `"ignored"` (accepted and dropped).
#' @param min_options Minimum option count; `NA_integer_` unless
#'   `options == "required"`.
#' @param answers_from,subquestions_from Which spec field the emitter writes
#'   as `<answers>` / `<subquestions>`, or `NA` for none.
#' @param relevance_role `"scalar"` (a `=` / `in` target), `"count"` (a
#'   `count()` target) or `"none"`.
#' @param implicit_codes Fixed scale codes a relevance condition may cite.
#' @param other_allowed,exclusive_allowed Whether options may carry
#'   `other = TRUE` / `exclusive = TRUE`.
#' @param max_answers_rule `"none"`, `"below_n"` (cap must be below the
#'   option count) or `"at_most_n"`.
#' @param implicit_min_answers The emitter adds `min_answers = 1` when the
#'   question is mandatory or capped.
#' @param quota_target A quota may hang off this kind: the question holds one
#'   answer per respondent (`relevance_role == "scalar"`) and that answer has
#'   a code a quota can name -- a declared option code, or one of
#'   `implicit_codes` for the fixed scales (`yesno` Y/N, `gender` M/F,
#'   `fivepoint` 1-5). A quota on the gender question is the commonest real
#'   quota there is.
#' @param collects_response The kind yields a response variable and counts as
#'   a question in summaries.
#' @keywords internal
#' @noRd
kind_def <- function(kind, type, theme, label, family = "scalar",
                     options = "forbidden", rows = "forbidden",
                     columns = "ignored",
                     min_options = NA_integer_,
                     answers_from = NA_character_,
                     subquestions_from = NA_character_,
                     relevance_role = "none", implicit_codes = character(),
                     other_allowed = FALSE, exclusive_allowed = FALSE,
                     max_answers_rule = "none", implicit_min_answers = FALSE,
                     quota_target = FALSE, collects_response = TRUE) {
  tri <- c("required", "forbidden", "ignored")
  stopifnot(
    is.character(kind), length(kind) == 1L,
    is.character(type), nchar(type) == 1L,
    is.character(theme), is.character(label),
    options %in% tri, rows %in% tri, columns %in% tri,
    is.integer(min_options),
    xor(is.na(min_options), identical(options, "required")),
    relevance_role %in% c("scalar", "count", "none"),
    max_answers_rule %in% c("none", "below_n", "at_most_n"),
    is.character(implicit_codes)
  )
  mget(names(formals(sys.function())), environment())
}

# One row per kind lssdoc can author. Every type letter and LimeSurvey 6
# theme name below is attested by the reference corpus of real exports
# (where each type's options live -- answers, subquestions, or neither).
# Types needing an unproven mechanism (dual-scale subquestions for array
# texts/numbers, unattested LS6 theme names) are deliberately absent:
# P, H, 1, ;, :, *, |, I -- one row each, with its own reason, in
# `lss_kinds_deferred` below. Adding a kind = adding ONE kind_def() line.
# Row order is user-visible (it is pasted into the unknown-kind error):
# append, never reorder.
lss_kind_defs <- list(
  kind_def("single",        "L", "listradio",         "Single choice (radio)",      "choice",  options = "required", rows = "ignored", min_options = 2L, answers_from = "options",      relevance_role = "scalar", other_allowed = TRUE, quota_target = TRUE),
  kind_def("dropdown",      "!", "list_dropdown",     "Single choice (dropdown)",   "choice",  options = "required", rows = "ignored", min_options = 2L, answers_from = "options",      relevance_role = "scalar", other_allowed = TRUE, quota_target = TRUE),
  kind_def("singlecomment", "O", "list_with_comment", "Single choice with comment", "choice",  options = "required", rows = "ignored", min_options = 2L, answers_from = "options",      relevance_role = "scalar", quota_target = TRUE),
  kind_def("multiple",      "M", "multiplechoice",    "Multiple choice",            "choice",  options = "required", rows = "ignored", min_options = 2L, subquestions_from = "options", relevance_role = "count", other_allowed = TRUE, exclusive_allowed = TRUE, max_answers_rule = "below_n"),
  kind_def("array",         "F", "arrays/array",      "Array (rows and columns)",   "array",   options = "ignored",  rows = "required", columns = "required",  subquestions_from = "rows", answers_from = "columns"),
  kind_def("array5",        "A", "arrays/5point",     "Array, 5-point scale",       "array",   options = "ignored",  rows = "required", columns = "forbidden", subquestions_from = "rows", implicit_codes = as.character(1:5)),
  kind_def("array10",       "B", "arrays/10point",    "Array, 10-point scale",      "array",   options = "ignored",  rows = "required", columns = "forbidden", subquestions_from = "rows", implicit_codes = as.character(1:10)),
  kind_def("arrayyesno",    "C", "arrays/yesnouncertain", "Array, yes/no/uncertain", "array",  options = "ignored",  rows = "required", columns = "forbidden", subquestions_from = "rows", implicit_codes = c("Y", "N", "U")),
  kind_def("arraytrend",    "E", "arrays/increasesamedecrease", "Array, increase/same/decrease", "array", options = "ignored", rows = "required", columns = "forbidden", subquestions_from = "rows", implicit_codes = c("I", "S", "D")),
  kind_def("ranking",       "R", "ranking",           "Ranking",                    "choice",  options = "required", rows = "ignored", min_options = 2L, answers_from = "options",      max_answers_rule = "at_most_n", implicit_min_answers = TRUE),
  kind_def("multitext",     "Q", "multipleshorttext", "Multiple short texts",       "battery", options = "required", rows = "ignored", min_options = 1L, subquestions_from = "options"),
  kind_def("multinumeric",  "K", "multiplenumeric",   "Multiple numeric inputs",    "battery", options = "required", rows = "ignored", min_options = 1L, subquestions_from = "options"),
  kind_def("text",          "T", "longfreetext",      "Long free text"),
  kind_def("shorttext",     "S", "shortfreetext",     "Short free text"),
  kind_def("hugetext",      "U", "hugefreetext",      "Huge free text"),
  kind_def("numeric",       "N", "numerical",         "Numeric input"),
  kind_def("date",          "D", "date",              "Date"),
  kind_def("yesno",         "Y", "yesno",             "Yes/no",            relevance_role = "scalar", implicit_codes = c("Y", "N"), quota_target = TRUE),
  kind_def("gender",        "G", "gender",            "Gender",            relevance_role = "scalar", implicit_codes = c("M", "F"), quota_target = TRUE),
  kind_def("fivepoint",     "5", "5pointchoice",      "Five-point choice", relevance_role = "scalar", implicit_codes = as.character(1:5), quota_target = TRUE),
  kind_def("display",       "X", "boilerplate",       "Text display",      "display", collects_response = FALSE)
)

# The single table every validator, the emitter and print.lss_spec() read.
# Columns are documented on kind_def() above.
lss_kinds <- local({
  col <- function(f, proto) vapply(lss_kind_defs, function(d) d[[f]], proto)
  tbl <- data.frame(
    kind = col("kind", ""), type = col("type", ""), theme = col("theme", ""),
    label = col("label", ""), family = col("family", ""),
    options = col("options", ""), rows = col("rows", ""),
    columns = col("columns", ""),
    min_options = col("min_options", NA_integer_),
    answers_from = col("answers_from", NA_character_),
    subquestions_from = col("subquestions_from", NA_character_),
    relevance_role = col("relevance_role", ""),
    other_allowed = col("other_allowed", NA),
    exclusive_allowed = col("exclusive_allowed", NA),
    max_answers_rule = col("max_answers_rule", ""),
    implicit_min_answers = col("implicit_min_answers", NA),
    quota_target = col("quota_target", NA),
    collects_response = col("collects_response", NA),
    stringsAsFactors = FALSE
  )
  tbl$implicit_codes <- lapply(lss_kind_defs, `[[`, "implicit_codes")
  stopifnot(
    nrow(tbl) == 21L, !anyDuplicated(tbl$kind), !anyDuplicated(tbl$type),
    !anyDuplicated(tbl$theme),
    identical(!is.na(tbl$min_options), tbl$options == "required"),
    # a quota names ONE answer code of a ONE-answer question: every scalar
    # kind that has codes -- declared options, or a fixed implicit scale --
    # can carry one, and no other kind can.
    identical(
      tbl$quota_target,
      tbl$relevance_role == "scalar" &
        (tbl$options == "required" | lengths(tbl$implicit_codes) > 0L)),
    # no exclusive <= other invariant: `exclude_all_others` and the native
    # other option are unrelated LimeSurvey mechanisms, and a kind may well
    # take exclusive options without taking an other option.
    all(tbl$implicit_min_answers <= (tbl$max_answers_rule != "none"))
  )
  row.names(tbl) <- NULL
  tbl
})

# ---- cross-kind authoring defaults ------------------------------------------

# What a spec field means when the author leaves it out. These are exactly the
# defaults the step-2 form template must pre-fill, so they live in one object
# rather than as literals scattered over the normalizer and the emitter:
# `lss_template_docx()` (step 2) writes a blank form from these values, and
# cannot drift from what `spec_normalize()` and `write_lss()` assume.
lss_spec_defaults <- list(
  mandatory        = FALSE,  # a question is optional unless it says otherwise
  relevance        = "1",    # LimeSurvey's always-true condition: always shown
  other            = FALSE,  # no native "other" option
  exclusive        = FALSE,  # no option that unchecks every other box
  option_code_from = 1L,     # options without a code are numbered 1..n, the
                             # `other` option skipped (LimeSurvey codes it)
  language         = "fr",   # the declared language when none is given
  primary_language = 1L      # languages[[1]] is the language write_lss() emits
)

# ---- LimeSurvey types deliberately not authorable yet ------------------------

# One row per type left out of `lss_kind_defs`, with the reason it is out, so
# that adding one later is a deliberate, informed change and not a guess.
# P, H, * and I would each be one more kind_def() line once their LimeSurvey 6
# theme name is attested by a real export; 1, ;, : and | additionally need a
# mechanism the emitter does not have (a second answer scale, upload
# attributes), hence one more column plus one more emitter reader.
lss_kinds_deferred <- data.frame(
  type = c("P", "H", "1", ";", ":", "*", "|", "I"),
  label = c("Multiple choice with comments", "Array by column",
            "Array (dual scale)", "Array (texts)", "Array (numbers)",
            "Equation", "File upload", "Language switch"),
  reason = c(
    "Subquestion mechanism identical to `multiple`, but its LS6 theme name is not attested.",
    "Rows-and-columns shape identical to `array` (F), but its LS6 theme name is not attested.",
    "Needs two answer scales (scale_id 0 and 1), which the emitter does not write.",
    "Needs dual-scale subquestions (the text columns live on scale_id 1): unproven mechanism.",
    "Same unproven dual-scale subquestion mechanism as `;`.",
    "Collects no response and carries its formula in an attribute whose LS6 theme name is not attested.",
    "Needs the file-upload attribute family (max size, allowed types), none of it attested.",
    "Not a question: a language selector, with no attested LS6 theme name."
  ),
  stringsAsFactors = FALSE
)

#' The kind table flattened for documentation
#'
#' `lss_kinds` carries a list column (`implicit_codes`), `NA` cells and
#' logicals, none of which render as a table. This returns the same
#' information one row per kind with every cell a short string, so the
#' vignette and the reference page print it with a single `knitr::kable()`
#' call instead of growing their own transformation. Documentation only:
#' nothing in the validator or the emitter reads it. The types left out are
#' in `lss_kinds_deferred`.
#' @keywords internal
#' @noRd
lss_kinds_reference <- function() {
  yes <- function(x) ifelse(x, "yes", "")
  blank_na <- function(x) ifelse(is.na(x), "", as.character(x))
  blank_none <- function(x) ifelse(x == "none", "", x)
  out <- data.frame(
    kind = lss_kinds$kind,
    label = lss_kinds$label,
    family = lss_kinds$family,
    type = lss_kinds$type,
    theme = lss_kinds$theme,
    options = lss_kinds$options,
    rows = lss_kinds$rows,
    columns = lss_kinds$columns,
    min_options = blank_na(lss_kinds$min_options),
    answers_from = blank_na(lss_kinds$answers_from),
    subquestions_from = blank_na(lss_kinds$subquestions_from),
    relevance = blank_none(lss_kinds$relevance_role),
    implicit_codes = vapply(lss_kinds$implicit_codes,
                            function(x) paste(x, collapse = ", "), character(1)),
    other = yes(lss_kinds$other_allowed),
    exclusive = yes(lss_kinds$exclusive_allowed),
    max_answers = blank_none(lss_kinds$max_answers_rule),
    implicit_min_answers = yes(lss_kinds$implicit_min_answers),
    quota_target = yes(lss_kinds$quota_target),
    collects_response = yes(lss_kinds$collects_response),
    stringsAsFactors = FALSE
  )
  row.names(out) <- NULL
  out
}

#' Is `x` the name of an authorable kind?
#'
#' FALSE -- never an error -- for `NULL`, a non-character, or anything but a
#' single non-missing string, so a malformed `kind` still raises the classed
#' spec error rather than a raw R condition.
#' @keywords internal
#' @noRd
is_kind <- function(x) {
  is.character(x) && length(x) == 1L && !is.na(x) && x %in% lss_kinds$kind
}

#' The whole metadata row for one kind
#'
#' Only reachable once [is_kind()] has passed; an unknown kind yields a row
#' of `NA`. Never read `implicit_codes` from it (that cell is a length-one
#' list): use [kind_implicit_codes()].
#' @keywords internal
#' @noRd
kind_row <- function(kind) {
  lss_kinds[match(kind, lss_kinds$kind), , drop = FALSE]
}

#' One metadata cell for one kind
#'
#' Returns an unnamed scalar of the column's own type, or `NA` for an unknown
#' kind -- never `integer(0)`, never an error.
#' @keywords internal
#' @noRd
kind_field <- function(kind, column) {
  i <- match(kind %||% NA_character_, lss_kinds$kind)
  if (length(i) != 1L || is.na(i)) return(NA)
  lss_kinds[[column]][[i]]
}

#' Fixed scale codes a relevance condition may cite, or NULL
#'
#' The only reader of the `implicit_codes` list column. `NULL` -- not
#' `character(0)` -- for kinds without an implicit scale, so the `%||%`
#' fallback to the declared option codes still fires.
#' @keywords internal
#' @noRd
kind_implicit_codes <- function(kind) {
  codes <- kind_field(kind, "implicit_codes")
  if (is.character(codes) && length(codes)) codes else NULL
}

#' Kinds whose `column` matches `value`, in table order
#' @keywords internal
#' @noRd
kinds_where <- function(column, value = TRUE) {
  lss_kinds$kind[lss_kinds[[column]] %in% value]
}

# ---- languages and localized texts ------------------------------------------

#' Resolve the declared languages from `languages` / `language`
#'
#' `language` is the original single-language argument, kept as an alias:
#' `languages[1]` is the primary language: the survey's base language in
#' the file `write_lss()` emits.
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
    return(language %||% lss_spec_defaults$language)
  }
  primary <- languages[[lss_spec_defaults$primary_language]]
  if (!is.null(language) && !identical(language, primary)) {
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
  primary <- languages[[lss_spec_defaults$primary_language]]
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
        "i" = "{.code languages[1]} is the survey base language {.fn write_lss} emits."),
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
    g$description <- spec_localize(g$description, langs,
                                   paste0("description of group ", gi))
    g$questions <- lapply(g$questions, function(q) {
      label <- paste0("question ", dQuote(esc(q$code %||% ""), FALSE))
      q$mandatory <- isTRUE(q$mandatory %||% lss_spec_defaults$mandatory)
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
    qu$limit <- normalize_quota_limit(qu$limit, k)
    qu
  })

  spec
}

#' Normalize the optional `limit` of a quota
#'
#' `NULL` -- not `0` -- when the author leaves it out, so `write_lss()` keeps
#' emitting the historical `qlimit = 0` and the form template can tell a
#' declared zero from an absent field. Anything else must be a single whole
#' number at or above zero: LimeSurvey stores `qlimit` as an unsigned
#' integer and silently clamps what it cannot read.
#' @keywords internal
#' @noRd
normalize_quota_limit <- function(limit, k) {
  if (is.null(limit)) return(NULL)
  n <- suppressWarnings(as.integer(limit))
  if (length(limit) != 1L || is.na(n) || n < 0L ||
      (is.numeric(limit) && limit != trunc(limit))) {
    lssdoc_abort(
      paste0("The limit of quota ", k,
             " must be a single whole number at or above zero."),
      class = "lssdoc_bad_spec"
    )
  }
  n
}

normalize_options <- function(options, languages, field) {
  n <- lss_spec_defaults$option_code_from - 1L
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
    o$other <- isTRUE(o$other %||% lss_spec_defaults$other)
    o$exclusive <- isTRUE(o$exclusive %||% lss_spec_defaults$exclusive)
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

# The question code and, when the caller knows it, the spec field the refusal
# is about travel as CONDITION FIELDS, not only inside the message. A caller
# that has to say where the problem is -- `read_form_docx()` naming the block
# and the form label of the field -- reads `spec_code` and `spec_field`
# instead of matching the code against the message, where `Q1` would steal an
# error about `Q10`. The message and the classes are unchanged.
spec_abort <- function(code, ..., field = NA_character_) {
  lssdoc_abort(
    c(paste0("Invalid specification for question {.val ", esc(code), "}."), ...),
    class = "lssdoc_bad_spec",
    spec_code = paste(as.character(code), collapse = " "),
    spec_field = field,
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

      if (!is_kind(q$kind)) {
        spec_abort(code, "x" = paste0(
          "Unknown kind {.val ", q$kind %||% "", "}: use one of ",
          paste0('"', lss_kinds$kind, '"', collapse = ", "), "."),
          field = "kind")
      }
      q_text <- loc_text(q$text)
      if (!is.character(q_text) || length(q_text) != 1L || !nzchar(q_text)) {
        spec_abort(code, "x" = "The question {.field text} is empty.", field = "text")
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
    if (!isTRUE(kind_field(target$kind, "quota_target"))) {
      lssdoc_abort(
        paste0("Quota on {.val ", esc(quota$question),
               "}: a quota needs a question with a single coded answer (",
               paste(kinds_where("quota_target"), collapse = ", "),
               "), and {.val ", esc(quota$question), "} is {.val ",
               target$kind, "}."),
        class = "lssdoc_bad_spec"
      )
    }
    # a fixed scale (yesno, gender, fivepoint) declares no option: its codes
    # are the kind's own, exactly as a relevance condition reads them
    codes <- kind_implicit_codes(target$kind) %||% option_codes(target$options)
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
  shape <- kind_row(q$kind)
  minimum <- shape$min_options
  if (!is.na(minimum) && length(q[["options"]] %||% list()) < minimum) {
    spec_abort(q$code, "x" = paste0(
      "{.val ", q$kind, "} needs at least ", minimum, " {.field options}."),
      field = "options")
  }
  if (shape$rows == "required" && shape$columns == "required") {
    if (!length(q[["rows"]] %||% list()) || !length(q[["columns"]] %||% list())) {
      spec_abort(q$code, "x" = "An array needs non-empty {.field rows} and {.field columns}.",
                 field = "rows")
    }
  }
  if (shape$rows == "required" && shape$columns == "forbidden") {
    if (!length(q[["rows"]] %||% list())) {
      spec_abort(q$code, "x" = paste0("{.val ", q$kind, "} needs non-empty {.field rows}."),
                 field = "rows")
    }
    if (length(q[["columns"]] %||% list())) {
      spec_abort(q$code, "x" = paste0(
        "{.val ", q$kind, "} carries an implicit scale: {.field columns} must stay empty."),
        field = "columns")
    }
  }
  if (shape$options == "forbidden" &&
      (length(q[["options"]] %||% list()) || length(q[["rows"]] %||% list()))) {
    spec_abort(q$code, "x" = paste0("{.val ", q$kind, "} questions carry no options."),
               field = "options")
  }
}

#' How long a code of one spec field may be, and why
#'
#' LimeSurvey does not store every item list in the same table, and the two
#' tables do not have the same column width: an ANSWER lives in
#' `answers.code`, a `varchar(5)`, while a SUBQUESTION lives in
#' `questions.title`, a `varchar(20)` -- the same column as a question code.
#' The limit therefore follows the STORAGE the kind routes the field to
#' (`subquestions_from` / `answers_from` in `lss_kinds`), never the field's
#' name: `array` rows are subquestions and take 20 characters, its columns
#' are answers and take 5. Real exports use the whole width (a row code
#' `STRESS` in `inst/extdata/demo_survey.lss`) and also use purely numeric
#' codes, so the character class is letters and digits, with no leading-letter
#' rule: the package must never refuse what LimeSurvey itself wrote.
#' A field the kind emits to neither table is capped at the narrower width.
#' @keywords internal
#' @noRd
option_code_width <- function(kind, field) {
  if (identical(field, kind_field(kind, "subquestions_from"))) 20L else 5L
}

validate_options <- function(q) {
  for (field in c("options", "rows", "columns")) {
    opts <- q[[field]]
    if (is.null(opts)) next
    codes <- option_codes(opts)
    if (anyDuplicated(codes)) {
      spec_abort(q$code, "x" = paste0("Duplicate option codes in {.field ", field, "}."),
                 field = field)
    }
    width <- option_code_width(q$kind, field)
    bad <- codes[!grepl(sprintf("^[A-Za-z0-9]{1,%d}$", width), codes)]
    if (length(bad)) {
      store <- if (width == 20L) {
        "LimeSurvey stores this list as subquestions, in 20 characters"
      } else {
        "LimeSurvey stores this list as answers, in 5 characters"
      }
      spec_abort(q$code, "x" = paste0(
        "Invalid option code {.val ", esc(bad[1L]),
        "} in {.field ", field,
        "}: 1-", width, " letters or digits (", store, ")."),
        field = field)
    }
    empty <- vapply(opts, function(o) {
      txt <- loc_text(o$text)
      !is.character(txt) || length(txt) != 1L || !nzchar(trimws(txt))
    }, logical(1))
    if (any(empty)) {
      spec_abort(q$code, "x" = paste0("Empty option text in {.field ", field, "}."),
                 field = field)
    }
    if (field != "options") {
      if (any(vapply(opts, function(o) isTRUE(o$other), logical(1)))) {
        spec_abort(q$code, "x" = "Array rows and columns cannot carry an {.field other} option.",
                   field = field)
      }
    }
  }
  # exclusive is a multiple-choice mechanism (exclude_all_others)
  if (!isTRUE(kind_field(q$kind, "exclusive_allowed")) &&
      any(vapply(q[["options"]] %||% list(), function(o) isTRUE(o$exclusive), logical(1)))) {
    spec_abort(q$code, "x" = "{.field exclusive} options only exist on {.val multiple} questions.",
               field = "exclusive")
  }
}

validate_other <- function(q) {
  others <- sum(vapply(q[["options"]] %||% list(), function(o) isTRUE(o$other), logical(1)))
  if (any(vapply(q[["options"]] %||% list(),
                 function(o) isTRUE(o$other) && isTRUE(o$exclusive), logical(1)))) {
    spec_abort(q$code,
      "x" = "The {.field other} option cannot be {.field exclusive}: LimeSurvey's exclusion mechanism only addresses coded options.",
      field = "exclusive")
  }
  if (others > 1L) {
    spec_abort(q$code, "x" = "At most one option can be {.field other}.",
               field = "options")
  }
  if (others == 1L && !isTRUE(kind_field(q$kind, "other_allowed"))) {
    # the list of kinds comes from the table, so it cannot drift from it
    allowed <- paste0("{.val ", kinds_where("other_allowed"), "}")
    n_allowed <- length(allowed)
    allowed <- if (n_allowed > 1L) {
      paste0(paste(allowed[-n_allowed], collapse = ", "), " and ", allowed[n_allowed])
    } else {
      allowed
    }
    spec_abort(q$code,
      "x" = paste0("The native {.field other} option only exists on ",
                   allowed, " questions."),
      "i" = "For a ranking, add it as a regular rankable item without a free-text field.",
      field = "options")
  }
  pos <- q$other_position
  if (!is.null(pos)) {
    if (others == 0L) {
      spec_abort(q$code, "x" = "{.field other_position} set but no {.field other} option.",
                 field = "other_position")
    }
    if (!pos %in% c("beginning", "end", "specific")) {
      spec_abort(q$code, "x" = '{.field other_position} must be "beginning", "end" or "specific".',
                 field = "other_position")
    }
    if (pos == "specific") {
      after <- as.character(q$other_position_code %||% "")
      if (!after %in% option_codes(q[["options"]])) {
        spec_abort(q$code,
          "x" = paste0("{.field other_position_code} {.val ", esc(after),
                       "} is not an option code of this question."),
          field = "other_position")
      }
    }
  }
}

validate_caps <- function(q) {
  cap <- q$max_answers
  if (is.null(cap)) return(invisible())
  rule <- kind_field(q$kind, "max_answers_rule")
  if (identical(rule, "none")) {
    spec_abort(q$code, "x" = "{.field max_answers} only applies to {.val multiple} and {.val ranking}.",
               field = "max_answers")
  }
  cap <- suppressWarnings(as.integer(cap))
  n <- length(option_codes(q[["options"]]))
  if (is.na(cap) || cap < 1L) {
    spec_abort(q$code, "x" = "{.field max_answers} must be a positive integer.",
               field = "max_answers")
  }
  if (identical(rule, "below_n") && cap >= n) {
    spec_abort(q$code, "x" = paste0("{.field max_answers} (", cap,
                                    ") must be below the number of options."),
               field = "max_answers")
  }
  if (identical(rule, "at_most_n") && cap > n) {
    spec_abort(q$code, "x" = paste0("{.field max_answers} (", cap,
                                    ") exceeds the number of rankable items."),
               field = "max_answers")
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
  ref_error <- function(...) spec_abort(code, ..., field = "relevance")

  check_ref <- function(var, values, count = FALSE) {
    target <- defined[[var]]
    if (is.null(target)) {
      ref_error("x" = paste0(
        "{.field relevance} cites {.val ", esc(var),
        "}, which is not defined earlier in the survey."))
    }
    role <- kind_field(target$kind, "relevance_role")
    if (count && !identical(role, "count")) {
      ref_error("x" = paste0("count() requires a {.val multiple} question, and {.val ",
                             esc(var), "} is {.val ", target$kind, "}."))
    }
    if (!count && !identical(role, "scalar")) {
      ref_error("x" = paste0(
        "{.field relevance} with = or in requires a single-valued question, and {.val ",
        esc(var), "} is {.val ", target$kind,
        "}. For a multiple-choice target, use count()."))
    }
    codes <- kind_implicit_codes(target$kind) %||% option_codes(target$options)
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
    "i" = "Use code = 1, code in [1, 2, autre] or count(code) >= 2.",
    field = "relevance")
}

`%||%` <- function(x, y) if (is.null(x)) y else x
