#' LimeSurvey question-type taxonomy
#'
#' Internal reference table mapping LimeSurvey single-letter question type
#' codes to a human-readable label and structural flags. The flags drive how
#' a question is laid out (which of answers, subquestions, and scales it
#' uses) and let [audit_lss()] check that a question carries the parts its
#' type requires.
#'
#' Columns:
#' * `code` -- the LimeSurvey type code as stored in `questions$type`.
#' * `label` -- a descriptive English label.
#' * `family` -- one of `"text"`, `"list"`, `"multiple"`, `"array"`,
#'   `"mask"`, `"display"`, `"other"`.
#' * `has_answers` -- the type uses predefined answer options (the `answers`
#'   section).
#' * `has_subquestions` -- the type uses subquestions (the `subquestions`
#'   section).
#' * `has_scales` -- the type uses two answer scales (`scale_id` 0 and 1).
#' * `display_only` -- the type renders text or computation only and
#'   collects no response.
#'
#' @return A data frame, one row per known type.
#' @keywords internal
#' @noRd
lss_question_types <- function() {
  rows <- list(
    c("S", "Short free text", "text", FALSE, FALSE, FALSE, FALSE),
    c("T", "Long free text", "text", FALSE, FALSE, FALSE, FALSE),
    c("U", "Huge free text", "text", FALSE, FALSE, FALSE, FALSE),
    c("Q", "Multiple short text", "text", FALSE, TRUE, FALSE, FALSE),
    c("N", "Numerical input", "mask", FALSE, FALSE, FALSE, FALSE),
    c("K", "Multiple numerical input", "mask", FALSE, TRUE, FALSE, FALSE),
    c("D", "Date/time", "mask", FALSE, FALSE, FALSE, FALSE),
    c("|", "File upload", "mask", FALSE, FALSE, FALSE, FALSE),
    c("L", "List (radio)", "list", TRUE, FALSE, FALSE, FALSE),
    c("!", "List (dropdown)", "list", TRUE, FALSE, FALSE, FALSE),
    c("O", "List with comment", "list", TRUE, FALSE, FALSE, FALSE),
    c("M", "Multiple choice", "multiple", FALSE, TRUE, FALSE, FALSE),
    c("P", "Multiple choice with comments", "multiple", FALSE, TRUE, FALSE, FALSE),
    c("F", "Array", "array", TRUE, TRUE, FALSE, FALSE),
    c("A", "Array (5 point choice)", "array", FALSE, TRUE, FALSE, FALSE),
    c("B", "Array (10 point choice)", "array", FALSE, TRUE, FALSE, FALSE),
    c("C", "Array (yes/no/uncertain)", "array", FALSE, TRUE, FALSE, FALSE),
    c("E", "Array (increase/same/decrease)", "array", FALSE, TRUE, FALSE, FALSE),
    c("H", "Array by column", "array", TRUE, TRUE, FALSE, FALSE),
    c("1", "Array (dual scale)", "array", TRUE, TRUE, TRUE, FALSE),
    c(":", "Array (numbers)", "array", FALSE, TRUE, FALSE, FALSE),
    c(";", "Array (texts)", "array", FALSE, TRUE, FALSE, FALSE),
    c("R", "Ranking", "list", TRUE, FALSE, FALSE, FALSE),
    c("G", "Gender", "list", FALSE, FALSE, FALSE, FALSE),
    c("Y", "Yes/No", "list", FALSE, FALSE, FALSE, FALSE),
    c("5", "5 point choice", "list", FALSE, FALSE, FALSE, FALSE),
    c("*", "Equation", "display", FALSE, FALSE, FALSE, TRUE),
    c("X", "Text display", "display", FALSE, FALSE, FALSE, TRUE),
    c("I", "Language switch", "other", FALSE, FALSE, FALSE, TRUE)
  )

  df <- as.data.frame(
    do.call(rbind, rows),
    stringsAsFactors = FALSE
  )
  names(df) <- c(
    "code", "label", "family",
    "has_answers", "has_subquestions", "has_scales", "display_only"
  )
  for (col in c("has_answers", "has_subquestions", "has_scales", "display_only")) {
    df[[col]] <- as.logical(df[[col]])
  }
  df
}

#' Look up the descriptive label for a question type code
#'
#' Vectorized over `code`. Unknown codes fall back to a clear placeholder so
#' the renderer never drops a question silently.
#'
#' @param code Character vector of LimeSurvey type codes.
#' @return Character vector of labels, the same length as `code`.
#' @keywords internal
#' @noRd
lss_type_label <- function(code) {
  types <- lss_question_types()
  idx <- match(code, types$code)
  label <- types$label[idx]
  unknown <- is.na(idx)
  label[unknown] <- paste0("Unknown type (", code[unknown], ")")
  label
}

#' Core LimeSurvey question theme names
#'
#' Modern LimeSurvey identifies a question by `question_theme_name` in
#' addition to the legacy `type` code. Plugin or extension question types
#' ship their own theme name with no legacy code of their own; this table
#' gives friendly labels for the core themes so `lss_question_label()` can
#' fall back to the theme when the legacy code is unknown.
#'
#' @return A named character vector: theme name -> label.
#' @keywords internal
#' @noRd
lss_core_themes <- function() {
  c(
    shortfreetext = "Short free text",
    longfreetext = "Long free text",
    hugefreetext = "Huge free text",
    multipleshorttext = "Multiple short text",
    numerical = "Numerical input",
    multiplenumeric = "Multiple numerical input",
    date = "Date/time",
    upload_files = "File upload",
    listradio = "List (radio)",
    list_dropdown = "List (dropdown)",
    list_with_comment = "List with comment",
    multiplechoice = "Multiple choice",
    multiplechoice_with_comments = "Multiple choice with comments",
    "arrays/array" = "Array",
    "arrays/5point" = "Array (5 point choice)",
    "arrays/10point" = "Array (10 point choice)",
    "arrays/yesnouncertain" = "Array (yes/no/uncertain)",
    "arrays/increasesamedecrease" = "Array (increase/same/decrease)",
    "arrays/column" = "Array by column",
    "arrays/dualscale" = "Array (dual scale)",
    "arrays/array_numbers" = "Array (numbers)",
    "arrays/array_texts" = "Array (texts)",
    ranking = "Ranking",
    gender = "Gender",
    yesno = "Yes/No",
    fivepointchoice = "5 point choice",
    equation = "Equation",
    boilerplate = "Text display",
    language = "Language switch"
  )
}

#' Best display label for a question, combining type code and theme name
#'
#' Prefers the legacy type-code label (always present and authoritative for
#' core types). When the code is unknown -- typically a plugin or extension
#' question type -- it falls back to the theme-name label, and finally to a
#' clear placeholder that still names both, so a question is never dropped or
#' mislabeled silently. Vectorized over `type` and `theme_name`.
#'
#' @param type Character vector of legacy type codes.
#' @param theme_name Character vector of `question_theme_name` values, the
#'   same length as `type` (or `NULL`).
#' @return Character vector of labels.
#' @keywords internal
#' @noRd
lss_question_label <- function(type, theme_name = NULL) {
  if (is.null(theme_name)) {
    theme_name <- rep(NA_character_, length(type))
  }
  types <- lss_question_types()
  themes <- lss_core_themes()

  code_label <- types$label[match(type, types$code)]
  theme_label <- unname(themes[theme_name])

  out <- code_label
  use_theme <- is.na(out) & !is.na(theme_label)
  out[use_theme] <- theme_label[use_theme]

  unknown <- is.na(out)
  out[unknown] <- vapply(
    which(unknown),
    function(i) {
      parts <- c(type[i], theme_name[i])
      parts <- parts[!is.na(parts) & nzchar(parts)]
      if (length(parts) == 0) {
        "Unknown type"
      } else {
        paste0("Unknown type (", paste(parts, collapse = " / "), ")")
      }
    },
    character(1)
  )
  out
}

#' Methodological label for a question type (MOSAiCH-style)
#'
#' Maps the LimeSurvey type to one of the high-level response categories
#' used in survey-methodology publications (ESS, MOSAiCH, panel surveys,
#' OECD): Single choice, Multiple choice, Text, Number, Date, Ranking,
#' File upload, Computed, Display. The aim is a label a reviewer can
#' read without knowing LimeSurvey:
#'
#' * The UI distinction "List (radio)" vs "List (dropdown)" disappears
#'   into "Single choice" -- the response semantics are identical and
#'   the UI control is an implementation detail.
#' * Predefined types (Yes/No, Gender, 5-point) collapse into "Single
#'   choice"; the actual value codes (Y/N, M/F, 1..5) appear in the
#'   Value section of the item table, which is the right place for
#'   the response domain.
#' * Structural fan-out ("Multiple numerical input" -> several Number
#'   subquestions, "Array" -> several Single choice subquestions) is
#'   conveyed by the `parent_subq` variable code and the per-subq
#'   blocks, not by a parenthetical on the Type cell.
#'
#' Vectorized over `type`. Falls back to the legacy label when the type
#' is unknown so a question is never dropped silently.
#'
#' @param type Character vector of LimeSurvey legacy type codes.
#' @param theme_name Character vector of `question_theme_name` values
#'   (same length as `type`, or `NULL`). Used as a fallback when the
#'   legacy code is unknown.
#' @return Character vector of methodological labels.
#' @keywords internal
#' @noRd
lss_methodological_label <- function(type, theme_name = NULL) {
  if (is.null(theme_name)) {
    theme_name <- rep(NA_character_, length(type))
  }
  map <- function(code) {
    # `switch()` has a formal parameter named `EXPR`; passing the
    # type code "E" as a case label would trigger an R CMD check
    # partial-argument-match note. Naming `EXPR =` explicitly tells
    # R that the first argument is the value to switch on and that
    # every named pair after it is a case label.
    switch(
      EXPR = as.character(code),
      # Single-choice family (all collapse to one category; the response
      # domain -- Y/N, M/F, 1-5, enumerated codes -- is in the Value
      # section).
      "L" = "Single choice", "!" = "Single choice", "Y" = "Single choice",
      "G" = "Single choice", "5" = "Single choice",
      "F" = "Single choice", "1" = "Single choice",
      "A" = "Single choice", "B" = "Single choice",
      "C" = "Single choice", "E" = "Single choice",
      "H" = "Single choice", ":" = "Single choice",
      "O" = "Single choice with comment",
      # Multiple-choice family.
      "M" = "Multiple choice", "P" = "Multiple choice with comment",
      # Text variants. S (short free text) is single-line, so it keeps
      # its own "(short)" label. T (long) and U (huge) free text are both
      # multi-line and differ only in textarea size, so they collapse to
      # "Text (long)" -- same response semantics, same data, like
      # List (radio) / (dropdown) -> Single choice.
      "S" = "Text (short)", "T" = "Text (long)", "U" = "Text (long)",
      "Q" = "Text",
      # Numeric (single or multi -- both yield numeric variables).
      "N" = "Number", "K" = "Number",
      ";" = "Text",
      # Special-purpose types.
      "D" = "Date",
      "R" = "Ranking",
      "|" = "File upload",
      "*" = "Computed",
      "X" = "Display",
      "I" = "Display",
      NA_character_
    )
  }
  out <- vapply(type, map, character(1L), USE.NAMES = FALSE)
  # Fall back to the legacy/theme label when the code is unknown so a
  # plugin question type still gets a sensible name.
  unknown <- is.na(out)
  if (any(unknown)) {
    out[unknown] <- lss_question_label(type[unknown], theme_name[unknown])
  }
  out
}

#' Return the taxonomy row for a single type code, or a default row
#'
#' @param code A length-one type code.
#' @return A one-row data frame. For an unknown code, flags default to
#'   `FALSE` and `family` is `"other"`.
#' @keywords internal
#' @noRd
lss_type_info <- function(code) {
  types <- lss_question_types()
  idx <- match(code, types$code)
  if (is.na(idx)) {
    return(data.frame(
      code = code,
      label = paste0("Unknown type (", code, ")"),
      family = "other",
      has_answers = FALSE,
      has_subquestions = FALSE,
      has_scales = FALSE,
      display_only = FALSE,
      stringsAsFactors = FALSE
    ))
  }
  types[idx, , drop = FALSE]
}
