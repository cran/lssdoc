#' Read a LimeSurvey `.lss` file
#'
#' Read a LimeSurvey survey structure export (`.lss`, an XML file) and turn
#' it into a structured `lss` object that the rest of the package can
#' audit ([audit_lss()]) and render ([render_questionnaire()],
#' [render_audit()]). Parsing is fully local: the file is never uploaded
#' anywhere.
#'
#' @param file Character. Path to a `.lss` file. Must be a single string
#'   pointing to an existing file, otherwise a classed error is raised
#'   (`lssdoc_bad_path`, `lssdoc_file_not_found`).
#'
#' @return An object of class `lss`: a list with the survey languages,
#'   metadata, and one data frame per `.lss` section. Structural sections
#'   (`surveys`, `groups`, `questions`, `subquestions`, `answers`,
#'   `question_attributes`, `conditions`) stay separate from the
#'   localized text sections (`survey_language_settings`, `group_l10ns`,
#'   `question_l10ns`, `answer_l10ns`), which carry the per-language
#'   titles, labels, and help texts. All values are read verbatim as
#'   character.
#'
#' @details
#' The `.lss` format is a LimeSurvey XML export. Since DBVersion 4xx/7xx
#' the translatable text lives in dedicated localization sections
#' (`*_l10ns`), keyed by language, while the structural sections hold
#' identifiers and settings. `read_lss()` reads every section into a tidy
#' data frame without mutating any user-facing identifier or text. A
#' field that is present but empty (e.g. `<help/>`) is read as `""`; a
#' field that is absent from a row is read as `NA`.
#'
#' @examples
#' # A synthetic four-language demo survey ships with the package.
#' demo <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
#' lss <- read_lss(demo)
#' lss$languages
#' @export
read_lss <- function(file) {
  if (!is.character(file) || length(file) != 1L) {
    lssdoc_abort(
      "{.arg file} must be a single file path.",
      class = "lssdoc_bad_path"
    )
  }
  if (!file.exists(file)) {
    lssdoc_abort(
      "Cannot find a file at {.path {file}}.",
      class = "lssdoc_file_not_found"
    )
  }

  # Pre-validate that the file begins with an XML tag before handing it to
  # libxml2. On some platforms (recent libxml2 builds) `read_xml()` aborts
  # the R process with an uncatchable C++ exception when given non-XML
  # input, so the `tryCatch()` below cannot guard against it. A well-formed
  # `.lss` starts with an XML declaration or a root tag, i.e. its first
  # non-whitespace byte is `<` (after an optional UTF-8/UTF-16 BOM).
  bytes <- as.integer(readBin(file, what = "raw", n = 1024L))
  if (length(bytes) >= 3L &&
      bytes[1L] == 0xEF && bytes[2L] == 0xBB && bytes[3L] == 0xBF) {
    bytes <- bytes[-(1:3)]                              # UTF-8 BOM
  } else if (length(bytes) >= 2L &&
             ((bytes[1L] == 0xFF && bytes[2L] == 0xFE) ||
              (bytes[1L] == 0xFE && bytes[2L] == 0xFF))) {
    bytes <- bytes[-(1:2)]                              # UTF-16 BOM
  }
  non_ws <- which(!(bytes %in% c(0x20, 0x09, 0x0D, 0x0A, 0x00)))
  first_byte <- if (length(non_ws)) bytes[non_ws[1L]] else NA_integer_
  if (is.na(first_byte) || first_byte != 0x3C) {        # 0x3C == "<"
    lssdoc_abort(
      c(
        "{.path {file}} is not valid XML.",
        "x" = "The file does not start with an XML tag."
      ),
      class = "lssdoc_invalid_xml"
    )
  }

  doc <- tryCatch(
    xml2::read_xml(file),
    error = function(e) {
      lssdoc_abort(
        c(
          "{.path {file}} is not valid XML.",
          "x" = conditionMessage(e)
        ),
        class = "lssdoc_invalid_xml"
      )
    }
  )

  doc_type <- lss_scalar(doc, "LimeSurveyDocType")
  if (is.na(doc_type) || doc_type != "Survey") {
    lssdoc_abort(
      c(
        "{.path {file}} does not look like a LimeSurvey survey export.",
        "i" = "Expected {.field LimeSurveyDocType} {.val Survey}, but found
               {.val {doc_type}}."
      ),
      class = "lssdoc_not_a_survey"
    )
  }

  db_version <- lss_scalar(doc, "DBVersion")
  lss_check_db_version(db_version, file)

  languages <- xml2::xml_text(
    xml2::xml_find_all(doc, "/document/languages/language")
  )
  surveys <- lss_section(doc, "surveys")
  base_language <- if (!is.null(surveys) && "language" %in% names(surveys)) {
    surveys$language[1]
  } else {
    NA_character_
  }

  structure(
    list(
      file = file,
      db_version = db_version,
      doc_type = doc_type,
      languages = languages,
      base_language = base_language,
      surveys = surveys,
      survey_language_settings = lss_section(doc, "surveys_languagesettings"),
      groups = lss_section(doc, "groups"),
      group_l10ns = lss_section(doc, "group_l10ns"),
      questions = lss_section(doc, "questions"),
      question_l10ns = lss_section(doc, "question_l10ns"),
      subquestions = lss_section(doc, "subquestions"),
      answers = lss_section(doc, "answers"),
      answer_l10ns = lss_section(doc, "answer_l10ns"),
      question_attributes = lss_section(doc, "question_attributes"),
      conditions = lss_section(doc, "conditions"),
      quotas = lss_section(doc, "quota"),
      quota_members = lss_section(doc, "quota_members"),
      quota_languagesettings = lss_section(doc, "quota_languagesettings")
    ),
    class = "lss"
  )
}

#' Validate the `.lss` DBVersion against the supported window
#'
#' The package was designed around DBVersion 4xx-7xx: the family of
#' schemas that splits structural and localized content into
#' `*_l10ns` sections. Older versions (< 400) use a flat schema and
#' parsing silently produces an `lss` object with missing translations.
#' Future versions (>= 800) have not been validated -- we warn but let
#' the call proceed so users can give feedback when a newer schema
#' lands.
#'
#' @keywords internal
#' @noRd
lss_check_db_version <- function(db_version, file) {
  v <- suppressWarnings(as.integer(db_version))
  if (is.na(v)) {
    # No DBVersion element or non-integer: very old or hand-crafted.
    lssdoc_warn(
      c(
        "Could not read {.field DBVersion} from {.path {file}}.",
        "i" = "lssdoc was designed for DBVersion 400-799. Parsing may produce incomplete results."
      ),
      class = "lssdoc_unknown_db_version"
    )
    return(invisible())
  }
  if (v < 400L) {
    lssdoc_abort(
      c(
        "{.path {file}} uses {.field DBVersion} {.val {db_version}}, which predates the {.code *_l10ns} schema.",
        "i" = "lssdoc supports DBVersion 400 and later (LimeSurvey 3.x and newer).",
        "i" = "Re-export the survey from a recent LimeSurvey installation, then retry."
      ),
      class = "lssdoc_unsupported_db_version"
    )
  }
  if (v >= 800L) {
    lssdoc_warn(
      c(
        "{.path {file}} uses {.field DBVersion} {.val {db_version}}, which is newer than the versions lssdoc has been validated against (400-799).",
        "i" = "Parsing will continue but please report any incorrect rendering at {.url https://github.com/amaltawfik/lssdoc/issues}."
      ),
      class = "lssdoc_untested_db_version"
    )
  }
  invisible()
}

#' Resolve a polymorphic input (path or `lss` object) to an `lss` object
#'
#' Shared validator used by every user-facing function that accepts both
#' a `.lss` path and a pre-parsed `lss` object. Centralizes the dispatch
#' and the error message.
#'
#' @keywords internal
#' @noRd
lss_resolve_input <- function(input, arg = "input") {
  if (inherits(input, "lss")) return(input)
  if (is.character(input) && length(input) == 1L && !is.na(input)) {
    return(read_lss(input))
  }
  lssdoc_abort(
    c(
      "{.arg {arg}} must be a path to a {.file .lss} file or an {.cls lss} object from {.fn read_lss}.",
      "x" = "Got {.cls {class(input)[1]}}."
    ),
    class = "lssdoc_bad_input"
  )
}

#' Read a scalar element directly under `<document>`
#'
#' @return A length-one character, or `NA_character_` if the element is
#'   absent.
#' @keywords internal
#' @noRd
lss_scalar <- function(doc, name) {
  node <- xml2::xml_find_first(doc, paste0("/document/", name))
  if (inherits(node, "xml_missing")) {
    return(NA_character_)
  }
  xml2::xml_text(node)
}

#' Read one `<fields>`/`<rows>` section into a data frame
#'
#' Returns a data frame with one column per declared `<fieldname>` and one
#' row per `<row>`, all character. A present-but-empty element reads as `""`;
#' an element absent from a row reads as `NA`. Returns `NULL` if the section
#' is missing, and a zero-row data frame (with columns) if it declares
#' fields but has no rows.
#'
#' @keywords internal
#' @noRd
lss_section <- function(doc, name) {
  node <- xml2::xml_find_first(doc, paste0("/document/", name))
  if (inherits(node, "xml_missing")) {
    return(NULL)
  }

  fields <- xml2::xml_text(xml2::xml_find_all(node, "./fields/fieldname"))
  if (length(fields) == 0) {
    return(NULL)
  }

  rows <- xml2::xml_find_all(node, "./rows/row")
  cols <- lapply(fields, function(field) {
    if (length(rows) == 0) {
      return(character(0))
    }
    vapply(
      rows,
      function(row) {
        cell <- xml2::xml_find_first(row, paste0("./", field))
        if (inherits(cell, "xml_missing")) {
          NA_character_
        } else {
          xml2::xml_text(cell)
        }
      },
      character(1)
    )
  })

  names(cols) <- fields
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

#' @export
print.lss <- function(x, ...) {
  cli::cli_h1("LimeSurvey structure (lss)")
  cli::cli_text("{.field File}: {.path {x$file}}")
  cli::cli_text("{.field Languages}: {.val {x$languages}}")
  if (!is.na(x$base_language)) {
    cli::cli_text("{.field Base language}: {.val {x$base_language}}")
  }
  n <- function(df) if (is.null(df)) 0L else nrow(df)
  counts <- c(
    Groups = n(x$groups),
    Questions = n(x$questions),
    Subquestions = n(x$subquestions),
    Answers = n(x$answers)
  )
  cli::cli_text(
    "{.field Counts}: ",
    paste(names(counts), counts, sep = " ", collapse = ", ")
  )
  invisible(x)
}
