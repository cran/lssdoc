# The Word authoring form, read side: `form.docx` -> `lss_spec`.
#
# `write_form_docx()` is the other half of this contract (see the head of
# R/write_form_docx.R): one top-level two-column table per block, the key on
# the left and the value on the right, every key a localized label recognized
# by its TEXT and never by its position, two empty paragraphs between blocks,
# never a merged cell, and the contract version in the custom document
# property `lssdoc-template-version`.
#
# Constraints this file obeys:
#
# * xml2 + `utils::unzip()` only. officer and flextable stay in Suggests and
#   are needed to WRITE a form, never to read one: an author hands back a
#   `.docx` and must be able to turn it into a `.lss` on a bare installation.
# * a value cell is CONTENT and nothing else; hints live in the key cell after
#   a soft return, and only the first line of a key cell is read.
# * a line is a paragraph OR a soft return (`w:br`): flextable can only emit
#   soft returns, a Word author types either, and both must mean "next value".
# * the case of a code is never changed (`A`, `Y`, `Q01Single` must survive).
#   Only keys,
#   the closed vocabularies (kind, yes/no, quota action, other position) and
#   the keywords of the filter mini-language are matched case- and
#   accent-insensitively, because Word capitalizes the first letter of a table
#   cell by default.
# * every refusal is classed (`lssdoc_bad_form` and one leaf class) and names
#   the block, the question code and the field, so that `check_form_docx()`
#   can tabulate them and an author can find the cell.

# ---- constants ---------------------------------------------------------------

# The WordprocessingML namespace is fixed by the standard; binding it to our
# own prefix (rather than reading the document's) survives a rewrite by
# LibreOffice or any editor that renames prefixes.
FORM_NS <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

# The chrome languages, in lookup order: the document's own language wins, the
# union of the five is the fallback, so a mixed-language document still parses.
FORM_CHROME_LANGS <- c("en", "fr", "de", "es", "it")

# Fixed transliteration table, not `iconv(to = "ASCII//TRANSLIT")`: the latter
# is platform-dependent (it yields "e" on Linux and "'e" on some Windows
# builds), and a key dictionary cannot depend on the machine.
FORM_ACCENTS_FROM <- paste0(
  "\u00E0\u00E1\u00E2\u00E3\u00E4\u00E5\u00E7\u00E8\u00E9\u00EA\u00EB",
  "\u00EC\u00ED\u00EE\u00EF\u00F1\u00F2\u00F3\u00F4\u00F5\u00F6",
  "\u00F9\u00FA\u00FB\u00FC\u00FD\u00FF\u0161\u017E")
FORM_ACCENTS_TO <- "aaaaaaceeeeiiiinooooouuuuyysz"

# ---- normalization -----------------------------------------------------------

#' Tame what Word inserts into any line, key or value alike
#'
#' Word's French typography inserts U+00A0 / U+202F before `:` `;` `!` `?`, a
#' Japanese keyboard produces the fullwidth `=`, and pasted text carries
#' zero-width joiners. None of that is content: it would break `1 = Label` and
#' `Title [fr]` while looking identical on screen. Everything else -- curly
#' quotes, accents, capitals -- is left alone here, because in a VALUE it is
#' content (`[form_norm()]` is the stricter sibling, for keys and vocabularies).
#' @keywords internal
#' @noRd
form_clean_line <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("[\u200B\u200C\u200D\u2060\uFEFF]", "", x)
  x <- gsub("[\u00A0\u2007\u2009\u202F\u205F\u3000]", " ", x)
  x <- gsub("\uFF1D", "=", x, fixed = TRUE)
  x <- gsub("\uFF3B", "[", x, fixed = TRUE)
  x <- gsub("\uFF3D", "]", x, fixed = TRUE)
  trimws(x)
}

#' Fold a line to its comparable letters, KEEPING its case
#'
#' Everything [form_norm()] does except the case folding: the quotes and the
#' typographic dashes go, the accents are transliterated in both cases, the
#' spacing is collapsed and a trailing colon or period is dropped. A code
#' captured out of a folded line is therefore the code the author typed, its
#' letter case included -- which the normalized copy cannot give, and a
#' LimeSurvey code is case-sensitive.
#' @keywords internal
#' @noRd
form_fold <- function(x) {
  x <- form_clean_line(x)
  x <- gsub("[\u2018\u2019\u201A\u201B\u201C\u201D\u201E\u201F\u00AB\u00BB\u2039\u203A\"']",
            "", x)
  x <- gsub("[\u2010-\u2015\u2212]", "-", x)
  x <- chartr(paste0(FORM_ACCENTS_FROM, toupper(FORM_ACCENTS_FROM)),
              paste0(FORM_ACCENTS_TO, toupper(FORM_ACCENTS_TO)), x)
  x <- gsub("[[:space:]]+", " ", x)
  x <- trimws(x)
  # "Reponses max." and "Type :" are the same key as "Reponses max" and
  # "Type": a trailing colon or period is punctuation, not part of a name.
  # The class holds the space too, so an abbreviation followed by a colon
  # ("Reponses max. :", as Word's French typography writes it) is stripped in
  # one pass.
  x <- sub("[[:space:].:]+$", "", x)
  trimws(x)
}

#' Normalize a key or a closed-vocabulary word for comparison
#'
#' Applied to BOTH sides of every comparison -- the cell and the chrome string
#' -- so the two can only agree or disagree on their letters. Never applied to
#' a label, a code or a question wording: those keep their typography.
#' @keywords internal
#' @noRd
form_norm <- function(x) {
  tolower(form_fold(x))
}

#' Lower-case only the 2-3 letter head of a LimeSurvey language code
#'
#' `pt-BR` and `de-informal` are written that way in LimeSurvey; Word
#' capitalizes the first letter of a cell, so `Fr` must read as `fr` while
#' `pt-BR` keeps its region.
#' @keywords internal
#' @noRd
form_lang_code <- function(x) {
  x <- trimws(as.character(x))
  head <- substr(x, 1L, min(3L, nchar(x)))
  m <- regmatches(x, regexpr("^[A-Za-z]{2,3}", x))
  if (!length(m)) return(x)
  paste0(tolower(m), substring(x, nchar(m) + 1L))
}

#' Split `Label [code]` into its label and its language code
#' @keywords internal
#' @noRd
form_split_suffix <- function(label) {
  m <- regmatches(
    label,
    regexec("^(.*?)\\s*\\[\\s*([A-Za-z]{2,3}(?:-[A-Za-z0-9]+)?)\\s*\\]\\s*$",
            label))[[1L]]
  if (length(m) == 3L) {
    list(label = trimws(m[[2L]]), lang = form_lang_code(m[[3L]]))
  } else {
    list(label = label, lang = NA_character_)
  }
}

#' Escape a literal string for use inside a regular expression
#' @keywords internal
#' @noRd
form_regex_escape <- function(x) {
  gsub("([.\\\\|()\\[\\]{}^$*+?])", "\\\\\\1", x, perl = TRUE)
}

# ---- conditions --------------------------------------------------------------

#' Raise a classed reader error, naming block, question code and field
#'
#' `.envir` is the frame the message is interpolated in: every caller builds
#' its message with `paste0()` and [esc()], so a brace that came from the
#' document can never be read as cli markup.
#' @keywords internal
#' @noRd
form_abort <- function(ctx, class, message, field = NA_character_,
                       where = NULL, .envir = parent.frame()) {
  lssdoc_abort(
    message,
    class = c(class, "lssdoc_bad_form"),
    form_block = where %||% ctx$where %||% NA_character_,
    form_code = ctx$code %||% NA_character_,
    form_field = field %||% NA_character_,
    call = .envir
  )
}

#' Evaluate one parsing step, collecting its error in `check_form_docx()`
#'
#' Strict mode ([read_form_docx()]) lets the condition through: the first
#' error stops the read. Collect mode ([check_form_docx()]) records it and
#' returns `default`, so the next block is still parsed and an author sees
#' every problem in one pass.
#' @keywords internal
#' @noRd
form_try <- function(ctx, expr, default = NULL) {
  if (!isTRUE(ctx$collect)) return(expr)
  tryCatch(
    expr,
    lssdoc_bad_form = function(cnd) {
      form_record(ctx, cnd)
      default
    }
  )
}

#' Append one condition to the collector
#' @keywords internal
#' @noRd
form_record <- function(ctx, cnd, severity = "error") {
  ctx$problems[[length(ctx$problems) + 1L]] <- list(
    severity = severity,
    class = class(cnd)[[1L]],
    block = as.character(cnd$form_block %||% NA_character_),
    code = as.character(cnd$form_code %||% NA_character_),
    field = as.character(cnd$form_field %||% NA_character_),
    message = conditionMessage(cnd)
  )
  invisible(NULL)
}

# ---- 1. the file: parts and version marker -----------------------------------

#' Unzip the two parts of a form document
#'
#' `word/document.xml` carries the blocks, `docProps/custom.xml` the contract
#' version. Only these two entries are extracted, and only with
#' [utils::unzip()], so reading a form never needs officer.
#'
#' @param path Path to the `.docx` file.
#' @return A list with `path`, `document` (an `xml_document`) and `custom`
#'   (an `xml_document` or `NULL` when the part is absent).
#' @keywords internal
#' @noRd
form_docx_parts <- function(path) {
  if (!is.character(path) || length(path) != 1L || is.na(path)) {
    lssdoc_abort("{.arg path} must be a single file path.",
                 class = c("lssdoc_bad_form_file", "lssdoc_bad_form"))
  }
  bad_file <- function(detail) {
    lssdoc_abort(
      c(paste0("{.path ", esc(path), "} is not a Word document (.docx)."),
        "x" = detail,
        "i" = "Hand back the {.file .docx} the form was written to, saved from Word or LibreOffice."),
      class = c("lssdoc_bad_form_file", "lssdoc_bad_form")
    )
  }
  if (!file.exists(path)) bad_file("The file does not exist.")
  listing <- tryCatch(utils::unzip(path, list = TRUE),
                      error = function(e) NULL, warning = function(w) NULL)
  if (is.null(listing) || !nrow(listing)) bad_file("The file is not a zip archive.")
  names <- listing$Name
  if (!"word/document.xml" %in% names) {
    bad_file("The archive has no {.file word/document.xml} entry.")
  }
  dir <- tempfile("lssdoc-form")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  wanted <- intersect(c("word/document.xml", "docProps/custom.xml"), names)
  utils::unzip(path, files = wanted, exdir = dir)
  document <- tryCatch(xml2::read_xml(file.path(dir, "word", "document.xml")),
                       error = function(e) NULL)
  if (is.null(document)) bad_file("Its {.file word/document.xml} entry is not readable XML.")
  custom <- NULL
  if ("docProps/custom.xml" %in% wanted) {
    custom <- tryCatch(xml2::read_xml(file.path(dir, "docProps", "custom.xml")),
                       error = function(e) NULL)
  }
  list(path = path, document = document, custom = custom)
}

#' Read one custom document property, whatever prefix the writer used
#'
#' `local-name()` sidesteps both the default namespace (xml2 would call it
#' `d1`) and any prefix LibreOffice adds when it rewrites the part.
#' @keywords internal
#' @noRd
form_custom_property <- function(custom, name) {
  if (is.null(custom)) return(NA_character_)
  node <- xml2::xml_find_first(
    custom,
    paste0("//*[local-name()='property'][@name='", name, "']"))
  if (inherits(node, "xml_missing")) return(NA_character_)
  value <- trimws(xml2::xml_text(node))
  if (!nzchar(value)) NA_character_ else value
}

#' The contract version of a form document, or a classed refusal
#'
#' The marker is the reader's proof that the document really is an lssdoc form
#' of the version this package speaks. Without it, the row labels below would
#' be matched against a document that never agreed to them.
#'
#' @param path Path to the `.docx` file (used in the message).
#' @param parts The result of [form_docx_parts()]; re-read from `path` when
#'   absent, so `form_marker(path)` works on its own.
#' @return The version, an integer.
#' @keywords internal
#' @noRd
form_marker <- function(path, parts = form_docx_parts(path)) {
  raw <- form_custom_property(parts$custom, "lssdoc-template-version")
  if (is.na(raw)) {
    lssdoc_abort(
      c(paste0("{.path ", esc(path),
               "} carries no {.field lssdoc-template-version} document property."),
        "i" = "Either it was not generated by lssdoc ({.fn lss_template_docx} / {.fn write_form_docx}),",
        "i" = "or its blocks were pasted into another document,",
        "i" = "or a tool stripped its properties: Word's Document Inspector, Google Docs, an online converter.",
        "i" = "Regenerate the form with {.fn lss_template_docx} or {.fn write_form_docx} and paste your text into it, then edit it in Word or LibreOffice."),
      class = c("lssdoc_bad_form_marker", "lssdoc_bad_form")
    )
  }
  version <- suppressWarnings(as.integer(raw))
  if (is.na(version) || !identical(version, as.integer(LSS_FORM_VERSION))) {
    lssdoc_abort(
      c(paste0("Form contract version {.val ", esc(raw),
               "} is not supported (this version of lssdoc reads version ",
               as.integer(LSS_FORM_VERSION), ")."),
        "i" = "Regenerate the form with {.fn lss_template_docx} and paste your content into it."),
      class = c("lssdoc_bad_form_marker", "lssdoc_bad_form")
    )
  }
  version
}

# ---- 2. structure: what a form block may not contain -------------------------

#' Refuse document-wide features that make a cell's text a half-truth
#'
#' Tracked changes hide deleted text in `w:delText` and show inserted text as
#' ordinary runs, so a text-only reader would parse a state that exists on
#' nobody's screen. A content control (`w:sdt`) can carry its own placeholder
#' text and its own value. Both are refused whole-document: they are one
#' command away from being fixed, and reading them half-right is worse.
#' @keywords internal
#' @noRd
form_refuse_document_features <- function(doc, ctx) {
  tracked <- xml2::xml_find_all(
    doc, "//w:ins | //w:del | //w:moveFrom | //w:moveTo", FORM_NS)
  if (length(tracked)) {
    form_abort(
      ctx, "lssdoc_bad_form_layout",
      c("The document contains tracked changes.",
        "i" = "Accept or reject every change in Word (Review > Accept > Accept All Changes), save, and read it again."),
      where = NA_character_)
  }
  sdt <- xml2::xml_find_all(doc, "//w:sdt | //w:customXml", FORM_NS)
  if (length(sdt)) {
    form_abort(
      ctx, "lssdoc_bad_form_layout",
      c("The document contains content controls, which a form block cannot hold.",
        "i" = "Copy the blocks from a fresh template ({.fn lss_template_docx}) and paste your text into their cells."),
      where = NA_character_)
  }
  invisible(NULL)
}

#' The lines of one cell, plus the automatic-numbering flag
#'
#' A line is a paragraph OR a soft return: flextable writes a multi-valued
#' cell with `w:br` (its model is one paragraph per cell), an author typing
#' Enter creates `w:p` and Shift+Enter creates `w:br`, and all three must mean
#' "next value".
#' @keywords internal
#' @noRd
form_cell_lines <- function(tc) {
  paras <- xml2::xml_find_all(tc, "w:p", FORM_NS)
  lines <- character(0)
  numbered <- FALSE
  for (p in paras) {
    num <- xml2::xml_find_first(p, "w:pPr/w:numPr/w:numId", FORM_NS)
    if (!inherits(num, "xml_missing")) {
      id <- suppressWarnings(as.integer(xml2::xml_attr(num, "w:val", FORM_NS)))
      if (is.na(id) || id != 0L) numbered <- TRUE
    }
    lines <- c(lines, form_para_lines(p))
  }
  list(lines = form_clean_line(lines), numbered = numbered)
}

#' The lines of one paragraph
#'
#' Word fragments a run at every spell-check or formatting boundary, so the
#' text of a paragraph is the concatenation of ALL its `w:t` descendants, in
#' document order, with the line-breaking and symbol elements mapped to what
#' they show.
#' @keywords internal
#' @noRd
form_para_lines <- function(p) {
  nodes <- xml2::xml_find_all(
    p,
    paste(".//w:t", ".//w:br", ".//w:cr", ".//w:tab", ".//w:sym",
          ".//w:noBreakHyphen", ".//w:softHyphen", sep = " | "),
    FORM_NS)
  if (!length(nodes)) return("")
  pieces <- vapply(nodes, function(n) {
    switch(
      xml2::xml_name(n),
      "t" = xml2::xml_text(n),
      "br" = {
        type <- xml2::xml_attr(n, "w:type", FORM_NS)
        if (is.na(type) || identical(type, "textWrapping")) "\n" else ""
      },
      "cr" = "\n",
      "tab" = " ",
      "sym" = {
        ch <- xml2::xml_attr(n, "w:char", FORM_NS)
        if (is.na(ch)) "" else intToUtf8(strtoi(ch, 16L))
      },
      "noBreakHyphen" = "-",
      "softHyphen" = "",
      ""
    )
  }, character(1))
  strsplit(paste0(pieces, collapse = ""), "\n", fixed = TRUE)[[1L]]
}

#' Check one row's shape and return it as cell lines
#'
#' Everything that would make the key/value pairing a guess in THIS row -- a
#' merged cell, a missing or extra cell -- is refused here, with the table and
#' the row named, before any text is interpreted.
#' @keywords internal
#' @noRd
form_table_row <- function(tr, n, i, where, ctx) {
  merged <- xml2::xml_find_all(
    tr,
    paste("w:tc/w:tcPr/w:gridSpan", "w:tc/w:tcPr/w:vMerge",
          "w:tc/w:tcPr/w:hMerge", sep = " | "),
    FORM_NS)
  spans <- suppressWarnings(as.integer(xml2::xml_attr(merged, "w:val", FORM_NS)))
  is_merge <- vapply(seq_along(merged), function(k) {
    nm <- xml2::xml_name(merged[[k]])
    if (identical(nm, "gridSpan")) !is.na(spans[[k]]) && spans[[k]] > 1L else TRUE
  }, logical(1))
  if (any(is_merge)) {
    form_abort(ctx, "lssdoc_bad_form_layout",
               c(paste0("Table ", n, ", row ", i, ": merged cells."),
                 "i" = "Form blocks are plain two-column tables: split the cells (Table Layout > Split Cells), or copy the block from a fresh template."),
               where = where)
  }
  cells <- xml2::xml_find_all(tr, "w:tc", FORM_NS)
  if (length(cells) != 2L) {
    n_cells <- length(cells)
    form_abort(ctx, "lssdoc_bad_form_layout",
               "Table {n}, row {i} has {n_cells} cell{?s}; a form block has exactly two (label, value).",
               where = where)
  }
  key <- form_cell_lines(cells[[1L]])
  value <- form_cell_lines(cells[[2L]])
  list(table = n, row = i,
       key_lines = key$lines, value_lines = value$lines,
       numbered = value$numbered)
}

#' Check one table's shape and return its rows as cell lines
#'
#' A form block is a plain two-column grid. A third column or a table inside a
#' cell condemns the whole table; a bad ROW is refused on its own, so in
#' `check_form_docx()` one merged cell in a table holding ten pasted blocks no
#' longer drops the other nine from the report.
#' @keywords internal
#' @noRd
form_table_rows <- function(tbl, n, ctx) {
  where <- paste0("table ", n)
  if (length(xml2::xml_find_all(tbl, ".//w:tbl", FORM_NS))) {
    form_abort(ctx, "lssdoc_bad_form_layout",
               c(paste0("Table ", n, " contains a nested table, which a form block cannot hold."),
                 "i" = "Copy the block from a fresh template and paste your text into its cells."),
               where = where)
  }
  cols <- xml2::xml_find_all(tbl, "w:tblGrid/w:gridCol", FORM_NS)
  if (length(cols) != 2L) {
    n_cols <- length(cols)
    form_abort(ctx, "lssdoc_bad_form_layout",
               "Table {n} has {n_cols} column{?s}; a form block has exactly two (label, value).",
               where = where)
  }
  rows <- xml2::xml_find_all(tbl, "w:tr", FORM_NS)
  out <- vector("list", length(rows))
  keep <- logical(length(rows))
  for (i in seq_along(rows)) {
    row <- form_try(ctx, form_table_row(rows[[i]], n, i, where, ctx))
    if (is.null(row)) next
    out[[i]] <- row
    keep[[i]] <- TRUE
  }
  out[keep]
}

# ---- 3. the chrome dictionaries ----------------------------------------------

# Built once per session: five chrome lists, the reverse maps of the block
# words and of the field labels, and the closed value vocabularies. They are
# pure functions of R/chrome_strings.R and of the kind table, so they cannot
# drift from what the writer emits.
form_dict_cache <- new.env(parent = emptyenv())

#' The five chrome lists, keyed by language
#' @keywords internal
#' @noRd
form_chrome_all <- function() {
  if (is.null(form_dict_cache$chrome)) {
    form_dict_cache$chrome <- stats::setNames(
      lapply(FORM_CHROME_LANGS, lss_chrome_strings), FORM_CHROME_LANGS)
  }
  form_dict_cache$chrome
}

#' Which fields each block kind accepts, and how each is read
#'
#' `loc` marks the fields that may carry a `[lang]` suffix; `lines` says
#' whether the value is one line (`"one"`), a text that may run over several
#' lines (`"many"`), or a list of values, one per line (`"list"`).
#' @keywords internal
#' @noRd
form_field_defs <- list(
  survey = list(
    title     = list(key = "form_title",          loc = TRUE,  lines = "one"),
    languages = list(key = "cover_languages",     loc = FALSE, lines = "one"),
    welcome   = list(key = "welcome_text_title",  loc = TRUE,  lines = "many"),
    end_text  = list(key = "end_text_title",      loc = TRUE,  lines = "many")
  ),
  group = list(
    title       = list(key = "form_title",        loc = TRUE,  lines = "one"),
    description = list(key = "description_title", loc = TRUE,  lines = "many")
  ),
  question = list(
    type           = list(key = "meta_type",           loc = FALSE, lines = "one"),
    mandatory      = list(key = "meta_mandatory",      loc = FALSE, lines = "one"),
    filter         = list(key = "meta_filter",         loc = FALSE, lines = "one"),
    wording        = list(key = "form_wording",        loc = TRUE,  lines = "many"),
    help           = list(key = "item_help",           loc = TRUE,  lines = "many"),
    options        = list(key = "item_options",        loc = TRUE,  lines = "list"),
    exclusive      = list(key = "item_exclusive",      loc = FALSE, lines = "list"),
    rows           = list(key = "form_rows",           loc = TRUE,  lines = "list"),
    columns        = list(key = "form_columns",        loc = TRUE,  lines = "list"),
    min_answers    = list(key = "form_min_answers",    loc = FALSE, lines = "one"),
    max_answers    = list(key = "form_max_answers",    loc = FALSE, lines = "one"),
    other_position = list(key = "form_other_position", loc = FALSE, lines = "one")
  ),
  quota = list(
    name      = list(key = "form_name",       loc = TRUE,  lines = "one"),
    limit     = list(key = "quota_limit",     loc = FALSE, lines = "one"),
    action    = list(key = "form_action",     loc = FALSE, lines = "one"),
    condition = list(key = "quota_condition", loc = FALSE, lines = "one"),
    message   = list(key = "form_message",    loc = TRUE,  lines = "many")
  )
)

#' Reverse map of the block title words: normalized word -> block kind
#' @keywords internal
#' @noRd
form_block_words <- function() {
  if (is.null(form_dict_cache$blocks)) {
    map <- list()
    for (lg in FORM_CHROME_LANGS) {
      chrome <- form_chrome_all()[[lg]]
      for (kind in c("survey", "group", "question", "quota")) {
        word <- form_norm(chrome[[paste0("form_block_", kind)]])
        map[[word]] <- c(map[[word]], stats::setNames(kind, lg))
      }
    }
    form_dict_cache$blocks <- map
  }
  form_dict_cache$blocks
}

#' The block kind a key cell opens, or `NA`
#' @keywords internal
#' @noRd
form_block_kind <- function(key) {
  hit <- form_block_words()[[form_norm(key)]]
  if (is.null(hit)) return(NA_character_)
  unname(hit[[1L]])
}

#' The chrome language a block title word belongs to, or `NA`
#' @keywords internal
#' @noRd
form_block_lang <- function(key) {
  hit <- form_block_words()[[form_norm(key)]]
  if (is.null(hit)) return(NA_character_)
  names(hit)[[1L]]
}

#' Reverse map of the field labels of one block kind: normalized label -> field
#'
#' Built over the five languages, the document's own first: a French author
#' filling an English template still gets their key recognized, and a
#' collision between two languages -- there is none, and a test asserts it --
#' would be resolved in favour of the document's language.
#' @keywords internal
#' @noRd
form_label_map <- function(block, lang = "en") {
  id <- paste0(block, "/", lang)
  if (is.null(form_dict_cache$labels)) form_dict_cache$labels <- list()
  if (is.null(form_dict_cache$labels[[id]])) {
    defs <- form_field_defs[[block]]
    map <- list()
    for (lg in unique(c(lang, FORM_CHROME_LANGS))) {
      chrome <- form_chrome_all()[[lg]]
      for (field in names(defs)) {
        label <- form_norm(chrome[[defs[[field]]$key]])
        if (is.null(map[[label]])) map[[label]] <- field
      }
    }
    form_dict_cache$labels[[id]] <- map
  }
  form_dict_cache$labels[[id]]
}

#' The labels of one block kind in one chrome language, for an error message
#' @keywords internal
#' @noRd
form_label_list <- function(block, chrome) {
  vapply(form_field_defs[[block]], function(d) chrome[[d$key]], character(1))
}

#' The chrome label of one field, for an error message
#' @keywords internal
#' @noRd
form_label_of <- function(block, field, chrome) {
  def <- form_field_defs[[block]][[field]]
  if (is.null(def)) field else chrome[[def$key]]
}

#' Closed vocabularies, normalized, across the five languages
#' @keywords internal
#' @noRd
form_vocabulary <- function() {
  if (is.null(form_dict_cache$vocab)) {
    pick <- function(key) {
      unname(vapply(form_chrome_all(), function(ch) form_norm(ch[[key]]),
                    character(1)))
    }
    form_dict_cache$vocab <- list(
      yes = unique(c(pick("mandatory_yes"), "yes", "y", "true", "1", "x",
                     "\u2713", "\u2714", "\u2611")),
      no = unique(c(pick("mandatory_no"), "no", "n", "false", "0")),
      # LimeSurvey's third mandatory state; `lss_spec()`'s `mandatory` is
      # logical, so it is named and refused rather than read as "yes".
      soft = "soft",
      terminate = unique(c(pick("quota_action_terminate"), "terminate", "1")),
      confirm = unique(c(pick("quota_action_confirm"), "confirm", "2")),
      pos_end = unique(c(pick("form_other_position_end"), "end")),
      pos_beginning = unique(c(pick("form_other_position_beginning"), "beginning"))
    )
  }
  form_dict_cache$vocab
}

#' Regular expressions for "After <code>", one per language plus the spec form
#' @keywords internal
#' @noRd
form_after_patterns <- function() {
  if (is.null(form_dict_cache$after)) {
    fmts <- unname(vapply(form_chrome_all(),
                          function(ch) ch$form_other_position_after_fmt,
                          character(1)))
    build <- function(f) {
      paste0("^\\s*",
             gsub("%s", "([A-Za-z0-9]{1,5})", form_regex_escape(f), fixed = TRUE),
             "\\s*$")
    }
    form_dict_cache$after <- list(
      clean = vapply(c(form_clean_line(fmts), "after %s", "specific %s",
                       "specific: %s", "specific:%s"),
                     build, character(1), USE.NAMES = FALSE),
      norm = vapply(c(form_norm(fmts), "after %s", "specific %s", "specific: %s",
                      "specific:%s"),
                    build, character(1), USE.NAMES = FALSE)
    )
  }
  form_dict_cache$after
}

#' Reverse map of the localized type labels: normalized label -> kinds
#'
#' The review templates' type labels are deliberately many-to-one (thirteen
#' kinds print "Single choice"), which is exactly why the Type cell carries the
#' kind CODE. A label is accepted only when it designates one kind; the English
#' kind labels of `lss_kinds` are one-to-one and always accepted.
#' @keywords internal
#' @noRd
form_type_labels <- function() {
  if (is.null(form_dict_cache$types)) {
    map <- list()
    add <- function(label, kind) {
      label <- form_norm(label)
      if (!nzchar(label)) return(invisible(NULL))
      map[[label]] <<- unique(c(map[[label]], kind))
      invisible(NULL)
    }
    for (i in seq_len(nrow(lss_kinds))) {
      kind <- lss_kinds$kind[[i]]
      add(lss_kinds$label[[i]], kind)
      for (lg in FORM_CHROME_LANGS) {
        add(lss_localized_type_label(
          list(type = lss_kinds$type[[i]], type_label = lss_kinds$label[[i]]),
          list(chrome = form_chrome_all()[[lg]])), kind)
      }
    }
    form_dict_cache$types <- map
  }
  form_dict_cache$types
}

#' Is this line the reserved "other" word?
#' @keywords internal
#' @noRd
form_is_other_word <- function(x) {
  form_norm(x) %in% lss_other_keywords()
}

# ---- 4. value parsers --------------------------------------------------------

#' One option / row / column line: `code = label`, a bare label, or Other
#'
#' `code = label` only when the left side is a single token: a label with a
#' verb ("Le salaire est = au marche") is a label, not a code, and a bare label
#' is auto-numbered by [lss_spec()] exactly as in R. The reserved word alone,
#' or on the left of `=`, is the native other option.
#' @keywords internal
#' @noRd
form_parse_item <- function(line, k, ctx, field_label) {
  bad <- function(detail, hint = NULL) {
    form_abort(ctx, "lssdoc_bad_form_value",
               c(paste0(ctx$where, ", field {.field ", esc(field_label),
                        "}, line ", k, ": ", detail), hint),
               field = field_label)
  }
  pos <- regexpr("=", line, fixed = TRUE)
  if (pos > 0L) {
    left <- trimws(substr(line, 1L, pos - 1L))
    right <- trimws(substring(line, pos + 1L))
    if (!grepl("[[:space:]]", left)) {
      if (form_is_other_word(left)) {
        if (!nzchar(right)) bad(paste0("empty label after {.code =}."))
        return(list(code = NA_character_, text = right, other = TRUE))
      }
      # The WIDEST code any list may carry (a subquestion code, stored in
      # `questions.title`, a varchar(20)). The reader only has to tell a code
      # apart from a label; which of the two widths this particular list is
      # held to -- 5 for a list LimeSurvey stores as answers, 20 for one it
      # stores as subquestions -- depends on the question's kind, and
      # `lss_spec()` applies it at assembly with the exact message.
      if (!grepl("^[A-Za-z0-9]{1,20}$", left)) {
        bad(paste0("invalid code {.val ", esc(left), "}."),
            c("i" = "Codes are letters and digits, at most 20 characters (at most 5 for an answer list)."))
      }
      if (!nzchar(right)) bad("empty label after {.code =}.")
      return(list(code = left, text = right, other = FALSE))
    }
  }
  if (form_is_other_word(line)) {
    return(list(code = NA_character_, text = line, other = TRUE))
  }
  list(code = NA_character_, text = line, other = FALSE)
}

#' Parse one language's lines of an option / row / column field
#' @keywords internal
#' @noRd
form_parse_items <- function(lines, ctx, field, field_label) {
  items <- lapply(seq_along(lines), function(k) {
    form_parse_item(lines[[k]], k, ctx, field_label)
  })
  codes <- vapply(items, function(i) i$code %||% NA_character_, character(1))
  dup <- which(duplicated(codes) & !is.na(codes))
  if (length(dup)) {
    first <- match(codes[[dup[[1L]]]], codes)
    form_abort(ctx, "lssdoc_bad_form_value",
               paste0(ctx$where, ", field {.field ", esc(field_label),
                      "}: code {.val ", esc(codes[[dup[[1L]]]]),
                      "} is given twice (lines ", first, " and ", dup[[1L]], ")."),
               field = field_label)
  }
  others <- which(vapply(items, function(i) isTRUE(i$other), logical(1)))
  if (length(others) > 1L) {
    form_abort(ctx, "lssdoc_bad_form_value",
               paste0(ctx$where, ", field {.field ", esc(field_label),
                      "}: at most one other option (lines ", others[[1L]],
                      " and ", others[[2L]], ")."),
               field = field_label)
  }
  if (length(others) && field %in% c("rows", "columns")) {
    form_abort(ctx, "lssdoc_bad_form_value",
               c(paste0(ctx$where, ", field {.field ", esc(field_label),
                        "}: {.val ", esc(lines[[others[[1L]]]]),
                        "} is reserved for the native other option."),
                 "i" = "Array rows and columns cannot carry one; give the line an explicit code to use it as a plain label."),
               field = field_label)
  }
  items
}

#' A whole number, or a classed refusal
#' @keywords internal
#' @noRd
form_parse_integer <- function(text, ctx, field_label, min = 1L) {
  clean <- form_clean_line(text)
  if (!grepl("^[0-9]+$", clean)) {
    form_abort(ctx, "lssdoc_bad_form_value",
               paste0(ctx$where, ", field {.field ", esc(field_label),
                      "}: expected a whole number (", min,
                      " or more), found {.val ", esc(text), "}."),
               field = field_label)
  }
  n <- as.integer(clean)
  if (n < min) {
    form_abort(ctx, "lssdoc_bad_form_value",
               paste0(ctx$where, ", field {.field ", esc(field_label),
                      "}: expected a whole number (", min,
                      " or more), found {.val ", esc(text), "}."),
               field = field_label)
  }
  n
}

#' Canonicalize the filter mini-language as typed in Word
#'
#' Word capitalizes the first letter of a table cell, so `count(` and `in [`
#' come back as `Count(` and `In [`. Only those keywords are lower-cased, with
#' anchored patterns: a question code and an answer code are case-sensitive in
#' LimeSurvey and are never touched. The localized other keyword is rewritten
#' to the canonical `autre` the spec understands, in the `[...]` list and in
#' the `code = value` form alike -- the English template's own hint documents
#' `Other`, so `Q1 = Other` has to read as `Q1 = autre`. Every other value
#' token is left exactly as typed.
#' @keywords internal
#' @noRd
form_canon_filter <- function(x) {
  x <- form_clean_line(x)
  x <- gsub("\u2265", ">=", x, fixed = TRUE)
  x <- gsub("\u2264", "<=", x, fixed = TRUE)
  x <- sub("^count\\s*\\(", "count(", x, ignore.case = TRUE, perl = TRUE)
  x <- sub("^([A-Za-z][A-Za-z0-9]*)\\s+in\\s+\\[", "\\1 in [", x,
           ignore.case = TRUE, perl = TRUE)
  m <- regmatches(x, regexec("^([A-Za-z][A-Za-z0-9]*) in \\[([^]]*)\\]$", x))[[1L]]
  if (length(m) == 3L) {
    tokens <- trimws(strsplit(m[[3L]], ",")[[1L]])
    canon <- vapply(tokens, function(tk) {
      if (form_norm(tk) %in% lss_other_keywords()) "autre" else tk
    }, character(1), USE.NAMES = FALSE)
    if (!identical(canon, tokens)) {
      x <- paste0(m[[2L]], " in [", paste(canon, collapse = ", "), "]")
    }
    return(x)
  }
  m <- regmatches(x, regexec("^([A-Za-z][A-Za-z0-9]*)\\s*=\\s*(.+)$", x))[[1L]]
  if (length(m) == 3L && form_norm(m[[3L]]) %in% lss_other_keywords()) {
    x <- paste0(m[[2L]], " = autre")
  }
  x
}

#' Position of the other option: `end`, `beginning` or `specific` + code
#' @keywords internal
#' @noRd
form_parse_other_position <- function(text, ctx, field_label) {
  clean <- form_clean_line(text)
  # the accent-insensitive copy keeps its case: the code it gives back is the
  # code the author typed ("Apres A3" must yield `A3`, never `a3`)
  folded <- form_fold(clean)
  norm <- tolower(folded)
  vocab <- form_vocabulary()
  if (norm %in% vocab$pos_end) return(list(position = "end"))
  if (norm %in% vocab$pos_beginning) return(list(position = "beginning"))
  pats <- form_after_patterns()
  for (p in pats$clean) {
    m <- regmatches(clean, regexec(p, clean, ignore.case = TRUE))[[1L]]
    if (length(m) == 2L) return(list(position = "specific", code = m[[2L]]))
  }
  for (p in pats$norm) {
    m <- regmatches(folded, regexec(p, folded, ignore.case = TRUE))[[1L]]
    if (length(m) == 2L) return(list(position = "specific", code = m[[2L]]))
  }
  chrome <- ctx$chrome
  form_abort(
    ctx, "lssdoc_bad_form_value",
    c(paste0(ctx$where, ", field {.field ", esc(field_label),
             "}: unrecognized position {.val ", esc(text), "}."),
      "i" = paste0("Use {.val ", esc(chrome$form_other_position_end), "}, {.val ",
                   esc(chrome$form_other_position_beginning), "} or {.val ",
                   esc(sprintf(chrome$form_other_position_after_fmt, "<code>")),
                   "}.")),
    field = field_label)
}

#' The kind a Type cell designates
#' @keywords internal
#' @noRd
form_parse_kind <- function(text, ctx, field_label) {
  norm <- form_norm(text)
  hit <- match(norm, lss_kinds$kind)
  if (!is.na(hit)) return(lss_kinds$kind[[hit]])
  cand <- form_type_labels()[[norm]]
  if (is.null(cand)) {
    form_abort(
      ctx, "lssdoc_bad_form_value",
      c(paste0(ctx$where, ", field {.field ", esc(field_label),
               "}: unknown type {.val ", esc(text), "}."),
        "i" = paste0("Use a kind code: ",
                     paste0("{.val ", lss_kinds$kind, "}", collapse = ", "), ".")),
      field = field_label)
  }
  if (length(cand) > 1L) {
    form_abort(
      ctx, "lssdoc_bad_form_value",
      c(paste0(ctx$where, ", field {.field ", esc(field_label),
               "}: type {.val ", esc(text), "} is ambiguous."),
        "x" = paste0("It names ", length(cand), " kinds: ",
                     paste0("{.val ", cand, "}", collapse = ", "), "."),
        "i" = "Write the kind code instead."),
      field = field_label)
  }
  cand[[1L]]
}

#' Yes / no, in every language, plus what an author types instead
#' @keywords internal
#' @noRd
form_parse_mandatory <- function(text, ctx, field_label) {
  norm <- form_norm(text)
  vocab <- form_vocabulary()
  if (!nzchar(norm) || norm %in% vocab$no) return(FALSE)
  if (norm %in% vocab$yes) return(TRUE)
  chrome <- ctx$chrome
  if (norm %in% vocab$soft) {
    form_abort(ctx, "lssdoc_bad_form_value",
               c(paste0(ctx$where, ", field {.field ", esc(field_label),
                        "}: soft mandatory is not supported."),
                 "i" = paste0("Use {.val ", esc(chrome$mandatory_yes),
                              "} or {.val ", esc(chrome$mandatory_no), "}.")),
               field = field_label)
  }
  form_abort(ctx, "lssdoc_bad_form_value",
             paste0(ctx$where, ", field {.field ", esc(field_label),
                    "}: expected {.val ", esc(chrome$mandatory_yes),
                    "} or {.val ", esc(chrome$mandatory_no),
                    "}, found {.val ", esc(text), "}."),
             field = field_label)
}

# ---- 5. one block's rows -----------------------------------------------------

#' Resolve the key of one row: its field, and its language suffix
#' @keywords internal
#' @noRd
form_resolve_key <- function(row, block, ctx) {
  key_line <- ""
  non_empty <- row$key_lines[nzchar(row$key_lines)]
  if (length(non_empty)) key_line <- non_empty[[1L]]
  values <- row$value_lines[nzchar(row$value_lines)]
  if (!nzchar(key_line) && !length(values)) return(NULL)
  if (!nzchar(key_line)) {
    snippet <- substr(values[[1L]], 1L, 40L)
    form_abort(ctx, "lssdoc_bad_form_key",
               c(paste0(ctx$where, ", row ", row$row, ": the value {.val ",
                        esc(snippet), "} has no field label in the left cell."),
                 "i" = "Type the field name in the left-hand cell, or clear the row."),
               field = NA_character_)
  }
  split <- form_split_suffix(key_line)
  field <- form_label_map(block, ctx$chrome_lang)[[form_norm(split$label)]]
  if (is.null(field)) {
    labels <- form_label_list(block, ctx$chrome)
    form_abort(
      ctx, "lssdoc_bad_form_key",
      c(paste0(ctx$where, ": unknown field {.val ", esc(key_line), "} (row ",
               row$row, ")."),
        "i" = paste0("Fields of a ",
                     esc(ctx$chrome[[paste0("form_block_", block)]]), " block: ",
                     paste0("{.val ", esc(labels), "}", collapse = ", "), ".")),
      field = NA_character_)
  }
  list(field = field, lang = split$lang, lines = values, row = row$row,
       numbered = row$numbered, key = key_line)
}

#' All the key/value rows of one block, keyed by (field, language)
#' @keywords internal
#' @noRd
form_block_entries <- function(block, ctx) {
  kind <- block$kind
  defs <- form_field_defs[[kind]]
  raw <- list()
  for (row in block$rows) {
    entry <- form_try(ctx, form_resolve_key(row, kind, ctx))
    if (!is.null(entry)) raw[[length(raw) + 1L]] <- entry
  }

  # The Survey block declares the content languages, and every later suffix is
  # checked against them: they must be read before any localizable row of the
  # document is interpreted.
  if (identical(kind, "survey")) {
    form_try(ctx, form_set_languages(raw, ctx))
  }
  if (is.null(ctx$languages)) ctx$languages <- lss_spec_defaults$language
  primary <- ctx$languages[[1L]]

  entries <- list()
  for (e in raw) {
    keep <- form_try(ctx, {
      def <- defs[[e$field]]
      label <- ctx$chrome[[def$key]]
      if (!is.na(e$lang) && !isTRUE(def$loc)) {
        form_abort(ctx, "lssdoc_bad_form_key",
                   paste0(ctx$where, ", field {.field ", esc(label),
                          "}: this field takes no language suffix."),
                   field = label)
      }
      if (!is.na(e$lang) && !e$lang %in% ctx$languages) {
        declared <- ctx$languages
        form_abort(
          ctx, "lssdoc_bad_form_key",
          c(paste0(ctx$where, ", field {.field ", esc(label),
                   "}: language {.val ", esc(e$lang), "} is not declared."),
            "i" = paste0("Declared languages: ",
                         paste0("{.val ", esc(declared), "}", collapse = ", "),
                         ".")),
          field = label)
      }
      if (isTRUE(e$numbered) && length(e$lines)) {
        form_abort(
          ctx, "lssdoc_bad_form_layout",
          c(paste0(ctx$where, ", field {.field ", esc(label),
                   "}: automatic list numbering detected; Word keeps the numbers out of the text."),
            "i" = "Remove the list format (Home > Numbering) and type the code yourself: {.code 1 = Label}."),
          field = label)
      }
      lang <- if (isTRUE(def$loc)) (if (is.na(e$lang)) primary else e$lang) else NA_character_
      seen <- Filter(function(x) identical(x$field, e$field) &&
                       identical(x$lang, lang), entries)
      if (length(seen)) {
        form_abort(ctx, "lssdoc_bad_form_key",
                   paste0(ctx$where, ", field {.field ", esc(label),
                          "}: given twice (rows ", seen[[1L]]$row, " and ",
                          e$row, ")."),
                   field = label)
      }
      if (identical(def$lines, "one") && length(e$lines) > 1L) {
        n_lines <- length(e$lines)
        form_abort(ctx, "lssdoc_bad_form_value",
                   paste0(ctx$where, ", field {.field ", esc(label),
                          "}: expects a single line; found ", n_lines, "."),
                   field = label)
      }
      e$lang <- lang
      e
    })
    if (!is.null(keep)) entries[[length(entries) + 1L]] <- keep
  }
  entries
}

#' Read the `Languages` row of the Survey block into `ctx$languages`
#' @keywords internal
#' @noRd
form_set_languages <- function(raw, ctx) {
  label <- ctx$chrome$cover_languages
  hit <- Filter(function(e) identical(e$field, "languages"), raw)
  if (!length(hit) || !length(hit[[1L]]$lines)) {
    form_abort(ctx, "lssdoc_bad_form_value",
               c(paste0(ctx$where, ", field {.field ", esc(label),
                        "}: empty; a value is required."),
                 "i" = "List the LimeSurvey language codes, primary first: {.code fr, en}."),
               field = label)
  }
  text <- hit[[1L]]$lines[[1L]]
  codes <- trimws(strsplit(text, "[,;[:space:]]+")[[1L]])
  codes <- codes[nzchar(codes)]
  codes <- vapply(codes, form_lang_code, character(1), USE.NAMES = FALSE)
  bad <- codes[!grepl("^[a-z]{2,3}(-[A-Za-z0-9]+)?$", codes)]
  if (!length(codes) || length(bad)) {
    form_abort(ctx, "lssdoc_bad_form_value",
               c(paste0(ctx$where, ", field {.field ", esc(label), "}: {.val ",
                        esc(if (length(bad)) bad[[1L]] else text),
                        "} is not a language code."),
                 "i" = "Use LimeSurvey codes, primary first: {.code fr}, {.code en}, {.code de-informal}, {.code pt-BR}."),
               field = label)
  }
  dup <- unique(codes[duplicated(codes)])
  if (length(dup)) {
    form_abort(ctx, "lssdoc_bad_form_value",
               paste0(ctx$where, ", field {.field ", esc(label),
                      "}: language {.val ", esc(dup[[1L]]), "} is listed twice."),
               field = label)
  }
  ctx$languages <- codes
  invisible(codes)
}

#' The lines of one field in one language, or `NULL`
#' @keywords internal
#' @noRd
form_lines_of <- function(entries, field, lang = NA_character_) {
  for (e in entries) {
    if (identical(e$field, field) && identical(e$lang, lang)) return(e$lines)
  }
  NULL
}

#' Collect a localizable text field into the named form `lss_spec()` accepts
#'
#' Every declared language must supply the text as soon as one of them does:
#' the spec refuses to author a missing translation, and the form boundary is
#' where the author can still fix it.
#'
#' @param join `TRUE` joins the lines of a cell with a newline (a wording, a
#'   title); `FALSE` keeps them as a vector (the welcome and end texts, whose
#'   lines are paragraphs).
#' @keywords internal
#' @noRd
form_localized <- function(entries, field, ctx, label, join = TRUE,
                           required = FALSE) {
  langs <- ctx$languages
  values <- lapply(langs, function(lg) form_lines_of(entries, field, lg))
  names(values) <- langs
  filled <- vapply(values, function(v) length(v) > 0L, logical(1))
  if (!any(filled)) {
    if (isTRUE(required)) {
      form_abort(ctx, "lssdoc_bad_form_value",
                 paste0(ctx$where, ", field {.field ", esc(label),
                        "}: empty; a value is required."),
                 field = label)
    }
    return(NULL)
  }
  if (!all(filled)) {
    absent <- langs[!filled]
    form_abort(
      ctx, "lssdoc_bad_form_key",
      c(paste0(ctx$where, ", field {.field ", esc(label),
               "}: no text for language {.val ", esc(absent[[1L]]), "}."),
        "i" = "Every declared language needs every text: a missing translation is exactly what {.fn audit_lss} flags on read."),
      field = label)
  }
  out <- lapply(values, function(v) if (isTRUE(join)) paste(v, collapse = "\n") else v)
  out[langs]
}

#' Collect an option / row / column field across the declared languages
#'
#' The primary language defines the list: how many items, which codes, which
#' one is the other option. A translation lines up with it, one line per item,
#' and may repeat the code or leave it out.
#' @keywords internal
#' @noRd
form_localized_items <- function(entries, field, ctx, label, required = FALSE) {
  langs <- ctx$languages
  primary <- langs[[1L]]
  lines <- lapply(langs, function(lg) form_lines_of(entries, field, lg))
  names(lines) <- langs
  filled <- vapply(lines, function(v) length(v) > 0L, logical(1))
  if (!any(filled)) {
    if (isTRUE(required)) {
      form_abort(ctx, "lssdoc_bad_form_value",
                 paste0(ctx$where, ", field {.field ", esc(label),
                        "}: empty; a value is required."),
                 field = label)
    }
    return(NULL)
  }
  if (!filled[[primary]]) {
    form_abort(
      ctx, "lssdoc_bad_form_key",
      paste0(ctx$where, ", field {.field ", esc(label),
             "}: no values for the primary language {.val ", esc(primary), "}."),
      field = label)
  }
  parsed <- lapply(langs[filled], function(lg) {
    form_parse_items(lines[[lg]], ctx, field, label)
  })
  names(parsed) <- langs[filled]
  if (!all(filled)) {
    absent <- langs[!filled]
    form_abort(
      ctx, "lssdoc_bad_form_key",
      c(paste0(ctx$where, ", field {.field ", esc(label),
               "}: no values for language {.val ", esc(absent[[1L]]), "}."),
        "i" = "Every declared language needs every label, one per line, in the order of the primary language."),
      field = label)
  }
  base <- parsed[[primary]]
  for (lg in setdiff(langs, primary)) {
    other <- parsed[[lg]]
    if (length(other) != length(base)) {
      n2 <- length(other)
      n1 <- length(base)
      form_abort(
        ctx, "lssdoc_bad_form_value",
        c(paste0(ctx$where, ", field {.field ", esc(label),
                 "}: {n2} value{?s} for [", esc(lg), "], {n1} for [",
                 esc(primary), "]."),
          "i" = "Translations line up with the primary language, one per line."),
        field = label)
    }
    for (k in seq_along(base)) {
      if (!identical(isTRUE(other[[k]]$other), isTRUE(base[[k]]$other))) {
        form_abort(
          ctx, "lssdoc_bad_form_value",
          paste0(ctx$where, ", field {.field ", esc(label), "}, line ", k,
                 ": the other option is not at the same position in [",
                 esc(lg), "] and [", esc(primary), "]."),
          field = label)
      }
      c2 <- other[[k]]$code
      c1 <- base[[k]]$code
      if (!is.na(c2) && !identical(c2, c1)) {
        form_abort(
          ctx, "lssdoc_bad_form_value",
          paste0(ctx$where, ", field {.field ", esc(label), "}, line ", k,
                 ": code {.val ", esc(c2), "} in [", esc(lg),
                 "] differs from {.val ", esc(c1 %||% ""), "} in [",
                 esc(primary), "]."),
          field = label)
      }
    }
  }
  lapply(seq_along(base), function(k) {
    texts <- lapply(langs, function(lg) parsed[[lg]][[k]]$text)
    names(texts) <- langs
    item <- list(text = texts, other = isTRUE(base[[k]]$other))
    if (!is.na(base[[k]]$code)) item$code <- base[[k]]$code
    item
  })
}

# ---- 6. blocks -> spec pieces -------------------------------------------------

#' Build the survey-level fields
#' @keywords internal
#' @noRd
form_build_survey <- function(entries, ctx) {
  chrome <- ctx$chrome
  list(
    title = form_localized(entries, "title", ctx, chrome$form_title,
                           required = TRUE),
    welcome = form_localized(entries, "welcome", ctx, chrome$welcome_text_title,
                             join = FALSE),
    end_text = form_localized(entries, "end_text", ctx, chrome$end_text_title,
                              join = FALSE)
  )
}

#' Build one group
#' @keywords internal
#' @noRd
form_build_group <- function(entries, ctx) {
  chrome <- ctx$chrome
  g <- list(
    title = form_localized(entries, "title", ctx, chrome$form_title,
                           required = TRUE),
    questions = list()
  )
  description <- form_localized(entries, "description", ctx,
                                chrome$description_title)
  if (!is.null(description)) g$description <- description
  g
}

#' Build one question
#'
#' The field SET is a pure function of the kind's row in `lss_kinds` -- the
#' same rule the writer implements -- so a kind added to the table is read
#' back for free. A row that does not apply to the kind is tolerated while it
#' is blank (an author may change the Type without deleting rows) and refused
#' as soon as it carries text.
#' @keywords internal
#' @noRd
form_build_question <- function(entries, code, ctx) {
  chrome <- ctx$chrome
  type_lines <- form_lines_of(entries, "type")
  if (is.null(type_lines) || !length(type_lines)) {
    form_abort(ctx, "lssdoc_bad_form_value",
               c(paste0(ctx$where, ", field {.field ", esc(chrome$meta_type),
                        "}: empty; a value is required."),
                 "i" = paste0("Use a kind code: ",
                              paste0("{.val ", lss_kinds$kind, "}", collapse = ", "),
                              ".")),
               field = chrome$meta_type)
  }
  kind <- form_parse_kind(type_lines[[1L]], ctx, chrome$meta_type)
  allowed <- form_kind_fields(kind)
  for (e in entries) {
    if (e$field %in% allowed || !length(e$lines)) next
    label <- form_label_of("question", e$field, chrome)
    kind_label <- lss_kinds$label[[match(kind, lss_kinds$kind)]]
    form_abort(
      ctx, "lssdoc_bad_form_key",
      c(paste0(ctx$where, ", field {.field ", esc(label),
               "}: does not apply to type {.val ", esc(kind), "} (",
               esc(kind_label), ")."),
        "i" = "Clear the cell, or change the type."),
      field = label)
  }

  q <- list(code = code, kind = kind)
  q$text <- form_localized(entries, "wording", ctx, chrome$form_wording,
                           required = TRUE)
  help <- form_localized(entries, "help", ctx, chrome$item_help)
  if (!is.null(help)) q$help <- help
  mandatory_lines <- form_lines_of(entries, "mandatory")
  q$mandatory <- if (is.null(mandatory_lines) || !length(mandatory_lines)) {
    lss_spec_defaults$mandatory
  } else {
    form_parse_mandatory(mandatory_lines[[1L]], ctx, chrome$meta_mandatory)
  }
  filter_lines <- form_lines_of(entries, "filter")
  if (!is.null(filter_lines) && length(filter_lines) &&
      nzchar(filter_lines[[1L]])) {
    q$relevance <- form_canon_filter(filter_lines[[1L]])
  }

  shape <- kind_row(kind)
  if ("options" %in% allowed) {
    q$options <- form_localized_items(entries, "options", ctx, chrome$item_options,
                                      required = TRUE)
    form_check_count(q$options, shape$min_options, ctx, chrome$item_options, kind)
  }
  if ("rows" %in% allowed) {
    q$rows <- form_localized_items(entries, "rows", ctx, chrome$form_rows,
                                   required = TRUE)
    form_check_count(q$rows, 1L, ctx, chrome$form_rows, kind)
  }
  if ("columns" %in% allowed) {
    q$columns <- form_localized_items(entries, "columns", ctx, chrome$form_columns,
                                      required = TRUE)
    form_check_count(q$columns, 1L, ctx, chrome$form_columns, kind)
  }
  if ("exclusive" %in% allowed) {
    q$options <- form_apply_exclusive(entries, q$options, ctx)
  }
  if ("max_answers" %in% allowed) {
    min_lines <- form_lines_of(entries, "min_answers")
    if (!is.null(min_lines) && length(min_lines)) {
      q$attributes <- list(
        min_answers = form_parse_integer(min_lines[[1L]], ctx,
                                         chrome$form_min_answers))
    }
    max_lines <- form_lines_of(entries, "max_answers")
    if (!is.null(max_lines) && length(max_lines)) {
      q$max_answers <- form_parse_integer(max_lines[[1L]], ctx,
                                          chrome$form_max_answers)
    }
  }
  if ("other_position" %in% allowed) {
    pos_lines <- form_lines_of(entries, "other_position")
    if (!is.null(pos_lines) && length(pos_lines)) {
      label <- chrome$form_other_position
      has_other <- any(vapply(q$options %||% list(),
                              function(o) isTRUE(o$other), logical(1)))
      if (!has_other) {
        form_abort(ctx, "lssdoc_bad_form_value",
                   c(paste0(ctx$where, ", field {.field ", esc(label),
                            "}: set, but the question has no other option."),
                     "i" = paste0("Add an option line reading {.val ",
                                  esc(chrome$form_other),
                                  "}, or clear this cell.")),
                   field = label)
      }
      pos <- form_parse_other_position(pos_lines[[1L]], ctx, label)
      q$other_position <- pos$position
      if (identical(pos$position, "specific")) {
        codes <- vapply(Filter(function(o) !isTRUE(o$other), q$options),
                        function(o) o$code %||% NA_character_, character(1))
        if (!pos$code %in% codes) {
          form_abort(ctx, "lssdoc_bad_form_value",
                     paste0(ctx$where, ", field {.field ", esc(label),
                            "}: {.val ", esc(pos$code),
                            "} is not an option code of this question."),
                     field = label)
        }
        q$other_position_code <- pos$code
      }
    }
  }
  q
}

#' Refuse a list shorter than the kind's minimum
#' @keywords internal
#' @noRd
form_check_count <- function(items, minimum, ctx, label, kind) {
  minimum <- if (is.na(minimum)) 1L else as.integer(minimum)
  n <- length(items %||% list())
  if (n >= minimum) return(invisible(NULL))
  form_abort(ctx, "lssdoc_bad_form_value",
             paste0(ctx$where, ", field {.field ", esc(label),
                    "}: {n} value{?s} found; {.val ", esc(kind),
                    "} needs at least {minimum}."),
             field = label)
}

#' Mark the options the Exclusive row names
#' @keywords internal
#' @noRd
form_apply_exclusive <- function(entries, options, ctx) {
  label <- ctx$chrome$item_exclusive
  lines <- form_lines_of(entries, "exclusive")
  if (is.null(lines) || !length(lines)) return(options)
  tokens <- trimws(unlist(strsplit(lines, "[,;]")))
  tokens <- tokens[nzchar(tokens)]
  primary <- ctx$languages[[1L]]
  for (tk in tokens) {
    hit <- NA_integer_
    for (k in seq_along(options)) {
      o <- options[[k]]
      if (isTRUE(o$other)) next
      code <- o$code %||% NA_character_
      label_k <- o$text[[primary]] %||% ""
      if (identical(tk, code) ||
          (!is.na(code) && identical(form_norm(tk), form_norm(code))) ||
          identical(form_norm(tk), form_norm(label_k))) {
        hit <- k
        break
      }
    }
    if (is.na(hit)) {
      form_abort(ctx, "lssdoc_bad_form_value",
                 c(paste0(ctx$where, ", field {.field ", esc(label), "}: {.val ",
                          esc(tk), "} is not an option code of this question."),
                   "i" = "List the codes of the options that uncheck every other box, one per line."),
                 field = label)
    }
    options[[hit]]$exclusive <- TRUE
  }
  options
}

#' Build one quota
#' @keywords internal
#' @noRd
form_build_quota <- function(entries, ctx) {
  chrome <- ctx$chrome
  quota <- list()
  name <- form_localized(entries, "name", ctx, chrome$form_name)
  if (!is.null(name)) quota$name <- name
  limit_lines <- form_lines_of(entries, "limit")
  quota$limit <- if (is.null(limit_lines) || !length(limit_lines)) {
    0L
  } else {
    form_parse_integer(limit_lines[[1L]], ctx, chrome$quota_limit, min = 0L)
  }
  action_lines <- form_lines_of(entries, "action")
  if (!is.null(action_lines) && length(action_lines)) {
    form_parse_quota_action(action_lines[[1L]], ctx, chrome$form_action)
  }
  condition <- form_lines_of(entries, "condition")
  if (is.null(condition) || !length(condition)) {
    form_abort(ctx, "lssdoc_bad_form_value",
               c(paste0(ctx$where, ", field {.field ", esc(chrome$quota_condition),
                        "}: empty; a value is required."),
                 "i" = "Write {.code code = value}: the question the quota watches and the answer that triggers it."),
               field = chrome$quota_condition)
  }
  parsed <- form_parse_condition(condition[[1L]], ctx, chrome$quota_condition)
  quota$question <- parsed$question
  quota$code <- parsed$code
  quota$message <- form_localized(entries, "message", ctx, chrome$form_message,
                                  required = TRUE)
  quota
}

#' The quota action: terminate, the only one `write_lss()` emits
#' @keywords internal
#' @noRd
form_parse_quota_action <- function(text, ctx, label) {
  norm <- form_norm(text)
  vocab <- form_vocabulary()
  if (!nzchar(norm) || norm %in% vocab$terminate) return("terminate")
  chrome <- ctx$chrome
  if (norm %in% vocab$confirm) {
    form_abort(ctx, "lssdoc_bad_form_value",
               c(paste0(ctx$where, ", field {.field ", esc(label), "}: {.val ",
                        esc(text), "} is not supported yet."),
                 "i" = paste0("{.fn write_lss} only emits {.val ",
                              esc(chrome$quota_action_terminate), "}.")),
               field = label)
  }
  form_abort(ctx, "lssdoc_bad_form_value",
             paste0(ctx$where, ", field {.field ", esc(label),
                    "}: unknown action {.val ", esc(text), "}; use {.val ",
                    esc(chrome$quota_action_terminate), "}."),
             field = label)
}

#' The quota condition: `code = value`
#' @keywords internal
#' @noRd
form_parse_condition <- function(text, ctx, label) {
  clean <- form_clean_line(text)
  m <- regmatches(
    clean,
    regexec("^([A-Za-z][A-Za-z0-9]*)\\s*=\\s*([A-Za-z0-9-]+)$", clean))[[1L]]
  if (length(m) != 3L) {
    form_abort(ctx, "lssdoc_bad_form_value",
               c(paste0(ctx$where, ", field {.field ", esc(label),
                        "}: expected {.code code = value}, found {.val ",
                        esc(text), "}."),
                 "i" = "A quota watches one answer of one question holding a single coded answer."),
               field = label)
  }
  if (form_is_other_word(m[[3L]])) {
    form_abort(ctx, "lssdoc_bad_form_value",
               paste0(ctx$where, ", field {.field ", esc(label),
                      "}: a quota cannot hang on the other option."),
               field = label)
  }
  list(question = m[[2L]], code = m[[3L]])
}

#' The question fields one kind accepts, straight from `lss_kinds`
#' @keywords internal
#' @noRd
form_kind_fields <- function(kind) {
  r <- kind_row(kind)
  fields <- c("type", "filter", "wording", "help")
  if (isTRUE(r$collects_response)) fields <- c(fields, "mandatory")
  if (identical(r$options, "required")) fields <- c(fields, "options")
  if (isTRUE(r$exclusive_allowed)) fields <- c(fields, "exclusive")
  if (identical(r$rows, "required")) fields <- c(fields, "rows")
  if (identical(r$columns, "required")) fields <- c(fields, "columns")
  if (!identical(r$max_answers_rule, "none")) {
    fields <- c(fields, "min_answers", "max_answers")
  }
  if (isTRUE(r$other_allowed)) fields <- c(fields, "other_position")
  fields
}

# ---- 7. the engine -----------------------------------------------------------

#' A fresh parsing context
#' @keywords internal
#' @noRd
form_new_ctx <- function(path, collect) {
  ctx <- new.env(parent = emptyenv())
  ctx$path <- path
  ctx$collect <- isTRUE(collect)
  ctx$problems <- list()
  ctx$chrome_lang <- "en"
  ctx$chrome <- lss_chrome_strings("en")
  ctx$languages <- NULL
  ctx$where <- NA_character_
  ctx$code <- NA_character_
  ctx
}

#' Walk the body: top-level tables, block boundaries, stray text
#'
#' A block opens at every title row, even in the middle of a table: pasting a
#' block under another one makes Word merge the two tables, and an author who
#' does that has not broken anything the reader cannot follow. The dual case
#' is NOT readable and must not be guessed: a block closes at every table
#' boundary too, so row 1 of every table has to name a block. A table that
#' opens with an ordinary key -- a deleted title row, a block Word split in
#' two, an unrelated two-column table pasted in -- is refused here rather than
#' absorbed into the block above, where its rows would be reported as
#' duplicate fields of the wrong question.
#' @keywords internal
#' @noRd
form_scan_body <- function(doc, ctx) {
  body <- xml2::xml_find_first(doc, "/w:document/w:body", FORM_NS)
  if (inherits(body, "xml_missing")) {
    form_abort(ctx, "lssdoc_bad_form_file",
               paste0("{.path ", esc(ctx$path), "} has no document body."))
  }
  children <- xml2::xml_children(body)
  names <- xml2::xml_name(children)
  blocks <- list()
  current <- NULL
  n_tables <- 0L
  for (i in seq_along(children)) {
    node <- children[[i]]
    if (identical(names[[i]], "p")) {
      if (n_tables == 0L) next  # the preamble (the document title) is ignored
      text <- trimws(paste(form_para_lines(node), collapse = " "))
      if (nzchar(text)) {
        snippet <- substr(text, 1L, 40L)
        n <- n_tables
        lssdoc_warn(
          c(paste0("Text outside the blocks was ignored after table ", n,
                   ": {.val ", esc(snippet), "}."),
            "i" = "Notes belong in a value cell; only the block tables are read."),
          class = c("lssdoc_form_stray_text", "lssdoc_bad_form_note"))
      }
      next
    }
    if (!identical(names[[i]], "tbl")) next
    n_tables <- n_tables + 1L
    n <- n_tables
    # a block never spans two tables: close the open one here, so the test
    # below fires on row 1 of every table
    if (!is.null(current)) blocks[[length(blocks) + 1L]] <- current
    current <- NULL
    orphan_reported <- FALSE
    rows <- form_try(ctx, form_table_rows(node, n, ctx), default = list())
    for (row in rows) {
      key <- ""
      non_empty <- row$key_lines[nzchar(row$key_lines)]
      if (length(non_empty)) key <- non_empty[[1L]]
      kind <- form_block_kind(key)
      if (!is.na(kind)) {
        if (!is.null(current)) blocks[[length(blocks) + 1L]] <- current
        values <- row$value_lines[nzchar(row$value_lines)]
        current <- list(kind = kind, lang = form_block_lang(key), table = n,
                        row = row$row,
                        title_value = if (length(values)) values[[1L]] else "",
                        title_values = values, rows = list())
        next
      }
      if (is.null(current)) {
        # strict mode stops here; collect mode reports the table once and
        # drops its orphan rows, so the problems of the next block still show
        if (!orphan_reported) {
          orphan_reported <- TRUE
          chrome <- ctx$chrome
          words <- vapply(c("survey", "group", "question", "quota"),
                          function(k) chrome[[paste0("form_block_", k)]],
                          character(1))
          form_try(ctx, form_abort(
            ctx, "lssdoc_bad_form_block",
            c(paste0("Table ", n, " does not start with a block title row (",
                     paste0("{.val ", esc(words), "}", collapse = ", "),
                     "); its first cell reads {.val ", esc(key), "}."),
              "i" = "Every block is a table whose first row names it."),
            where = paste0("table ", n)))
        }
        next
      }
      current$rows[[length(current$rows) + 1L]] <- row
    }
  }
  if (!is.null(current)) blocks[[length(blocks) + 1L]] <- current
  if (!n_tables) {
    form_abort(ctx, "lssdoc_bad_form_block",
               c(paste0("No block table found in {.path ", esc(ctx$path), "}."),
                 "i" = "A form is a series of two-column tables; write one with {.fn lss_template_docx}."))
  }
  blocks
}

#' Name a block in every message it can raise
#' @keywords internal
#' @noRd
form_where <- function(kind, n, code, chrome) {
  word <- chrome[[paste0("form_block_", kind)]]
  if (identical(kind, "survey")) return(paste0(word, " block"))
  if (identical(kind, "question")) {
    return(if (is.na(code) || !nzchar(code)) {
      paste0(word, " block ", n, " (no code)")
    } else {
      paste0(word, " ", code)
    })
  }
  paste0(word, " block ", n)
}

#' Read the code out of a Question block's title row
#' @keywords internal
#' @noRd
form_title_code <- function(block, n, ctx) {
  chrome <- ctx$chrome
  code <- trimws(block$title_value)
  if (!nzchar(code)) {
    form_abort(
      ctx, "lssdoc_bad_form_block",
      c(paste0(ctx$where, ": the title row has no code."),
        "i" = paste0("Type the question code in the cell next to {.val ",
                     esc(chrome$form_block_question),
                     "}: a letter, then letters or digits, at most 20 characters.")),
      field = chrome$form_code)
  }
  if (!grepl("^[A-Za-z][A-Za-z0-9]{0,19}$", code)) {
    form_abort(
      ctx, "lssdoc_bad_form_value",
      c(paste0(ctx$where, ": {.val ", esc(code), "} is not a valid question code."),
        "x" = "Codes start with a letter, use only letters and digits, at most 20 characters."),
      field = chrome$form_code)
  }
  code
}

#' Parse the whole document into an `lss_spec`
#'
#' Returns the spec, or `NULL` in collect mode when it could not be assembled.
#' @keywords internal
#' @noRd
form_parse <- function(ctx) {
  parts <- form_docx_parts(ctx$path)
  ctx$version <- form_marker(ctx$path, parts)
  marker_lang <- form_custom_property(parts$custom, "lssdoc-chrome-lang")
  if (!is.na(marker_lang) && marker_lang %in% FORM_CHROME_LANGS) {
    ctx$chrome_lang <- marker_lang
    ctx$chrome <- lss_chrome_strings(marker_lang)
  }
  form_refuse_document_features(parts$document, ctx)
  blocks <- form_scan_body(parts$document, ctx)

  # The chrome language of the document, when the property did not give it,
  # is the language of its first block word.
  if (is.na(marker_lang) && length(blocks) && !is.na(blocks[[1L]]$lang)) {
    ctx$chrome_lang <- blocks[[1L]]$lang
    ctx$chrome <- lss_chrome_strings(blocks[[1L]]$lang)
  }

  survey <- NULL
  groups <- list()
  quotas <- list()
  codes <- character(0)
  code_tables <- integer(0)   # the table each accepted code was first seen in
  n_group <- 0L
  n_quota <- 0L
  n_question <- 0L
  pending_group <- NULL   # a group whose questions have not been seen yet

  close_group <- function() {
    if (is.null(pending_group)) return(invisible(NULL))
    if (!pending_group$n_questions) {
      title <- loc_text(pending_group$group$title, ctx$languages[[1L]])
      where <- form_where("group", pending_group$index, NA_character_, ctx$chrome)
      ctx$where <- where
      ctx$code <- NA_character_
      form_abort(ctx, "lssdoc_bad_form_block",
                 c(paste0(where, " ({.val ", esc(title), "}) contains no question."),
                   "i" = "Every group needs at least one question; delete the group or add one."),
                 where = where)
    }
    groups[[length(groups) + 1L]] <<- pending_group$group
    pending_group <<- NULL
    invisible(NULL)
  }

  for (b in blocks) {
    kind <- b$kind
    if (identical(kind, "survey")) {
      n <- 1L
    } else if (identical(kind, "group")) {
      n_group <- n_group + 1L
      n <- n_group
    } else if (identical(kind, "quota")) {
      n_quota <- n_quota + 1L
      n <- n_quota
    } else {
      n_question <- n_question + 1L
      n <- n_question
    }
    ctx$where <- form_where(kind, n, NA_character_, ctx$chrome)
    ctx$code <- NA_character_

    if (identical(kind, "survey")) {
      if (!is.null(survey)) {
        form_try(ctx, form_abort(
          ctx, "lssdoc_bad_form_block",
          c(paste0("Table ", b$table, " is a second {.val ",
                   esc(ctx$chrome$form_block_survey),
                   "} block; a form declares the survey once."),
            "i" = "Keep one survey block, at the top of the document.")))
        next
      }
      if (length(groups) || !is.null(pending_group) || length(quotas)) {
        form_try(ctx, form_abort(
          ctx, "lssdoc_bad_form_block",
          paste0("The {.val ", esc(ctx$chrome$form_block_survey),
                 "} block must be the first table of the document.")))
      }
    } else if (is.null(survey) && is.null(ctx$languages)) {
      form_try(ctx, form_abort(
        ctx, "lssdoc_bad_form_block",
        c(paste0(ctx$where, " appears before the {.val ",
                 esc(ctx$chrome$form_block_survey), "} block."),
          "i" = "A form opens with the survey block: it declares the title and the languages.")))
    }

    if (identical(kind, "question")) {
      code <- form_try(ctx, form_title_code(b, n, ctx))
      if (!is.null(code)) {
        seen <- match(code, codes)
        if (!is.na(seen)) {
          first <- code_tables[[seen]]
          form_try(ctx, form_abort(
            ctx, "lssdoc_bad_form_value",
            c(paste0("{.val ", esc(code),
                     "}: duplicate question code, already used by table ", first, "."),
              "i" = "Codes are variable names and must be unique."),
            field = ctx$chrome$form_code))
          next
        }
        codes <- c(codes, code)
        code_tables <- c(code_tables, b$table)
      }
      ctx$code <- code %||% NA_character_
      ctx$where <- form_where("question", n, ctx$code, ctx$chrome)
      if (is.null(code)) next
    } else if (length(b$title_values)) {
      form_try(ctx, form_abort(
        ctx, "lssdoc_bad_form_block",
        c(paste0(ctx$where, ": unexpected text {.val ", esc(b$title_value),
                 "} next to the block title."),
          "i" = "This cell stays empty.")))
    }

    entries <- form_block_entries(b, ctx)

    if (identical(kind, "survey")) {
      survey <- form_try(ctx, form_build_survey(entries, ctx),
                         default = list(title = NULL))
      next
    }
    if (identical(kind, "group")) {
      form_try(ctx, close_group())
      g <- form_try(ctx, form_build_group(entries, ctx))
      if (!is.null(g)) {
        pending_group <- list(group = g, index = n, n_questions = 0L)
      } else {
        pending_group <- NULL
      }
      next
    }
    if (identical(kind, "quota")) {
      quota <- form_try(ctx, form_build_quota(entries, ctx))
      if (!is.null(quota)) quotas[[length(quotas) + 1L]] <- quota
      next
    }
    # a question. `n_group` counts the group BLOCKS seen, accepted or not:
    # a question that follows a group block which failed to parse has not
    # "appeared before any group block", and must fall through to the
    # read-but-drop branch below so its own problems are reported too.
    if (is.null(pending_group) && !n_group) {
      form_try(ctx, form_abort(
        ctx, "lssdoc_bad_form_block",
        c(paste0(ctx$where, " appears before any {.val ",
                 esc(ctx$chrome$form_block_group), "} block."),
          "i" = "Every question belongs to the group opened by the last group block above it.")))
      next
    }
    if (is.null(pending_group)) {
      # the group block itself failed to parse: its questions are still read
      # so that the author sees their problems too, but they land nowhere
      q <- form_try(ctx, form_build_question(entries, ctx$code, ctx))
      next
    }
    pending_group$n_questions <- pending_group$n_questions + 1L
    q <- form_try(ctx, form_build_question(entries, ctx$code, ctx))
    if (!is.null(q)) {
      pending_group$group$questions[[length(pending_group$group$questions) + 1L]] <- q
    }
  }
  ctx$where <- NA_character_
  ctx$code <- NA_character_
  form_try(ctx, close_group())

  if (is.null(survey)) {
    form_abort(ctx, "lssdoc_bad_form_block",
               c(paste0("{.path ", esc(ctx$path), "} has no {.val ",
                        esc(ctx$chrome$form_block_survey), "} block."),
                 "i" = "A form opens with the survey block: it declares the title and the languages."))
  }
  if (isTRUE(ctx$collect) && length(ctx$problems)) return(NULL)

  form_assemble(survey, groups, quotas, ctx)
}

# The spec names its fields; the form names them in the author's language.
# Only the three that are spelled differently need a line.
FORM_SPEC_FIELDS <- c(kind = "type", text = "wording", relevance = "filter")

#' Hand the pieces to `lss_spec()` and re-signal what it refuses
#'
#' `lss_spec()` owns the cross-field rules (a filter may not cite a question
#' asked later, a cap must stay below the option count, a quota needs a
#' single-choice target). Its refusal is re-signalled with the form's own class
#' so a caller can dispatch on `lssdoc_bad_form`, while the original condition
#' stays available as the parent and both messages are shown.
#'
#' Where the refusal happened comes from the condition itself: `spec_abort()`
#' carries `spec_code` and `spec_field`. Matching the code against the message
#' is the fallback for a refusal raised outside `spec_abort()`, and it is
#' anchored on a non-alphanumeric boundary, so `Q1` can no longer steal an
#' error about `Q10`.
#' @keywords internal
#' @noRd
form_assemble <- function(survey, groups, quotas, ctx) {
  codes <- unlist(lapply(groups, function(g) {
    vapply(g$questions, function(q) q$code, character(1))
  }), use.names = FALSE)
  withCallingHandlers(
    tryCatch(
      lss_spec(title = survey$title, groups = groups,
               languages = ctx$languages, welcome = survey$welcome,
               end_text = survey$end_text,
               quotas = if (length(quotas)) quotas else NULL),
      lssdoc_bad_spec = function(cnd) {
        message <- conditionMessage(cnd)
        where <- NA_character_
        code <- NA_character_
        field <- NA_character_
        said <- as.character(cnd$spec_code %||% NA_character_)[[1L]]
        if (!is.na(said) && nzchar(said) && said %in% codes) {
          code <- said
        } else {
          for (cd in codes) {
            anchored <- paste0("(^|[^A-Za-z0-9])", form_regex_escape(cd),
                               "($|[^A-Za-z0-9])")
            if (grepl(anchored, message)) {
              code <- cd
              break
            }
          }
        }
        if (!is.na(code)) {
          where <- form_where("question", NA_integer_, code, ctx$chrome)
        }
        said_field <- as.character(cnd$spec_field %||% NA_character_)[[1L]]
        if (!is.na(said_field) && nzchar(said_field)) {
          mapped <- unname(FORM_SPEC_FIELDS[said_field])
          field <- form_label_of("question",
                                 if (is.na(mapped)) said_field else mapped,
                                 ctx$chrome)
        }
        head <- paste0(if (is.na(where)) "The form" else where,
                       if (is.na(field)) "" else paste0(", field {.field ",
                                                        esc(field), "}"),
                       ": the specification it describes is invalid.")
        lssdoc_abort(
          c(head, "x" = esc(message)),
          class = c("lssdoc_bad_form_spec", "lssdoc_bad_form", "lssdoc_bad_spec"),
          parent = cnd,
          form_block = where,
          form_code = code,
          form_field = field,
          call = rlang::caller_env()
        )
      }
    ),
    lssdoc_warning = function(w) invokeRestart("muffleWarning")
  )
}

# ---- 8. the exported readers --------------------------------------------------

#' Read a Word authoring form back into a survey specification
#'
#' `r lifecycle::badge("experimental")`
#'
#' \strong{Experimental.} Parse a `.docx` **authoring form** -- the document
#' [write_form_docx()] and [lss_template_docx()] produce, filled in by an
#' author in Word -- back into an [lss_spec()], ready for [write_lss()] and a
#' LimeSurvey import. The round trip is exact: a specification written as a
#' form and read back describes the same survey.
#'
#' Reading needs no suggested package: only \pkg{xml2} and [utils::unzip()],
#' so an author's file can be turned into a `.lss` on a bare installation.
#' \pkg{officer} and \pkg{flextable} are needed to WRITE a form, never to read
#' one.
#'
#' @param path Character. Path of the `.docx` form to read.
#'
#' @return An [lss_spec()] object.
#'
#' @details
#' The contract the document must honor -- all of it written by
#' [write_form_docx()], and spelled out in the blank template's hints:
#'
#' * one top-level two-column table per block (Survey, Group, Question,
#'   Quota), the key on the left and the value on the right; the first row of
#'   a block names it, and a title row starts a new block even in the middle
#'   of a table (pasting a block makes Word merge two tables).
#' * keys are matched by their TEXT, ignoring case, accents, quotes and a
#'   trailing colon, against the labels of all five chrome languages, so a
#'   French author can fill an English template. Only the first line of a key
#'   cell is read: the muted hints under the key are ignored.
#' * a value cell holds one value per line, where a line is a paragraph or a
#'   soft return (Enter or Shift+Enter). Options, rows and columns are read as
#'   `code = label`, or as a bare label that gets auto-numbered; the reserved
#'   word (`Other`, `Autre`, `Sonstiges`, `Otro`, `Altro`) alone or on the
#'   left of `=` is the native other option.
#' * the Type cell carries the kind code (`single`, `array5`, ...); a
#'   localized type label is accepted only when it names exactly one kind.
#' * `Mandatory` takes the yes/no words of any language (and `y`/`n`,
#'   `true`/`false`, `1`/`0`); blank means no. The `Filter` cell takes the
#'   mini-language of [lss_spec()] (`Q1 = 1`, `Q2 in [1, autre]`,
#'   `count(Q3) >= 2`); its keywords are matched whatever Word capitalized,
#'   and the case of a question or answer code is never changed.
#' * when the survey declares several languages, every localizable key is
#'   suffixed with a language code -- `Wording [fr]`, `Wording [en]` -- and
#'   every declared language must supply every text.
#' * the file must carry the custom document property
#'   `lssdoc-template-version`: it is the reader's proof that the document
#'   agreed to this contract. Tracked changes, content controls, merged cells,
#'   a third column, a nested table and Word's automatic list numbering are
#'   each refused with a classed error naming the block and the field.
#'
#' Errors carry the class `lssdoc_bad_form` plus one leaf class
#' (`lssdoc_bad_form_file`, `_marker`, `_layout`, `_block`, `_key`, `_value`,
#' `_spec`), and every message names the block, the question code and the
#' field. Use [check_form_docx()] to list every problem of a document at once
#' instead of stopping at the first.
#'
#' @examples
#' if (requireNamespace("officer", quietly = TRUE) &&
#'     requireNamespace("flextable", quietly = TRUE)) {
#'   form <- tempfile(fileext = ".docx")
#'   lss_template_docx(form, lang = "fr", kinds = c("single", "text"))
#'   spec <- read_form_docx(form)
#'   spec
#' }
#' @seealso [write_form_docx()] and [lss_template_docx()] to write the form,
#'   [check_form_docx()] for a dry run, [lss_spec()], [write_lss()].
#' @export
read_form_docx <- function(path) {
  ctx <- form_new_ctx(path, collect = FALSE)
  form_parse(ctx)
}

#' Report every problem of a Word authoring form at once
#'
#' `r lifecycle::badge("experimental")`
#'
#' \strong{Experimental.} Run [read_form_docx()]'s parser in dry-run mode: an
#' author iterates on a form until it is clean, and fixing one problem per
#' round trip is not a workflow. The same rules, the same messages; the first
#' error simply does not stop the read.
#'
#' A problem in one block does not hide the problems of the next: parsing
#' resumes at the following block. Three problems are still fatal, because
#' nothing can be read past them: an unreadable file, a missing or
#' unsupported contract version, and a document-wide structural refusal
#' (tracked changes, content controls). When one of those fires it is the only
#' row reported.
#'
#' @param path Character. Path of the `.docx` form to check.
#'
#' @return An object of class `lss_form_check`: a data frame with one row per
#'   problem and the columns `severity` (`"error"` or `"warning"`), `class`
#'   (the condition class), `block`, `code`, `field` and `message`. Zero rows
#'   means the document reads: [read_form_docx()] will return a specification.
#'   A `print()` method summarizes it.
#'
#' @examples
#' if (requireNamespace("officer", quietly = TRUE) &&
#'     requireNamespace("flextable", quietly = TRUE)) {
#'   form <- tempfile(fileext = ".docx")
#'   lss_template_docx(form, lang = "fr", kinds = c("single", "text"))
#'   check_form_docx(form)
#' }
#' @seealso [read_form_docx()], [write_form_docx()], [lss_template_docx()].
#' @export
check_form_docx <- function(path) {
  ctx <- form_new_ctx(path, collect = TRUE)
  tryCatch(
    withCallingHandlers(
      form_parse(ctx),
      lssdoc_warning = function(w) {
        form_record(ctx, w, severity = "warning")
        invokeRestart("muffleWarning")
      }
    ),
    lssdoc_bad_form = function(cnd) form_record(ctx, cnd)
  )
  form_check_result(ctx)
}

#' Turn the collector into the returned data frame
#' @keywords internal
#' @noRd
form_check_result <- function(ctx) {
  fields <- c("severity", "class", "block", "code", "field", "message")
  column <- function(name) {
    if (!length(ctx$problems)) return(character(0))
    vapply(ctx$problems, function(p) as.character(p[[name]] %||% NA_character_),
           character(1))
  }
  out <- data.frame(stats::setNames(lapply(fields, column), fields),
                    stringsAsFactors = FALSE)
  row.names(out) <- NULL
  attr(out, "file") <- ctx$path
  structure(out, class = c("lss_form_check", "data.frame"))
}

#' @param x An `lss_form_check` object.
#' @param n Maximum number of problems to print; `Inf` for all.
#' @param ... Ignored.
#' @rdname check_form_docx
#' @export
print.lss_form_check <- function(x, ..., n = 20L) {
  cli::cli_h1("lssdoc form check")
  cli::cli_text("{.field File}: {.path {attr(x, 'file')}}")
  total <- nrow(x)
  if (!total) {
    cli::cli_alert_success("No problems found: the form reads.")
    return(invisible(x))
  }
  n_errors <- sum(x$severity == "error")
  n_warnings <- total - n_errors
  cli::cli_text(
    "{.strong {total}} problem{?s}: {n_errors} error{?s}, {n_warnings} warning{?s}."
  )
  cap <- if (is.finite(n)) min(as.integer(n), total) else total
  symbols <- c(error = "x", warning = "warning")
  for (i in seq_len(cap)) {
    p <- x[i, , drop = FALSE]
    head <- sub("\n.*$", "", p$message)
    where <- if (is.na(p$block) || !nzchar(p$block)) "" else paste0(p$block, ": ")
    cli::cli_bullets(stats::setNames(
      paste0("{.strong ", esc(where), "}", esc(head)),
      symbols[[p$severity]]))
  }
  if (cap < total) {
    remaining <- total - cap
    cli::cli_text(
      "{.emph \u2026 and {remaining} more problem{?s}.} ",
      "Use {.code as.data.frame(x)} to see them all, or ",
      "{.code print(x, n = Inf)} to expand here.")
  }
  invisible(x)
}

# ---- 9. canonical comparison --------------------------------------------------

#' Put a specification in canonical form, for comparing two of them
#'
#' The round-trip test asserts that a specification written as a form and read
#' back describes the SAME survey, not that two R objects happen to share a
#' field order. `identical()` on the raw objects would fail on differences the
#' contract does not carry -- a count typed as `"3"` in R and read back as
#' `3L`, an optional field left out on one side and set to its default on the
#' other. This function removes exactly those differences and nothing else, so
#' `identical(canonical_spec(a), canonical_spec(b))` is the honest statement.
#'
#' What it normalizes, and only this:
#'
#' * **field order**: survey, group, question, option and quota fields are
#'   rebuilt in a fixed order; fields absent on both sides stay absent.
#' * **localized texts**: reduced to a named list over the declared languages,
#'   each language a single string, the lines of a multi-line text joined with
#'   `"\n"` (the form writes and reads back lines, and `c("a", "b")` and
#'   `"a\nb"` describe the same text). Empty texts become `NULL`.
#' * **counts**: `max_answers`, `attributes$min_answers` and a quota's `limit`
#'   are coerced to integer; an absent limit becomes `0L`, the value
#'   [write_lss()] emits for it.
#' * **defaults written as values**: `mandatory` becomes a logical scalar
#'   (`FALSE` when absent); a `relevance` equal to the always-true `"1"`
#'   becomes `NULL`; `other_position` becomes `"end"` when the question has an
#'   other option and no position of its own, and is dropped when it has no
#'   other option; `other_position_code` is kept only for `"specific"`.
#' * **option items**: reduced to `code` (character, `NA` for the other
#'   option), `text`, `other` and `exclusive` (logical scalars).
#' * **kind applicability**: a question keeps only the fields its kind
#'   carries, so a field the form cannot write cannot make the comparison fail.
#'
#' It does NOT sort anything: order is meaning here (options, rows, columns,
#' questions and groups are ordered), and two specifications that differ in
#' order must differ.
#'
#' @param spec An [lss_spec()] object.
#' @return A plain nested list, with no class.
#' @keywords internal
#' @noRd
canonical_spec <- function(spec) {
  langs <- spec$languages %||% spec$language %||% lss_spec_defaults$language
  text <- function(x) {
    if (is.null(x)) return(NULL)
    if (!is.list(x)) x <- stats::setNames(list(as.character(x)), langs[[1L]])
    out <- lapply(langs, function(lg) {
      v <- x[[lg]]
      if (is.null(v)) return(NULL)
      v <- paste(as.character(v), collapse = "\n")
      if (nzchar(v)) v else NULL
    })
    names(out) <- langs
    out <- out[!vapply(out, is.null, logical(1))]
    if (!length(out)) NULL else out
  }
  int <- function(x, default = NULL) {
    if (is.null(x)) return(default)
    as.integer(x)
  }
  items <- function(x) {
    if (is.null(x)) return(NULL)
    lapply(x, function(o) list(
      code = if (isTRUE(o$other)) NA_character_ else as.character(o$code %||% NA_character_),
      text = text(o$text),
      other = isTRUE(o$other),
      exclusive = isTRUE(o$exclusive)
    ))
  }

  question <- function(q) {
    fields <- form_kind_fields(q$kind)
    out <- list(code = as.character(q$code), kind = as.character(q$kind),
                text = text(q$text), help = text(q$help),
                mandatory = isTRUE(q$mandatory))
    rel <- q$relevance
    if (!is.null(rel) && !identical(as.character(rel), lss_spec_defaults$relevance)) {
      out$relevance <- as.character(rel)
    }
    if ("options" %in% fields) out$options <- items(q[["options"]])
    if ("rows" %in% fields) out$rows <- items(q[["rows"]])
    if ("columns" %in% fields) out$columns <- items(q[["columns"]])
    if ("max_answers" %in% fields) {
      out$max_answers <- int(q$max_answers)
      min_answers <- (q$attributes %||% list())[["min_answers"]]
      if (!is.null(min_answers)) out$attributes <- list(min_answers = int(min_answers))
    }
    extra <- (q$attributes %||% list())
    extra <- extra[setdiff(names(extra), "min_answers")]
    if (length(extra)) {
      extra <- extra[order(names(extra))]
      out$attributes <- c(out$attributes %||% list(),
                          lapply(extra, as.character))
    }
    if ("other_position" %in% fields) {
      has_other <- any(vapply(q[["options"]] %||% list(),
                              function(o) isTRUE(o$other), logical(1)))
      if (has_other) {
        pos <- as.character(q$other_position %||% "end")
        out$other_position <- pos
        if (identical(pos, "specific")) {
          out$other_position_code <- as.character(q$other_position_code %||% "")
        }
      }
    }
    out[!vapply(out, is.null, logical(1))]
  }

  group <- function(g) {
    out <- list(title = text(g$title), description = text(g$description),
                questions = lapply(g$questions, question))
    out[!vapply(out, is.null, logical(1))]
  }

  quota <- function(qu) {
    out <- list(question = as.character(qu$question),
                code = as.character(qu$code),
                name = text(qu$name),
                limit = int(qu$limit, 0L),
                message = text(qu$message))
    out[!vapply(out, is.null, logical(1))]
  }

  out <- list(
    languages = as.character(langs),
    title = text(spec$title),
    welcome = text(spec$welcome),
    end_text = text(spec$end_text),
    groups = lapply(spec$groups, group),
    quotas = lapply(spec$quotas %||% list(), quota)
  )
  out[!vapply(out, is.null, logical(1))]
}
