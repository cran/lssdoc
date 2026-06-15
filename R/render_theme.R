# Theme defaults and argument normalization / validation.
#
# Extracted from R/render_lss_docx.R. Contains the editorial petrol-blue
# theme used by the cards and table templates, plus the validators for
# the user-facing colors, ont, ont_code, logo, uthors and
# description arguments. The validators emit classed errors so the
# rendering pipeline can be programmed against them.

#' Centralized visual theme for the rendered document
#'
#' `base_size` is the body type size in points (default 10): the question
#' text, item tables, meta band, quotas, variable index and cover metadata
#' all derive from it, so a single argument scales the whole document up
#' (e.g. `base_size = 12` for a roomier single-language render). The cover
#' title and subtitle keep their fixed title-page hierarchy.
#'
#' @keywords internal
#' @noRd
lss_render_theme <- function(base_size = 10L) {
  base_size <- as.integer(base_size)
  list(
    # Editorial petrol-blue palette tuned for scientific questionnaire
    # documentation. Replaces the earlier Office-blue family (#1F4E79 /
    # #2E75B6) which read as "Microsoft default". The petrol primary +
    # blue-green accent + soft blue-gray grid family follow the pattern
    # used by Pew Research / OECD / ESS publications.
    color_primary = "#133B52",
    color_accent  = "#3A7C8C",
    color_band    = "#E9F2F6",
    # Dark table header tone (secondary of the petrol family). Reserved
    # for the meta-table column headers so every item opens with a
    # white-on-dark "new variable" banner. Distinct from color_primary
    # so the group banners (1.5 pt #133B52 line + #133B52 title text)
    # keep their visual dominance over per-item banners.
    color_band_dark = "#1F4E5F",
    color_zebra   = "#F4F8FA",
    color_grid    = "#D3DCE2",
    color_text    = "#222222",
    color_muted   = "#6E6E6E",
    color_white   = "#FFFFFF",
    color_warning = "#C45911",
    color_error   = "#9E1B1B",
    color_note    = "#5B5B5B",

    # Single source of truth for the printable body width. This is the
    # A4-portrait default (8.27 in page - 2x0.98 in margins ~= 6.30 in); the
    # meta table, item table, audit/quota tables, welcome/end-text blocks,
    # and shared-scale table all use this width so group headings, the TOC,
    # and every table land on the same left and right edges. render_lss_docx()
    # overrides it per page orientation via lss_content_width_in() (9.72 in
    # for A4 landscape, 14.56 in for A3), so landscape widens every panel
    # automatically. Changing this value alone re-aligns the whole document.
    content_width_in = 6.30,

    # Body font: Calibri is pre-installed on Windows (and metric-substituted
    # with Carlito on Mac/Linux LibreOffice), so column widths computed at
    # render time match what the reader will see in every environment.
    # Override at the call site via render_questionnaire(font = "...") when the
    # reader's machine has a preferred face installed (Source Sans 3,
    # IBM Plex Sans, a corporate brand font, ...).
    font_body = "Calibri",
    # Monospace font used on code-like cells (the variable name column, the
    # raw relevance expression, the variable index entries). Consolas is
    # installed on every recent Windows; LibreOffice substitutes a similar
    # mono face, and Word on macOS picks Menlo. Override via
    # render_questionnaire(font_code = "JetBrains Mono") for sharper code style.
    font_code = "Consolas",
    # Content sizes derive from base_size so one argument scales the whole
    # body. Defaults (base_size 10): meta 8, lang header 9, question 10,
    # subq/answer 9, help 8, heading2 11, heading1 14.
    size_meta = base_size - 2L,
    size_lang_header = base_size - 1L,
    size_question = base_size,
    size_subq = base_size - 1L,
    size_answer = base_size - 1L,
    size_help = base_size - 2L,
    size_heading1 = base_size + 4L,
    size_heading2 = base_size + 1L,
    # Cover keeps its own title-page hierarchy; the metadata table follows
    # the body size so it harmonizes with the rest of the document.
    size_cover_title = 22L,
    size_cover_subtitle = 16L,
    size_cover_meta = base_size,

    empty_marker = "\u2014"
  )
}

#' Validate and normalize the `authors` argument
#'
#' Accepts three input shapes and returns a single normalized form: a
#' `list` (possibly empty) of `list(name, affiliation, orcid)`, each
#' a single character string (`""` when the field was not supplied).
#' Returning the same shape regardless of input means the renderer
#' does not need to branch on the API form.
#'
#' Recognized shapes:
#' - `NULL` -> `NULL` (renderer omits the block);
#' - **character vector** (named or unnamed): each entry becomes one
#'   author, with `affiliation = ""` if unnamed and `orcid = ""` always;
#' - **list of named lists**: each element is treated as a structured
#'   author with the fields `name` (required), `affiliation`, and
#'   `orcid` (both optional).
#'
#' @keywords internal
#' @noRd
lss_normalize_authors <- function(authors) {
  if (is.null(authors)) return(NULL)
  fail <- function() {
    lssdoc_abort(
      c(
        "{.arg authors} must be {.code NULL}, a character vector, or a list of named lists.",
        "i" = "See {.fn render_questionnaire} for the accepted shapes."
      ),
      class = "lssdoc_bad_authors"
    )
  }
  if (is.character(authors)) {
    if (any(is.na(authors))) fail()
    nm <- names(authors)
    out <- lapply(seq_along(authors), function(i) {
      name <- if (!is.null(nm) && nzchar(nm[i])) nm[i] else as.character(authors[i])
      affil <- if (!is.null(nm) && nzchar(nm[i])) as.character(authors[i]) else ""
      list(name = name, affiliation = affil, orcid = "")
    })
    return(out)
  }
  if (is.list(authors)) {
    out <- lapply(authors, function(a) {
      if (!is.list(a) || is.null(a$name) || !nzchar(trimws(as.character(a$name)))) fail()
      list(
        name = as.character(a$name),
        affiliation = if (is.null(a$affiliation)) "" else as.character(a$affiliation),
        orcid = if (is.null(a$orcid)) "" else as.character(a$orcid)
      )
    })
    return(out)
  }
  fail()
}

#' Normalize the `description` argument: NULL or a single non-empty
#' string. Returns NULL for missing/empty input so the renderer can
#' simply skip the block.
#'
#' @keywords internal
#' @noRd
lss_normalize_description <- function(description) {
  if (is.null(description)) return(NULL)
  if (!is.character(description) || length(description) != 1L ||
      is.na(description)) {
    lssdoc_abort(
      "{.arg description} must be {.code NULL} or a single string.",
      class = "lssdoc_bad_description"
    )
  }
  if (!nzchar(trimws(description))) return(NULL)
  description
}

#' Validate the optional font arguments: NULL or a single non-empty string.
#' We do not check that the font is installed (impossible to enumerate
#' reliably across Word / LibreOffice / Pages); if the reader's machine is
#' missing it, their application substitutes its own fallback face.
#' @keywords internal
#' @noRd
lss_validate_font <- function(value, arg_name) {
  if (is.null(value)) return(invisible())
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    lssdoc_abort(
      "{.arg {arg_name}} must be {.code NULL} or a single non-empty string.",
      class = "lssdoc_bad_font"
    )
  }
  invisible()
}

#' Validate and normalize the `colors` argument
#'
#' Returns a named list keyed by `color_<name>` (the actual theme
#' keys) so the renderer can `modifyList(theme, colors)` without
#' re-mapping. Accepts `NULL` (no override), a list with the eight
#' palette names (`primary`, `accent`, `band`, `band_dark`, `zebra`,
#' `grid`, `text`, `muted`), each value a hex color string
#' (`"#XXXXXX"` or `"#XXX"`).
#'
#' Unknown names are rejected with a classed condition so a typo
#' (e.g. `"primay"`) never silently turns into a no-op.
#'
#' @keywords internal
#' @noRd
lss_validate_colors <- function(colors) {
  if (is.null(colors)) return(NULL)
  if (!is.list(colors) || is.null(names(colors)) ||
      any(!nzchar(names(colors)))) {
    lssdoc_abort(
      "{.arg colors} must be {.code NULL} or a named list of hex colors.",
      class = "lssdoc_bad_colors"
    )
  }
  allowed <- c("primary", "accent", "band", "band_dark", "zebra",
               "grid", "text", "muted")
  unknown <- setdiff(names(colors), allowed)
  if (length(unknown) > 0L) {
    lssdoc_abort(
      c(
        "{.arg colors} has unknown name{?s}: {.val {unknown}}.",
        "i" = "Accepted: {.val {allowed}}."
      ),
      class = "lssdoc_bad_colors"
    )
  }
  hex_re <- "^#(?:[0-9a-fA-F]{3}|[0-9a-fA-F]{6})$"
  bad_hex <- vapply(
    colors,
    function(v) {
      !is.character(v) || length(v) != 1L || is.na(v) ||
        !grepl(hex_re, v)
    },
    logical(1L)
  )
  if (any(bad_hex)) {
    lssdoc_abort(
      c(
        "Every value in {.arg colors} must be a hex string ({.code \"#XXXXXX\"} or {.code \"#XXX\"}).",
        "i" = "Invalid: {.val {names(colors)[bad_hex]}}."
      ),
      class = "lssdoc_bad_colors"
    )
  }
  stats::setNames(colors, paste0("color_", names(colors)))
}

#' Validate the optional logo argument: NULL or an existing image path
#' @keywords internal
#' @noRd
lss_validate_logo <- function(logo) {
  if (is.null(logo)) {
    return(invisible())
  }
  if (!is.character(logo) || length(logo) != 1L || is.na(logo)) {
    lssdoc_abort(
      "{.arg logo} must be {.code NULL} or a single file path.",
      class = "lssdoc_bad_logo"
    )
  }
  if (!file.exists(logo)) {
    lssdoc_abort(
      "Cannot find a logo file at {.path {logo}}.",
      class = "lssdoc_logo_not_found"
    )
  }
  ext <- tolower(tools::file_ext(logo))
  if (!ext %in% c("png", "jpg", "jpeg")) {
    lssdoc_abort(
      c(
        "{.arg logo} must be a PNG or JPEG image; got {.val {ext}}.",
        "i" = "Convert your image to PNG or JPEG and retry."
      ),
      class = "lssdoc_bad_logo_format"
    )
  }
  invisible()
}

#' Human-readable label for a language code
#' @keywords internal
#' @noRd
