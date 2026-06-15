#' Render a LimeSurvey questionnaire to a Word or PDF document
#'
#' Build a professional questionnaire document from a LimeSurvey survey,
#' displaying up to four languages side by side. Each question becomes a
#' compact `flextable` with a meta header (variable code, type, mandatory,
#' filter) shown once, language column headers, the question text per
#' language, and the subquestion or answer-option rows underneath -- codes
#' on the left, labels per language on the right. Headings, a metadata
#' cover page, an optional table of contents, and an optional audit summary
#' tie the document together. Rendering uses the suggested packages
#' \pkg{officer} and \pkg{flextable}; both must be installed.
#'
#' @param input Either a path to a `.lss` file (character string) or a
#'   pre-parsed `lss` object returned by [read_lss()]. Passing a path
#'   parses it on the fly. Passing an `lss` object avoids re-parsing
#'   when the same survey is rendered with different options (e.g.
#'   multiple language subsets) in the same session.
#' @param output Character. Path to the file to create. The extension
#'   determines the output format: `.docx` writes a Word document
#'   directly, `.pdf` writes a Word document into a temporary location
#'   and converts it locally via LibreOffice (or Word, on Windows).
#'   Any other extension is rejected with `lssdoc_bad_output_ext`.
#' @param languages Character vector of language codes to display,
#'   **in the order they will appear as columns**. `NULL` (default)
#'   keeps all languages found in the `.lss` file in the order of the
#'   `<languages>` section. Acts both as a subset filter and an
#'   ordering: e.g. `c("en", "fr")` puts English first, `c("fr", "en")`
#'   puts French first. `languages[1]` is treated as the primary
#'   language (TOC entries, group fallback). Requesting a language
#'   absent from the survey is an error (`lssdoc_unknown_language`).
#' @param template Output style. One of `"cards"` (the default) or
#'   `"table"`.
#'   * `"cards"` renders one detached pair of tables per item (meta
#'     table + item table), stacked vertically, with content
#'     languages displayed side-by-side in the item table.
#'   * `"table"` renders a single dense table covering the whole
#'     document: every variable is one tinted Question row carrying
#'     `No | Variable | Type | Mand. | Filter`, followed by one or
#'     more white Value rows. Group banners (and a group's
#'     description, when the author wrote one) become section rows;
#'     the column header repeats on every page. The body font
#'     steps down for three or four languages so the columns stay
#'     within the portrait content width.
#' @param layout Reserved for future use. Currently `"auto"` only.
#' @param show_audit Logical. If `TRUE` (default), include an audit
#'   summary section near the top and inline markers on questions
#'   that carry findings. Set to `FALSE` for a clean reading copy.
#' @param show_help Logical. If `TRUE` (default), include question
#'   help texts under the question text.
#' @param show_attrs Character vector of question attributes to surface
#'   under the question text when present. Default keeps the attributes
#'   that change how respondents see the item: `"prefix"`, `"suffix"`,
#'   `"other_replace_text"`, `"validation"`. Add `"exclude_all_others"`
#'   or `"exclude_all_others_auto"` to also surface the row-level
#'   exclusivity flags (debug-style). Pass `character(0)` to hide all.
#' @param show_technical_attrs Logical. If `TRUE`, include technical
#'   attributes such as `answer_order` and `location_*`. `FALSE` (the
#'   default) hides them.
#' @param page_format Page format. One of `"auto"` (the default),
#'   `"A4-portrait"`, `"A4-landscape"`, or `"A3"`. `"auto"` is
#'   template-aware and never follows the language count (four languages
#'   fit on A4 portrait): the `"cards"` template gets A4 portrait, while
#'   the dense `"table"` template gets A4 landscape, which it needs to
#'   stay readable. Pass an explicit value to override; selecting a wider
#'   page makes every panel (meta table, item table, audit/quota tables
#'   and the dense codebook table) expand automatically to fill the
#'   content width, in both templates.
#' @param show_toc Logical. If `TRUE` (default), include a table of
#'   contents listing the groups (skipped automatically when the
#'   survey has fewer than two groups). For per-variable navigation,
#'   use `show_index`.
#' @param show_index Logical. If `TRUE` (default), append a variable
#'   index at the end of the document listing every item code with
#'   its number, sorted alphabetically.
#' @param show_quotas Logical. If `TRUE` (default), append a quotas
#'   section (after the end text, before the variable index) listing
#'   each sampling quota: its localized name, status (active, limit and
#'   action when full), the membership condition resolved to question
#'   codes and answer labels, and the localized "quota full" message.
#'   Skipped when the survey defines no quotas.
#' @param show_header_title Logical. If `TRUE` (default), show the
#'   survey title at the top right of every page (one line per
#'   displayed language, truncated to 80 characters). `FALSE` keeps
#'   only the `X/Y` page counter at the bottom right.
#' @param show_source Logical. If `TRUE` (default), show the
#'   **Source file** name and **Survey ID** rows in the cover
#'   metadata table. Pass `FALSE` to hide both (e.g. when sharing
#'   without exposing the LimeSurvey internals).
#' @param show_item_heading Logical. If `FALSE` (the default), the
#'   meta table starts each item directly, for a compact layout. If
#'   `TRUE`, a bold `"N. variable"` heading is added above each item
#'   for scroll-time navigation.
#' @param show_raw_filter Logical. If `FALSE` (the default), the
#'   Filter cell shows only the human-readable form (e.g.
#'   `Q1 = 1`) -- editorial codebook style, matching ESS / MOSAiCH /
#'   GESIS conventions. Set to `TRUE` to also surface the raw
#'   LimeSurvey relevance expression underneath in small italic
#'   gray (e.g. `!is_empty(Q1.NAOK) && (Q1.NAOK == 1)`), useful for
#'   QA cross-checks. The raw form is always shown when the plain
#'   form could not be simplified.
#' @param show_groups Logical. If `TRUE` (default), show the group
#'   banners (cards layout) or group rows (table layout). Pass
#'   `FALSE` to flatten the document into a single sequence of items
#'   with no section breaks (useful when groups exist only as
#'   internal organization).
#' @param show_welcome Logical. If `TRUE` (default), include the
#'   survey's multilingual welcome text (`surveyls_welcometext`) as
#'   a side-by-side block (cards) or embedded row (table).
#' @param show_endtext Logical. If `TRUE` (default), include the
#'   survey's multilingual end text (`surveyls_endtext`). Same
#'   treatment as `show_welcome`.
#' @param show_description Logical. If `TRUE` (default), include the
#'   survey's multilingual description (`surveyls_description`) --
#'   the "what this survey is about" intro that LimeSurvey shows
#'   above the welcome text on the landing page.
#' @param show_consent Logical. If `TRUE` (default), render a data
#'   protection and consent block in the front matter (before the
#'   welcome text): the survey's privacy policy notice
#'   (`surveyls_policy_notice`) and its consent checkbox label
#'   (`surveyls_policy_notice_label`), side by side across languages,
#'   with the checkbox drawn as an empty box. Skipped when the survey
#'   turns the policy notice off or carries no notice text.
#' @param show_privacy_settings Logical. If `FALSE` (the default),
#'   omit the survey-level privacy / tracking flags from the cover.
#'   Set to `TRUE` to surface `anonymized`, `save` partial,
#'   `datestamp`, `ipaddr`, and `refurl` rows -- useful for ethics
#'   committee submissions.
#' @param show_admin_settings Logical. If `FALSE` (the default),
#'   omit the survey-level administrative settings. Set to `TRUE`
#'   to surface `alias`, end URL with description, and `active`
#'   flag rows.
#' @param title Optional override of the survey title shown on the
#'   cover and the top-right header. `NULL` (default) uses the
#'   per-language titles from the `.lss` survey settings. Pass a
#'   single string to use the same title in every displayed
#'   language, or a named character vector keyed by language code
#'   (e.g. `c(fr = "Mon titre", de = "Mein Titel")`) for per-language
#'   overrides.
#' @param logo Optional path (character) to a PNG or JPEG image
#'   displayed at the top of the cover page. `NULL` (default) keeps
#'   the cover logo-free, matching the neutral style of
#'   survey-methodology references (ESS, MOSAiCH, Panel). The `.lss`
#'   file does not embed a logo, so this image must be supplied by
#'   the caller.
#' @param logo_width,logo_height Image dimensions in inches.
#'   Defaults `1.5` and `0.75`, tuned to a 2:1 logo. Resize or
#'   pre-crop your image to fit a different aspect ratio.
#' @param font Optional body font name (character). `NULL` (default)
#'   keeps Calibri, which is pre-installed on every recent Windows
#'   Office and metric-substituted with Carlito (OFL) on Mac and
#'   Linux LibreOffice, so column widths stay stable across
#'   platforms. Pass any string to override (e.g. `"Source Sans 3"`,
#'   `"IBM Plex Sans"`, or a corporate brand font); install the font
#'   on the machine that opens the document, otherwise the reader's
#'   application substitutes its own fallback face.
#' @param font_code Optional monospace font name (character) used
#'   for code-like content: the variable column in each meta table,
#'   the raw relevance expression under each filter cell, and the
#'   variable index entries. `NULL` (default) keeps Consolas; pass
#'   `"JetBrains Mono"` or `"IBM Plex Mono"` for sharper code style.
#' @param colors Optional named list of hex color overrides for the
#'   editorial petrol-blue palette. `NULL` (default) keeps the
#'   package palette intact. Accepted names: `"primary"` (group
#'   filets, headers text and item top borders), `"accent"`
#'   (hyperlinks, ORCID iD, URL auto-links, group under-line in
#'   cards), `"band"` (light header backgrounds), `"band_dark"`
#'   (the meta-table dark header in cards), `"zebra"` (the
#'   very-light tint on Question rows in the `table` template),
#'   `"grid"` (the 0.5 pt border color), `"text"`, `"muted"`. Each
#'   value must be a hex string (`"#XXXXXX"` or `"#XXX"`). Unknown
#'   keys are rejected (`lssdoc_bad_colors`). Useful for honoring an
#'   institutional brand: e.g.
#'   `colors = list(primary = "#5C9F1A", accent = "#7FA82E")`
#'   produces a LimeSurvey-green document.
#' @param authors Optional credit block for the questionnaire's
#'   designers, displayed on the cover page below the subtitle.
#'   Each author is shown centered on its own line as
#'   `Name -- Affiliation`; when an ORCID iD is provided, a smaller
#'   monospace line below shows `ORCID 0000-0000-0000-0000` as a
#'   hyperlink to `https://orcid.org/<id>`. Accepts:
#'   * `NULL` (default): no authorship block.
#'   * An **unnamed character vector** (`c("Jane Doe", "John Doe")`):
#'     each entry becomes a line with no affiliation.
#'   * A **named character vector**
#'     (`c("Jane Doe" = "HESAV")`): names are authors,
#'     values are affiliations. Use `""` to render an author without
#'     affiliation.
#'   * A **list of named lists** for the full form, e.g.
#'     `list(list(name = "Jane Doe", affiliation = "HESAV",
#'     orcid = "0009-0001-2345-6789"), list(name = "John Doe",
#'     affiliation = "HESAV"))`. The `name` field is required;
#'     `affiliation` and `orcid` are optional.
#' @param description Optional free-form text (single string) shown
#'   on the cover page below the authors block. `NULL` (default)
#'   omits the block. Useful for a citation hint, a funding
#'   acknowledgement, a methodology note, or a link to a related
#'   publication. Line breaks (`\n`) split the block into separate
#'   centered lines; `http://` and `https://` tokens are rendered
#'   as clickable hyperlinks.
#' @param chrome_lang Language used for the **chrome** of the document
#'   (column headers, row labels, navigation titles, type labels,
#'   Value descriptors, audit section). One of `"en"`, `"fr"`,
#'   `"de"`, `"es"`, `"it"`. `NULL` (default) follows `languages[1]`
#'   when supported, otherwise falls back to `"en"`. Independent
#'   from `languages`, which controls the survey's content columns:
#'   e.g. `chrome_lang = "en"` with `languages = c("fr", "en")`
#'   produces an English-labelled document with French and English
#'   content. Spanish and Italian translations should be reviewed
#'   by a native speaker before publishing an official document.
#' @param variable_names How response-variable names are written, so the
#'   document matches the data file the reader holds. One of:
#'   * `"brackets"` (default) -- the exact column names of the
#'     **CSV / Excel data export**, so the variable index reproduces the
#'     raw data file column for column: `parent[subq]`,
#'     `parent[subq][1]` (dual scale), `parent[CH]` / `parent[other]`
#'     (multiple choice), `parent[59842]` (ranking, by answer id),
#'     `parent[_Ccomment]` (list-with-comment).
#'   * `"underscore"` -- the sanitized code form used by the Expression
#'     Manager / relevance equations and the SPSS / Stata / R exports
#'     (`parent_subq`, `parent_subq_1`).
#'   The two-dimensional arrays (array of numbers / texts with a second
#'   axis) and ranking questions are expanded so every produced column
#'   appears as its own entry either way.
#' @param base_size Body type size in points (default `10`). One lever
#'   scales the whole document: question text, item tables, the meta band,
#'   the quotas table, the variable index and the cover metadata all
#'   follow it, while headings and answer/help text keep their relative
#'   offsets. Useful for a roomier single-language render (e.g. `12`). The
#'   cover title and subtitle keep their fixed title-page sizes. Accepted
#'   range: 7 to 16.
#'
#' @section "LimeSurvey last save" date on the cover:
#' The cover metadata table carries a row labelled
#' *"LimeSurvey last save"* (or its localized equivalent). It is read
#' verbatim from the `surveys.lastmodified` column of the `.lss`,
#' which is the only timestamp LimeSurvey writes into the export --
#' no other table (`questions`, `question_l10ns`, `answer_l10ns`,
#' `groups`, etc.) carries a per-row modification date. The row is
#' named "last save" rather than "last modified" because LimeSurvey
#' only bumps that field reliably when the user clicks **Save** on a
#' survey-level form (Settings tab); editing a question text, an
#' answer label, or a translation through the Question Editor does
#' **not** consistently update it across LimeSurvey versions. If the
#' date looks stale relative to your most recent edits, the
#' workaround is to open *Survey settings* in LimeSurvey, click
#' **Save** (no other change needed), then re-export the `.lss`. The
#' next render will show the bumped timestamp.
#'
#' @section Field-update prompt in Word:
#' Opening the rendered `.docx` in Microsoft Word may surface a
#' security-style prompt: *"This document contains fields that may
#' refer to other files. Do you want to update the fields in this
#' document?"*. This is expected: the package marks the page-number
#' and bookmark-reference fields as needing a refresh so the footer
#' shows the correct page count and the table of contents links
#' resolve to the right pages on first open (this is also what makes
#' headless PDF conversion via LibreOffice produce correctly
#' paginated output without a manual F9). Clicking **Yes** is safe --
#' the document has no `INCLUDETEXT`, `INCLUDEPICTURE`-linked, or DDE
#' fields; the only external links are the ORCID and DOI URLs in the
#' cover credits, which are static `HYPERLINK` targets and not fetched
#' on update.
#'
#' @section PDF output:
#' When `output` ends in `.pdf`, the function first renders a `.docx`
#' to a temporary location and then converts it locally via
#' LibreOffice headless (or Word on Windows). LibreOffice
#' (`soffice` executable) must be installed and on `PATH`; otherwise
#' a classed error explains how to install it. Conversion stays on
#' the user's machine: no upload, no network call. LibreOffice
#' headless does not refresh Word field values (TOC, page counts)
#' during conversion, so the table of contents may appear empty in
#' the converted PDF. To obtain a PDF with a populated TOC, render to
#' `.docx` instead, open it in Word (the TOC refreshes automatically)
#' and use `File > Save As > PDF`.
#'
#' @return The `output` path, invisibly.
#'
#' @seealso [render_audit()] for the audit-only document;
#'   [audit_lss()] to inspect findings in the console without
#'   rendering; [read_lss()] to pre-parse a `.lss` file once and
#'   render multiple variants.
#'
#' @examples
#' \dontrun{
#' file <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
#'
#' # One-shot: parse + render Word document
#' render_questionnaire(file, tempfile(fileext = ".docx"))
#'
#' # Same call, PDF output (format inferred from extension)
#' render_questionnaire(file, tempfile(fileext = ".pdf"))
#'
#' # Parse once, render several variants without re-parsing
#' lss <- read_lss(file)
#' render_questionnaire(lss, tempfile(fileext = ".docx"),
#'                      languages = "en")
#' render_questionnaire(lss, tempfile(fileext = ".docx"),
#'                      template = "table",
#'                      languages = c("en", "fr"))
#'
#' # Branded cover with authors block and palette override
#' render_questionnaire(
#'   lss,
#'   tempfile(fileext = ".docx"),
#'   template    = "table",
#'   chrome_lang = "en",
#'   colors      = list(primary = "#5C9F1A", accent = "#7FA82E"),
#'   authors     = list(
#'     list(name = "Jane Doe", affiliation = "HESAV",
#'          orcid = "0009-0001-2345-6789"),
#'     list(name = "John Doe", affiliation = "HESAV",
#'          orcid = "0009-0002-3456-7890")
#'   )
#' )
#' }
#' @export
render_questionnaire <- function(
  input,
  output,
  languages = NULL,
  template = c("cards", "table"),
  layout = c("auto", "side-by-side", "stacked"),
  show_audit = TRUE,
  show_help = TRUE,
  show_attrs = c("prefix", "suffix", "other_replace_text", "validation"),
  show_technical_attrs = FALSE,
  page_format = c("auto", "A4-portrait", "A4-landscape", "A3"),
  show_toc = TRUE,
  show_index = TRUE,
  show_quotas = TRUE,
  show_header_title = TRUE,
  show_source = TRUE,
  show_item_heading = FALSE,
  show_raw_filter = FALSE,
  show_groups = TRUE,
  show_welcome = TRUE,
  show_endtext = TRUE,
  show_description = TRUE,
  show_consent = TRUE,
  show_privacy_settings = FALSE,
  show_admin_settings = FALSE,
  title = NULL,
  logo = NULL,
  logo_width = 1.5,
  logo_height = 0.75,
  font = NULL,
  font_code = NULL,
  colors = NULL,
  authors = NULL,
  description = NULL,
  chrome_lang = NULL,
  variable_names = c("brackets", "underscore"),
  base_size = 10L
) {
  lss <- lss_resolve_input(input)
  if (!is.character(output) || length(output) != 1L || is.na(output)) {
    lssdoc_abort(
      "{.arg output} must be a single file path.",
      class = "lssdoc_bad_output"
    )
  }
  args <- mget(setdiff(names(formals()), "input"))
  args$lss <- lss
  do.call(
    if (identical(lss_detect_output_format(output), "pdf"))
      .render_questionnaire_pdf
    else
      .render_questionnaire_docx,
    args
  )
}

#' Detect the output format from the file extension
#'
#' @keywords internal
#' @noRd
lss_detect_output_format <- function(output) {
  ext <- tolower(tools::file_ext(output))
  if (identical(ext, "docx")) return("docx")
  if (identical(ext, "pdf")) return("pdf")
  lssdoc_abort(
    c(
      "Output path must end in {.file .docx} or {.file .pdf}.",
      "x" = "Got {.file {basename(output)}}."
    ),
    class = "lssdoc_bad_output_ext"
  )
}

#' Render a questionnaire as PDF via a temporary .docx
#'
#' @keywords internal
#' @noRd
.render_questionnaire_pdf <- function(lss, output, ...) {
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  .render_questionnaire_docx(lss, tmp, ...)
  .docx_to_pdf(tmp, output)
}
