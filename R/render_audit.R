#' Render the audit as a focused Word or PDF document
#'
#' Build a short, action-oriented document containing only the audit
#' findings: the same cover page as the full questionnaire document,
#' summary counts, then one table per severity (errors, warnings,
#' notes) listing every finding with its location and message. Use it
#' for QA follow-up or to share issues with a colleague without
#' distributing the full questionnaire.
#'
#' @param input Either a path to a `.lss` file (character string) or a
#'   pre-parsed `lss` object returned by [read_lss()]. Passing a path
#'   parses it on the fly; passing an `lss` object avoids re-parsing in
#'   a workflow that already inspected the audit.
#' @param output Character. Path to the file to create. The extension
#'   determines the output format: `.docx` writes a Word document
#'   directly, `.pdf` writes a Word document into a temporary location
#'   and converts it locally via LibreOffice (or Word, on Windows).
#'   Any other extension is rejected with `lssdoc_bad_output_ext`.
#' @param languages Character vector of language codes used on the
#'   cover page. `NULL` (default) keeps all languages of the survey in
#'   their declared order.
#' @param logo Optional path (character) to a PNG or JPEG image
#'   displayed at the top of the cover page. `NULL` (default) keeps
#'   the cover logo-free.
#' @param logo_width,logo_height Image dimensions in inches. Defaults
#'   `1.5` and `0.75`, tuned to a 2:1 logo. Resize or pre-crop your
#'   image to fit a different aspect ratio.
#' @param font Optional body font name (character). `NULL` (default)
#'   keeps Calibri. See [render_questionnaire()] for guidance on
#'   overrides.
#' @param font_code Optional monospace font (character) used for
#'   code-like content (variable codes, raw expressions). `NULL`
#'   (default) keeps Consolas.
#' @param colors Optional named list of hex color overrides for the
#'   editorial petrol-blue palette. `NULL` (default) keeps the
#'   package palette intact. Same shape and accepted names as in
#'   [render_questionnaire()].
#' @param authors,description Optional cover-page credit block
#'   (`authors`) and free-form note (`description`). `NULL` (default)
#'   for both. Same shapes as in [render_questionnaire()].
#' @param chrome_lang Language used for the document chrome (column
#'   headers, row labels, audit section). One of `"en"`, `"fr"`,
#'   `"de"`, `"es"`, `"it"`. `NULL` (default) follows `languages[1]`
#'   when supported, otherwise falls back to `"en"`.
#'
#' @return The `output` path, invisibly.
#'
#' @seealso [audit_lss()] to inspect the same findings in the console;
#'   [render_questionnaire()] for the full questionnaire document.
#'
#' @examples
#' \dontrun{
#' # One-shot (path -> .docx)
#' render_audit(
#'   system.file("extdata", "demo_survey.lss",
#'               package = "lssdoc"),
#'   tempfile(fileext = ".docx")
#' )
#'
#' # PDF output -- same call, just pass a .pdf path
#' render_audit("survey.lss", "qa.pdf")
#' }
#' @export
render_audit <- function(
  input,
  output,
  languages = NULL,
  logo = NULL,
  logo_width = 1.5,
  logo_height = 0.75,
  font = NULL,
  font_code = NULL,
  colors = NULL,
  authors = NULL,
  description = NULL,
  chrome_lang = NULL
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
      .render_audit_pdf
    else
      .render_audit_docx,
    args
  )
}

#' Render an audit document as PDF via a temporary .docx
#'
#' @keywords internal
#' @noRd
.render_audit_pdf <- function(lss, output, ...) {
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  .render_audit_docx(lss, tmp, ...)
  .docx_to_pdf(tmp, output)
}
