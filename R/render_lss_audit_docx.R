#' Render the audit as a focused Word document (internal)
#'
#' Internal Word-document renderer used by [render_audit()] for the `.docx`
#' branch. The user-facing API and argument documentation live on
#' [render_audit()].
#'
#' @keywords internal
#' @noRd
.render_audit_docx <- function(lss, output, languages = NULL,
                                  logo = NULL,
                                  logo_width = 1.5,
                                  logo_height = 0.75,
                                  font = NULL,
                                  font_code = NULL,
                                  colors = NULL,
                                  authors = NULL,
                                  description = NULL,
                                  chrome_lang = NULL) {
  if (!inherits(lss, "lss")) {
    lssdoc_abort(
      "{.arg lss} must be an {.cls lss} object from {.fn read_lss}.",
      class = "lssdoc_bad_lss"
    )
  }
  if (!is.character(output) || length(output) != 1L || is.na(output)) {
    lssdoc_abort(
      "{.arg output} must be a single file path.",
      class = "lssdoc_bad_output"
    )
  }
  lss_validate_logo(logo)
  lss_validate_font(font, "font")
  lss_validate_font(font_code, "font_code")
  colors <- lss_validate_colors(colors)
  authors <- lss_normalize_authors(authors)
  description <- lss_normalize_description(description)
  for (pkg in c("officer", "flextable")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      lssdoc_abort(                                     # nocov start
        c(
          "Rendering a {.file .docx} document requires the {.pkg {pkg}} package.",
          "i" = "Install it with {.run install.packages(\"{pkg}\")}."
        ),
        class = "lssdoc_missing_suggest"
      )                                                 # nocov end
    }
  }

  cli::cli_progress_bar(
    name = "Building audit document",
    total = 3L,
    clear = TRUE
  )
  cli::cli_progress_update(set = 0L, status = "Running audit")
  model <- lss_model(lss, languages = languages)
  theme <- lss_render_theme()
  if (!is.null(font)) theme$font_body <- font
  if (!is.null(font_code)) theme$font_code <- font_code
  if (!is.null(colors)) theme <- utils::modifyList(theme, colors)
  chrome_lang <- lss_resolve_chrome_lang(chrome_lang, model$languages)
  theme$chrome <- lss_chrome_strings(chrome_lang)
  theme$chrome_lang <- chrome_lang
  audit <- audit_lss(lss)

  cli::cli_progress_update(set = 1L, status = "Rendering cover and findings")
  doc <- officer::read_docx()
  doc <- lss_render_cover(
    doc, lss, model, theme,
    subtitle = theme$chrome$cover_subtitle_audit,
    logo = logo, logo_width = logo_width, logo_height = logo_height,
    authors = authors, description = description
  )
  doc <- officer::body_add_break(doc)
  doc <- lss_render_audit_full(doc, audit, theme)

  cli::cli_progress_update(
    set = 2L, status = sprintf("Writing %s", basename(output))
  )
  section <- lss_render_section_props("A4-portrait", length(model$languages))
  doc <- officer::body_set_default_section(doc, section)
  print(doc, target = output)
  cli::cli_progress_update(set = 3L)
  cli::cli_progress_done()

  abs_path <- tryCatch(
    normalizePath(output, winslash = "/", mustWork = TRUE),
    error = function(e) output
  )
  size_kb <- round(file.size(output) / 1024)
  cli::cli_alert_success(
    "Saved {.file {abs_path}} ({size_kb} KB, {audit$n_findings} finding{?s})"
  )
  invisible(output)
}

#' Render the full audit body: summary plus one table per severity
#' @keywords internal
#' @noRd
lss_render_audit_full <- function(doc, audit, theme) {
  chrome <- theme$chrome
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      chrome$audit_findings_title,
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_heading1,
        bold = TRUE, color = theme$color_primary
      )
    )),
    style = "heading 1"
  )

  if (audit$n_findings == 0) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(officer::ftext(
        chrome$audit_no_anomalies,
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_question,
          color = theme$color_text, italic = TRUE
        )
      ))
    )
    return(doc)
  }

  summary_line <- sprintf(
    chrome$audit_summary_fmt,
    audit$n_findings, audit$n_errors, audit$n_warnings, audit$n_notes
  )
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      summary_line,
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_question,
        color = theme$color_text
      )
    ))
  )

  # Section titles per severity bucket. We pluralize the chrome
  # `audit_severity_*` token by hand for the most common case (English
  # +s) since per-language plural forms are out of scope here -- the
  # primary semantic info (severity color + count) is what the
  # reviewer scans for.
  sev_meta <- list(
    error = list(title = chrome$audit_severity_error,
                 color = theme$color_error),
    warning = list(title = chrome$audit_severity_warning,
                   color = theme$color_warning),
    note = list(title = chrome$audit_severity_note,
                color = theme$color_note)
  )
  for (sev in names(sev_meta)) {
    rows <- audit$findings[audit$findings$severity == sev, , drop = FALSE]
    if (nrow(rows) == 0) next
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(officer::ftext(
        sprintf("%s (%d)", sev_meta[[sev]]$title, nrow(rows)),
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_heading2,
          bold = TRUE, color = sev_meta[[sev]]$color
        )
      )),
      style = "heading 2"
    )
    ft <- flextable::flextable(
      rows[, c("check", "location", "language", "message"), drop = FALSE]
    )
    ft <- flextable::set_header_labels(
      ft,
      check    = chrome$audit_col_check,
      location = chrome$audit_col_location,
      language = chrome$audit_col_language,
      message  = chrome$audit_col_message
    )
    ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
    ft <- flextable::fontsize(ft, size = theme$size_answer, part = "all")
    ft <- flextable::bold(ft, part = "header")
    ft <- flextable::color(ft, color = theme$color_primary, part = "header")
    ft <- flextable::bg(ft, bg = theme$color_band, part = "header")
    ft <- flextable::border_remove(ft)
    thin <- officer::fp_border(color = theme$color_grid, width = 0.5)
    ft <- flextable::hline(ft, border = thin, part = "all")
    ft <- flextable::valign(ft, valign = "top", part = "body")
    ft <- flextable::valign(ft, valign = "center", part = "header")
    ft <- flextable::padding(ft, padding = 2, part = "all")
    # Cell-symmetric for readable columns (check, location, message):
    # header and body both left -- the column label reads as a word
    # above text paragraphs. "language" is asymmetric on purpose:
    # header left (the word "Language") above body codes centered
    # (the two-letter atoms `fr`, `de`, `en`).
    ft <- flextable::align(ft, align = "left",   part = "all")
    ft <- flextable::align(ft, align = "center", j = "language", part = "body")
    ft <- flextable::width(ft, j = "check", width = 1.6, unit = "in")
    ft <- flextable::width(ft, j = "location", width = 2.2, unit = "in")
    ft <- flextable::width(ft, j = "language", width = 0.5, unit = "in")
    ft <- flextable::width(ft, j = "message", width = 3.6, unit = "in")
    doc <- flextable::body_add_flextable(doc, ft, align = "center")
  }
  doc
}
