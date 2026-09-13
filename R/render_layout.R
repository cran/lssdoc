# Word page layout: section properties, header/footer construction, title resolution and the post-render field-update injection.
#
# Extracted from R/render_lss_docx.R.

#' Inject `<w:updateFields w:val="true"/>` into the .docx settings.xml
#'
#' This Word setting tells the reader application (Word, LibreOffice) to
#' refresh every field in the document when it is opened. For our use
#' that means the table of contents and the page-number fields populate
#' immediately, without the reader having to press F9. It also propagates
#' to headless PDF conversion via LibreOffice, which honors the flag.
#'
#' Post-processes the .docx zip in place: unzips to a temp directory,
#' modifies `word/settings.xml`, repacks. Silent no-op if the file does
#' not contain a settings.xml (it always does for officer output, but
#' we stay defensive).
#'
#' @keywords internal
#' @noRd
lss_inject_update_fields <- function(docx_path) {
  if (!file.exists(docx_path)) return(invisible(docx_path))
  # Silently no-op when `zip` is missing; the .docx is still valid, just
  # without auto-refresh. (In practice `zip` is always available when
  # `officer` is, because officer imports it.)
  if (!requireNamespace("zip", quietly = TRUE)) {
    return(invisible(docx_path))                        # nocov
  }
  docx_path <- normalizePath(docx_path, mustWork = TRUE)
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  utils::unzip(docx_path, exdir = tmp)

  # Step 1: settings.xml -- tell Word to update fields on open.
  settings_file <- file.path(tmp, "word", "settings.xml")
  if (file.exists(settings_file)) {
    raw_in <- readBin(settings_file, what = "raw",
                      n = file.info(settings_file)$size)
    content <- rawToChar(raw_in)
    content <- gsub("<w:updateFields\\b[^/]*/>", "", content, perl = TRUE)
    content <- sub(
      "</w:settings>",
      "<w:updateFields w:val=\"true\"/></w:settings>",
      content, fixed = TRUE
    )
    writeBin(charToRaw(content), settings_file, useBytes = TRUE)
  }

  # Step 2: mark every field 'dirty' so Word refreshes them on open.
  # Fields live in document.xml (the TOC) but ALSO in footerN.xml /
  # headerN.xml (the PAGE and NUMPAGES fields), so we patch every XML
  # part that might contain a fldChar.
  word_dir <- file.path(tmp, "word")
  field_files <- list.files(
    word_dir,
    pattern = "^(document|footer[0-9]*|header[0-9]*)\\.xml$",
    full.names = TRUE
  )
  for (f in field_files) {
    raw_in <- readBin(f, what = "raw", n = file.info(f)$size)
    content <- rawToChar(raw_in)
    # Strip any existing w:dirty on begin fldChars so the subsequent add
    # does not duplicate.
    content <- gsub(
      '(<w:fldChar [^/>]*?w:fldCharType="begin"[^/>]*?)\\s+w:dirty="[^"]*"([^/>]*?)/>',
      '\\1\\2/>',
      content, perl = TRUE
    )
    # Add w:dirty="1" to every begin fldChar.
    content <- gsub(
      '(<w:fldChar [^/>]*?w:fldCharType="begin"[^/>]*?)/>',
      '\\1 w:dirty="1"/>',
      content, perl = TRUE
    )
    writeBin(charToRaw(content), f, useBytes = TRUE)
  }

  # Repack: switch to the temp dir so the relative paths returned by
  # list.files are stored as-is inside the .docx zip
  # ('word/settings.xml', etc.), matching the layout Word and LibreOffice
  # expect. `all.files = TRUE` is critical: a .docx contains hidden
  # entries like `_rels/.rels` whose basename starts with a dot.
  if (file.exists(docx_path)) file.remove(docx_path)
  files <- list.files(tmp, recursive = TRUE, all.files = TRUE,
                      no.. = TRUE)
  old_wd <- getwd()
  setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)
  zip::zip(zipfile = docx_path, files = files, compression_level = 9)
  invisible(docx_path)
}

#' Resolve the final per-language titles from the user `title` argument
#'
#' Returns a named character vector keyed by `langs`. When `title` is
#' `NULL`, the survey-language settings of the `.lss` are used. A bare
#' string is used for every language. A named vector is matched on
#' language code; missing entries fall back to the `.lss` setting.
#'
#' @keywords internal
#' @noRd
lss_resolve_titles <- function(title, lss, langs) {
  defaults <- lss_header_titles(lss, langs)
  names(defaults) <- langs
  if (is.null(title)) {
    return(defaults)
  }
  if (length(title) == 1L && is.null(names(title))) {
    return(stats::setNames(rep(as.character(title), length(langs)), langs))
  }
  out <- defaults
  for (lg in intersect(langs, names(title))) {
    out[[lg]] <- as.character(title[[lg]])
  }
  out
}

#' Per-language survey titles for the header, in the order of `langs`.
#'
#' Empty / missing titles produce empty strings (the header helper
#' skips them); the order of the returned vector matches `langs` so
#' the primary language shows on the top line.
#'
#' @keywords internal
#' @noRd
lss_header_titles <- function(lss, langs) {
  ls_settings <- lss$survey_language_settings
  if (is.null(ls_settings) || nrow(ls_settings) == 0L) {
    return(rep("", length(langs)))
  }
  vapply(langs, function(lg) {
    v <- ls_settings$surveyls_title[ls_settings$surveyls_language == lg]
    if (length(v) == 0L) "" else as.character(v[1])
  }, character(1))
}

# Side margins (in) applied on every page, both orientations. The usable
# content width derives from this, so it is defined once and shared by
# lss_content_width_in() and lss_render_section_props().
lss_margin_side_in <- 0.98

#' Usable body width (in) for a page format
#'
#' The meta table, item table, audit table, quota table and the dense
#' table are all laid out to this width so they sit flush
#' between the margins instead of overflowing the page or leaving an
#' empty band. It depends ONLY on the page orientation (never on the
#' language count): portrait fits 6.30 in, A4 landscape 9.72 in, A3
#' landscape 14.56 in. `"auto"` resolves to portrait, matching
#' [lss_render_section_props()].
#'
#' Anchored on the canonical 6.30 in portrait width (the theme default)
#' plus the page-width delta of the wider format, so the portrait number
#' stays exactly 6.30 and the landscape / A3 widths land just inside the
#' real printable width (page width - 2 x 0.98 in margin), never over it.
#'
#' @keywords internal
#' @noRd
lss_content_width_in <- function(page_format) {
  extra <- switch(
    if (identical(page_format, "auto")) "A4-portrait" else page_format,
    "A4-portrait"  = 0,
    "A4-landscape" = 11.69 - 8.27,
    "A3"           = 16.53 - 8.27
  )
  6.30 + extra
}

#' Pick page-section properties for the given format and language count
#'
#' Also installs the default footer with a centered "Page X of Y" field so
#' every page carries a number. The first page (cover) keeps the same
#' footer; turning it off on the cover would require a Word "different
#' first page" section setting that `prop_section()` does not expose.
#'
#' @keywords internal
#' @noRd
lss_render_section_props <- function(page_format, n_langs,
                                     theme = lss_render_theme(),
                                     header_titles = character(0)) {
  if (identical(page_format, "auto")) {
    # Portrait for every language count: the body panels are all sized
    # to the 6.30 in portrait content width, so landscape would only add
    # empty right margin. Callers wanting landscape pass it explicitly.
    page_format <- "A4-portrait"
  }
  size <- switch(
    page_format,
    "A4-portrait"  = officer::page_size(width = 8.27, height = 11.69, orient = "portrait"),
    "A4-landscape" = officer::page_size(width = 11.69, height = 8.27, orient = "landscape"),
    "A3"           = officer::page_size(width = 16.53, height = 11.69, orient = "landscape")
  )
  # 2.5 cm side margins on A4 portrait leave exactly theme$content_width_in
  # (6.30 in) for body content, so the meta table, item table, welcome
  # block, shared scale, and any other 6.30-in panel align flush with the
  # left and right margins. Top and bottom margins are slightly larger
  # (1.0 in) than the sides so the running header and body keep some air
  # between them; otherwise the first line of body content lands right
  # under the title strip.
  margin_side <- lss_margin_side_in
  margin_vert <- 1.0
  officer::prop_section(
    page_size = size,
    page_margins = officer::page_mar(
      top = margin_vert, bottom = margin_vert,
      left = margin_side, right = margin_side,
      header = 0.4, footer = 0.4
    ),
    header_default = lss_build_header(theme, header_titles),
    footer_default = lss_build_footer(theme)
  )
}

#' Page header with the survey title(s), right-aligned, one line per language.
#'
#' Each language gets its own line so long titles never collide. Titles
#' longer than 80 characters are truncated with a trailing ellipsis to
#' keep the header to a single visual line per language.
#'
#' @param header_titles Character vector, one entry per displayed language.
#'   Empty entries are skipped. When the vector is empty (the user opted
#'   out via `show_header_title = FALSE`), the header is left blank.
#' @keywords internal
#' @noRd
lss_build_header <- function(theme, header_titles = character(0)) {
  # Return NULL (not an empty block_list) when there is no title to
  # show; officer chokes on an empty header_default during section
  # processing.
  if (length(header_titles) == 0L) return(NULL)
  muted <- officer::fp_text(
    font.family = theme$font_body,
    font.size = theme$size_meta,
    color = theme$color_muted
  )
  right_align <- officer::fp_par(text.align = "right")
  pars <- lapply(header_titles, function(t) {
    if (is.null(t) || is.na(t) || !nzchar(trimws(t))) return(NULL)
    officer::fpar(
      officer::ftext(lss_truncate_title(t), prop = muted),
      fp_p = right_align
    )
  })
  pars <- Filter(Negate(is.null), pars)
  if (length(pars) == 0L) return(NULL)
  do.call(officer::block_list, pars)
}

#' Page footer with a compact `X/Y` page counter, right-aligned.
#' @keywords internal
#' @noRd
lss_build_footer <- function(theme) {
  # Two points larger than the running header (size_meta) so the X/Y page
  # counter stays legible -- 9 pt read a touch small for the footer, 10 pt
  # gives it presence without competing with the body.
  muted <- officer::fp_text(
    font.family = theme$font_body,
    font.size = theme$size_meta + 2L,
    color = theme$color_muted
  )
  officer::block_list(
    officer::fpar(
      officer::run_word_field("PAGE", prop = muted),
      officer::ftext("/", prop = muted),
      officer::run_word_field("NUMPAGES", prop = muted),
      fp_p = officer::fp_par(text.align = "right")
    )
  )
}

