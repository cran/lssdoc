#' Render a questionnaire to a Word document (internal)
#'
#' Internal Word-document renderer used by [render_questionnaire()] for the
#' `.docx` branch. The user-facing API and argument documentation live on
#' [render_questionnaire()].
#'
#' @keywords internal
#' @noRd
.render_questionnaire_docx <- function(
  lss,
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
  template <- rlang::arg_match(template)
  layout <- rlang::arg_match(layout)
  page_format <- rlang::arg_match(page_format)
  variable_names <- rlang::arg_match(variable_names)
  if (!is.numeric(base_size) || length(base_size) != 1L || is.na(base_size) ||
      base_size < 7L || base_size > 16L) {
    lssdoc_abort(
      "{.arg base_size} must be a single number between 7 and 16 (points).",
      class = "lssdoc_bad_base_size"
    )
  }
  base_size <- as.integer(round(base_size))
  # "auto" is template-aware: the dense codebook ("table") is too wide to
  # read in portrait, so it defaults to A4 landscape; the spacious "cards"
  # layout stacks comfortably in portrait. An explicit page_format is always
  # honored as given, for either template.
  if (identical(page_format, "auto")) {
    page_format <- if (identical(template, "table")) {
      "A4-landscape"
    } else {
      "A4-portrait"
    }
  }
  # Page orientation follows the template, never the language count: "auto"
  # gives cards A4 portrait and the dense table A4 landscape (resolved just
  # above). Landscape / A3 stay available as explicit opt-ins via
  # `page_format`; selecting one widens every panel through
  # lss_content_width_in() (see below).
  lss_validate_logo(logo)
  lss_validate_font(font, "font")
  lss_validate_font(font_code, "font_code")
  colors <- lss_validate_colors(colors)
  authors <- lss_normalize_authors(authors)
  description <- lss_normalize_description(description)
  for (pkg in c("officer", "flextable")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      lssdoc_abort(
        c(
          "Rendering a {.file .docx} document requires the {.pkg {pkg}} package.",
          "i" = "Install it with {.run install.packages(\"{pkg}\")}."
        ),
        class = "lssdoc_missing_suggest"
      )
    }
  }

  # Build the model first so we know the group count and can budget the
  # progress bar correctly. This step is fast (< 1 s).
  model <- lss_model(lss, languages = languages)
  langs <- model$languages
  theme <- lss_render_theme(base_size)
  # The usable body width follows the chosen page orientation (never the
  # language count): every full-width panel (meta table, item table, audit
  # and quota tables, the dense codebook table) lays out to this width, so
  # passing page_format = "A4-landscape" / "A3" widens them automatically.
  theme$content_width_in <- lss_content_width_in(page_format)
  # Response-variable naming style: "brackets" (CSV/Excel export form, the
  # default, so the variable index matches the raw data column for column)
  # or "underscore" (the EM / SPSS / Stata code form).
  theme$variable_names <- variable_names
  if (!is.null(font)) theme$font_body <- font
  if (!is.null(font_code)) theme$font_code <- font_code
  if (!is.null(colors)) theme <- utils::modifyList(theme, colors)
  # Resolve the chrome language now that we know the content languages,
  # then attach the localized chrome strings to the theme so every
  # renderer helper can pick them up via `theme$chrome$<key>` without
  # plumbing an extra argument.
  chrome_lang <- lss_resolve_chrome_lang(chrome_lang, langs)
  theme$chrome <- lss_chrome_strings(chrome_lang)
  theme$chrome_lang <- chrome_lang
  n_groups <- length(model$groups)

  # Single progress bar covering the entire render. The total is
  # `n_groups + 3` ticks: one for audit, one for the cover/TOC/welcome
  # block, n_groups for each group's items, and one for the file write.
  # The status text changes per phase, so the user sees both a smoothly
  # filling bar AND a label for what is happening right now -- without
  # the multi-line "step ... done" cascade that the previous design
  # produced. cli detects non-interactive sessions automatically and
  # downgrades to plain messages; suppressMessages() silences entirely.
  total <- n_groups + 3L
  cli::cli_progress_bar(
    name = "Rendering questionnaire",
    total = total,
    clear = TRUE
  )

  cli::cli_progress_update(
    set = 0L,
    status = if (isTRUE(show_audit)) "Running audit" else "Skipping audit"
  )
  audit_idx <- if (isTRUE(show_audit)) {
    lss_audit_index(audit_lss(lss))
  } else {
    NULL
  }
  state <- lss_render_state(model)
  state$show_raw_filter <- isTRUE(show_raw_filter)
  state$show_item_heading <- isTRUE(show_item_heading)
  state$show_attrs <- show_attrs
  resolved_titles <- lss_resolve_titles(title, lss, langs)
  section <- lss_render_section_props(
    page_format, length(langs),
    theme = theme,
    header_titles = if (isTRUE(show_header_title)) {
      unname(resolved_titles)
    } else {
      character(0)
    }
  )

  cli::cli_progress_update(set = 1L, status = "Cover, TOC and welcome")
  doc <- officer::read_docx()
  doc <- lss_render_cover(
    doc, lss, model, theme,
    logo = logo, logo_width = logo_width, logo_height = logo_height,
    show_source = isTRUE(show_source),
    show_privacy_settings = isTRUE(show_privacy_settings),
    show_admin_settings = isTRUE(show_admin_settings),
    titles = resolved_titles,
    authors = authors,
    description = description
  )
  doc <- officer::body_add_break(doc)
  # Which top-level sections the table of contents should list (and link
  # to). Computed up front so the early TOC agrees with what is rendered
  # later; the bookmarks it links to are added on each section heading.
  toc_sections <- list(
    audit = isTRUE(show_audit) && !is.null(audit_idx) &&
      nrow(audit_idx$findings) > 0,
    consent = isTRUE(show_consent) && lss_consent_present(lss, langs),
    quotas = isTRUE(show_quotas) && !is.null(lss$quotas) &&
      nrow(lss$quotas) > 0,
    index = isTRUE(show_index) &&
      any(vapply(model$groups, function(g) length(g$questions) > 0L, logical(1)))
  )
  if (isTRUE(show_toc) && length(model$groups) >= 2L) {
    doc <- lss_render_toc(doc, model, theme, sections = toc_sections)
    doc <- officer::body_add_break(doc)
  }
  if (isTRUE(show_audit) && !is.null(audit_idx) && nrow(audit_idx$findings) > 0) {
    # The table of contents already inserts a page break after itself, so
    # the audit section starts on a fresh page without a second break
    # (which produced an empty page between them).
    doc <- lss_render_audit_section(doc, audit_idx, theme)
  }

  # Front matter: the data-protection / consent gate the respondent
  # meets before the questions, rendered once for both templates.
  if (isTRUE(show_consent)) {
    doc <- lss_render_consent(doc, lss, langs, theme)
  }

  # Questionnaire section heading: anchors the TOC "Questionnaire" entry;
  # the group headings follow as styled sub-headings of this section.
  doc <- lss_render_section_heading(
    doc, theme, theme$chrome$cover_subtitle_review,
    lss_section_bookmark("questionnaire")
  )

  if (identical(template, "table")) {
    # Dense codebook layout: description / welcome / group / question /
    # value / endtext all become rows of one big flextable when their
    # respective `show_*` flag is on. Skipped rows simply drop out of
    # the row list.
    table_rows <- list()
    if (isTRUE(show_description)) {
      desc_row <- lss_table_text_row(
        lss, langs, "surveyls_description", "description"
      )
      if (!is.null(desc_row)) {
        table_rows[[length(table_rows) + 1L]] <- desc_row
      }
    }
    if (isTRUE(show_welcome)) {
      welcome_row <- lss_table_text_row(
        lss, langs, "surveyls_welcometext", "welcome"
      )
      if (!is.null(welcome_row)) {
        table_rows[[length(table_rows) + 1L]] <- welcome_row
      }
    }
    for (i in seq_along(model$groups)) {
      cli::cli_progress_update(
        set = 2L + i - 1L,
        status = sprintf("Group %d/%d", i, n_groups)
      )
      table_rows <- c(
        table_rows,
        lss_table_template_rows_for_group(
          model$groups[[i]], langs, theme,
          show_help = show_help, show_groups = show_groups, state = state
        )
      )
    }
    if (isTRUE(show_endtext)) {
      endtext_row <- lss_table_text_row(
        lss, langs, "surveyls_endtext", "endtext"
      )
      if (!is.null(endtext_row)) {
        table_rows[[length(table_rows) + 1L]] <- endtext_row
      }
    }
    doc <- lss_render_table_template(
      doc, table_rows, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      state = state
    )
  } else {
    if (isTRUE(show_description)) {
      doc <- lss_render_description(doc, lss, langs, theme)
    }
    if (isTRUE(show_welcome)) {
      doc <- lss_render_welcome(doc, lss, langs, theme)
    }
    for (i in seq_along(model$groups)) {
      cli::cli_progress_update(
        set = 2L + i - 1L,
        status = sprintf("Group %d/%d", i, n_groups)
      )
      doc <- lss_render_group(
        doc, model$groups[[i]], langs, theme,
        show_help = show_help,
        show_attrs = show_attrs,
        show_technical_attrs = show_technical_attrs,
        show_groups = show_groups,
        audit_idx = audit_idx,
        state = state
      )
    }
  }
  # End text already rendered as a row inside the codebook table
  # when template == "table"; only the cards layout needs the
  # separate body-level paragraph here, and only when the caller
  # has not opted out via `show_endtext = FALSE`.
  if (!identical(template, "table") && isTRUE(show_endtext)) {
    doc <- lss_render_endtext(doc, lss, langs, theme)
  }
  # Back matter: quotas (sampling caps) then the variable index. Both are
  # reference material about the survey structure, not part of the
  # respondent flow, so they follow the end text.
  if (isTRUE(show_quotas)) {
    doc <- lss_render_quotas(doc, lss, langs, theme)
  }
  if (isTRUE(show_index) && length(state$index_entries) > 0L) {
    doc <- lss_render_index(doc, state$index_entries, theme)
  }

  cli::cli_progress_update(
    set = 2L + n_groups,
    status = sprintf("Writing %s", basename(output))
  )
  doc <- officer::body_set_default_section(doc, section)
  print(doc, target = output)
  # Make Word and LibreOffice refresh fields (TOC, PAGE, NUMPAGES) when
  # the document is opened, so the reader does not need to press F9 and
  # so headless PDF conversion picks up the populated TOC.
  lss_inject_update_fields(output)
  cli::cli_progress_update(set = total)
  cli::cli_progress_done()

  abs_path <- tryCatch(
    normalizePath(output, winslash = "/", mustWork = TRUE),
    error = function(e) output
  )
  size_kb <- round(file.size(output) / 1024)
  cli::cli_alert_success(
    "Saved {.file {abs_path}} ({size_kb} KB, {n_groups} group{?s})"
  )
  invisible(output)
}




# Theme and small helpers ------------------------------------------------





# Block-to-paragraph conversion -----------------------------------------




# Cover, TOC, welcome, endtext ------------------------------------------







# Group and question rendering ------------------------------------------




























# Small text helpers -----------------------------------------------------



