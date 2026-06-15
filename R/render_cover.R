# Cover page rendering: title block, authors, description, metadata table.
#
# Extracted from R/render_lss_docx.R. Contains the visual top of the
# document -- title, subtitle, authors with ORCID hyperlinks,
# description with auto-detected URL links, and the metadata table
# (source file, survey id, languages, counts, last save). The four
# functions are: lss_render_cover (orchestration),
# lss_render_authors_block, lss_render_description_block, and the
# helper lss_split_text_urls which detects http(s) tokens in
# free-form text for clickable hyperlinks.

#' Cover page with the survey title in every language and a metadata table
#' @keywords internal
#' @noRd
lss_render_cover <- function(doc, lss, model, theme,
                             subtitle = NULL,
                             logo = NULL,
                             logo_width = 1.5,
                             logo_height = 0.75,
                             show_source = TRUE,
                             show_privacy_settings = FALSE,
                             show_admin_settings = FALSE,
                             titles = NULL,
                             authors = NULL,
                             description = NULL) {
  if (is.null(subtitle)) subtitle <- theme$chrome$cover_subtitle_review
  ls_settings <- lss$survey_language_settings
  langs <- model$languages
  if (is.null(titles)) {
    titles <- stats::setNames(lss_header_titles(lss, langs), langs)
  }

  # Optional logo at the very top of the cover page.
  if (!is.null(logo)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::external_img(
          src = logo, width = logo_width, height = logo_height
        ),
        fp_p = officer::fp_par(text.align = "center", padding.bottom = 6)
      )
    )
  }

  # Title in every language (largest font).
  for (lg in langs) {
    title <- titles[[lg]]
    if (is.null(title) || is.na(title) || !nzchar(title)) {
      title <- "(untitled survey)"
    }
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          title,
          prop = officer::fp_text(
            font.family = theme$font_body,
            font.size = theme$size_cover_title,
            color = theme$color_primary,
            bold = TRUE
          )
        ),
        fp_p = officer::fp_par(text.align = "center", padding.top = 6, padding.bottom = 2)
      )
    )
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        subtitle,
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_cover_subtitle,
          color = theme$color_muted, italic = TRUE
        )
      ),
      fp_p = officer::fp_par(text.align = "center")
    )
  )

  doc <- lss_render_authors_block(doc, authors, theme)
  doc <- lss_render_description_block(doc, description, theme)

  # Metadata table.
  n_groups <- length(model$groups)
  n_q <- sum(vapply(model$groups, function(g) length(g$questions), integer(1)))
  n_subq <- if (is.null(lss$subquestions)) 0L else nrow(lss$subquestions)
  n_ans  <- if (is.null(lss$answers)) 0L else nrow(lss$answers)
  sid <- if (!is.null(lss$surveys) && "sid" %in% names(lss$surveys)) {
    lss$surveys$sid[1]
  } else {
    NA_character_
  }
  last_mod <- if (!is.null(lss$surveys) && "lastmodified" %in% names(lss$surveys)) {
    lss$surveys$lastmodified[1]
  } else {
    NA_character_
  }
  none <- theme$empty_marker

  chrome <- theme$chrome
  field_pairs <- list()
  if (isTRUE(show_source)) {
    field_pairs[[chrome$cover_source_file]] <- basename(lss$file)
    field_pairs[[chrome$cover_survey_id]]   <- if (is.na(sid) || !nzchar(sid)) none else sid
  }
  field_pairs[[chrome$cover_languages]]      <- paste(langs, collapse = ", ")
  field_pairs[[chrome$cover_groups]]         <- as.character(n_groups)
  field_pairs[[chrome$cover_questions]]      <- as.character(n_q)
  field_pairs[[chrome$cover_subquestions]]   <- as.character(n_subq)
  field_pairs[[chrome$cover_answer_options]] <- as.character(n_ans)
  field_pairs[[chrome$cover_last_modified]]  <- if (is.na(last_mod) || !nzchar(last_mod)) none else last_mod
  field_pairs[[chrome$cover_generated]]      <- format(Sys.time(), "%Y-%m-%d %H:%M")

  # Optional administrative settings (alias, end URL, active flag).
  # `surveyls_alias`, `surveyls_url`, `surveyls_urldescription` live on
  # `survey_language_settings`; `active` lives on `surveys`. Each row
  # is emitted only when the underlying value is non-empty so the
  # block stays compact for surveys that do not configure them.
  if (isTRUE(show_admin_settings)) {
    primary <- langs[1L]
    ls_primary <- if (!is.null(ls_settings) && nrow(ls_settings) > 0L) {
      ls_settings[ls_settings$surveyls_language == primary, , drop = FALSE]
    } else NULL
    pull_ls <- function(col) {
      if (is.null(ls_primary) || nrow(ls_primary) == 0L) return("")
      v <- ls_primary[[col]]
      if (is.null(v) || length(v) == 0L || is.na(v[1])) return("")
      v[1]
    }
    pull_survey <- function(col) {
      if (is.null(lss$surveys) || !(col %in% names(lss$surveys))) return("")
      v <- lss$surveys[[col]][1L]
      if (is.null(v) || is.na(v)) return("") else v
    }
    alias <- pull_ls("surveyls_alias")
    if (nzchar(trimws(alias))) {
      field_pairs[[chrome$cover_alias]] <- alias
    }
    end_url <- pull_ls("surveyls_url")
    if (nzchar(trimws(end_url))) {
      field_pairs[[chrome$cover_end_url]] <- end_url
    }
    end_url_desc <- pull_ls("surveyls_urldescription")
    if (nzchar(trimws(end_url_desc))) {
      field_pairs[[chrome$cover_end_url_description]] <- end_url_desc
    }
    active <- pull_survey("active")
    if (nzchar(trimws(active))) {
      field_pairs[[chrome$cover_active]] <- lss_yes_no(active, theme)
    }
  }

  # Optional privacy / tracking settings (anonymized, save partial,
  # datestamp, IP, referrer). LimeSurvey stores them on the
  # `surveys` table as Y/N flags. Each row uses the localized yes/no
  # token so the block reads in the chrome language.
  if (isTRUE(show_privacy_settings)) {
    pull_survey <- function(col) {
      if (is.null(lss$surveys) || !(col %in% names(lss$surveys))) return("")
      v <- lss$surveys[[col]][1L]
      if (is.null(v) || is.na(v)) return("") else v
    }
    yn_row <- function(label, col) {
      raw <- pull_survey(col)
      if (!nzchar(trimws(raw))) return()
      field_pairs[[label]] <<- lss_yes_no(raw, theme)
    }
    yn_row(chrome$cover_anonymized,        "anonymized")
    yn_row(chrome$cover_save_partial,      "save")
    yn_row(chrome$cover_timestamp,         "datestamp")
    yn_row(chrome$cover_ip_recorded,       "ipaddr")
    yn_row(chrome$cover_referrer_recorded, "refurl")
  }

  meta <- data.frame(
    Field = names(field_pairs),
    Value = unlist(field_pairs, use.names = FALSE),
    stringsAsFactors = FALSE
  )
  ft <- flextable::flextable(meta)
  ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::bold(ft, j = 1, part = "body")
  ft <- flextable::color(ft, j = 1, color = theme$color_primary, part = "body")
  ft <- flextable::fontsize(ft, size = theme$size_cover_meta, part = "all")
  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  ft <- flextable::width(ft, j = 1, width = 1.4, unit = "in")
  ft <- flextable::width(ft, j = 2, width = 3.2, unit = "in")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(
    ft, border = officer::fp_border(color = theme$color_band, width = 0.5),
    part = "body"
  )
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- flextable::body_add_flextable(doc, ft, align = "center")
  doc
}

#' Render the authors block on the cover page
#'
#' Each author becomes one centered line: `Name \u2014 Affiliation` (the
#' em-dash is omitted when affiliation is empty). When an ORCID iD is
#' supplied, a smaller monospace line below shows
#' `ORCID 0000-0000-0000-0000` as a hyperlink to
#' `https://orcid.org/<id>`. The whole block is muted gray so it
#' supports the title typographically rather than competing with it.
#'
#' @keywords internal
#' @noRd
lss_render_authors_block <- function(doc, authors, theme) {
  if (is.null(authors) || length(authors) == 0L) return(doc)
  name_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_cover_meta + 1L,
    color = theme$color_text
  )
  affil_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_cover_meta,
    color = theme$color_muted, italic = TRUE
  )
  orcid_label_props <- officer::fp_text(
    font.family = theme$font_code, font.size = theme$size_cover_meta - 1L,
    color = theme$color_muted
  )
  orcid_link_props <- officer::fp_text(
    font.family = theme$font_code, font.size = theme$size_cover_meta - 1L,
    color = theme$color_accent, underlined = TRUE
  )
  doc <- officer::body_add_par(doc, "", style = "Normal")
  for (i in seq_along(authors)) {
    a <- authors[[i]]
    chunks <- list(officer::ftext(a$name, prop = name_props))
    if (nzchar(trimws(a$affiliation))) {
      chunks[[length(chunks) + 1L]] <- officer::ftext(
        paste0("  \u2014  ", a$affiliation), prop = affil_props
      )
    }
    doc <- officer::body_add_fpar(
      doc,
      do.call(officer::fpar, c(chunks, list(
        fp_p = officer::fp_par(
          text.align = "center",
          padding.top = if (i == 1L) 6 else 2,
          padding.bottom = if (nzchar(trimws(a$orcid))) 0 else 2
        )
      )))
    )
    if (nzchar(trimws(a$orcid))) {
      orcid_id <- trimws(a$orcid)
      orcid_url <- paste0("https://orcid.org/", orcid_id)
      doc <- officer::body_add_fpar(
        doc,
        officer::fpar(
          officer::ftext(paste0(theme$chrome$orcid_label, " "), prop = orcid_label_props),
          officer::hyperlink_ftext(
            href = orcid_url, text = orcid_id, prop = orcid_link_props
          ),
          fp_p = officer::fp_par(text.align = "center", padding.bottom = 2)
        )
      )
    }
  }
  doc
}

#' Render the optional free-form description block on the cover page
#'
#' Splits the input string on newlines (each becomes a centered line).
#' Within each line, any `http://` or `https://` URL token is rendered
#' as a clickable hyperlink (officer's `hyperlink_ftext`), so a DOI
#' permalink or article URL becomes navigable. Trailing punctuation
#' (`.,;:)`) attached to a URL is stripped from the link target and
#' kept as plain text so the reader sees `(see https://example.org).`
#' with the URL only spanning the actual address.
#'
#' @keywords internal
#' @noRd
lss_render_description_block <- function(doc, description, theme) {
  if (is.null(description) || !nzchar(trimws(description))) return(doc)
  text_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_cover_meta,
    color = theme$color_muted, italic = TRUE
  )
  link_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_cover_meta,
    color = theme$color_accent, italic = TRUE
  )
  doc <- officer::body_add_par(doc, "", style = "Normal")
  lines <- strsplit(description, "\n", fixed = TRUE)[[1L]]
  for (li in seq_along(lines)) {
    chunks <- lss_split_text_urls(lines[li], text_props, link_props)
    if (length(chunks) == 0L) next
    doc <- officer::body_add_fpar(
      doc,
      do.call(officer::fpar, c(chunks, list(
        fp_p = officer::fp_par(
          text.align = "center",
          padding.top = if (li == 1L) 8 else 0,
          padding.bottom = 2
        )
      )))
    )
  }
  doc
}

#' Split a text fragment into alternating plain-text and hyperlink
#' chunks based on detected `http(s)://...` URLs.
#'
#' Trailing punctuation common at sentence ends is moved out of the
#' link so the URL target stays clean. Returns a list of
#' `officer::ftext()` / `officer::hyperlink_ftext()` chunks suitable
#' for splicing into `officer::fpar()`.
#'
#' @keywords internal
#' @noRd
lss_split_text_urls <- function(text, text_props, link_props) {
  if (!nzchar(text)) return(list())
  pattern <- "https?://[^\\s]+"
  m <- gregexpr(pattern, text, perl = TRUE)[[1L]]
  if (m[1L] == -1L) {
    return(list(officer::ftext(text, prop = text_props)))
  }
  starts <- as.integer(m)
  lens   <- attr(m, "match.length")
  out <- list()
  cursor <- 1L
  for (k in seq_along(starts)) {
    s <- starts[k]
    e <- s + lens[k] - 1L
    if (s > cursor) {
      out[[length(out) + 1L]] <- officer::ftext(
        substr(text, cursor, s - 1L), prop = text_props
      )
    }
    url <- substr(text, s, e)
    # Strip trailing sentence punctuation from the link target.
    trail <- ""
    while (nchar(url) > 0L &&
           substr(url, nchar(url), nchar(url)) %in% c(".", ",", ";", ":", ")", "]", "}", "!", "?")) {
      trail <- paste0(substr(url, nchar(url), nchar(url)), trail)
      url <- substr(url, 1L, nchar(url) - 1L)
    }
    out[[length(out) + 1L]] <- officer::hyperlink_ftext(
      href = url, text = url, prop = link_props
    )
    if (nzchar(trail)) {
      out[[length(out) + 1L]] <- officer::ftext(trail, prop = text_props)
    }
    cursor <- e + 1L
  }
  if (cursor <= nchar(text)) {
    out[[length(out) + 1L]] <- officer::ftext(
      substr(text, cursor, nchar(text)), prop = text_props
    )
  }
  out
}

