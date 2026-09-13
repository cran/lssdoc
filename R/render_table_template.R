#' Render the survey content as a single dense table
#'
#' Alternative to the per-item "cards" layout. Produces one big
#' flextable where each variable is a single row, the meta fields
#' (No, Variable, Type, Mandatory, Filter) sit in the first five
#' columns, and one Question column per content language holds the
#' question stem, the subquestion label when applicable, optional
#' help text, and the response modalities stacked underneath the
#' question text.
#'
#' Group banners become merged section rows inside the table with a
#' dark petrol band so a reviewer sees a clear visual break between
#' sections; the column header repeats on every page automatically
#' (flextable's default OOXML output).
#'
#' @keywords internal
#' @noRd
lss_render_table_template <- function(doc, rows, langs, theme,
                                      show_help, show_attrs, state) {
  if (length(rows) == 0L) return(doc)

  chrome <- theme$chrome
  meta_cols <- c("Field", "No", "Variable", "Type", "Mandatory",
                 "Filter", "Value")
  lang_cols <- paste0("Q_", langs)
  all_cols <- c(meta_cols, lang_cols)

  # ---- Build the data frame skeleton ------------------------------
  # Table layout: every variable produces one tinted Question row
  # carrying the meta (No, Variable, Type, Mandatory, Filter) and the
  # localized question text, followed by N white Value rows (one per
  # enumerated answer code), the rest empty. Section rows span the
  # whole table in a dark petrol band.
  df <- as.data.frame(
    matrix("", nrow = length(rows), ncol = length(all_cols)),
    stringsAsFactors = FALSE
  )
  names(df) <- all_cols

  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (identical(r$kind, "description")) {
      df$Field[i] <- chrome$description_title
    } else if (identical(r$kind, "welcome")) {
      df$Field[i] <- chrome$welcome_text_title
    } else if (identical(r$kind, "endtext")) {
      df$Field[i] <- chrome$end_text_title
    } else if (identical(r$kind, "group")) {
      df$Field[i] <- chrome$item_group
      # Group name composed below per language via flextable::compose.
    } else if (identical(r$kind, "group_description")) {
      # Field stays blank: the row reads as the group banner's intro
      # text. The localized description is composed below.
    } else if (identical(r$kind, "scale_header")) {
      # Dual-scale separator: announce "Value (scale N)" on the
      # Value column. Lang cells stay empty.
      df$Value[i] <- r$text
    } else if (identical(r$kind, "value")) {
      df$Value[i] <- as.character(r$code)
      # Labels per language composed via flextable::compose() below.
    } else if (identical(r$kind, "mc_option")) {
      # Multiple-choice option: the option's full variable code in the
      # Variable column and its binary coding token in the Value column,
      # so the row is a self-contained dictionary entry. The option
      # label per language is composed below.
      df$Variable[i] <- r$variable
      df$Value[i]    <- r$value_token
    } else if (identical(r$kind, "mc_exclusive")) {
      # Exclusive-option note: a localized sentence in the Field column,
      # spanning nothing else; language cells stay empty.
      df$Field[i] <- chrome$item_exclusive
    } else if (identical(r$kind, "order_note")) {
      # Answer / option order note: a chrome annotation spanning the
      # language columns (composed below), under a "Value" / "Options"
      # Field label carried on the row.
      df$Field[i] <- r$field_label
    } else {
      # Question row (leaf / subq / other).
      df$Field[i]     <- chrome$item_question
      df$No[i]        <- as.character(r$no)
      df$Variable[i]  <- r$variable
      df$Type[i]      <- r$type_label
      df$Mandatory[i] <- r$mandatory_label
      # Filter and Value cells composed below.
    }
  }

  ft <- flextable::flextable(df)
  # In the dense table template the Mandatory header uses the
  # abbreviated chrome string (`Mand.` / `Oblig.` / `Pflicht` /
  # `Obblig.`) so the column can stay narrow and the question
  # languages get >=50% of the page width. The cards template keeps
  # the full localized word.
  ft <- flextable::set_header_labels(
    ft, values = stats::setNames(
      as.list(c(
        chrome$item_field,
        chrome$meta_no, chrome$meta_variable, chrome$meta_type,
        chrome$meta_mandatory_short, chrome$meta_filter, chrome$item_value,
        lss_language_label(langs)
      )),
      all_cols
    )
  )

  # ---- Compose rich cells per row ---------------------------------
  filter_plain_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_meta,
    color = theme$color_text
  )
  # Raw expression in the monospace face, muted + italic so it reads as
  # secondary to the humanized form above it. Held at size_meta - 1 (the
  # legibility floor for a code expression in this dense table); Consolas
  # already looks a touch larger than Calibri at an equal size, so this
  # reads about level with the body rather than larger.
  filter_raw_props <- officer::fp_text(
    font.family = theme$font_code, font.size = theme$size_meta - 1L,
    color = theme$color_muted, italic = TRUE
  )

  # The dense table uses ONE body size everywhere (question stems,
  # option / answer labels, welcome / end text, the value descriptors and
  # the chrome annotation rows), matching the meta and value cells -- a
  # uniform table reads as a data dictionary rather than prose. The size
  # auto-reduces by 1 pt from three languages on, the same rule
  # lss_table_template_polish() applies to the meta/value columns, so
  # composed and non-composed cells stay in lockstep.
  body_size <- if (length(langs) >= 3L) theme$size_meta - 1L else theme$size_meta

  # Codes in the dense table use the body font (not the monospace
  # font): Consolas is wide and pushes the long variable names to wrap,
  # eating width the language columns need. Bold + primary still mark
  # these cells as codes. (The cards template keeps the monospace face,
  # where width is not as constrained.)
  value_code_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_primary
  )
  # Value-cell descriptors ([num], [text], ...) and the chrome annotation
  # rows (exclusive note, answer/option order note) share the body size so
  # they never read a point larger than the surrounding cells, including
  # when the table steps down to 7 pt for three or four languages.
  value_descriptor_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_muted, italic = TRUE
  )

  value_label_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_text
  )
  empty_marker_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_muted
  )

  plain_lang_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_text
  )
  # Group name sits at the table body size (not a heading size): the
  # bold primary colour and the saturated group banner already mark it
  # as a section divider, so a larger size would just break the even
  # rhythm of the dense table rows.
  group_name_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_primary, bold = TRUE
  )

  for (i in seq_along(rows)) {
    r <- rows[[i]]
    kind <- r$kind
    if (identical(kind, "scale_header")) {
      # Scale header carries text already in df$Value -- no rich
      # composition. Polish will tint the row.
      next
    }

    if (identical(kind, "welcome") || identical(kind, "endtext") ||
        identical(kind, "description") || identical(kind, "group_description")) {
      # Welcome / End text / group intro: full HTML content per language
      # via the same lss_compose() helper the cards template uses
      # for the side-by-side block, so paragraphs and formatting
      # are preserved.
      for (lg in langs) {
        ft <- flextable::compose(
          ft, i = i, j = paste0("Q_", lg),
          value = lss_compose(r$text_by_lang[[lg]], theme,
                              size = body_size)
        )
      }
      next
    }

    if (identical(kind, "group")) {
      # Group row: localized name in bold primary per language.
      for (lg in langs) {
        name <- r$name_by_lang[[lg]]
        if (is.null(name) || is.na(name) || !nzchar(name)) name <- ""
        ft <- flextable::compose(
          ft, i = i, j = paste0("Q_", lg),
          value = flextable::as_paragraph(flextable::as_chunk(
            name, props = group_name_props
          ))
        )
      }
      next
    }

    if (identical(kind, "value") || identical(kind, "mc_option")) {
      # Value row (enumerated answer code) OR multiple-choice option
      # row: the language columns carry the answer / option label.
      # `df` already holds the code (Value column) or the full option
      # variable name (Variable column). Empty cells fall back to the
      # muted em-dash.
      for (lg in langs) {
        label <- lss_html_to_text(r$labels[[lg]])
        if (!nzchar(label)) {
          ft <- flextable::compose(
            ft, i = i, j = paste0("Q_", lg),
            value = flextable::as_paragraph(flextable::as_chunk(
              theme$empty_marker, props = empty_marker_props
            ))
          )
        } else {
          ft <- flextable::compose(
            ft, i = i, j = paste0("Q_", lg),
            value = flextable::as_paragraph(flextable::as_chunk(
              label, props = value_label_props
            ))
          )
        }
      }
      next
    }

    if (identical(kind, "mc_exclusive") || identical(kind, "order_note")) {
      # Chrome annotation rows (exclusive-option note; answer / option
      # order note): the localized sentence in each language column,
      # muted italic, mirroring the cards layout. The Field column
      # already carries the row label ("Exclusive" / "Value" / "Options").
      # Polish merges the language columns so it shows once.
      for (lg in langs) {
        ft <- flextable::compose(
          ft, i = i, j = paste0("Q_", lg),
          value = flextable::as_paragraph(flextable::as_chunk(
            r$text, props = value_descriptor_props
          ))
        )
      }
      next
    }

    # Question row (leaf / subq / other) ----------------------------
    ft <- flextable::compose(
      ft, i = i, j = "Filter",
      value = lss_table_filter_paragraph(
        r$relevance, theme,
        plain_props = filter_plain_props,
        raw_props = filter_raw_props,
        show_raw = isTRUE(state$show_raw_filter)
      )
    )
    # For non-enumerated types (M/P/N/K/T/S/U/D/...), the implicit
    # response-domain descriptor goes in the Value cell of the
    # Question row. For enumerated types (L/F/1/...) the cell stays
    # empty because the codes appear in their own Value rows below.
    ft <- flextable::compose(
      ft, i = i, j = "Value",
      value = lss_table_value_paragraph(
        r, theme,
        code_props = value_code_props,
        descriptor_props = value_descriptor_props
      )
    )
    for (lg in langs) {
      ft <- flextable::compose(
        ft, i = i, j = paste0("Q_", lg),
        value = lss_table_question_paragraph(r, lg, theme,
                                             show_help = show_help,
                                             body_size = body_size)
      )
    }
  }

  # Polish applies row-type-aware styling (section merge + petrol
  # band, Q-row tint, scale_header tint, widths, etc.).
  ft <- lss_table_template_polish(ft, theme, rows, n_lang = length(langs))

  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  doc
}

#' Build the Welcome / End text row for the table, when
#' the survey has non-empty content in at least one displayed
#' language. Returns `NULL` if all languages are empty.
#'
#' `field` is the `survey_language_settings` column (typically
#' `surveyls_welcometext` or `surveyls_endtext`); `kind` is the row
#' marker ("welcome" / "endtext") used by the rendering loop.
#' @keywords internal
#' @noRd
lss_table_text_row <- function(lss, langs, field, kind) {
  ls <- lss$survey_language_settings
  if (is.null(ls) || nrow(ls) == 0L) return(NULL)
  vals <- vapply(langs, function(lg) {
    v <- ls[[field]][ls$surveyls_language == lg]
    if (length(v) == 0L) NA_character_ else v[1]
  }, character(1))
  if (!any(!is.na(vals) & nzchar(trimws(vals)))) return(NULL)
  list(
    kind = kind,
    text_by_lang = stats::setNames(as.list(vals), langs)
  )
}

#' Walk the model and produce the flat list of rows that will become
#' the table -- alternating "section" markers (group
#' banners) and "item" rows (one per variable).
#'
#' Each "item" row is normalized in this builder so the main
#' renderer never has to look at the question structure again:
#' `no`, `variable` (the LimeSurvey data column, `parent_subq` for
#' compound subqs), `type_label`, `mandatory_label`, `relevance`,
#' plus pre-extracted texts per language (`parent_text`, `subq_text`,
#' `help`) and the question/subquestion model objects so the cell
#' composer can pull the answer scale or the implicit-coding
#' descriptor at render time.
#'
#' Also advances `state$item_no`, `state$group_index` and
#' `state$index_entries` so the rest of the document (TOC, Variable
#' index, navigation) stays consistent with the table layout. The
#' progress bar is updated at each group boundary.
#'
#' @keywords internal
#' @noRd
lss_table_template_rows_for_group <- function(g, langs, theme,
                                              show_help, state,
                                              show_groups = TRUE) {
  chrome <- theme$chrome
  rows <- list()
  # The group index must still advance even when the row itself is
  # hidden -- the variable index and audit references depend on it.
  state$group_index <- state$group_index + 1L
  if (isTRUE(show_groups)) {
    # Group row: Field = "Group" label, language columns hold the
    # localized name prefixed by the running group index.
    group_names <- stats::setNames(
      lapply(langs, function(lg) {
        raw <- if (!is.null(g$names[[lg]])) g$names[[lg]] else NA_character_
        if (is.null(raw) || is.na(raw) || !nzchar(raw)) {
          raw <- paste0("Group ", g$gid)
        }
        sprintf("%d. %s", state$group_index,
                lss_strip_group_number_prefix(raw))
      }),
      langs
    )
    rows[[length(rows) + 1L]] <- list(
      kind = "group",
      name_by_lang = group_names
    )
    # Group description (when the author wrote one): a row right under
    # the banner carrying the localized intro text, mirroring what the
    # cards template renders below the group title. Skipped when every
    # language is blank, so a group without a description adds no row.
    desc_vals <- vapply(langs, function(lg) {
      v <- if (!is.null(g$descriptions[[lg]])) g$descriptions[[lg]] else NA_character_
      if (is.null(v) || is.na(v)) NA_character_ else as.character(v)
    }, character(1))
    if (any(!is.na(desc_vals) & nzchar(trimws(desc_vals)))) {
      rows[[length(rows) + 1L]] <- list(
        kind = "group_description",
        text_by_lang = stats::setNames(as.list(desc_vals), langs)
      )
    }
  }

  # Build one Question row per variable (carrying the meta and the
  # question/subq/help text) followed by N Value rows (one per
  # enumerated answer code, each carrying its label per language).
  # Variables with no enumerated answers produce only the Question
  # row -- the implicit-coding descriptor sits in their Value cell.
  emit_question_row <- function(kind, no, variable, q, sq = NULL,
                                other_q = NULL) {
    list(
      kind = kind, no = no, variable = variable,
      type_label = if (identical(kind, "other")) chrome$type_text_other
                   else lss_localized_type_label(q, theme),
      mandatory_label = lss_yes_no(
        if (identical(kind, "other")) "N" else q$mandatory, theme
      ),
      relevance = q$relevance,
      parent_text = stats::setNames(
        lapply(langs, function(lg) q$texts[[lg]]$question), langs
      ),
      subq_text = if (!is.null(sq)) {
        stats::setNames(
          lapply(langs, function(lg) sq$texts[[lg]]$question), langs
        )
      } else NULL,
      help = if (identical(kind, "other")) NULL else {
        stats::setNames(
          lapply(langs, function(lg) q$texts[[lg]]$help), langs
        )
      },
      q = q, sq = sq, other_q = other_q
    )
  }

  emit_value_rows_for <- function(q) {
    # Predefined labelled scales (C/E/Y/G) store no answers in the .lss but
    # carry fixed localizable codes -- list them like a stored scale so the
    # dense table documents the value domain instead of leaving it blank.
    if (length(q$answers) == 0L) {
      labelled <- lss_predefined_labelled(q$type)
      if (is.null(labelled)) return(list())
      out <- list()
      for (cl in labelled) {
        out[[length(out) + 1L]] <- list(
          kind = "value",
          code = cl[1],
          labels = stats::setNames(
            rep(list(chrome[[cl[2]]]), length(langs)), langs
          )
        )
      }
      return(out)
    }
    multi_scale <- !is.null(q$scales) && length(q$scales) > 1L
    bundles <- if (multi_scale) q$scales else list(q$answers)
    out <- list()
    for (si in seq_along(bundles)) {
      ans <- bundles[[si]]
      if (length(ans) == 0L) next
      if (multi_scale) {
        # Dual-scale separator: a tinted scale-header row carrying
        # "Value (scale N)" on the left so the reader sees a break
        # between the two response axes.
        out[[length(out) + 1L]] <- list(
          kind = "scale_header",
          text = sprintf(chrome$item_value_scale_fmt, si)
        )
      }
      for (a in ans) {
        out[[length(out) + 1L]] <- list(
          kind = "value",
          code = a$code,
          labels = stats::setNames(
            lapply(langs, function(lg) a$labels[[lg]]), langs
          )
        )
      }
    }
    out
  }

  # Value rows for a plain answer list (one scale of a dual-scale array),
  # without the "Value (scale N)" separators that emit_value_rows_for adds
  # for the combined view -- here each scale is its own single-choice row.
  emit_values <- function(ans) {
    out <- list()
    for (a in ans) {
      out[[length(out) + 1L]] <- list(
        kind = "value",
        code = a$code,
        labels = stats::setNames(
          lapply(langs, function(lg) a$labels[[lg]]), langs
        )
      )
    }
    out
  }

  # An answer / option order annotation row, or NULL when the order is
  # the (silent) default. `attr` is answer_order (leaf single choice) or
  # subquestion_order (multiple choice); `field_label` is the row label
  # shown in the Field column ("Value" / "Options").
  order_note_row <- function(q, attr, field_label) {
    note <- lss_answer_order_note(q, theme, attr = attr)
    if (is.null(note)) return(NULL)
    list(kind = "order_note", text = note, field_label = field_label)
  }

  # One grouped block for a multiple-choice question: a parent
  # Question row (stem once, Variable = parent_*, the implicit
  # "Y/blank" coding in the Value cell) followed by one row per option
  # carrying that option's FULL variable code -- so the dense table
  # keeps every exported variable on its own findable row -- with the
  # option label in the language columns. Options are not numbered
  # individually (one No for the question), but each option variable is
  # registered in the variable index.
  emit_multiple_choice <- function(q) {
    out <- list()
    state$item_no <- state$item_no + 1L
    parent_no <- state$item_no
    # The parent row is a pure question header: stem, type, filter, but
    # NO variable code and NO value. In this dense table every option
    # row below is a real variable that carries its own name AND its own
    # value domain, so the header itself documents neither -- it is not a
    # data column. `mc_parent = TRUE` tells the renderer to leave its
    # Value cell blank (lss_table_value_paragraph would otherwise emit
    # the type's implicit token here).
    prow <- emit_question_row("leaf", parent_no, "", q = q)
    prow$mc_parent <- TRUE
    out[[length(out) + 1L]] <- prow
    # Each option's binary coding token (Y = selected, blank = not).
    # Documented per option, in the Value column, so every variable row
    # is a self-contained dictionary entry (name | value domain | label).
    value_token <- "Y/blank"
    for (sq in q$subquestions) {
      item_code <- lss_variable_name(q$code, sq$code,
                                     style = theme$variable_names)
      state$index_entries[[length(state$index_entries) + 1L]] <- list(
        code = item_code, no = parent_no
      )
      out[[length(out) + 1L]] <- list(
        kind = "mc_option",
        variable = item_code,
        value_token = value_token,
        labels = stats::setNames(
          lapply(langs, function(lg) sq$texts[[lg]]$question), langs
        )
      )
    }
    excl <- lss_exclusive_codes(q)
    if (length(excl) > 0L) {
      out[[length(out) + 1L]] <- list(
        kind = "mc_exclusive",
        text = sprintf(chrome$exclusive_text_fmt, paste(excl, collapse = ", "))
      )
    }
    onr <- order_note_row(q, "subquestion_order", chrome$item_order)
    if (!is.null(onr)) out[[length(out) + 1L]] <- onr
    if (identical(q$other, "Y")) {
      state$item_no <- state$item_no + 1L
      item_code <- lss_other_variable(q, theme$variable_names)
      state$index_entries[[length(state$index_entries) + 1L]] <- list(
        code = item_code, no = state$item_no
      )
      out[[length(out) + 1L]] <- emit_question_row(
        "other", state$item_no, item_code, q = q, other_q = q
      )
    }
    out
  }

  # One grouped block for a ranking question: a parent stem row, one
  # position column per item (named by the item's answer id, the CSV form
  # `code[aid]`, carrying its rank), then the rankable items as value rows
  # (the codes that can appear as a cell's value).
  emit_ranking <- function(q) {
    out <- list()
    state$item_no <- state$item_no + 1L
    prow <- emit_question_row("leaf", state$item_no, "", q = q)
    prow$mc_parent <- TRUE
    out[[length(out) + 1L]] <- prow
    for (i in seq_along(q$answers)) {
      a <- q$answers[[i]]
      item_code <- lss_variable_name(q$code, a$aid, style = theme$variable_names)
      state$index_entries[[length(state$index_entries) + 1L]] <- list(
        code = item_code, no = state$item_no
      )
      out[[length(out) + 1L]] <- list(
        kind = "mc_option", variable = item_code, value_token = "",
        labels = stats::setNames(
          rep(list(sprintf(chrome$item_rank_fmt, i)), length(langs)), langs
        )
      )
    }
    out <- c(out, emit_values(q$answers))
    onr <- order_note_row(q, "answer_order", chrome$item_order)
    if (!is.null(onr)) out[[length(out) + 1L]] <- onr
    out
  }

  for (q in g$questions) {
      info <- lss_type_info(q$type)
      if (identical(info$family, "multiple") &&
          length(q$subquestions) > 0L) {
        rows <- c(rows, emit_multiple_choice(q))
      } else if (identical(q$type, "R")) {
        rows <- c(rows, emit_ranking(q))
      } else if (isTRUE(info$has_subquestions) && length(q$subquestions) > 0L) {
        # Dual-scale arrays (type 1): one row per (subquestion x scale),
        # each a single-choice variable named `<q>_<subq>_<0|1>`, with the
        # scale (dualscale header) shown in the Question cell and just that
        # scale's answers below -- mirroring the cards layout.
        dual_scale <- isTRUE(info$has_scales) &&
          !is.null(q$scales) && length(q$scales) > 1L
        sq_scale <- function(s) {
          # Defensive default: a subquestion without a scale_id maps to scale
          # "0". covr cannot trace this closure because it is only ever invoked
          # through vapply()/Filter(), so its body reads as uncovered even when
          # the enclosing 2-D array path runs; excluded from coverage.
          # nocov start
          if (is.null(s$scale_id) || is.na(s$scale_id)) "0" else as.character(s$scale_id)
          # nocov end
        }
        two_d <- !dual_scale &&
          length(unique(vapply(q$subquestions, sq_scale, character(1)))) > 1L
        if (two_d) {
          # Two-dimensional array: rows (scale 0) x columns (scale 1) ->
          # one variable per cell, `parent[row_col]`, with the column label
          # in the Question cell (reusing the scale_header line).
          rows_sq <- Filter(function(s) sq_scale(s) == "0", q$subquestions)
          cols_sq <- Filter(function(s) sq_scale(s) != "0", q$subquestions)
          for (rsq in rows_sq) for (csq in cols_sq) {
            state$item_no <- state$item_no + 1L
            item_code <- lss_variable_name(
              q$code, paste0(rsq$code, "_", csq$code),
              style = theme$variable_names
            )
            state$index_entries[[length(state$index_entries) + 1L]] <- list(
              code = item_code, no = state$item_no
            )
            qr <- emit_question_row("subq", state$item_no, item_code,
                                    q = q, sq = rsq)
            qr$scale_header <- stats::setNames(
              lapply(langs, function(lg) csq$texts[[lg]]$question), langs
            )
            rows[[length(rows) + 1L]] <- qr
            rows <- c(rows, emit_value_rows_for(q))
          }
        } else {
        for (sq in q$subquestions) {
          if (dual_scale) {
            for (si in seq_along(q$scales)) {
              state$item_no <- state$item_no + 1L
              item_code <- lss_variable_name(q$code, sq$code, si,
                                             theme$variable_names)
              state$index_entries[[length(state$index_entries) + 1L]] <- list(
                code = item_code, no = state$item_no
              )
              qr <- emit_question_row("subq", state$item_no, item_code,
                                      q = q, sq = sq)
              hdr <- lss_dualscale_header(q, si, langs)
              qr$scale_header <- if (!is.null(hdr)) {
                hdr
              } else {
                stats::setNames(
                  rep(list(paste(chrome$item_scale, si)), length(langs)), langs
                )
              }
              rows[[length(rows) + 1L]] <- qr
              rows <- c(rows, emit_values(q$scales[[si]]))
            }
          } else {
            state$item_no <- state$item_no + 1L
            item_code <- lss_variable_name(q$code, sq$code,
                                           style = theme$variable_names)
            state$index_entries[[length(state$index_entries) + 1L]] <- list(
              code = item_code, no = state$item_no
            )
            rows[[length(rows) + 1L]] <- emit_question_row(
              "subq", state$item_no, item_code, q = q, sq = sq
            )
            rows <- c(rows, emit_value_rows_for(q))
          }
        }
        }
        if (identical(q$other, "Y")) {
          state$item_no <- state$item_no + 1L
          item_code <- lss_other_variable(q, theme$variable_names)
          state$index_entries[[length(state$index_entries) + 1L]] <- list(
            code = item_code, no = state$item_no
          )
          rows[[length(rows) + 1L]] <- emit_question_row(
            "other", state$item_no, item_code, q = q, other_q = q
          )
        }
      } else {
        state$item_no <- state$item_no + 1L
        state$index_entries[[length(state$index_entries) + 1L]] <- list(
          code = q$code, no = state$item_no
        )
        rows[[length(rows) + 1L]] <- emit_question_row(
          "leaf", state$item_no, q$code, q = q
        )
        rows <- c(rows, emit_value_rows_for(q))
        onr <- order_note_row(q, "answer_order", chrome$item_order)
        if (!is.null(onr)) rows[[length(rows) + 1L]] <- onr
        # Single-choice "Other" free-text column.
        if (identical(q$other, "Y")) {
          state$item_no <- state$item_no + 1L
          item_code <- lss_other_variable(q, theme$variable_names)
          state$index_entries[[length(state$index_entries) + 1L]] <- list(
            code = item_code, no = state$item_no
          )
          rows[[length(rows) + 1L]] <- emit_question_row(
            "other", state$item_no, item_code, q = q, other_q = q
          )
        }
        # List-with-comment (type O) free-text comment column.
        if (identical(q$type, "O")) {
          state$item_no <- state$item_no + 1L
          item_code <- lss_variable_name(q$code, "_Ccomment",
                                         style = theme$variable_names)
          state$index_entries[[length(state$index_entries) + 1L]] <- list(
            code = item_code, no = state$item_no
          )
          cr <- emit_question_row("leaf", state$item_no, item_code, q = q)
          cr$q$type <- "S"
          cr$type_label <- chrome$type_text_short
          rows[[length(rows) + 1L]] <- cr
        }
      }
  }
  rows
}

#' Build the flextable paragraph for the Filter cell of a row (the
#' table template). Mirrors the meta-table convention from the
#' cards path: human-readable form on top, raw LimeSurvey expression
#' beneath in a smaller italic mono.
#' @keywords internal
#' @noRd
lss_table_filter_paragraph <- function(relevance, theme,
                                       plain_props, raw_props,
                                       show_raw = TRUE) {
  filter_raw <- if (is.null(relevance) || is.na(relevance) ||
                    !nzchar(relevance)) {
    "1"
  } else {
    lss_strip_outer_parens(relevance)
  }
  filter_plain <- lss_humanize_relevance(filter_raw, theme)
  chunks <- list(flextable::as_chunk(filter_plain, props = plain_props))
  if (isTRUE(show_raw) && !identical(filter_plain, filter_raw)) {
    chunks <- c(
      chunks,
      list(flextable::as_chunk("\n", props = plain_props)),
      list(flextable::as_chunk(filter_raw, props = raw_props))
    )
  }
  do.call(flextable::as_paragraph, chunks)
}

#' Build the flextable paragraph for the Value cell of a row.
#'
#' Compact, language-independent summary of the response domain so a
#' reviewer can scan the codes column without reading the Question
#' columns. Conventions:
#'
#' * Enumerated answers with sequential integer codes (`1..N`) ->
#'   the range `1-N` in mono primary (e.g. "1-5", "1-7").
#' * Enumerated with non-sequential codes -> the codes joined by
#'   commas in mono primary (e.g. "1, 2, 99").
#' * Dual-scale arrays -> per-scale summary on its own line
#'   ("Scale 1: 1-5", "Scale 2: 1-3").
#' * Implicit codings (multi-choice, yes/no, gender, 5-point) ->
#'   short fixed token in mono primary ("Y/blank", "Y/N", "M/F",
#'   "1-5").
#' * Open-ended types -> descriptor in italic muted ("[num]",
#'   "[text]", "[date]", "[file]", "[calc]", "[rank]").
#' * Other item and section rows -> empty cell.
#'
#' @keywords internal
#' @noRd
lss_table_value_paragraph <- function(row, theme, code_props, descriptor_props) {
  # The table layout devotes a dedicated Value row to every
  # enumerated code, so the Question row's Value cell stays empty
  # for enumerated types -- only non-enumerated types receive an
  # inline descriptor in the Question row itself.
  if (identical(row$kind, "other")) {
    return(flextable::as_paragraph(flextable::as_chunk(
      "", props = descriptor_props
    )))
  }
  # Multiple-choice parent header: not a data column, so it documents no
  # value domain of its own -- each option row below carries its Y/blank
  # token instead.
  if (isTRUE(row$mc_parent)) {
    return(flextable::as_paragraph(flextable::as_chunk(
      "", props = descriptor_props
    )))
  }
  q <- row$q
  # Enumerated stored answers, and the labelled predefined scales
  # (C/E/Y/G), are documented in their own Value rows below, so the
  # Question row's Value cell stays empty for them.
  if (length(q$answers) > 0L || !is.null(lss_predefined_labelled(q$type))) {
    return(flextable::as_paragraph(flextable::as_chunk(
      "", props = descriptor_props
    )))
  }
  # Implicit codings: short mono token. The numeric N-point scales carry
  # no labels, so a compact range token in the Value cell is the dense
  # equivalent of the cards "%d-point scale" descriptor: 5-point single
  # choice (5) and 5-point array (A) -> "1-5"; 10-point array (B) -> "1-10".
  short_code <- switch(
    EXPR = q$type,
    "M" = "Y/blank",
    "P" = "Y/blank",
    "5" = "1-5",
    "A" = "1-5",
    "B" = "1-10",
    NULL
  )
  if (!is.null(short_code)) {
    return(flextable::as_paragraph(flextable::as_chunk(
      short_code, props = code_props
    )))
  }
  descriptor <- switch(
    EXPR = q$type,
    "N" = "[num]",
    "K" = "[num]",
    ":" = "[num]",
    "S" = "[text]",
    "T" = "[text]",
    "U" = "[text]",
    ";" = "[text]",
    "Q" = "[text]",
    "D" = "[date]",
    "*" = "[calc]",
    "R" = "[rank]",
    "|" = "[file]",
    "X" = "\u2014",
    "\u2014"
  )
  flextable::as_paragraph(flextable::as_chunk(
    descriptor, props = descriptor_props
  ))
}

#' Compact comma-or-range string for an enumerated answer list.
#'
#' If the codes are sequential integers `1..N` (or `0..N`) -> `"1-N"`
#' (or `"0-N"`). Otherwise -> the codes joined by ", ".
#'
#' @keywords internal
#' @noRd
lss_table_value_codes <- function(answers) {
  codes <- vapply(answers, function(a) as.character(a$code), character(1L))
  if (length(codes) == 0L) return("")
  nums <- suppressWarnings(as.integer(codes))
  if (!anyNA(nums) && length(nums) >= 2L) {
    sorted <- sort(nums)
    if (identical(sorted, seq.int(sorted[1L], sorted[length(sorted)]))) {
      return(sprintf("%d-%d", sorted[1L], sorted[length(sorted)]))
    }
  }
  paste(codes, collapse = ", ")
}

#' Build the flextable paragraph for one Question cell (one row,
#' one language).
#'
#' Stacks the question stem, the subquestion label (compound rows),
#' the help text (when present and `show_help`), and the response
#' modalities (the answer scale or the implicit-coding descriptor).
#' Each layer has its own typography: stem in the body color at
#' question size, subq in the same size but italic to distinguish,
#' help small and gray, answer codes in mono primary, answer labels
#' in body text.
#' @keywords internal
#' @noRd
lss_table_question_paragraph <- function(row, lg, theme, show_help,
                                         body_size = theme$size_meta) {
  # One uniform size for stem, subquestion label and help; the visual
  # distinction between them is carried by style (italic, muted), not
  # by size, so the dense table stays uniform.
  size_q  <- body_size
  size_sq <- body_size
  size_h  <- body_size

  plain <- function(size = size_q, color = theme$color_text,
                    italic = FALSE, bold = FALSE,
                    font = theme$font_body) {
    officer::fp_text(
      font.family = font, font.size = size, color = color,
      italic = italic, bold = bold
    )
  }

  br <- function() {
    flextable::as_chunk("\n", props = plain())
  }

  chunks <- list()
  add_text <- function(text, props) {
    if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return()
    chunks[[length(chunks) + 1L]] <<- flextable::as_chunk(text, props = props)
  }
  add_line <- function(text, props) {
    if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return()
    if (length(chunks) > 0L) chunks[[length(chunks) + 1L]] <<- br()
    add_text(text, props)
  }

  # Question stem (parent for compound rows, leaf question for leaf
  # rows). For the Other item the customized "Other:" prompt
  # replaces the stem.
  if (identical(row$kind, "other")) {
    add_text(lss_table_other_prompt(row$other_q, lg), plain())
  } else {
    add_text(lss_html_to_text(row$parent_text[[lg]]), plain())
  }

  # Subquestion label below the stem (compound rows only), in italic so
  # the eye separates "what's being asked" (stem) from "what this row
  # narrows it to" (subq). The second-axis facet -- a dual-scale header or
  # a 2-D array column -- is appended in parentheses ("Parliament (Trust)",
  # "Trying new things (Today)"), with no empty parentheses when absent.
  if (identical(row$kind, "subq")) {
    s <- lss_html_to_text(row$subq_text[[lg]])
    f <- if (!is.null(row$scale_header)) {
      lss_html_to_text(row$scale_header[[lg]])
    } else {
      ""
    }
    s_ok <- !is.na(s) && nzchar(trimws(s))
    f_ok <- !is.na(f) && nzchar(trimws(f))
    txt <- if (f_ok) {
      if (s_ok) paste0(s, " (", f, ")") else f
    } else {
      s
    }
    add_line(txt, plain(size = size_sq, italic = TRUE))
  }

  # Help (optional), small muted italic.
  if (isTRUE(show_help) && !identical(row$kind, "other")) {
    help_text <- lss_html_to_text(row$help[[lg]])
    if (!is.null(help_text) && !is.na(help_text) && nzchar(trimws(help_text))) {
      add_line(
        paste0("\u00AB ", help_text, " \u00BB"),
        plain(size = size_h, color = theme$color_muted, italic = TRUE)
      )
    }
  }

  if (length(chunks) == 0L) {
    chunks <- list(flextable::as_chunk(
      theme$empty_marker,
      props = plain(color = theme$color_muted)
    ))
  }
  do.call(flextable::as_paragraph, chunks)
}

#' Resolve the customized prompt of the LimeSurvey "Other:" input
#' for a language, or fall back to a generic "Other:" label.
#' @keywords internal
#' @noRd
lss_table_other_prompt <- function(q, lg) {
  if (is.null(q$attributes) || nrow(q$attributes) == 0L) return("Other:")
  attrs <- q$attributes[q$attributes$attribute == "other_replace_text", ,
                        drop = FALSE]
  if (nrow(attrs) == 0L) return("Other:")
  lang_hit <- attrs$value[attrs$language == lg]
  if (length(lang_hit) > 0L && nzchar(trimws(lang_hit[1]))) return(lang_hit[1])
  empty_lang <- attrs$value[!nzchar(attrs$language)]
  if (length(empty_lang) > 0L && nzchar(trimws(empty_lang[1]))) {
    return(empty_lang[1])
  }
  "Other:"
}

#' Implicit-coding text for a question with no enumerated answers.
#' Mirrors the language map of `lss_value_implicit_row()` but
#' returns the descriptor as a single string suitable for
#' embedding inside a Question cell.
#' @keywords internal
#' @noRd
lss_table_implicit_value_text <- function(q, theme) {
  chrome <- theme$chrome
  switch(
    q$type,
    "M" = chrome$value_multi_y_blank,
    "P" = chrome$value_multi_y_blank_with_comment,
    "Y" = chrome$value_yes_no,
    "G" = chrome$value_gender,
    "5" = chrome$value_5point,
    "N" = chrome$value_numeric_input,
    "K" = chrome$value_numeric_input,
    "S" = chrome$value_free_text_short,
    "T" = chrome$value_free_text,
    "U" = chrome$value_free_text,
    "D" = chrome$value_date_input,
    "*" = chrome$value_computed,
    "R" = chrome$value_ranking,
    "|" = chrome$value_file_upload,
    NULL
  )
}

#' Apply the visual polish (band, borders, widths, section-row
#' merge and dark band) to the table flextable.
#' @keywords internal
#' @noRd
lss_table_template_polish <- function(ft, theme, rows, n_lang) {
  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  # Auto-reduce body and header font sizes when the table holds 3+
  # languages so the translation paragraphs keep enough breathing
  # room per cell. With 2 languages each gets >=2.6 in at 8 pt body;
  # with 3 languages 1.75 in, with 4 languages 1.32 in -- 7 pt body
  # is the editorial floor (matches GESIS / OECD survey documentation for
  # multi-column layouts).
  body_size <- if (n_lang >= 3L) theme$size_meta - 1L else theme$size_meta
  header_size <- if (n_lang >= 3L) theme$size_lang_header - 1L else theme$size_lang_header
  ft <- flextable::fontsize(ft, size = body_size, part = "body")
  ft <- flextable::fontsize(ft, size = header_size, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = theme$color_white, part = "header")
  ft <- flextable::bg(ft, bg = theme$color_band_dark, part = "header")

  # Field column body: bold primary so "Question" reads as a row
  # label, with the same petrol-band header (empty text) as the rest
  # of the meta header.
  ft <- flextable::bold(ft, j = "Field", part = "body")
  ft <- flextable::color(ft, j = "Field", color = theme$color_primary,
                         part = "body")

  # Variable column: body font (Calibri), bold, primary. The dense
  # table deliberately avoids the monospace face here -- Consolas is
  # wide and wraps the longer parent_subqcode names, stealing width from
  # the language columns; bold + primary already signal "code". The
  # cards meta band keeps the monospace face (width is not as tight).
  ft <- flextable::font(ft, j = "Variable", fontname = theme$font_body,
                        part = "body")
  ft <- flextable::bold(ft, j = "Variable", part = "body")
  ft <- flextable::color(ft, j = "Variable", color = theme$color_primary,
                         part = "body")

  # Value column body: body font, primary, NOT bold -- the answer codes
  # / coding tokens are secondary reference (the variable name is the
  # emphasized identifier), so a regular weight keeps the column light.
  # The dual-scale "Value (scale N)" header rows are re-bolded below.
  ft <- flextable::font(ft, j = "Value", fontname = theme$font_body,
                        part = "body")
  ft <- flextable::color(ft, j = "Value", color = theme$color_primary,
                         part = "body")

  # Alignment per column, content-driven:
  # - Field, Variable: left both (text / identifier)
  # - No: right both (digits stack as a column)
  # - Type, Mandatory: center both (short categorical tokens)
  # - Value: header center (the word "Value" / "Valeur" / "Wert"
  #   reads as a section title), body right (numeric answer codes
  #   align as a column, Stata / SPSS / GESIS convention)
  # - Language columns: header center (`Francais`, `Deutsch`,
  #   `English` read as section titles above the translation
  #   paragraph), body left (text reads L->R)
  ft <- flextable::align(ft, align = "left",   part = "all")
  ft <- flextable::align(ft, j = "No",        align = "right",  part = "all")
  ft <- flextable::align(ft, j = "Type",      align = "center", part = "all")
  ft <- flextable::align(ft, j = "Mandatory", align = "center", part = "all")
  ft <- flextable::align(ft, j = "Value",     align = "right",  part = "body")
  ft <- flextable::align(ft, j = "Value",     align = "center", part = "header")
  # Language columns are the last `n_lang` columns of the table (the
  # meta columns occupy positions 1:7). Body stays left (translations
  # read L->R); header centered so the language name sits as a
  # section title above the column.
  n_meta <- 7L
  lang_j <- seq.int(n_meta + 1L, length.out = n_lang)
  ft <- flextable::align(ft, j = lang_j, align = "center", part = "header")
  ft <- flextable::align(ft, j = lang_j, align = "left",   part = "body")

  # Borders: soft grid only, no per-row primary outline.
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = theme$color_grid, width = 0.5)
  ft <- flextable::hline(ft, border = thin, part = "all")
  ft <- flextable::vline(ft, border = thin, part = "all")
  ft <- flextable::vline_left(ft, border = thin, part = "all")
  ft <- flextable::vline_right(ft, border = thin, part = "all")

  # Body cells are top-aligned so multiline language content reads from
  # the top. Header labels sit centered in the tinted band.
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::valign(ft, valign = "center", part = "header")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2,
                           padding.left = 3, padding.right = 3, part = "all")

  # Column widths. Tight on the meta columns to give the language
  # columns >=50% of the total width (the translation paragraphs
  # are the primary content of the table). Short header labels
  # are allowed to wrap to two lines ("Single | choice") -- the
  # visual cost is small and the question-column gain is large.
  #   Field     0.62  - "Welcome" (7) and "Question" (8) fit on
  #                     one line; longer localized labels
  #                     ("Description", 11) wrap.
  #   No        0.30  - 3 digits in 11 pt body font ("999" max).
  #   Variable  1.30  - common `parent_subq` codes fit on a single
  #                     11 pt Consolas line; codes >14 chars wrap.
  #   Type      0.62  - one-word labels ("Computed", "Number",
  #                     "Display", "Ranking") stay on one line
  #                     ("Computed", 8 chars at 8 pt, needs ~0.50 in
  #                     plus cell padding, which 0.55 clipped);
  #                     multi-word labels ("Single choice") still
  #                     wrap, an acceptable cost on the per-variable
  #                     header row.
  #   Mandatory 0.50  - uses the abbreviated header
  #                     (`meta_mandatory_short`); the widest
  #                     localized variant ("Pflicht", 7 chars at
  #                     8 pt bold ~0.42 in) fits with thin margin.
  #   Filter    0.58  - editorial default (show_raw_filter = FALSE)
  #                     shows only the plain form; "All" fits, and a
  #                     real condition (`workstatus = 1`) wrapped
  #                     already at 0.65, so ceding 0.07 in to Type
  #                     costs nothing visible.
  #   Value     0.55  - 1-3 digit codes in 11 pt Consolas bold
  #                     ("1", "12", "999").
  # Total meta = 4.47; the language columns split the remaining width.
  # The total follows the page orientation (theme$content_width_in: 6.30 in
  # portrait, 9.72 in A4 landscape, 14.56 in A3) and NEVER the language
  # count -- four languages fit on an A4 portrait page; reach for landscape
  # via page_format when the columns get too tight, not automatically. The
  # 0.40 in floor is a hard legibility minimum (the body font already steps
  # down to 7 pt from three languages on); if it binds, the page format is
  # too narrow for that many languages and landscape is the fix.
  meta_w <- 0.62 + 0.30 + 1.30 + 0.62 + 0.50 + 0.58 + 0.55
  total_w <- theme$content_width_in
  lang_w <- max((total_w - meta_w) / max(n_lang, 1L), 0.40)
  ft <- flextable::width(ft, j = "Field",     width = 0.62, unit = "in")
  ft <- flextable::width(ft, j = "No",        width = 0.30, unit = "in")
  ft <- flextable::width(ft, j = "Variable",  width = 1.30, unit = "in")
  ft <- flextable::width(ft, j = "Type",      width = 0.62, unit = "in")
  ft <- flextable::width(ft, j = "Mandatory", width = 0.50, unit = "in")
  ft <- flextable::width(ft, j = "Filter",    width = 0.58, unit = "in")
  ft <- flextable::width(ft, j = "Value",     width = 0.55, unit = "in")
  for (idx in seq_len(n_lang)) {
    ft <- flextable::width(ft, j = 7L + idx, width = lang_w, unit = "in")
  }

  # Row-type indices for selective styling. The hierarchy is
  # group (most saturated) > question (zebra) > value (white);
  # welcome / endtext sit outside the data flow with accent borders.
  kinds <- vapply(rows, function(r) as.character(r$kind), character(1L))
  group_idx        <- which(kinds == "group")
  group_desc_idx   <- which(kinds == "group_description")
  question_idx     <- which(kinds %in% c("leaf", "subq", "other"))
  scale_header_idx <- which(kinds == "scale_header")
  welcome_idx      <- which(kinds %in% c("welcome", "endtext", "description"))
  exclusive_idx    <- which(kinds == "mc_exclusive")
  order_note_idx   <- which(kinds == "order_note")

  # Annotation rows (exclusive-option note, answer / option order note)
  # carry the same chrome sentence in every language column (a structural
  # annotation, not a translation), so merge the language columns into one
  # spanning cell -- the note shows once across the content width instead
  # of being repeated per language.
  if (n_lang > 1L) {
    for (ei in c(exclusive_idx, order_note_idx)) {
      ft <- flextable::merge_at(ft, i = ei, j = lang_j, part = "body")
    }
  }

  # Question rows: lighter zebra tint so the eye reads them as
  # "secondary" relative to group banners but still distinct from
  # the white value rows.
  for (qi in question_idx) {
    ft <- flextable::bg(ft, i = qi, bg = theme$color_zebra, part = "body")
  }
  # Scale-header rows (dual-scale arrays only) reuse the zebra tint
  # but bold the Value cell so they read as "subsection within the
  # values".
  for (sh in scale_header_idx) {
    ft <- flextable::bg(ft, i = sh, bg = theme$color_zebra, part = "body")
    ft <- flextable::bold(ft, i = sh, j = "Value", part = "body")
  }
  # Group rows: medium tint + 1.0 pt primary top filet (drawn as
  # the bottom border of the row above) to signal "new section
  # starts here" without dominating the page like the old dark
  # petrol banner did. Width harmonized with the cards template
  # group filet (1 pt) so the two layouts feel like the same
  # document family. When the group is the very first body row
  # the filet is dropped -- the table's natural top border already
  # marks the document opening.
  primary_filet <- officer::fp_border(color = theme$color_primary,
                                      width = 1)
  for (gi in group_idx) {
    ft <- flextable::bg(ft, i = gi, bg = theme$color_band, part = "body")
    if (gi > 1L) {
      ft <- flextable::hline(ft, i = gi - 1L, border = primary_filet,
                             part = "body")
    }
    ft <- flextable::padding(ft, i = gi, padding.top = 6, padding.bottom = 6,
                             padding.left = 4, padding.right = 4,
                             part = "body")
  }
  # Group description rows: same band tint as the group banner above so
  # the two read as a single section block; lighter padding since the
  # intro text follows the title directly.
  for (gd in group_desc_idx) {
    ft <- flextable::bg(ft, i = gd, bg = theme$color_band, part = "body")
    ft <- flextable::padding(ft, i = gd, padding.top = 4, padding.bottom = 6,
                             padding.left = 4, padding.right = 4,
                             part = "body")
  }
  # Welcome / End text rows: white background framed by 1 pt accent
  # borders so they read as "preface" / "epilogue" content sitting
  # outside the table's data flow. Same row-1 guard as for the
  # group filet.
  accent_border <- officer::fp_border(color = theme$color_accent, width = 1)
  for (wi in welcome_idx) {
    if (wi > 1L) {
      ft <- flextable::hline(ft, i = wi - 1L, border = accent_border,
                             part = "body")
    }
    ft <- flextable::hline(ft, i = wi, border = accent_border,
                           part = "body")
    ft <- flextable::padding(ft, i = wi, padding.top = 8, padding.bottom = 8,
                             padding.left = 4, padding.right = 4,
                             part = "body")
  }

  ft
}

# Helpers ----------------------------------------------------------

#' Replace `NULL` or empty strings with a fallback. Used inside the
#' rich-cell composer to avoid `as_chunk(NA)` or `as_chunk(NULL)`
#' which flextable rejects.
#' @keywords internal
#' @noRd
`%||_%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || is.na(a) || !nzchar(a)) b else a
}

# lss_html_to_text() is defined in R/html.R; the table template
# relies on the canonical implementation to keep stem / subq /
# answer-label text identical to what the cards template renders.
