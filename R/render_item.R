# Item rendering for the cards template: groups, leaf items, compound items, parent stems, subquestions, shared scales, answer rows, attribute rows, item-table assembly.
#
# Extracted from R/render_lss_docx.R.

#' Render one group: a banner, optional description, then each item
#'
#' Groups become a visible section banner (bold colored paragraph, not a
#' Heading style) followed by their items. Items themselves use Heading 1
#' so Word numbers them sequentially across the whole document, which keeps
#' the navigation index flat and ignores the group hierarchy in numbering.
#'
#' @keywords internal
#' @noRd
lss_render_group <- function(doc, group, langs, theme,
                             show_help, show_attrs, show_technical_attrs,
                             audit_idx, state,
                             show_groups = TRUE) {
  # The group index must still advance even when the banner itself is
  # hidden -- bookmarks, audit references and the TOC depend on it.
  state$group_index <- state$group_index + 1L
  # Render as a styled paragraph (no Heading 1 style) so Word does NOT
  # add its own list number, and the heading stays typographically
  # uniform (one font face/size/colour for the whole title).
  #
  # The asymmetric padding (24 pt above, 8 pt below) signals "section
  # break" at the right strength: the air above is roughly three times
  # the air below, so the eye reads the title as bound to the questions
  # that follow it. The under-line at 1 pt gives a clean banner finish
  # without enclosing the title in a box -- thinner than the previous
  # 1.5 pt so it does not visually compete with the dark meta-table
  # header bands of the items below it.
  if (isTRUE(show_groups)) {
    gname <- lss_first_label(group$names, langs)
    if (is.na(gname)) gname <- paste0("Group ", group$gid)
    # Strip a leading numeric prefix written by the LimeSurvey author:
    # group headings are not numbered (the name carries the meaning, and
    # forced numbering competed with the section structure), so a
    # leftover "1." would look like stray text.
    gname <- lss_strip_group_number_prefix(gname)
    heading_text <- gname
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          heading_text,
          prop = officer::fp_text(
            font.family = theme$font_body, font.size = theme$size_heading1,
            bold = TRUE, color = theme$color_primary
          )
        ),
        fp_p = officer::fp_par(
          padding.top = 24, padding.bottom = 8,
          border.bottom = officer::fp_border(
            color = theme$color_primary, width = 1
          ),
          # Keep the group title with the content that follows so it is
          # never stranded alone at the foot of a page.
          keep_with_next = TRUE
        )
      )
    )
    # Anchor the group heading with a bookmark so the manual TOC entries
    # can hyperlink to it.
    doc <- officer::body_bookmark(doc, lss_group_bookmark(state$group_index))
  }
  any_desc <- any(vapply(
    group$descriptions, function(v) !is.null(v) && !is.na(v) && nzchar(trimws(v)),
    logical(1)
  ))
  if (any_desc) {
    doc <- lss_render_lang_block(doc, group$descriptions, langs, theme,
                                 size = theme$size_subq, italic = TRUE)
  }

  for (q in group$questions) {
    doc <- lss_render_question_block(
      doc, q, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      show_technical_attrs = show_technical_attrs,
      audit_idx = audit_idx,
      state = state
    )
  }
  doc
}

#' Dispatch a parent question to leaf or compound rendering
#' @keywords internal
#' @noRd
lss_render_question_block <- function(doc, q, langs, theme,
                                      show_help, show_attrs,
                                      show_technical_attrs, audit_idx, state) {
  info <- lss_type_info(q$type)
  if (identical(q$type, "R")) {
    # Ranking: N items -> N data columns, one per rank position, named by
    # the i-th item's answer id (LimeSurvey's CSV form, e.g.
    # devicerank[59842]). Rendered like multiple choice.
    doc <- lss_render_ranking_item(
      doc, q, langs, theme,
      show_help = show_help, show_attrs = show_attrs,
      audit_idx = audit_idx, state = state
    )
  } else if (isTRUE(info$has_subquestions) && length(q$subquestions) > 0L) {
    doc <- lss_render_compound_question(
      doc, q, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      show_technical_attrs = show_technical_attrs,
      audit_idx = audit_idx,
      info = info,
      state = state
    )
  } else {
    doc <- lss_render_leaf_item(
      doc, q, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      show_technical_attrs = show_technical_attrs,
      audit_idx = audit_idx,
      item_code = q$code,
      texts_by_lang = lapply(langs, function(lg) q$texts[[lg]]$question),
      help_by_lang = lapply(langs, function(lg) q$texts[[lg]]$help),
      state = state
    )
  }
  # When the question's `other` flag is set, LimeSurvey adds a free-text
  # input as a sibling response variable named `parent_other`. We
  # document it as an item in its own right so the reader sees the
  # variable and the customized prompt.
  if (identical(q$other, "Y")) {
    doc <- lss_render_other_item(
      doc, q, langs, theme,
      audit_idx = audit_idx,
      state = state
    )
  }
  # List-with-comment (type O) adds a free-text comment column.
  if (identical(q$type, "O")) {
    doc <- lss_render_comment_item(
      doc, q, langs, theme,
      audit_idx = audit_idx,
      state = state
    )
  }
  doc
}

#' Render a compound question (a parent with subquestions)
#'
#' Variable-centric rendering: each subquestion becomes its own
#' self-contained block (meta table + item table), with the parent stem
#' shown as the "Question" row, the subquestion label as a "Subquestion"
#' row, and the answer modalities (when any) repeated underneath. The
#' parent code itself is not surfaced as a meta entry because LimeSurvey
#' does not create a data variable named after the parent of an array,
#' multiple-choice, or multi-numerical question -- only the
#' `parent_subqcode` columns exist in the export. Repeating the stem and
#' the scale per subquestion is intentionally redundant: every variable
#' the reviewer can encounter in the dataset has all of its context in a
#' single block.
#'
#' @keywords internal
#' @noRd
lss_render_compound_question <- function(doc, q, langs, theme,
                                         show_help, show_attrs,
                                         show_technical_attrs, audit_idx,
                                         info, state) {
  # Multiple-choice questions (types M / P) are question-centric: the
  # stem appears once and every option is a row carrying its own Yes/No
  # variable code. Arrays, multiple-numeric and multiple-short-text keep
  # the variable-centric per-subquestion blocks (each subquestion is a
  # genuinely distinct input, not one option of a shared battery).
  if (identical(info$family, "multiple")) {
    return(lss_render_multiple_choice(
      doc, q, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      audit_idx = audit_idx,
      state = state
    ))
  }
  # Dual-scale arrays (type 1): each subquestion yields TWO data columns,
  # one per scale, and each column is a single-choice variable in its own
  # right. We therefore render one self-contained block per
  # (subquestion x scale), exactly like a plain array decomposes into one
  # single-choice block per subquestion -- with a "Scale" row naming the
  # scale (the dualscale header) and the real per-scale variable name
  # (`<q>_<subq>_<1|2>`, the 1-based scale index LimeSurvey uses in its
  # data export, e.g. `trustinstitutions[PARL][1]`). The dual-scale
  # grouping is a presentation detail with no methodological weight, so we
  # do not preserve it specially.
  dual_scale <- isTRUE(info$has_scales) &&
    !is.null(q$scales) && length(q$scales) > 1L
  # Two-dimensional arrays (array numbers ":", array texts ";", array by
  # column "H"): subquestions live on two axes -- rows on scale 0, columns
  # on scale 1 -- so each data column is the (row x column) cross product,
  # e.g. `adaptability[NEW_NOW]`. Render one block per cell, with the row
  # label on the "Subquestion" line and the column label on a "Column"
  # line.
  sq_scale <- function(s) {
    if (is.null(s$scale_id) || is.na(s$scale_id)) "0" else as.character(s$scale_id)
  }
  two_d <- !dual_scale &&
    length(unique(vapply(q$subquestions, sq_scale, character(1)))) > 1L
  if (two_d) {
    rows_sq <- Filter(function(s) sq_scale(s) == "0", q$subquestions)
    cols_sq <- Filter(function(s) sq_scale(s) != "0", q$subquestions)
    for (rsq in rows_sq) {
      for (csq in cols_sq) {
        item_code <- lss_variable_name(
          q$code, paste0(rsq$code, "_", csq$code),
          style = theme$variable_names
        )
        doc <- lss_render_subq_item(
          doc, q, rsq, langs, theme,
          item_code = item_code,
          show_help = show_help,
          show_attrs = show_attrs,
          audit_idx = audit_idx,
          state = state,
          column = csq
        )
      }
    }
    return(doc)
  }
  for (sq in q$subquestions) {
    if (dual_scale) {
      for (si in seq_along(q$scales)) {
        item_code <- lss_variable_name(q$code, sq$code, si,
                                       theme$variable_names)
        doc <- lss_render_subq_item(
          doc, q, sq, langs, theme,
          item_code = item_code,
          show_help = show_help,
          show_attrs = show_attrs,
          audit_idx = audit_idx,
          state = state,
          scale = list(
            answers = q$scales[[si]],
            header = lss_dualscale_header(q, si, langs),
            index = si
          )
        )
      }
    } else {
      doc <- lss_render_subq_item(
        doc, q, sq, langs, theme,
        item_code = lss_variable_name(q$code, sq$code,
                                      style = theme$variable_names),
        show_help = show_help,
        show_attrs = show_attrs,
        audit_idx = audit_idx,
        state = state
      )
    }
  }
  doc
}

#' Render a ranking question (R) as one grouped card
#'
#' A ranking of N items produces N data columns, one per rank position,
#' which LimeSurvey names by the i-th item's answer id (e.g.
#' `devicerank[59842]`). Rendered like multiple choice: the variable
#' family `parent[*]` in the meta band, a "Positions" section listing each
#' position column (the bare answer id, with its rank), and a "Value"
#' section listing the rankable items (the codes that can appear as a
#' cell's value). Every position column is registered in the variable
#' index.
#'
#' @keywords internal
#' @noRd
lss_render_ranking_item <- function(doc, q, langs, theme,
                                    show_help, show_attrs, audit_idx, state) {
  chrome <- theme$chrome
  state$item_no <- state$item_no + 1L
  item_no <- state$item_no
  n <- length(q$answers)
  for (a in q$answers) {
    state$index_entries[[length(state$index_entries) + 1L]] <- list(
      code = lss_variable_name(q$code, a$aid, style = theme$variable_names),
      no = item_no
    )
  }
  doc <- lss_render_item_spacer(doc, theme)
  if (isTRUE(state$show_item_heading)) {
    audit_marker <- lss_audit_marker(q$code, audit_idx, theme)
    heading_text <- sprintf("%d. %s", item_no, q$code)
    if (!is.null(audit_marker)) {
      heading_text <- paste0(heading_text, "  ", audit_marker$text)
    }
    heading_prop <- officer::fp_text(
      font.family = theme$font_body, font.size = theme$size_heading2,
      bold = TRUE,
      color = if (is.null(audit_marker)) theme$color_text else audit_marker$color
    )
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(heading_text, prop = heading_prop),
        fp_p = officer::fp_par(padding.top = 8, padding.bottom = 2)
      )
    )
  }
  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = item_no,
    variable = lss_variable_name(q$code, "*", style = theme$variable_names),
    type = q$type, type_label = lss_localized_type_label(q, theme),
    mandatory = q$mandatory, relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  doc <- lss_render_intra_item_gap(doc, theme)

  parent_text <- lapply(langs, function(lg) q$texts[[lg]]$question)
  parent_help <- lapply(langs, function(lg) q$texts[[lg]]$help)
  blank <- stats::setNames(as.list(rep("", length(langs))), langs)
  rows <- list()
  rows[[length(rows) + 1L]] <- list(
    label = chrome$item_question,
    texts = stats::setNames(parent_text, langs),
    size = theme$size_question
  )
  if (isTRUE(show_help) && lss_any_present(parent_help)) {
    rows[[length(rows) + 1L]] <- list(
      label = chrome$item_help,
      texts = stats::setNames(parent_help, langs),
      size = theme$size_help, color = theme$color_muted, italic = TRUE
    )
  }
  rows <- c(rows, lss_attr_rows(q, langs, theme, show_attrs))
  # Positions section: one row per rank slot. The bare answer id is the
  # suffix that fills the `[*]` family in the meta band; the rank it
  # records is the chrome annotation across the language columns.
  rows[[length(rows) + 1L]] <- list(
    label = sprintf("%s (%d)", chrome$item_positions, n),
    texts = blank, size = theme$size_meta, section_header = TRUE
  )
  for (i in seq_along(q$answers)) {
    rows[[length(rows) + 1L]] <- list(
      label = as.character(q$answers[[i]]$aid),
      texts = stats::setNames(
        rep(list(sprintf(chrome$item_rank_fmt, i)), length(langs)), langs
      ),
      size = theme$size_answer, code_row = TRUE, span_note = TRUE
    )
  }
  # Value section: the rankable items -- the codes that can appear as a
  # position column's value. A non-default presentation order of the
  # items (answer_order) is noted on the section header.
  vrows <- list(list(
    label = chrome$item_value, texts = blank,
    size = theme$size_meta, section_header = TRUE
  ))
  for (a in q$answers) {
    vrows[[length(vrows) + 1L]] <- list(
      label = a$code,
      texts = stats::setNames(lapply(langs, function(lg) a$labels[[lg]]), langs),
      size = theme$size_answer, value_row = TRUE
    )
  }
  vrows <- lss_apply_order_note(vrows, lss_answer_order_note(q, theme),
                               langs, theme)
  rows <- c(rows, vrows)
  lss_render_item_table(doc, theme, langs, rows)
}

#' Render a multiple-choice question (M / P) as one grouped card
#'
#' Question-centric layout: a single meta band (with the variable family
#' shown as `parent_*`), the stem once, then an `Options` section listing
#' every response option as a row -- its full LimeSurvey variable code
#' (`parent_subqcode`) in monospace next to the option label per language
#' -- and a single localized coding line (Y = selected, blank = not).
#' Every individual option variable is still registered in the variable
#' index, so a reviewer looking up `parent_subqcode` is routed back to
#' this question. The parent code itself is never a data column, hence the
#' `parent_*` family notation rather than a bare parent code.
#'
#' @keywords internal
#' @noRd
lss_render_multiple_choice <- function(doc, q, langs, theme,
                                       show_help, show_attrs, audit_idx, state) {
  state$item_no <- state$item_no + 1L
  item_no <- state$item_no
  # Register every option variable in the index (lookup preserved).
  for (sq in q$subquestions) {
    state$index_entries[[length(state$index_entries) + 1L]] <- list(
      code = lss_variable_name(q$code, sq$code, style = theme$variable_names),
      no = item_no
    )
  }

  doc <- lss_render_item_spacer(doc, theme)
  if (isTRUE(state$show_item_heading)) {
    audit_marker <- lss_audit_marker(q$code, audit_idx, theme)
    heading_text <- sprintf("%d. %s", item_no, q$code)
    if (!is.null(audit_marker)) {
      heading_text <- paste0(heading_text, "  ", audit_marker$text)
    }
    heading_prop <- officer::fp_text(
      font.family = theme$font_body, font.size = theme$size_heading2,
      bold = TRUE,
      color = if (is.null(audit_marker)) theme$color_text else audit_marker$color
    )
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(heading_text, prop = heading_prop),
        fp_p = officer::fp_par(padding.top = 8, padding.bottom = 2)
      )
    )
  }

  # Meta band: the variable FAMILY (parent_*), not a single column -- the
  # type / mandatory / filter are all question-level and shared by every
  # option.
  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = item_no,
    variable = lss_variable_name(q$code, "*", style = theme$variable_names),
    type = q$type, type_label = lss_localized_type_label(q, theme),
    mandatory = q$mandatory, relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  doc <- lss_render_intra_item_gap(doc, theme)

  parent_text <- lapply(langs, function(lg) q$texts[[lg]]$question)
  parent_help <- lapply(langs, function(lg) q$texts[[lg]]$help)

  rows <- list()
  rows[[length(rows) + 1L]] <- list(
    label = theme$chrome$item_question,
    texts = stats::setNames(parent_text, langs),
    size = theme$size_question
  )
  if (isTRUE(show_help) && lss_any_present(parent_help)) {
    rows[[length(rows) + 1L]] <- list(
      label = theme$chrome$item_help,
      texts = stats::setNames(parent_help, langs),
      size = theme$size_help,
      color = theme$color_muted,
      italic = TRUE
    )
  }
  # Parent-level attributes (prefix, suffix, validation, ...).
  rows <- c(rows, lss_attr_rows(q, langs, theme, show_attrs))
  # Options section header, with the option count. A non-default
  # presentation order of the options (subquestion_order = random /
  # alphabetical) is noted in the language-column area of this header.
  opt_note <- lss_answer_order_note(q, theme, attr = "subquestion_order")
  rows[[length(rows) + 1L]] <- list(
    label = sprintf("%s (%d)", theme$chrome$item_options,
                    length(q$subquestions)),
    texts = if (is.null(opt_note)) {
      stats::setNames(as.list(rep("", length(langs))), langs)
    } else {
      stats::setNames(rep(list(opt_note), length(langs)), langs)
    },
    size = theme$size_meta,
    section_header = TRUE,
    section_with_text = !is.null(opt_note),
    span_note = !is.null(opt_note),
    color = if (!is.null(opt_note)) theme$color_muted else NULL,
    italic = !is.null(opt_note)
  )
  # One row per option: the option suffix only (the LimeSurvey
  # subquestion code) in monospace, next to the option label per
  # language. The shared prefix is announced once in the meta band as
  # `<parent>_*`, so the full column name is fully determined on the
  # card (`<parent>_*` + suffix) without repeating the long prefix on
  # every row, which would wrap the narrow label column. The complete
  # names `<parent>_<suffix>` are also listed verbatim in the variable
  # index for direct lookup.
  for (sq in q$subquestions) {
    rows[[length(rows) + 1L]] <- list(
      label = sq$code,
      texts = stats::setNames(
        lapply(langs, function(lg) sq$texts[[lg]]$question), langs
      ),
      size = theme$size_answer,
      code_row = TRUE
    )
  }
  # Exclusive option(s): a subquestion flagged `exclude_all_others`
  # (e.g. "None of the above") clears every other selection when
  # checked. Surface it once, naming the option suffix(es) -- the same
  # information the per-subquestion layout shows on the exclusive item.
  excl_codes <- lss_exclusive_codes(q)
  if (length(excl_codes) > 0L) {
    excl_text <- sprintf(theme$chrome$exclusive_text_fmt,
                         paste(excl_codes, collapse = ", "))
    rows[[length(rows) + 1L]] <- list(
      label = theme$chrome$item_exclusive,
      texts = stats::setNames(rep(list(excl_text), length(langs)), langs),
      size = theme$size_meta,
      color = theme$color_muted,
      italic = TRUE,
      span_note = TRUE
    )
  }
  # Coding descriptor as the same "Value" band row every other variable
  # type uses (lss_value_implicit_row), so the multiple-choice card is
  # consistent with single-choice, Yes/No, numeric, etc. -- one "Value"
  # row carrying the localized "Y = selected, blank = not selected"
  # (with the comment-variable note for type P).
  vrow <- lss_value_implicit_row(q, langs, theme)
  if (!is.null(vrow)) rows[[length(rows) + 1L]] <- vrow

  lss_render_item_table(doc, theme, langs, rows)
}

#' Render the LimeSurvey "Other:" text input as a standalone item
#'
#' When a question has `other = "Y"`, LimeSurvey generates an
#' additional response variable named `<parent>_other` that holds the
#' free text the respondent typed in the "Other:" field. The prompt
#' shown next to that input can be customized via the
#' `other_replace_text` attribute (per language). We surface this as
#' its own numbered item so the reader sees the variable code in the
#' index and meta table, alongside any other items.
#'
#' @keywords internal
#' @noRd
lss_render_other_item <- function(doc, q, langs, theme, audit_idx, state) {
  state$item_no <- state$item_no + 1L
  item_code <- lss_other_variable(q, theme$variable_names)
  state$index_entries[[length(state$index_entries) + 1L]] <- list(
    code = item_code, no = state$item_no
  )
  doc <- lss_render_item_spacer(doc, theme)

  # Look up the customized "Other:" prompt per language; fall back to a
  # generic "Other:" label when the attribute is missing.
  prompt_for_lang <- function(lg) {
    if (is.null(q$attributes) || nrow(q$attributes) == 0L) return("Other:")
    attrs <- q$attributes[q$attributes$attribute == "other_replace_text", , drop = FALSE]
    if (nrow(attrs) == 0L) return("Other:")
    lang_hit <- attrs$value[attrs$language == lg]
    if (length(lang_hit) > 0L && nzchar(trimws(lang_hit[1]))) return(lang_hit[1])
    empty_lang <- attrs$value[!nzchar(attrs$language)]
    if (length(empty_lang) > 0L && nzchar(trimws(empty_lang[1]))) return(empty_lang[1])
    "Other:"
  }
  texts_by_lang <- stats::setNames(lapply(langs, prompt_for_lang), langs)

  if (isTRUE(state$show_item_heading)) {
    heading_text <- sprintf("%d. %s", state$item_no, item_code)
    heading_prop <- officer::fp_text(
      font.family = theme$font_body, font.size = theme$size_heading2,
      bold = TRUE, color = theme$color_text
    )
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(heading_text, prop = heading_prop),
        fp_p = officer::fp_par(padding.top = 8, padding.bottom = 2)
      )
    )
  }

  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = state$item_no,
    variable = item_code,
    type = "T", type_label = theme$chrome$type_text_other,
    mandatory = "N",
    relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  doc <- lss_render_intra_item_gap(doc, theme)
  rows <- list(list(
    label = theme$chrome$item_question,
    texts = texts_by_lang,
    size = theme$size_question
  ))
  # The "Other:" field is a single-line free-text input, so document its
  # response shape in the Value section like any other free-text item.
  rows[[length(rows) + 1L]] <- list(
    label = theme$chrome$item_value,
    texts = stats::setNames(
      rep(list(theme$chrome$value_free_text_short), length(langs)), langs
    ),
    size = theme$size_answer,
    section_header = TRUE,
    section_with_text = TRUE,
    span_note = TRUE
  )
  lss_render_item_table(doc, theme, langs, rows)
}

#' Render the free-text comment of a "list with comment" question (type O)
#'
#' A list-with-comment question yields a second data column holding the
#' respondent's free-text comment, which LimeSurvey names `<parent>[_Ccomment]`
#' in its CSV/Excel export. We surface it as its own numbered item so the
#' reader sees the variable in the index and meta table alongside the list.
#'
#' @keywords internal
#' @noRd
lss_render_comment_item <- function(doc, q, langs, theme, audit_idx, state) {
  state$item_no <- state$item_no + 1L
  item_code <- lss_variable_name(q$code, "_Ccomment",
                                 style = theme$variable_names)
  state$index_entries[[length(state$index_entries) + 1L]] <- list(
    code = item_code, no = state$item_no
  )
  doc <- lss_render_item_spacer(doc, theme)
  if (isTRUE(state$show_item_heading)) {
    heading_text <- sprintf("%d. %s", state$item_no, item_code)
    heading_prop <- officer::fp_text(
      font.family = theme$font_body, font.size = theme$size_heading2,
      bold = TRUE, color = theme$color_text
    )
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(heading_text, prop = heading_prop),
        fp_p = officer::fp_par(padding.top = 8, padding.bottom = 2)
      )
    )
  }
  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = state$item_no,
    variable = item_code,
    type = "S", type_label = theme$chrome$type_text_short,
    mandatory = "N",
    relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  doc <- lss_render_intra_item_gap(doc, theme)
  stem <- lapply(langs, function(lg) q$texts[[lg]]$question)
  rows <- list(
    list(label = theme$chrome$item_question,
         texts = stats::setNames(stem, langs),
         size = theme$size_question),
    # A "Comment" line names what this free-text column records, then the
    # Value section documents its response shape (single-line free text).
    list(label = theme$chrome$item_comment,
         texts = stats::setNames(
           rep(list(theme$chrome$value_free_text_short), length(langs)), langs
         ),
         size = theme$size_answer, section_header = TRUE,
         section_with_text = TRUE, span_note = TRUE)
  )
  lss_render_item_table(doc, theme, langs, rows)
}

#' Render a subquestion as a fully self-contained numbered item
#'
#' Each subquestion of a compound question (array, multiple choice,
#' multiple numerical, dual-scale array) is rendered as its own block
#' with the same shape as a leaf item:
#'
#' - meta table keyed by `parent_subqcode` (the actual data variable),
#' - item table whose first row ("Question") is the parent stem,
#'   second row ("Subquestion") is the subquestion label, optional
#'   "Help" row from the parent, then any subquestion-level attributes,
#'   then -- for types that carry an enumerated scale -- the parent's
#'   answer modalities repeated as a "Value" section + value rows.
#'
#' @keywords internal
#' @noRd
lss_render_subq_item <- function(doc, q, sq, langs, theme,
                                 item_code, show_help, show_attrs,
                                 audit_idx, state, scale = NULL,
                                 column = NULL) {
  state$item_no <- state$item_no + 1L
  state$index_entries[[length(state$index_entries) + 1L]] <- list(
    code = item_code, no = state$item_no
  )
  doc <- lss_render_item_spacer(doc, theme)
  if (isTRUE(state$show_item_heading)) {
    audit_marker <- lss_audit_marker(item_code, audit_idx, theme)
    heading_text <- sprintf("%d. %s", state$item_no, item_code)
    if (!is.null(audit_marker)) {
      heading_text <- paste0(heading_text, "  ", audit_marker$text)
    }
    heading_prop <- officer::fp_text(
      font.family = theme$font_body, font.size = theme$size_heading2,
      bold = TRUE,
      color = if (is.null(audit_marker)) theme$color_text else audit_marker$color
    )
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(heading_text, prop = heading_prop),
        fp_p = officer::fp_par(padding.top = 8, padding.bottom = 2)
      )
    )
  }
  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = state$item_no,
    variable = item_code,
    type = q$type, type_label = lss_localized_type_label(q, theme),
    mandatory = q$mandatory, relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  doc <- lss_render_intra_item_gap(doc, theme)

  parent_text <- lapply(langs, function(lg) q$texts[[lg]]$question)
  parent_help <- lapply(langs, function(lg) q$texts[[lg]]$help)
  subq_text  <- stats::setNames(
    lapply(langs, function(lg) sq$texts[[lg]]$question), langs
  )

  rows <- list()
  rows[[length(rows) + 1L]] <- list(
    label = theme$chrome$item_question,
    texts = stats::setNames(parent_text, langs),
    size = theme$size_question
  )
  # The "Subquestion" line carries the row item, with the second-axis
  # facet appended in parentheses when present: the dual-scale header
  # ("Parliament (Trust)") or the 2-D array column ("Trying new things
  # (Today)"). One self-contained, jargon-free item label for both, with no
  # empty parentheses when the facet has no label in a given language.
  subq_merged <- lapply(langs, function(lg) {
    s <- subq_text[[lg]]
    f <- lss_subq_facet(scale, column, lg, theme)
    s_ok <- !is.null(s) && !is.na(s) && nzchar(trimws(s))
    if (!is.null(f)) {
      if (s_ok) paste0(s, " (", f, ")") else f
    } else {
      s
    }
  })
  if (lss_any_present(subq_merged)) {
    rows[[length(rows) + 1L]] <- list(
      label = theme$chrome$item_subquestion,
      texts = stats::setNames(subq_merged, langs),
      size = theme$size_subq
    )
  }
  if (isTRUE(show_help) && lss_any_present(parent_help)) {
    rows[[length(rows) + 1L]] <- list(
      label = theme$chrome$item_help,
      texts = stats::setNames(parent_help, langs),
      size = theme$size_help,
      color = theme$color_muted,
      italic = TRUE
    )
  }
  # Subquestion-level attributes first, then parent-level (prefix,
  # suffix, validation, ...). `exclude_all_others*` are deliberately
  # filtered out of these generic loops: their meaning is "this single
  # subquestion is the exclusive one" and surfacing them on every
  # subquestion would lie about the rule. We emit one targeted row via
  # `lss_exclusive_row()` instead, only when THIS subquestion is the
  # named exclusive entry.
  rows <- c(rows, lss_attr_rows(sq, langs, theme, show_attrs))
  rows <- c(rows, lss_attr_rows(q, langs, theme, show_attrs))
  exclusive <- lss_exclusive_row(q, sq, langs, theme)
  if (!is.null(exclusive)) rows[[length(rows) + 1L]] <- exclusive
  # Value section. For a dual-scale block this is THIS scale's answers
  # only (the block is a single-choice variable). Otherwise: enumerated
  # codes (F, 1) or a single implicit-format row describing the response
  # shape (M/P "Y selected", K "Numeric", ...).
  if (!is.null(scale)) {
    rows <- c(rows, lss_value_section_rows(scale$answers, langs, theme))
  } else if (length(q$answers) > 0L) {
    rows <- c(rows, lss_answer_rows(q, langs, theme))
  } else {
    pre <- lss_predefined_value_rows(q, langs, theme)
    if (!is.null(pre)) {
      rows <- c(rows, pre)
    } else {
      vrow <- lss_value_implicit_row(q, langs, theme)
      if (!is.null(vrow)) rows[[length(rows) + 1L]] <- vrow
    }
  }
  doc <- lss_render_item_table(doc, theme, langs, rows)
  doc
}

#' Render a leaf question (no subquestions) as a numbered item
#' @keywords internal
#' @noRd
lss_render_leaf_item <- function(doc, q, langs, theme,
                                 show_help, show_attrs, show_technical_attrs,
                                 audit_idx, item_code,
                                 texts_by_lang, help_by_lang, state) {
  state$item_no <- state$item_no + 1L
  state$index_entries[[length(state$index_entries) + 1L]] <- list(
    code = item_code, no = state$item_no
  )
  doc <- lss_render_item_spacer(doc, theme)
  if (isTRUE(state$show_item_heading)) {
    audit_marker <- lss_audit_marker(item_code, audit_idx, theme)
    heading_text <- sprintf("%d. %s", state$item_no, item_code)
    if (!is.null(audit_marker)) {
      heading_text <- paste0(heading_text, "  ", audit_marker$text)
    }
    heading_prop <- officer::fp_text(
      font.family = theme$font_body, font.size = theme$size_heading2,
      bold = TRUE,
      color = if (is.null(audit_marker)) theme$color_text else audit_marker$color
    )
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(heading_text, prop = heading_prop),
        fp_p = officer::fp_par(padding.top = 8, padding.bottom = 2)
      )
    )
  }

  # Structured meta table: No | Variable | Type | Mandatory | Filter
  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = state$item_no,
    variable = q$code,
    type = q$type, type_label = lss_localized_type_label(q, theme),
    mandatory = q$mandatory, relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  doc <- lss_render_intra_item_gap(doc, theme)

  # Build the unified item table: Question, optional Help, then one
  # row per answer option (for has_answers leaf types like L, !, O).
  rows <- list()
  rows[[length(rows) + 1L]] <- list(
    label = theme$chrome$item_question,
    texts = stats::setNames(texts_by_lang, langs),
    size = theme$size_question
  )
  if (isTRUE(show_help) && lss_any_present(help_by_lang)) {
    rows[[length(rows) + 1L]] <- list(
      label = theme$chrome$item_help,
      texts = stats::setNames(help_by_lang, langs),
      size = theme$size_help,
      color = theme$color_muted,
      italic = TRUE
    )
  }
  # Question attributes (prefix, suffix, validation, ...) as italic rows
  # inside the item table itself, between Help and the Value section.
  rows <- c(rows, lss_attr_rows(q, langs, theme, show_attrs))
  # Value section: enumerated codes (L, !, F, 1) when q$answers is
  # populated; otherwise a single implicit-format row describing the
  # response shape (Y "Y = Yes, N = No", N "Numeric input", T "Free
  # text", ...). Skips entirely for X (boilerplate).
  if (length(q$answers) > 0L) {
    vrows <- lss_answer_rows(q, langs, theme)
  } else {
    pre <- lss_predefined_value_rows(q, langs, theme)
    if (!is.null(pre)) {
      vrows <- pre
    } else {
      vrow <- lss_value_implicit_row(q, langs, theme)
      vrows <- if (!is.null(vrow)) list(vrow) else list()
    }
  }
  # Note a non-default answer order ("Labels shown in random / alphabetical
  # order") on the Value header. Leaf single-choice only -- arrays are
  # handled through the subquestion branch and intentionally left aside.
  vrows <- lss_apply_order_note(vrows, lss_answer_order_note(q, theme),
                                langs, theme)
  rows <- c(rows, vrows)
  doc <- lss_render_item_table(doc, theme, langs, rows)
  doc
}

#' Per-language header text for one scale of a dual-scale array (type 1)
#'
#' Reads `dualscale_headerA` (scale 1) or `dualscale_headerB` (scale 2)
#' from the question attributes and returns a list named by language, or
#' `NULL` when the question defines no header for that scale.
#'
#' @keywords internal
#' @noRd
lss_dualscale_header <- function(q, scale_idx, langs) {
  if (is.null(q$attributes) || nrow(q$attributes) == 0L) return(NULL)
  attr_name <- if (scale_idx == 1L) "dualscale_headerA" else "dualscale_headerB"
  rows <- q$attributes[q$attributes$attribute == attr_name, , drop = FALSE]
  if (nrow(rows) == 0L) return(NULL)
  pick <- function(lg) {
    v <- rows$value[rows$language == lg]
    if (length(v) && !is.na(v[1]) && nzchar(trimws(v[1]))) return(v[1])
    ev <- rows$value[is.na(rows$language) | !nzchar(rows$language)]
    if (length(ev) && !is.na(ev[1]) && nzchar(trimws(ev[1]))) ev[1] else NA_character_
  }
  vals <- stats::setNames(lapply(langs, pick), langs)
  if (all(vapply(vals, function(v) is.na(v) || !nzchar(trimws(v)), logical(1)))) {
    return(NULL)
  }
  vals
}

#' Second-axis facet label for a subquestion block, in one language
#'
#' The facet appended in parentheses to the "Subquestion" line: for a
#' dual-scale array, the translated `dualscale_header` (e.g. "Trust"), or a
#' "Scale N" fallback when the author left it blank; for a 2-D array, the
#' column subquestion label (e.g. "Today"). Returns `NULL` when there is no
#' facet for this language, so the caller can omit the parentheses.
#'
#' @keywords internal
#' @noRd
lss_subq_facet <- function(scale, column, lg, theme) {
  if (!is.null(scale)) {
    h <- if (!is.null(scale$header)) scale$header[[lg]] else NULL
    if (!is.null(h) && !is.na(h) && nzchar(trimws(h))) return(h)
    return(sprintf("%s %d", theme$chrome$item_scale, scale$index))
  }
  if (!is.null(column)) {
    v <- column$texts[[lg]]$question
    if (!is.null(v) && !is.na(v) && nzchar(trimws(v))) return(v)
  }
  NULL
}

#' "Value" section header plus one row per answer for a plain answer list
#'
#' The single-scale core of [lss_answer_rows()]: a "Value" section header
#' followed by one row per code (code on the left, label per language on
#' the right). Used directly for one scale of a dual-scale array, where
#' each scale is rendered as its own single-choice block.
#'
#' @keywords internal
#' @noRd
lss_value_section_rows <- function(answers, langs, theme) {
  out <- list(list(
    label = theme$chrome$item_value,
    texts = stats::setNames(as.list(rep("", length(langs))), langs),
    size = theme$size_meta, section_header = TRUE
  ))
  for (a in answers) {
    out[[length(out) + 1L]] <- list(
      label = a$code,
      texts = stats::setNames(lapply(langs, function(lg) a$labels[[lg]]), langs),
      size = theme$size_answer, value_row = TRUE
    )
  }
  out
}

#' Build the rows that document the answer scale of a (sub)question
#'
#' Emits a "Value" section header followed by one row per answer option,
#' code on the left, label per language on the right. Returns an empty
#' list when the question carries no enumerated answers (e.g.
#' multiple-choice M, free numeric input K) -- in those cases the coding
#' row already documents the response value mapping. Dual-scale arrays
#' (type 1) no longer reach this helper: each scale is rendered as its
#' own single-choice block via [lss_value_section_rows()] upstream, so
#' the multi-scale branch here is retained only as a defensive fallback.
#'
#' @keywords internal
#' @noRd
lss_answer_rows <- function(q, langs, theme) {
  if (length(q$answers) == 0L) return(list())
  out <- list()
  multi_scale <- !is.null(q$scales) && length(q$scales) > 1L
  bundles <- if (multi_scale) q$scales else list(q$answers)
  for (si in seq_along(bundles)) {
    answers <- bundles[[si]]
    if (length(answers) == 0L) next
    if (multi_scale) {
      # Dual-scale array: label the scale and, when the question defines
      # them, show the per-scale header (dualscale_headerA / _headerB),
      # translated across the language columns.
      hdr <- lss_dualscale_header(q, si, langs)
      out[[length(out) + 1L]] <- if (!is.null(hdr)) {
        list(label = sprintf(theme$chrome$item_value_scale_fmt, si),
             texts = hdr, size = theme$size_meta,
             section_header = TRUE, section_with_text = TRUE)
      } else {
        list(label = sprintf(theme$chrome$item_value_scale_fmt, si),
             texts = stats::setNames(as.list(rep("", length(langs))), langs),
             size = theme$size_meta, section_header = TRUE)
      }
    } else {
      out[[length(out) + 1L]] <- list(
        label = theme$chrome$item_value,
        texts = stats::setNames(as.list(rep("", length(langs))), langs),
        size = theme$size_meta, section_header = TRUE
      )
    }
    for (a in answers) {
      out[[length(out) + 1L]] <- list(
        label = a$code,
        texts = stats::setNames(lapply(langs, function(lg) a$labels[[lg]]), langs),
        size = theme$size_answer,
        value_row = TRUE
      )
    }
  }
  out
}

#' Render a unified item table with a left "Label" column
#'
#' Builds a single flextable per item with the layout
#' `Language | Fran\u00E7ais | Deutsch | ...` as header and one body row per
#' content element (`Question`, `Help`, `Value 1`, `Value 2`, ...).
#' Each row carries its own label so the document reads as
#' self-describing: the reviewer sees `Question:` and `Help:` rather
#' than having to infer it from position. This is the ESS/MOSAiCH
#' convention where each tabular row names what it represents.
#'
#' @param rows A list of `list(label = "...", texts = list(lang = "..."), size = ...)`.
#'   `texts` is a named list keyed by language code; `size` is the body
#'   font size for that row (defaults to `theme$size_question` when
#'   omitted). Rows missing or empty across every language are kept
#'   (we never silently drop content), with cells filled by the muted
#'   em-dash placeholder.
#' @keywords internal
#' @noRd
lss_render_item_table <- function(doc, theme, langs, rows) {
  if (length(rows) == 0L) return(doc)

  df <- data.frame(
    Label = vapply(rows, function(r) r$label, character(1)),
    stringsAsFactors = FALSE
  )
  for (lg in langs) df[[lg]] <- ""

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft,
    values = c(
      list(Label = theme$chrome$item_language),
      stats::setNames(as.list(lss_language_label(langs)), langs)
    )
  )
  for (i in seq_along(rows)) {
    # The cards item table renders every row at one uniform size (the
    # content size, theme$size_question), matching the meta band. The
    # question reads as a questionnaire; the row-type and code
    # distinctions are carried by weight, italic and colour -- not size.
    # The per-row `size` field is kept on the row specs for the table
    # template, which sizes differently.
    sz <- theme$size_question
    italic <- isTRUE(rows[[i]]$italic)
    color <- if (!is.null(rows[[i]]$color)) rows[[i]]$color else theme$color_text
    is_section <- isTRUE(rows[[i]]$section_header)
    section_with_text <- isTRUE(rows[[i]]$section_with_text)
    is_blank <- isTRUE(rows[[i]]$blank)
    for (lg in langs) {
      if ((is_section && !section_with_text) || is_blank) {
        # Section header row, or a value row with no stored label (an
        # N-point numeric scale): language cells stay truly blank (no
        # em-dash placeholder), so the absent label is not mistaken for
        # a missing translation.
        ft <- flextable::compose(
          ft, i = i, j = lg,
          value = flextable::as_paragraph(flextable::as_chunk(""))
        )
      } else {
        ft <- flextable::compose(
          ft, i = i, j = lg,
          value = lss_compose(rows[[i]]$texts[[lg]], theme,
                              size = sz, color = color, italic_default = italic)
        )
      }
    }
  }
  ft <- lss_table_polish(ft, theme, lang_cols = langs,
                         body_size = theme$size_question,
                         header_size = theme$size_question)
  ft <- flextable::bold(ft, j = "Label", part = "body")
  ft <- flextable::color(ft, j = "Label", color = theme$color_primary, part = "body")
  ft <- flextable::align(ft, j = "Label", align = "left", part = "body")
  # Answer-code rows (label = "1", "2", ...) are centered to match the
  # shared scale convention from earlier renders: the Value section header
  # Code cells share one treatment across the whole document: answer
  # values (Value section, `value_row`) and multiple-choice option
  # suffixes (Options section, `code_row`) are both short LimeSurvey
  # codes, so both render in the monospace face, right-aligned. Right
  # alignment is the value-label listing convention (SPSS `label list`,
  # Stata, GESIS, the Swiss Household Panel): the code is pushed against
  # the adjacent language column so it sits next to the label it maps
  # to, and numeric codes line up by units (1, 2, ..., 10). The section
  # labels (Question, Help, Options, Value) stay left -- they are words,
  # not codes -- so the column reads "label on the left, code hugging
  # its meaning on the right".
  for (i in seq_along(rows)) {
    if (isTRUE(rows[[i]]$value_row) || isTRUE(rows[[i]]$code_row)) {
      # Value / option codes use the body font (NOT the monospace face):
      # the monospace face is reserved for the variable name and the raw
      # LimeSurvey filter expression -- the tokens a user copies verbatim.
      # Answer codes are short data values, read fine in the body font,
      # and keeping them out of monospace lightens the column.
      ft <- flextable::font(ft, i = i, j = "Label",
                            fontname = theme$font_body, part = "body")
      ft <- flextable::align(ft, i = i, j = "Label",
                             align = "right", part = "body")
      # Codes / values are a secondary reference, not bold: the row-type
      # labels (Question, Value, Options) keep the emphasis, the codes
      # read in a regular weight so the column stays light.
      ft <- flextable::bold(ft, i = i, j = "Label", bold = FALSE,
                            part = "body")
    }
  }
  # Label column width: a single fixed value, identical for every item
  # table and every chrome language. It is intentionally NOT derived per
  # language: the meta band above uses fixed column widths too, and a
  # width that changed with the chrome language would be unprincipled and
  # fragile (it would rest on per-character font estimates that can wrap
  # a long label). 1.0 in is the floor at the 9 pt bold label font -- it
  # fits the longest row label across all supported languages (French
  # "Sous-question", Spanish "Opciones (11)") on one line. The language
  # columns split the remainder.
  label_w <- 1.0
  total_w <- theme$content_width_in
  lang_w <- (total_w - label_w) / length(langs)
  ft <- flextable::width(ft, j = "Label", width = label_w, unit = "in")
  for (lg in langs) {
    ft <- flextable::width(ft, j = lg, width = lang_w, unit = "in")
  }
  # Section-header rows share the header's light-blue band background so
  # the Language / Value bands are visually consistent.
  for (i in seq_along(rows)) {
    if (isTRUE(rows[[i]]$section_header)) {
      ft <- flextable::bg(ft, i = i, bg = theme$color_band, part = "body")
    }
  }
  # Chrome-note rows (the coding descriptor, the exclusive note) carry
  # the same chrome-language sentence in every language column -- it is
  # not a translation but a structural annotation. Merge the language
  # columns for those rows so the note appears once, spanning the
  # content width, instead of being repeated per language.
  if (length(langs) > 1L) {
    for (i in seq_along(rows)) {
      if (isTRUE(rows[[i]]$span_note)) {
        ft <- flextable::merge_at(ft, i = i, j = langs, part = "body")
      }
    }
  }
  flextable::body_add_flextable(doc, ft, align = "left")
}

#' Build a single-row "Value" section header carrying the response
#' format descriptor for question types that have no enumerated answer
#' table.
#'
#' Every variable in the dataset has a domain of valid responses; the
#' Value section of the item table is where reviewers expect to find
#' it. For enumerated types (L, !, F, 1) the rows come from
#' [lss_answer_rows()]; for the rest we emit a single section-header
#' row that holds the descriptor in the language columns (with the
#' band tone background, matching the enumerated case visually).
#' Returns `NULL` only for X (boilerplate/display-only), which stores
#' no response.
#'
#' @keywords internal
#' @noRd
lss_value_implicit_row <- function(q, langs, theme) {
  chrome <- theme$chrome
  text <- switch(
    q$type,
    # Multi-choice subquestions: each subq is a binary Y/blank flag.
    "M" = chrome$value_multi_y_blank,
    "P" = chrome$value_multi_y_blank_with_comment,
    # Pre-defined enumerated types with implicit (not stored) codes.
    "Y" = chrome$value_yes_no,
    "G" = chrome$value_gender,
    "5" = chrome$value_5point,
    # Numeric inputs. K shares N's descriptor; the multi-variable
    # fan-out is conveyed by the `Type` cell and the `parent_subq`
    # variable code, not by a parenthetical on Value.
    "N" = chrome$value_numeric_input,
    "K" = chrome$value_numeric_input,
    # Free-text inputs. S is single-line (no line breaks in the data);
    # T and U are both multi-line (U is only a larger textarea), so they
    # share the multi-line descriptor.
    "S" = chrome$value_free_text_short,
    "T" = chrome$value_free_text,
    "U" = chrome$value_free_text,
    "Q" = chrome$value_free_text_short,
    # Array (Texts): one free-text cell per subquestion (multi-line
    # textarea like T). Array (Numbers): one numeric cell per
    # subquestion. The structural fan-out is in the Type cell.
    ";" = chrome$value_free_text,
    ":" = chrome$value_numeric_input,
    # Date / time picker.
    "D" = chrome$value_date_input,
    # Equation: server-computed value, not respondent-entered.
    "*" = chrome$value_computed,
    # Ranking: respondent orders the subquestions.
    "R" = chrome$value_ranking,
    # File upload.
    "|" = chrome$value_file_upload,
    # Anything else (including X = boilerplate / display-only) gets no
    # Value section, since the variable carries no response in the data.
    NULL
  )
  if (is.null(text)) return(NULL)
  list(
    label = chrome$item_value,
    texts = stats::setNames(rep(list(text), length(langs)), langs),
    size = theme$size_answer,
    section_header = TRUE,
    section_with_text = TRUE,
    # The descriptor is the same chrome sentence in every language, so
    # the renderer spans it once across the language columns.
    span_note = TRUE
  )
}

#' Code/label pairs for the labelled predefined scales LimeSurvey builds
#' in (and therefore does not store in the `.lss`)
#'
#' Yes/No/Uncertain (C), Increase/Same/Decrease (E), Yes/No (Y) and
#' gender (G) carry fixed, localizable labels. Returns a list of
#' `c(code, chrome_key)` pairs (the chrome key resolves to the localized
#' label via `theme$chrome[[key]]`), or `NULL` for any other type. Shared
#' by the cards Value rows ([lss_predefined_value_rows()]) and the table
#' questionnaire so both document the same mapping.
#'
#' @keywords internal
#' @noRd
lss_predefined_labelled <- function(type) {
  # `EXPR = type` is explicit so the "E" case label is not seen as a
  # partial match for switch()'s `EXPR` formal (R CMD check NOTE).
  switch(
    EXPR = type,
    "C" = list(c("Y", "value_array_yes"), c("U", "value_array_uncertain"),
               c("N", "value_array_no")),
    "E" = list(c("I", "value_array_increase"), c("S", "value_array_same"),
               c("D", "value_array_decrease")),
    "Y" = list(c("Y", "value_array_yes"), c("N", "value_array_no")),
    "G" = list(c("M", "value_gender_male"), c("F", "value_gender_female")),
    NULL
  )
}

#' Value rows for question types whose response scale is predefined by
#' LimeSurvey and therefore NOT stored in the `.lss`
#'
#' Some array / choice types carry a fixed scale that LimeSurvey builds
#' in (so `q$answers` is empty): Yes/No/Uncertain (C), Increase/Same/
#' Decrease (E), Yes/No (Y), and the 5- and 10-point arrays (A, B) and
#' the 5-point single choice (5). For the labelled scales we list each
#' code with its localized label, like a stored answer scale. For the
#' numeric N-point scales LimeSurvey stores no labels at all, so we put
#' a "%d-point scale (1 to %d)" descriptor on the Value header and list
#' the bare points 1..N (their language cells are intentionally blank).
#' Returns `NULL` for any other type, so the caller falls back to the
#' implicit single-line descriptor.
#'
#' @keywords internal
#' @noRd
lss_predefined_value_rows <- function(q, langs, theme) {
  chrome <- theme$chrome
  blank <- stats::setNames(as.list(rep("", length(langs))), langs)
  header <- function(text = NULL) {
    if (is.null(text)) {
      list(label = chrome$item_value, texts = blank, size = theme$size_meta,
           section_header = TRUE)
    } else {
      list(label = chrome$item_value,
           texts = stats::setNames(rep(list(text), length(langs)), langs),
           size = theme$size_meta, section_header = TRUE,
           section_with_text = TRUE, span_note = TRUE)
    }
  }
  value_row <- function(code, label) {
    list(label = code,
         texts = stats::setNames(rep(list(label), length(langs)), langs),
         size = theme$size_answer, value_row = TRUE)
  }

  # Labelled predefined scales: list each code with its localized label.
  labelled <- lss_predefined_labelled(q$type)
  if (!is.null(labelled)) {
    rows <- list(header())
    for (cl in labelled) {
      rows[[length(rows) + 1L]] <- value_row(cl[1], chrome[[cl[2]]])
    }
    return(rows)
  }

  # Numeric N-point scales (no stored labels): descriptor on the header,
  # then the bare points 1..N with blank language cells.
  npoint <- switch(q$type, "5" = 5L, "A" = 5L, "B" = 10L, NA_integer_)
  if (!is.na(npoint)) {
    rows <- list(header(sprintf(chrome$value_npoint_fmt, npoint, npoint)))
    for (i in seq_len(npoint)) {
      rows[[length(rows) + 1L]] <- list(
        label = as.character(i), texts = blank,
        size = theme$size_answer, value_row = TRUE, blank = TRUE
      )
    }
    return(rows)
  }
  NULL
}

#' First non-empty value of a question-level attribute
#'
#' `answer_order` / `subquestion_order` are stored without a language
#' (one global value per question), so this returns the first non-blank
#' `value` across all rows for `attr_name`, or `NULL` when absent.
#'
#' @keywords internal
#' @noRd
lss_question_attr_value <- function(q, attr_name) {
  if (is.null(q$attributes) || nrow(q$attributes) == 0L) return(NULL)
  hit <- q$attributes[q$attributes$attribute == attr_name, , drop = FALSE]
  if (nrow(hit) == 0L) return(NULL)
  v <- hit$value[!is.na(hit$value) & nzchar(trimws(hit$value))]
  if (length(v) == 0L) return(NULL)
  v[1]
}

#' Localized "Labels shown in random / alphabetical order" note for a
#' randomized or alphabetized answer / option order
#'
#' Reads the `answer_order` (leaf single choice) or `subquestion_order`
#' (multiple choice) attribute and maps it to a chrome note. Only
#' non-default orders produce a note: `normal` (and any unknown value)
#' returns `NULL`, so the document stays quiet about the default. The
#' combined `random_alphabetical` mode (randomize, then keep alphabetical
#' as fallback) is reported as random -- the randomization is what a
#' reviewer needs to know about.
#'
#' @keywords internal
#' @noRd
lss_answer_order_note <- function(q, theme, attr = "answer_order") {
  val <- lss_question_attr_value(q, attr)
  if (is.null(val)) return(NULL)
  switch(
    tolower(trimws(val)),
    "random"              = theme$chrome$value_order_random,
    "random_alphabetical" = theme$chrome$value_order_random,
    "alphabetical"        = theme$chrome$value_order_alphabetical,
    NULL
  )
}

#' Attach the answer-order note to a block of Value rows
#'
#' Places the note on the "Value" section header, in the language-column
#' area (spanning the columns as a single annotation, like the other
#' chrome notes). When that header already carries an implicit-format
#' descriptor (e.g. "Numeric input"), the note is appended to it; when
#' the header is otherwise blank (enumerated or labelled scales), the
#' note becomes its text, rendered muted and italic. Returns the rows
#' unchanged when there is no note or no section header to attach to.
#'
#' @keywords internal
#' @noRd
lss_apply_order_note <- function(value_rows, note, langs, theme) {
  if (is.null(note) || length(value_rows) == 0L) return(value_rows)
  idx <- NULL
  for (i in seq_along(value_rows)) {
    if (isTRUE(value_rows[[i]]$section_header)) { idx <- i; break }
  }
  if (is.null(idx)) return(value_rows)
  row <- value_rows[[idx]]
  if (isTRUE(row$section_with_text)) {
    # Append to the existing descriptor, keeping its styling.
    row$texts <- stats::setNames(lapply(langs, function(lg) {
      base <- row$texts[[lg]]
      if (is.null(base) || !nzchar(trimws(base))) note else paste0(base, " ", note)
    }), langs)
  } else {
    row$texts <- stats::setNames(rep(list(note), length(langs)), langs)
    row$section_with_text <- TRUE
    row$span_note <- TRUE
    row$color <- theme$color_muted
    row$italic <- TRUE
  }
  value_rows[[idx]] <- row
  value_rows
}

#' Insert a small vertical spacer before each item so consecutive
#' meta tables do not touch.
#'
#' Carries `keep_with_next` so the spacer stays anchored to its own
#' meta table at the top of the next item, which in turn keeps with
#' the gap and the item table (full meta -> gap -> item chain on
#' a single page).
#' @keywords internal
#' @noRd
lss_render_item_spacer <- function(doc, theme) {
  officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(" ", prop = officer::fp_text(
        font.family = theme$font_body,
        font.size = theme$size_meta
      )),
      fp_p = officer::fp_par(padding.top = 14, padding.bottom = 0,
                             keep_with_next = TRUE)
    )
  )
}

#' Thin breathing space between the meta table and the item table
#' so they read as two separate panels rather than one continuous block.
#' Carries `keep_with_next` so Word does not break a page between the
#' meta table and the item table that follows: the dark band stays
#' anchored to its content.
#' @keywords internal
#' @noRd
lss_render_intra_item_gap <- function(doc, theme) {
  officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(" ", prop = officer::fp_text(
        font.family = theme$font_body,
        font.size = 4
      )),
      fp_p = officer::fp_par(keep_with_next = TRUE)
    )
  )
}

#' Render a one-row flextable with one column per language
#'
#' Generic helper used everywhere a snippet of text needs to be shown side
#' by side per language (stems, descriptions, help, subquestion items).
#'
#' @keywords internal
#' @noRd
lss_render_lang_block <- function(doc, texts_by_lang, langs, theme,
                                  size = theme$size_question,
                                  color = theme$color_text,
                                  italic = FALSE,
                                  show_header = FALSE) {
  df <- as.data.frame(
    matrix("", nrow = 1L, ncol = length(langs)),
    stringsAsFactors = FALSE
  )
  names(df) <- langs
  ft <- flextable::flextable(df)
  if (isTRUE(show_header)) {
    ft <- flextable::set_header_labels(
      ft, values = stats::setNames(lss_language_label(langs), langs)
    )
  } else {
    ft <- flextable::delete_part(ft, part = "header")
  }
  for (lg in langs) {
    ft <- flextable::compose(
      ft, i = 1L, j = lg,
      value = lss_compose(
        texts_by_lang[[lg]], theme,
        size = size, color = color, italic_default = italic
      )
    )
  }
  ft <- lss_table_polish(ft, theme, lang_cols = langs)
  flextable::body_add_flextable(doc, ft, align = "left")
}

#' Build item-table rows for the requested question attributes
#'
#' Each rendered attribute becomes one labelled row inside the item
#' table (Prefix, Suffix, Validation, ...), in italic muted gray so it
#' stays visually distinct from the question text and the answer
#' values. Attributes that are empty in every language are skipped.
#' `other_replace_text` is omitted because it is documented as its own
#' numbered item via `lss_render_other_item()`.
#'
#' @keywords internal
#' @noRd
lss_attr_rows <- function(q, langs, theme, show_attrs) {
  if (length(show_attrs) == 0L || is.null(q$attributes)) {
    return(list())
  }
  attrs <- q$attributes
  rows <- list()
  for (attr_name in setdiff(show_attrs, "other_replace_text")) {
    hit <- attrs$attribute == attr_name
    if (!any(hit)) next
    matches <- attrs[hit, , drop = FALSE]
    per_lang <- vapply(langs, function(lg) {
      lang_hit <- matches$value[matches$language == lg]
      if (length(lang_hit) > 0L && nzchar(trimws(lang_hit[1]))) return(lang_hit[1])
      empty_lang <- matches$value[!nzchar(matches$language)]
      if (length(empty_lang) > 0L && nzchar(trimws(empty_lang[1]))) return(empty_lang[1])
      ""
    }, character(1))
    if (!lss_any_present(as.list(per_lang))) next

    # Skip technical attributes when they hold their default/inactive
    # value (so a reviewer never sees a noisy row like
    # `Exclude_all_others_auto = 0` that documents the absence of a
    # behavior). For attributes that ARE active, rewrite the value
    # into a sentence a methodologist can act on.
    fmt <- lss_format_attr(attr_name, per_lang, langs)
    if (is.null(fmt)) next

    rows[[length(rows) + 1L]] <- list(
      label = fmt$label,
      texts = fmt$texts,
      size = theme$size_meta,
      color = theme$color_muted,
      italic = TRUE
    )
  }
  rows
}

#' Format a question/subquestion attribute for display in the item table
#'
#' Returns a `list(label, texts)` ready to be wrapped into an
#' attribute row, or `NULL` when the attribute should be hidden.
#'
#' `exclude_all_others` and `exclude_all_others_auto` are intentionally
#' suppressed here. They live on the parent question of a compound
#' multi-choice question, and surfacing them through the generic
#' attribute loop repeats the same exclusion notice on every
#' subquestion. They are handled specially in `lss_render_subq_item()`
#' where the renderer knows the current subquestion code and can
#' target the message at the right row only.
#'
#' All other attributes pass through with a Title-Case label and the
#' raw per-language value.
#'
#' @keywords internal
#' @noRd
lss_format_attr <- function(attr_name, per_lang, langs) {
  if (attr_name %in% c("exclude_all_others", "exclude_all_others_auto")) {
    return(NULL)
  }
  list(
    label = tools::toTitleCase(attr_name),
    texts = stats::setNames(as.list(per_lang), langs)
  )
}

#' If the parent question of a compound multi-choice question declares
#' an `exclude_all_others` attribute, emit an "Exclusive" row only on
#' the subquestion(s) whose code is named in the attribute value
#'
#' LimeSurvey stores `exclude_all_others` on the parent `qid` with the
#' value being one (or several, comma-separated) subquestion titles
#' that, when checked, clear every other selection. We use the
#' subquestion code to decide whether THIS subquestion is the
#' exclusive one, and only then render a single italic row that names
#' the parent variable so a reviewer knows what gets cleared.
#'
#' Returns `NULL` (i.e. no row) when the attribute is absent or when
#' the current subquestion is not in the exclusion list.
#'
#' @keywords internal
#' @noRd
#' Extract the exclusive subquestion codes of a question
#'
#' Returns the subquestion codes listed in the parent's
#' `exclude_all_others` attribute (a comma-separated list), or an empty
#' character vector when the question has no exclusive option. Shared by
#' the per-subquestion path ([lss_exclusive_row()]) and the grouped
#' multiple-choice card.
#'
#' @keywords internal
#' @noRd
lss_exclusive_codes <- function(q) {
  if (is.null(q$attributes) || nrow(q$attributes) == 0L) return(character(0))
  hit <- q$attributes[q$attributes$attribute == "exclude_all_others", , drop = FALSE]
  if (nrow(hit) == 0L) return(character(0))
  raw <- trimws(as.character(hit$value[1L]))
  if (!nzchar(raw)) return(character(0))
  codes <- trimws(strsplit(raw, ",", fixed = TRUE)[[1L]])
  codes[nzchar(codes)]
}

lss_exclusive_row <- function(q, sq, langs, theme) {
  targets <- lss_exclusive_codes(q)
  if (!(sq$code %in% targets)) return(NULL)
  text <- sprintf(theme$chrome$exclusive_text_fmt, q$code)
  list(
    label = theme$chrome$item_exclusive,
    texts = stats::setNames(rep(list(text), length(langs)), langs),
    size = theme$size_meta,
    color = theme$color_muted,
    italic = TRUE,
    span_note = TRUE
  )
}

