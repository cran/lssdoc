#' Write a survey specification to an importable `.lss` file
#'
#' `r lifecycle::badge("experimental")`
#'
#' \strong{Experimental.} Turn an [lss_spec()] specification into a
#' LimeSurvey structure file (`.lss`) that imports directly through
#' *Create survey -> Import*. The output targets LimeSurvey 6
#' (DBVersion 700). The emitted file can be
#' read back with [read_lss()], checked with [audit_lss()] and rendered
#' with [render_questionnaire()] -- so the document reviewers read is
#' produced from the very file LimeSurvey receives.
#'
#' @param spec An [lss_spec()] object, or a plain list with the same
#'   structure (it is then validated through `lss_spec()` first).
#' @param file Character. Path of the `.lss` file to write.
#' @param sid Integer. Survey id embedded in the file. LimeSurvey assigns
#'   a fresh id on import when this one is taken, so the value rarely
#'   matters.
#' @param settings Named list of `surveys`-table fields overriding the
#'   built-in defaults (e.g. `list(anonymized = "Y", showprogress = "Y")`).
#'   The defaults ship with the package and come from a real LimeSurvey 6
#'   export, scrubbed -- see `lss_default_surveys_fields` in the sources
#'   for the rationale.
#'
#' @return Invisibly, the path to the written file.
#'
#' @details
#' Mapping choices, each validated against real LimeSurvey 6 imports:
#'
#' * Each kind maps to a LimeSurvey type and theme attested by a corpus
#'   of real exports (see `lss_kind_map` in the sources). Options of
#'   single-choice lists, rankings and array columns are emitted as
#'   `answers`; options of multiple-choice questions, item batteries and
#'   array rows as `subquestions`; scalar kinds and implicit scales
#'   (yes/no, gender, five-point, 5/10-point arrays) emit none.
#' * The native `other` option is emitted as `other = "Y"` plus the
#'   localized attribute `other_replace_text`. Localized attributes MUST
#'   carry the language code: emitted without one, LimeSurvey silently
#'   ignores them and shows its default wording. Global attributes
#'   (`exclude_all_others`, `max_answers`, ...) stay language-less.
#' * `other_position` / `other_position_code` control where the other
#'   option is displayed; `exclude_all_others` accepts several codes
#'   separated by `;`.
#' * Relevance equations are translated from the minimal syntax of
#'   [lss_spec()] into ExpressionScript (`code.NAOK == "1"`).
#' * Quotas are emitted with limit zero and the terminate action.
#' * A mandatory or capped ranking also receives `min_answers = 1`,
#'   overridable through the question's `attributes`.
#'
#' @section Languages:
#' The spec model is multilingual ([lss_spec()] accepts `languages` and
#' per-language texts); the emitter is not yet. This version writes the
#' primary language -- `languages[1]` -- only, exactly as it did when a
#' spec could hold a single language. A spec declaring more than one
#' language raises a classed error (`lssdoc_unsupported_multilang`) rather
#' than silently dropping the translations; multi-language emission is
#' planned for 0.3.0.
#'
#' @examples
#' spec <- lss_spec(
#'   title = "Demo",
#'   groups = list(list(title = "G", questions = list(
#'     list(code = "q1", kind = "single", text = "Oui ou non ?",
#'          options = list(list(text = "Oui"), list(text = "Non")))
#'   )))
#' )
#' out <- tempfile(fileext = ".lss")
#' write_lss(spec, out)
#' audit_lss(out)
#' @seealso [lss_spec()], [read_lss()], [audit_lss()],
#'   [render_questionnaire()].
#' @export
write_lss <- function(spec, file, sid = 100001L, settings = list()) {
  if (!inherits(spec, "lss_spec")) {
    if (!is.list(spec)) {
      lssdoc_abort("{.arg spec} must be an {.fn lss_spec} object or a list.",
                   class = "lssdoc_bad_spec")
    }
    spec <- lss_spec(
      title = spec$title, groups = spec$groups,
      languages = spec$languages %||% spec$language %||% "fr",
      welcome = spec$welcome, end_text = spec$end_text,
      quotas = spec$quotas
    )
  }
  if (!is.character(file) || length(file) != 1L) {
    lssdoc_abort("{.arg file} must be a single file path.",
                 class = "lssdoc_bad_path")
  }
  if (!is.numeric(sid) || length(sid) != 1L || !is.finite(sid) ||
      sid != trunc(sid) || sid < 1) {
    lssdoc_abort("{.arg sid} must be a single positive whole number.",
                 class = "lssdoc_bad_sid")
  }
  sid <- as.integer(sid)
  # an lss_spec object may have been mutated after construction: the
  # revalidation is cheap and prevents emitting a silently wrong file
  spec_validate(spec)

  # The spec model is multilingual; the emitter is not yet. This is the one
  # place where the limitation is enforced, so the rest of the package can
  # already be written against per-language texts.
  languages <- spec$languages %||% spec$language
  if (length(languages) > 1L) {
    primary <- languages[[1L]]
    lssdoc_abort(
      c("Multi-language emission is not supported in this version of lssdoc.",
        "x" = "The spec declares {.val {languages}}.",
        "i" = "{.fn write_lss} emits the primary language ({.val {primary}}) only; multi-language emission is planned for 0.3.0.",
        "i" = "Write one file per language, or declare a single language in {.arg languages}."),
      class = "lssdoc_unsupported_multilang"
    )
  }

  emit <- lss_emitter(spec, sid, settings)
  xml2::write_xml(emit$doc, file, options = c("format", "no_declaration"))
  # LimeSurvey expects an explicit XML declaration; write_xml() with
  # "no_declaration" plus a manual prepend reproduces the exact framing
  # validated by real imports.
  txt <- readLines(file, warn = FALSE, encoding = "UTF-8")
  writeLines(c('<?xml version="1.0" encoding="UTF-8"?>', txt), file,
             useBytes = TRUE)

  cli::cli_alert_success(
    "Wrote {.path {file}} ({emit$n_questions} question{?s}, {emit$n_groups} group{?s}, {length(spec$quotas)} quota{?s})."
  )
  invisible(file)
}

# ---- relevance translation -------------------------------------------------

#' Translate the minimal condition syntax into ExpressionScript
#'
#' The forms are validated upstream by `spec_validate()`; here we only
#' translate. `autre` maps to LimeSurvey's native other code `-oth-`.
#' For `count()`, the checkbox fields of a multiple-choice question are
#' `code_1.NAOK`, `code_2.NAOK`, ... -- the other option lives in
#' `code_other` and is not counted.
#' @keywords internal
#' @noRd
translate_relevance <- function(expr, defined) {
  expr <- trimws(expr %||% "")
  if (!nzchar(expr)) return("1")
  ls_code <- function(x) ifelse(tolower(trimws(x)) == "autre", "-oth-", trimws(x))

  m <- regmatches(expr, regexec(
    "^count\\(([A-Za-z][A-Za-z0-9]*)\\)\\s*(>=|>|==|<=|<)\\s*([0-9]+)$", expr))[[1]]
  if (length(m) == 4L) {
    codes <- option_codes(defined[[m[2]]]$options)
    fields <- paste0(m[2], "_", codes, ".NAOK")
    return(sprintf("count(%s) %s %s", paste(fields, collapse = ", "), m[3], m[4]))
  }
  m <- regmatches(expr, regexec(
    "^([A-Za-z][A-Za-z0-9]*)\\s+in\\s+\\[([^]]+)\\]$", expr))[[1]]
  if (length(m) == 3L) {
    codes <- ls_code(strsplit(m[3], ",")[[1]])
    return(paste0("(", paste(sprintf('%s.NAOK == "%s"', m[2], codes),
                             collapse = " or "), ")"))
  }
  m <- regmatches(expr, regexec(
    "^([A-Za-z][A-Za-z0-9]*)\\s*=\\s*([A-Za-z0-9-]+)$", expr))[[1]]
  if (length(m) != 3L) {
    lssdoc_abort(
      "Internal error: unvalidated relevance expression reached the emitter.",
      class = "lssdoc_bad_spec"
    )
  }
  sprintf('%s.NAOK == "%s"', m[2], ls_code(m[3]))
}

# ---- emission --------------------------------------------------------------

# One entry per supported kind. Every mapping below is attested by the
# reference corpus of real exports (where each type's options live --
# answers, subquestions, or neither -- and its LimeSurvey 6 theme name).
# Types needing an unproven mechanism (dual-scale subquestions for array
# texts/numbers, unattested LS6 theme names) are deliberately absent.
lss_kind_map <- list(
  single        = list(type = "L", theme = "listradio"),
  dropdown      = list(type = "!", theme = "list_dropdown"),
  singlecomment = list(type = "O", theme = "list_with_comment"),
  multiple      = list(type = "M", theme = "multiplechoice"),
  array         = list(type = "F", theme = "arrays/array"),
  array5        = list(type = "A", theme = "arrays/5point"),
  array10       = list(type = "B", theme = "arrays/10point"),
  arrayyesno    = list(type = "C", theme = "arrays/yesnouncertain"),
  arraytrend    = list(type = "E", theme = "arrays/increasesamedecrease"),
  ranking       = list(type = "R", theme = "ranking"),
  multitext     = list(type = "Q", theme = "multipleshorttext"),
  multinumeric  = list(type = "K", theme = "multiplenumeric"),
  text          = list(type = "T", theme = "longfreetext"),
  shorttext     = list(type = "S", theme = "shortfreetext"),
  hugetext      = list(type = "U", theme = "hugefreetext"),
  numeric       = list(type = "N", theme = "numerical"),
  date          = list(type = "D", theme = "date"),
  yesno         = list(type = "Y", theme = "yesno"),
  gender        = list(type = "G", theme = "gender"),
  fivepoint     = list(type = "5", theme = "5pointchoice"),
  display       = list(type = "X", theme = "boilerplate")
)

#' Build the XML document for a validated spec
#' @keywords internal
#' @noRd
lss_emitter <- function(spec, sid, settings) {
  lang <- spec$language
  st <- new.env(parent = emptyenv())
  st$answers <- list(); st$answer_l10ns <- list()
  st$groups <- list(); st$group_l10ns <- list()
  st$questions <- list(); st$subquestions <- list()
  st$question_l10ns <- list(); st$qattrs <- list()
  st$gid <- sid * 10L; st$qid <- sid * 100L
  st$aid <- sid * 100L; st$lid <- 1L

  defined <- list()
  qid_of <- list()

  for (gi in seq_along(spec$groups)) {
    g <- spec$groups[[gi]]
    st$gid <- st$gid + 1L
    st$groups[[length(st$groups) + 1L]] <- list(
      gid = st$gid, sid = sid, group_order = gi,
      randomization_group = "", grelevance = "1")
    st$group_l10ns[[length(st$group_l10ns) + 1L]] <- list(
      id = st$lid, gid = st$gid, group_name = loc_text(g$title, lang),
      description = "",
      language = lang, sid = sid, group_order = gi,
      randomization_group = "", grelevance = "1")
    st$lid <- st$lid + 1L

    for (qi in seq_along(g$questions)) {
      q <- g$questions[[qi]]
      map <- lss_kind_map[[q$kind]]
      st$qid <- st$qid + 1L
      parent <- st$qid
      qid_of[[q$code]] <- parent

      other_opt <- Filter(function(o) isTRUE(o$other), q$options %||% list())
      opts <- Filter(function(o) !isTRUE(o$other), q$options %||% list())

      st$questions[[length(st$questions) + 1L]] <- list(
        qid = parent, parent_qid = 0L, sid = sid, gid = st$gid,
        type = map$type, title = q$code, preg = "",
        other = if (length(other_opt)) "Y" else "N",
        mandatory = if (isTRUE(q$mandatory)) "Y" else "N",
        question_order = qi, scale_id = 0L, same_default = 0L,
        relevance = translate_relevance(q$relevance, defined),
        modulename = "", encrypted = "N",
        question_theme_name = map$theme, same_script = 0L)
      st$question_l10ns[[length(st$question_l10ns) + 1L]] <- list(
        id = st$lid, qid = parent, question = loc_text(q$text, lang),
        help = loc_text(q$help, lang), language = lang, script = "")
      st$lid <- st$lid + 1L

      add_answers <- function(items) {
        for (k in seq_along(items)) {
          it <- items[[k]]
          st$aid <- st$aid + 1L
          st$answers[[length(st$answers) + 1L]] <- list(
            aid = st$aid, qid = parent, code = it$code,
            sortorder = k - 1L, assessment_value = 0L, scale_id = 0L)
          st$answer_l10ns[[length(st$answer_l10ns) + 1L]] <- list(
            id = st$lid, aid = st$aid, answer = loc_text(it$text, lang),
            language = lang)
          st$lid <- st$lid + 1L
        }
      }
      add_subquestions <- function(items) {
        for (k in seq_along(items)) {
          it <- items[[k]]
          st$qid <- st$qid + 1L
          st$subquestions[[length(st$subquestions) + 1L]] <- list(
            qid = st$qid, parent_qid = parent, sid = sid, gid = st$gid,
            type = map$type, title = it$code, preg = "", other = "N",
            mandatory = "N", question_order = k, scale_id = 0L,
            same_default = 0L, relevance = "1", modulename = "",
            encrypted = "N", question_theme_name = "", same_script = 0L)
          st$question_l10ns[[length(st$question_l10ns) + 1L]] <- list(
            id = st$lid, qid = st$qid, question = loc_text(it$text, lang),
            help = "", language = lang, script = "")
          st$lid <- st$lid + 1L
        }
      }

      switch(q$kind,
        single        = add_answers(opts),
        dropdown      = add_answers(opts),
        singlecomment = add_answers(opts),
        ranking       = add_answers(opts),
        multiple      = add_subquestions(opts),
        multitext     = add_subquestions(opts),
        multinumeric  = add_subquestions(opts),
        array         = { add_subquestions(q$rows); add_answers(q$columns) },
        array5        = add_subquestions(q$rows),
        array10       = add_subquestions(q$rows),
        arrayyesno    = add_subquestions(q$rows),
        arraytrend    = add_subquestions(q$rows)
      )

      attr_add <- function(name, value, language = "") {
        st$qattrs[[length(st$qattrs) + 1L]] <- list(
          qid = parent, attribute = name, value = value, language = language)
      }
      if (!is.null(q$max_answers)) attr_add("max_answers", q$max_answers)
      if (q$kind == "ranking" &&
          (isTRUE(q$mandatory) || !is.null(q$max_answers)) &&
          is.null((q$attributes %||% list())[["min_answers"]])) {
        attr_add("min_answers", 1L)
      }
      if (length(other_opt)) {
        # localized attribute: MUST carry the language code, or LimeSurvey
        # silently ignores it and shows its default "Other:" wording
        attr_add("other_replace_text", loc_text(other_opt[[1L]]$text, lang),
                 language = lang)
        if (!is.null(q$other_position)) {
          attr_add("other_position", q$other_position)
          if (identical(q$other_position, "specific")) {
            attr_add("other_position_code", q$other_position_code)
          }
        }
      }
      excl <- option_codes(Filter(function(o) isTRUE(o$exclusive), opts))
      if (q$kind == "multiple" && length(excl)) {
        attr_add("exclude_all_others", paste(excl, collapse = ";"))
      }
      for (nm in names(q$attributes %||% list())) {
        attr_add(nm, q$attributes[[nm]])
      }

      defined[[q$code]] <- q
    }
  }

  doc <- xml2::xml_new_root("document")
  xml2::xml_add_child(doc, "LimeSurveyDocType", "Survey")
  xml2::xml_add_child(doc, "DBVersion", LSS_DBVERSION)
  langs <- xml2::xml_add_child(doc, "languages")
  xml2::xml_add_child(langs, "language", lang)

  add_section(doc, "answers",
              c("aid", "qid", "code", "sortorder", "assessment_value", "scale_id"),
              st$answers)
  add_section(doc, "answer_l10ns", c("id", "aid", "answer", "language"),
              st$answer_l10ns)
  add_section(doc, "groups",
              c("gid", "sid", "group_order", "randomization_group", "grelevance"),
              st$groups)
  add_section(doc, "group_l10ns",
              c("id", "gid", "group_name", "description", "language", "sid",
                "group_order", "randomization_group", "grelevance"),
              st$group_l10ns)
  question_fields <- c(
    "qid", "parent_qid", "sid", "gid", "type", "title", "preg", "other",
    "mandatory", "question_order", "scale_id", "same_default", "relevance",
    "modulename", "encrypted", "question_theme_name", "same_script")
  add_section(doc, "questions", question_fields, st$questions)
  add_section(doc, "subquestions", question_fields, st$subquestions)
  add_section(doc, "question_l10ns",
              c("id", "qid", "question", "help", "language", "script"),
              st$question_l10ns)
  add_section(doc, "question_attributes",
              c("qid", "attribute", "value", "language"), st$qattrs)

  if (length(spec$quotas)) {
    quota <- list(); members <- list(); qls <- list()
    for (k in seq_along(spec$quotas)) {
      qu <- spec$quotas[[k]]
      quota_name <- loc_text(qu$name, lang, default = NULL) %||% qu$question
      quota[[k]] <- list(id = k, sid = sid, name = quota_name,
                         qlimit = 0L, action = 1L, active = 1L,
                         autoload_url = 0L)
      members[[k]] <- list(id = k, sid = sid, qid = qid_of[[qu$question]],
                           quota_id = k, code = qu$code)
      qls[[k]] <- list(quotals_id = k, quotals_quota_id = k,
                       quotals_language = lang,
                       quotals_name = quota_name,
                       quotals_message = loc_text(qu$message, lang),
                       quotals_url = "", quotals_urldescrip = "")
    }
    add_section(doc, "quota",
                c("id", "sid", "name", "qlimit", "action", "active", "autoload_url"),
                quota)
    add_section(doc, "quota_members",
                c("id", "sid", "qid", "quota_id", "code"), members)
    add_section(doc, "quota_languagesettings",
                c("quotals_id", "quotals_quota_id", "quotals_language",
                  "quotals_name", "quotals_message", "quotals_url",
                  "quotals_urldescrip"), qls)
  }

  # the defaults already carry `language` and `additional_languages`:
  # overriding them in place avoids emitting a duplicated field
  surveys_row <- lss_default_surveys_fields
  surveys_row[["language"]] <- lang
  surveys_row[["additional_languages"]] <- ""
  if (length(settings)) {
    if (is.null(names(settings)) || any(!nzchar(names(settings)))) {
      lssdoc_abort("Every {.arg settings} element must be named.",
                   class = "lssdoc_bad_settings")
    }
    reserved <- intersect(names(settings), c("language", "additional_languages"))
    if (length(reserved)) {
      lssdoc_abort(
        c("{.arg settings} cannot override {.val {reserved}}.",
          "i" = "The survey language comes from the spec; overriding it here would leave the localized sections in another language."),
        class = "lssdoc_bad_settings"
      )
    }
    unknown <- setdiff(names(settings), names(surveys_row))
    if (length(unknown)) {
      lssdoc_abort("Unknown {.arg settings} field{?s}: {.val {unknown}}.",
                   class = "lssdoc_bad_settings")
    }
    bad_value <- vapply(settings, function(v) {
      is.null(v) || length(v) != 1L || is.na(v) || !is.character(v)
    }, logical(1))
    if (any(bad_value)) {
      lssdoc_abort(
        c("Invalid {.arg settings} value{?s} for {.val {names(settings)[bad_value]}}.",
          "i" = "Each value must be a single character string, as LimeSurvey stores it (e.g. \"Y\", \"N\", \"I\")."),
        class = "lssdoc_bad_settings"
      )
    }
    surveys_row <- utils::modifyList(surveys_row, settings)
  }
  surveys_row <- c(list(sid = sid), surveys_row)
  add_section(doc, "surveys", names(surveys_row), list(surveys_row))

  ls_row <- c(
    list(surveyls_survey_id = sid, surveyls_language = lang,
         surveyls_title = loc_text(spec$title, lang),
         surveyls_welcometext = as_html_block(loc_text(spec$welcome, lang,
                                                       default = NULL)),
         surveyls_endtext = as_html_block(loc_text(spec$end_text, lang,
                                                   default = NULL))),
    lss_default_language_settings)
  add_section(doc, "surveys_languagesettings", names(ls_row), list(ls_row))

  list(doc = doc,
       n_groups = length(st$groups),
       n_questions = sum(vapply(st$questions,
                                function(q) q$type != "X", logical(1))))
}

#' Wrap plain paragraphs in <p> tags; pass HTML through verbatim
#' @keywords internal
#' @noRd
as_html_block <- function(x) {
  if (is.null(x) || !length(x)) return("")
  x <- as.character(x)
  if (length(x) == 1L && grepl("^\\s*<", x)) return(x)
  paste0("<p>", paste(x, collapse = "</p><p>"), "</p>")
}

#' Append one `<fields>/<rows>` section to the document
#' @keywords internal
#' @noRd
add_section <- function(doc, name, fields, rows) {
  sec <- xml2::xml_add_child(doc, name)
  fl <- xml2::xml_add_child(sec, "fields")
  for (f in fields) xml2::xml_add_child(fl, "fieldname", f)
  rs <- xml2::xml_add_child(sec, "rows")
  for (r in rows) {
    row <- xml2::xml_add_child(rs, "row")
    for (f in fields) {
      v <- r[[f]]
      xml2::xml_add_child(row, f, if (is.null(v)) "" else as.character(v))
    }
  }
  invisible(sec)
}
